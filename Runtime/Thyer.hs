{-# OPTIONS_GHC -funbox-strict-fields #-}

-- An implementation of a Thyer lazy specializer. From:
-- Lazy Specialization
-- by Michael Jonathan Thyer (1999).

-- Memoizing substitutions not implemented.

module Runtime.Thyer (eval) where

import qualified Depth
import qualified HOAS
import qualified IORefRef as Ref
import Control.Applicative
import Control.Monad ((<=<))

data Blocked
    = Blocked
    | Unblocked
    deriving (Eq)

type NodeRef a = Ref.Ref (Node a)

data Node a = Node {
    nodeBlocked :: !Blocked,
    nodeDepth   :: !Int,
    nodeData    :: !(NodeData a)
  }

data NodeData a
    = Lambda !(NodeRef a)
    | Apply  !(NodeRef a) !(NodeRef a)
    | Subst  !(NodeRef a) !Int !(NodeRef a) !Int   -- body var arg shift
    | Var
    | Prim   a

-- reduce reduces its argument to whnf *destructively*.  It returns the reduced 
-- node for convenience.  reduce x = reduce x >> Ref.read x.
reduce :: (HOAS.Primitive a) => NodeRef a -> IO (Node a)
reduce ref = do
    node <- Ref.read ref
    if nodeBlocked node == Blocked then return node else do
    case nodeData node of
        Apply f arg -> do
            fnode <- reduce f
            case nodeData fnode of
                Lambda body -> do
                    let bind = nodeDepth fnode + 1

                        -- shift is the amount by which the depths of f's nodes are expected
                        -- to change.
                        shift = nodeDepth node - bind

                        -- This is (nodeDepth fnode) instead of (nodeDepth node) in Thyer's
                        -- paper. It is somewhat irrelevant since we only check depths when we
                        -- are substituting through a node, and we never subsitute through a 
                        -- subst node, but I believe this makes more sense.
                        node' = Node Unblocked (nodeDepth node) (Subst body bind arg shift)
                    Ref.write ref node'
                    reduce ref
                Prim p -> do
                    argnode <- reduce arg
                    case nodeData argnode of
                        Prim p'   -> sideEffect (Ref.write ref) (Node Blocked 0 (Prim $ p `HOAS.apply` p'))
                        Apply {}  -> blocked
                        Var {}    -> blocked
                        Lambda {} -> fail "Can't apply primitive to lambda"
                        Subst {}  -> fail "Bug - reduced expression ended up a subst"
                _ -> blocked
        Subst body var arg shift -> do
            -- This is the code that has the specializing effect.  We *reduce*
            -- the body, including application nodes, before substituting into it.  
            -- A simple lazy evaluator would just push down the substitution through
            -- any type of node, including applications.  cf. Thyer p. 122.
            reduce body
            Ref.link ref =<< subst body var arg shift
            reduce ref
        _ -> blocked
    where
    blocked = do
        node <- Ref.read ref
        sideEffect (Ref.write ref) $! node { nodeBlocked = Blocked }

sideEffect :: (a -> IO ()) -> a -> IO a
sideEffect f x = f x >> return x

-- subst returns body with the variable at depth bind substituted for arg.  It
-- does not modify its arguments.
subst :: NodeRef a -> Int -> NodeRef a -> Int -> IO (NodeRef a)
subst body bind arg shift = do  
    bodynode <- Ref.read body
    -- if the depth of the body is less than the depth of the variable we are 
    -- substituting, the variable cannot possibly occur in the body, so just 
    -- dissolve away.
    if nodeDepth bodynode < bind then return body else do
    
    let newdepth = nodeDepth bodynode + shift
    case nodeData bodynode of
        Var | nodeDepth bodynode == bind -> return arg
            | otherwise                  -> Ref.new (Node Blocked newdepth Var)
        Lambda body -> do
            substbody <- Ref.new (Node Unblocked (newdepth+1) (Subst body bind arg shift))
            Ref.new (Node Unblocked newdepth (Lambda substbody))
        Apply f x -> do
            f' <- Ref.new (Node Unblocked newdepth (Subst f bind arg shift)) 
            x' <- Ref.new (Node Unblocked newdepth (Subst x bind arg shift))
            Ref.new (Node Unblocked newdepth (Apply f' x'))
        _ -> return body

fromDepth :: Depth.ExpNode a -> IO (NodeRef a)
fromDepth (d, n) = case n of
    Depth.Lambda body -> Ref.new . Node Unblocked d . Lambda =<< fromDepth body
    Depth.Apply f x   -> Ref.new =<< Node Unblocked d <$> liftA2 Apply (fromDepth f) (fromDepth x)
    Depth.Var         -> Ref.new (Node Blocked d Var)
    Depth.Prim x      -> Ref.new . Node Blocked d . Prim $ x

getValue :: (HOAS.Primitive a) => NodeRef a -> IO a
getValue ref = do
    refnode <- reduce ref
    case nodeData refnode of
        Prim x -> return x
        _ -> fail "Not a value"

eval :: (HOAS.Primitive a) => Depth.Depth a -> IO a
eval = getValue <=< fromDepth . Depth.getDepth

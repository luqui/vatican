module Thyer (eval) where

import qualified Depth
import qualified HOAS
import qualified IndirRef as Ref
import Control.Applicative
import Control.Monad ((<=<))

type NodeRef a = Ref.Ref (Node a)

data Node a = Node {
    nodeDepth :: Int,
    nodeData  :: NodeData a
  }

data NodeData a
    = Lambda (NodeRef a)
    | Apply  (NodeRef a) (NodeRef a)
    | Subst  (NodeRef a) Int (NodeRef a) Int   -- body var arg shift
    | Var
    | Prim   a

reduce :: (HOAS.Primitive a) => NodeRef a -> IO (Node a)
reduce ref = do
    node <- Ref.read ref
    case nodeData node of
        Apply f arg -> do
            fnode <- reduce f
            case nodeData fnode of
                Lambda body -> do
                    let bind = nodeDepth fnode + 1
                        shift = nodeDepth node - bind   -- ???
                        node' = Node (nodeDepth fnode) (Subst body bind arg shift)
                    Ref.write ref node'
                    reduce ref
                Prim p -> do
                    argnode <- reduce arg
                    case nodeData argnode of
                        Prim p'   -> do
                            let red = Node 0 (Prim (p `HOAS.apply` p'))
                            Ref.write ref red
                            return red
                        Apply {}  -> blocked
                        Var {}    -> blocked
                        Lambda {} -> fail "Can't apply primitive to lambda"
                        Subst {}  -> fail "Bug - reduced expression ended up a subst"
                _ -> blocked
        Subst body var arg shift -> do
            reduce body
            substituted <- subst body var arg shift
            Ref.link ref substituted
            reduce ref  -- huh...
        _ -> blocked
    where
    blocked = Ref.read ref

subst :: NodeRef a -> Int -> NodeRef a -> Int -> IO (NodeRef a)
subst body bind arg shift = do  
    bodynode <- Ref.read body
    let newdepth = nodeDepth bodynode + shift
    if nodeDepth bodynode < bind then return body else do
    copy <- case nodeData bodynode of
                Var | nodeDepth bodynode == bind -> return arg
                    | otherwise                  -> Ref.new (Node newdepth Var)
                Lambda body -> do
                    substbody <- Ref.new (Node (newdepth+1) (Subst body bind arg shift))
                    Ref.new (Node newdepth (Lambda substbody))
                Apply f x -> do
                    f' <- Ref.new (Node newdepth (Subst f bind arg shift)) 
                    x' <- Ref.new (Node newdepth (Subst x bind arg shift))
                    Ref.new (Node newdepth (Apply f' x'))
                _ -> return body
    return copy

fromDepth :: Depth.ExpNode a -> IO (NodeRef a)
fromDepth (d, n) = case n of
    Depth.Lambda body -> Ref.new . Node d . Lambda =<< fromDepth body
    Depth.Apply f x   -> Ref.new =<< Node d <$> liftA2 Apply (fromDepth f) (fromDepth x)
    Depth.Var         -> Ref.new (Node d Var)
    Depth.Prim x      -> Ref.new . Node d . Prim $ x

getValue :: (HOAS.Primitive a) => NodeRef a -> IO a
getValue ref = do
    refnode <- reduce ref
    case nodeData refnode of
        Prim x -> return x
        _ -> fail "Not a value"

eval :: (HOAS.Primitive a) => Depth.Depth a -> IO a
eval = getValue <=< fromDepth . Depth.getDepth

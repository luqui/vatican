{-# LANGUAGE RecursiveDo, PatternGuards #-}

module Vatican 
    ( Primitive(..)
    , Term, eval
    , (%), fun, prim, let_
    )
where

import Data.IORef
import Control.Monad (forM_, (<=<), when)
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import System.Process (system)
import Data.Maybe (fromJust, catMaybes)
import Data.List (delete)

class (Show a) => Primitive a where
    apply :: a -> a -> a

data UplinkType = UplinkAppL | UplinkAppR | UplinkLambda | UplinkVar
    deriving (Eq)

type Uplink a = (UplinkType, NodeRef a)

data NodeData a
    = AppNode (NodeRef a) (NodeRef a)
    | LambdaNode (NodeRef a) (NodeRef a)
    | VarNode
    | PrimNode a

data Node a = Node {
    nodeCache :: Maybe (NodeRef a),
    nodeUplinks :: [Uplink a],
    nodeData :: NodeData a
  }

type NodeRef a = IORef (Node a)


upcopy :: NodeRef a -> NodeRef a -> Uplink a -> IO ()
upcopy stop newchild (uplinkType, intoref) | intoref == stop = return ()
                                           | otherwise = do
    into <- readIORef intoref

    let traverse newnode = mapM_ (upcopy stop newnode) (nodeUplinks into)
    
    case nodeData into of
        AppNode left right -> do
            case nodeCache into of
                Nothing -> do
                    let dat' | UplinkAppL <- uplinkType = AppNode newchild right
                             | UplinkAppR <- uplinkType = AppNode left newchild
                    newnode <- newNodeRef dat'
                    setCache intoref (Just newnode)
                    traverse newnode
                Just cache -> do
                    case uplinkType of
                        UplinkAppL -> replaceLeft newchild cache
                        UplinkAppR -> replaceRight newchild cache
        LambdaNode var body -> do
            var' <- newNodeRef VarNode
            lambda' <- newNodeRef (LambdaNode var' newchild)
            setCache intoref (Just lambda')
            upcopy lambda' var' (UplinkVar, var)
            traverse lambda'
        VarNode -> do
            setCache intoref (Just newchild)
            traverse newchild

setCache :: NodeRef a -> Maybe (NodeRef a) -> IO ()
setCache ref newcache = modifyIORef ref (\n -> n { nodeCache = newcache })

newNodeRef :: NodeData a -> IO (NodeRef a)
newNodeRef dat = newIORef $ Node { nodeCache = Nothing, nodeUplinks = [], nodeData = dat }

replaceLeft :: NodeRef a -> NodeRef a -> IO ()
replaceLeft newchild node = modifyIORef node $ \n -> n { nodeData = go (nodeData n) }
    where
    go (AppNode l r) = AppNode newchild r

replaceRight :: NodeRef a -> NodeRef a -> IO ()
replaceRight newchild node = modifyIORef node $ \n -> n { nodeData = go (nodeData n) }
    where
    go (AppNode l r) = AppNode l newchild

replaceBody :: NodeRef a -> NodeRef a -> IO ()
replaceBody newchild node = modifyIORef node $ \n -> n { nodeData = go (nodeData n) }
    where
    go (LambdaNode v b) = LambdaNode v newchild

addUplink :: Uplink a -> NodeRef a -> IO ()
addUplink uplink node = modifyIORef node $ \n -> n { nodeUplinks = uplink : nodeUplinks n }

deleteUplink :: Uplink a -> NodeRef a -> IO ()
deleteUplink uplink node = modifyIORef node $ \n -> n { nodeUplinks = delete uplink (nodeUplinks n) }

getLeft :: NodeRef a -> IO (NodeRef a)
getLeft ref = (\(AppNode l r) -> l) . nodeData <$> readIORef ref

getRight :: NodeRef a -> IO (NodeRef a)
getRight ref = (\(AppNode l r) -> r) . nodeData <$> readIORef ref

getBody :: NodeRef a -> IO (NodeRef a)
getBody ref = (\(LambdaNode _ b) -> b) . nodeData <$> readIORef ref

clear :: NodeRef a -> IO ()
clear noderef = do
    node <- readIORef noderef
    forM_ (nodeUplinks node) $ \(uplinkType, uplinkRef) -> do
        upnode <- readIORef uplinkRef
        case nodeCache upnode of
            Nothing -> return ()
            Just cache -> do
                case nodeData upnode of
                    AppNode _ _ -> do
                        addUplink (UplinkAppL, cache) =<< getLeft cache
                        addUplink (UplinkAppR, cache) =<< getRight cache
                        setCache uplinkRef Nothing
                    LambdaNode var _ -> do
                        addUplink (UplinkLambda, cache) =<< getBody cache
                        setCache uplinkRef Nothing
                        clear var
                clear uplinkRef
    setCache noderef Nothing

cleanup :: NodeRef a -> IO ()
cleanup noderef = do
    node <- readIORef noderef
    when (null (nodeUplinks node)) $ case nodeData node of
        AppNode left right -> do
            deleteUplink (UplinkAppL, noderef) left
            cleanup left
            deleteUplink (UplinkAppR, noderef) right
            cleanup right
        LambdaNode var body -> do
            deleteUplink (UplinkLambda, noderef) body
            cleanup body
        _ -> return ()

upreplace :: NodeRef a -> Uplink a -> IO ()
upreplace newchild (uplinkType, intoref) = do
    into <- readIORef intoref
    case (nodeData into, uplinkType) of
        (AppNode left right, UplinkAppL) -> do
            deleteUplink (UplinkAppL, intoref) left
            replaceLeft newchild intoref
            addUplink (UplinkAppL, intoref) newchild
            cleanup left
        (AppNode left right, UplinkAppR) -> do
            deleteUplink (UplinkAppR, intoref) right
            replaceRight newchild intoref
            addUplink (UplinkAppR, intoref) newchild
            cleanup right
        (LambdaNode var body, UplinkLambda) -> do
            deleteUplink (UplinkLambda, intoref) body
            replaceBody newchild intoref
            addUplink (UplinkLambda, intoref) newchild
            cleanup body
            

betaReduce :: NodeRef a -> IO (NodeRef a)
betaReduce appref = do
    app <- readIORef appref
    let AppNode leftref rightref = nodeData app
    left <- readIORef leftref
    let LambdaNode varref bodyref = nodeData left
    var <- readIORef varref
    result <- case nodeUplinks var of
        [] -> return bodyref
        _ -> do
            upcopy leftref rightref (UplinkVar, varref)
            result <- fromJust . nodeCache <$> (readIORef =<< getBody leftref)
            setCache leftref Nothing
            clear varref
            return result
    mapM_ (upreplace result) =<< nodeUplinks <$> readIORef appref
    return result

hnfReduce :: (Primitive a) => NodeRef a -> IO ()
hnfReduce noderef = do
    node <- readIORef noderef
    case nodeData node of
        LambdaNode var body -> hnfReduce body 
        AppNode left right -> do
            hnfReduce left
            left' <- readIORef =<< getLeft noderef 
            case nodeData left' of
                LambdaNode {} -> hnfReduce =<< betaReduce noderef
                PrimNode p -> do
                    hnfReduce right
                    right' <- readIORef =<< getRight noderef
                    case nodeData right' of
                        PrimNode p' -> do
                            result <- newNodeRef $ PrimNode (p `apply` p')
                            mapM_ (upreplace result) =<< nodeUplinks <$> readIORef noderef
                        _ -> return ()
                _ -> return ()
        _ -> return ()

graphviz :: (Primitive a) => NodeRef a -> IO String
graphviz noderef_ = do
    output <- evalStateT (execWriterT (go noderef_)) ([], 0)
    return $ "digraph Lambda {\n" ++ output ++ "}\n"
    where
    go noderef = do
        cached <- lift $ gets (lookup noderef . fst)
        case cached of
            Just ident -> return ident
            Nothing -> do
                (seen, ident) <- lift get
                lift $ put ((noderef,ident):seen, ident+1)
                node <- liftIO $ readIORef noderef
                let color | noderef == noderef_ = "color=orange,style=filled"
                          | otherwise           = "colorblack"
                forM_ (nodeUplinks node) $ \(ty, uplink) -> do
                    uplinkid <- go uplink
                    tell $ "p" ++ show ident ++ " -> p" ++ show uplinkid ++ " [weight=1,color=red];\n"
                case nodeData node of
                    AppNode left right -> do
                        tell $ "p" ++ show ident ++ " [label=\"*\"," ++ color ++ "];\n"
                        leftid <- go left
                        tell $ "p" ++ show ident ++ " -> p" ++ show leftid ++ " [weight=1,color=\"#007f00\",label=\"fv\"];\n"
                        rightid <- go right
                        tell $ "p" ++ show ident ++ " -> p" ++ show rightid ++ " [weight=1,label=\"av\"];\n"
                    LambdaNode var body -> do
                        tell $ "p" ++ show ident ++ " [label=\"\\\\\"," ++ color ++ "];\n"
                        bodyid <- go body
                        tell $ "p" ++ show ident ++ " -> p" ++ show bodyid ++ " [weight=1];\n"
                        varid <- go var
                        tell $ "p" ++ show ident ++ " -> p" ++ show varid ++ " [weight=0,color=blue];\n"
                    VarNode -> do
                        tell $ "p" ++ show ident ++ " [label=\"x\"," ++ color ++ "];\n"
                    PrimNode x -> do
                        tell $ "p" ++ show ident ++ " [label=\"" ++ show x ++ "\"];\n"
                case nodeCache node of
                    Just cacher -> do
                        cacheid <- go cacher
                        tell $ "p" ++ show ident ++ " -> p" ++ show cacheid ++ " [weight=0,style=dotted];\n"
                    Nothing -> return ()
                return ident

runGraphviz :: (Primitive a) => NodeRef a -> IO ()
runGraphviz node = do
    writeFile "graph.dot" =<< graphviz node
    system "dot -T png -o graph.png graph.dot"
    system "eog graph.png"
    return ()

eval :: (Primitive a) => Term a -> IO a
eval t = do
    noderef <- getTerm $ fun (\z -> t)
    hnfReduce noderef
    dat <- nodeData <$> (readIORef =<< getBody noderef)
    case dat of
        PrimNode p -> return p
        _ -> fail "Not a primitive!"
    

newtype Term a = Term { getTerm :: IO (NodeRef a) }

infixl 9 %
(%) :: Term a -> Term a -> Term a
Term left % Term right = Term $ do
    left' <- left
    right' <- right
    newref <- newNodeRef $ AppNode left' right'
    addUplink (UplinkAppL, newref) left'
    addUplink (UplinkAppR, newref) right'
    return newref

fun :: (Term a -> Term a) -> Term a
fun bodyf = Term $ do
    var <- newNodeRef $ VarNode
    body <- getTerm . bodyf . Term $ return var
    newref <- newNodeRef $ LambdaNode var body
    addUplink (UplinkLambda, newref) body
    return newref

prim :: a -> Term a
prim x = Term $ do
    newref <- newNodeRef $ PrimNode x
    return newref

let_ :: Term a -> (Term a -> Term a) -> Term a
let_ (Term exp) bodyf = Term $ do
    ref <- exp
    getTerm . bodyf . Term $ return ref

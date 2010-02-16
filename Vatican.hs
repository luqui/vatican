{-# LANGUAGE RecursiveDo #-}

module Vatican where

import Data.IORef
import Control.Monad (forM_, (<=<), when)
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import System.Process (system)
import Data.Maybe (fromJust, catMaybes)
import Data.List (delete)


data UplinkType = UplinkAppL | UplinkAppR | UplinkLambda | UplinkVar
    deriving (Eq)

type Uplink = (UplinkType, NodeRef)

data NodeData
    = AppNode NodeRef NodeRef
    | LambdaNode NodeRef NodeRef
    | VarNode

data Node = Node {
    nodeCache :: Maybe NodeRef,
    nodeUplinks :: [Uplink],
    nodeData :: NodeData
  }

type NodeRef = IORef Node


upcopy :: NodeRef -> NodeRef -> Uplink -> IO ()
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

setCache :: NodeRef -> Maybe NodeRef -> IO ()
setCache ref newcache = modifyIORef ref (\n -> n { nodeCache = newcache })

newNodeRef :: NodeData -> IO NodeRef
newNodeRef dat = newIORef $ Node { nodeCache = Nothing, nodeUplinks = [], nodeData = dat }

replaceLeft :: NodeRef -> NodeRef -> IO ()
replaceLeft newchild node = modifyIORef node $ \n -> n { nodeData = go (nodeData n) }
    where
    go (AppNode l r) = AppNode newchild r

replaceRight :: NodeRef -> NodeRef -> IO ()
replaceRight newchild node = modifyIORef node $ \n -> n { nodeData = go (nodeData n) }
    where
    go (AppNode l r) = AppNode l newchild

replaceBody :: NodeRef -> NodeRef -> IO ()
replaceBody newchild node = modifyIORef node $ \n -> n { nodeData = go (nodeData n) }
    where
    go (LambdaNode v b) = LambdaNode v newchild

addUplink :: Uplink -> NodeRef -> IO ()
addUplink uplink node = modifyIORef node $ \n -> n { nodeUplinks = uplink : nodeUplinks n }

deleteUplink :: Uplink -> NodeRef -> IO ()
deleteUplink uplink node = modifyIORef node $ \n -> n { nodeUplinks = delete uplink (nodeUplinks n) }

getLeft :: NodeRef -> IO NodeRef
getLeft ref = (\(AppNode l r) -> l) . nodeData <$> readIORef ref

getRight :: NodeRef -> IO NodeRef
getRight ref = (\(AppNode l r) -> r) . nodeData <$> readIORef ref

getBody :: NodeRef -> IO NodeRef
getBody ref = (\(LambdaNode _ b) -> b) . nodeData <$> readIORef ref

clear :: NodeRef -> IO ()
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

cleanup :: NodeRef -> IO ()
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

upreplace :: NodeRef -> Uplink -> IO ()
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
            

betaReduce :: NodeRef -> IO NodeRef
betaReduce appref = do
    putStrLn "About to reduce"
    runGraphviz appref
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
    putStrLn "Done reducing"
    runGraphviz result
    return result

hnfReduce :: NodeRef -> IO ()
hnfReduce noderef = do
    node <- readIORef noderef
    case nodeData node of
        LambdaNode var body -> hnfReduce body 
        AppNode left right -> do
            hnfReduce left
            left' <- readIORef =<< getLeft noderef 
            case nodeData left' of
                LambdaNode {} -> hnfReduce =<< betaReduce noderef
                _ -> return ()
        _ -> return ()

graphviz :: NodeRef -> IO String
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
                case nodeCache node of
                    Just cacher -> do
                        cacheid <- go cacher
                        tell $ "p" ++ show ident ++ " -> p" ++ show cacheid ++ " [weight=0,style=dotted];\n"
                    Nothing -> return ()
                return ident

runGraphviz :: NodeRef -> IO ()
runGraphviz node = do
    writeFile "graph.dot" =<< graphviz node
    system "dot -T png -o graph.png graph.dot"
    system "eog graph.png"
    return ()

app :: IO NodeRef -> IO NodeRef -> IO NodeRef
app left right = do
    left' <- left
    right' <- right
    newref <- newNodeRef $ AppNode left' right'
    addUplink (UplinkAppL, newref) left'
    addUplink (UplinkAppR, newref) right'
    return newref

fun :: (IO NodeRef -> IO NodeRef) -> IO NodeRef
fun bodyf = do
    var <- newNodeRef $ VarNode
    body <- bodyf (return var)
    newref <- newNodeRef $ LambdaNode var body
    addUplink (UplinkLambda, newref) body
    return newref

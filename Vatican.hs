module Vatican where

import Data.IORef
import Control.Monad (forM_, (<=<), when)
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import System.Process (system)
import Data.Maybe (fromJust)
import Data.List (delete)

data UplinkType = UplinkAppL | UplinkAppR  | UplinkLambda
    deriving (Eq)

type Uplink = (UplinkType, NodeRef)

type NodeRef = IORef Node

data Node = Node {
    nodeUplinks :: [Uplink],
    nodeData :: NodeData,
    nodeCache :: IORef (Maybe NodeRef)
}

data NodeData 
    = AppNode NodeRef NodeRef
    | LambdaNode NodeRef NodeRef
    | VarNode

newNode :: NodeData -> IO Node
newNode dat = do
    cache <- newIORef Nothing
    return $ Node { nodeUplinks = [], nodeData = dat, nodeCache = cache }

newNodeRef :: NodeData -> IO NodeRef
newNodeRef = newIORef <=< newNode 

replaceData :: NodeData -> Node -> Node
replaceData dat node = node { nodeData = dat }

addUplink :: Uplink -> Node -> Node
addUplink uplink node = node { nodeUplinks = nodeUplinks node ++ [uplink] }

upcopy :: NodeRef -> [Uplink] -> IO NodeRef
upcopy newchild_ uplinks_ = do
    ret <- newIORef Nothing

    let writeUplinks :: NodeRef -> [Uplink] -> IO ()
        writeUplinks copied [] = writeIORef ret $ Just copied
        writeUplinks copied xs = mapM_ (recurse copied) xs

        recurse :: NodeRef -> Uplink -> IO ()
        recurse newchild (uplinkType, intoref) = do
            into <- readIORef intoref
            cache <- readIORef (nodeCache into)
            case cache of
                Nothing -> do
                    copied <- newNodeRef =<< copy newchild uplinkType into
                    writeIORef (nodeCache into) $ Just copied
                    writeUplinks copied (nodeUplinks into)
                Just copied -> do
                    writeIORef copied =<< newNode =<< copy newchild uplinkType =<< readIORef copied

    writeUplinks newchild_ uplinks_
    clearCaches newchild_ uplinks_ 

    fromJust <$> readIORef ret

copy :: NodeRef -> UplinkType -> Node -> IO NodeData
copy newchild UplinkAppL into = do
    let AppNode left right = nodeData into
    return $ AppNode left newchild
copy newchild UplinkAppR into = do
    let AppNode left right = nodeData into
    return $ AppNode newchild right
copy newchild UplinkLambda into = do
    let LambdaNode var body = nodeData into
    newvar <- newNodeRef $ VarNode
    newbody <- upcopy newvar =<< (nodeUplinks <$> readIORef var)
    return $ LambdaNode newvar newbody

clearCaches :: NodeRef -> [Uplink] -> IO ()
clearCaches sourceref uplinks = do
    source <- readIORef sourceref
    newUplinks <- mapM (\(ty,ref) -> ((,) ty) . fromJust <$> (readIORef =<< nodeCache <$> readIORef ref)) uplinks
    writeIORef (nodeCache source) Nothing
    let source' = source { nodeUplinks = newUplinks }
    writeIORef sourceref source'
    forM_ uplinks $ (\(ty,ref) -> clearCaches ref =<< (nodeUplinks <$> readIORef ref))

upreplace :: NodeRef -> NodeRef -> IO ()
upreplace newchild oldchild = do
    oldnode <- readIORef oldchild
    forM_ (nodeUplinks oldnode) $ \uplink@(typ,intoref) -> do
        into <- readIORef intoref
        case typ of
            UplinkAppL -> let AppNode left right = nodeData into in 
                          unlink uplink left >> return (AppNode newchild right)
            UplinkAppR -> let AppNode left right = nodeData into in 
                          unlink uplink right >> return (AppNode left newchild)
            UplinkLambda -> let LambdaNode var body = nodeData into in
                            unlink uplink body >> return (AppNode var newchild)
        writeIORef intoref =<< flip replaceData into <$> copy newchild typ into
    newnode <- readIORef newchild
    writeIORef newchild $ newnode { nodeUplinks = nodeUplinks newnode ++ nodeUplinks oldnode }

partialUnlink :: Uplink -> NodeRef -> IO ()
partialUnlink uplink noderef = do
    node <- readIORef noderef
    let node' = node { nodeUplinks = delete uplink (nodeUplinks node) }
    writeIORef noderef node'

unlink :: Uplink -> NodeRef -> IO ()
unlink uplink noderef = do
    partialUnlink uplink noderef
    node <- readIORef noderef

    when (null (nodeUplinks node)) $ 
        case nodeData node of
            AppNode left right  -> unlink (UplinkAppL, noderef) left >> unlink (UplinkAppR, noderef) right
            LambdaNode var body -> unlink (UplinkLambda, noderef) body
            VarNode             -> return ()

hnfReduce :: NodeRef -> IO NodeRef
hnfReduce noderef = do
    node <- readIORef noderef
    case nodeData node of
        AppNode leftref rightref -> do
            hnfReduce leftref
            left <- readIORef leftref
            case nodeData left of
                LambdaNode var body -> do
                    partialUnlink (UplinkLambda, leftref) body
                    partialUnlink (UplinkAppR, noderef) rightref
                    substituted <- upcopy rightref =<< nodeUplinks <$> readIORef var
                    upreplace substituted noderef
                    return substituted
                other -> return noderef
            
        LambdaNode var body -> hnfReduce body >> return noderef
        VarNode -> return noderef

graphviz :: NodeRef -> IO String
graphviz noderef = do
    output <- evalStateT (execWriterT (go noderef)) ([], 0)
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
                case nodeData node of
                    AppNode left right -> do
                        tell $ "p" ++ show ident ++ " [label=\"*\"];\n"
                        leftid <- go left
                        tell $ "p" ++ show ident ++ " -> p" ++ show leftid ++ " [color=\"#007f00\",label=\"fv\"];\n"
                        rightid <- go right
                        tell $ "p" ++ show ident ++ " -> p" ++ show rightid ++ " [label=\"av\"];\n"
                    LambdaNode var body -> do
                        tell $ "p" ++ show ident ++ " [label=\"\\\\\"];\n"
                        bodyid <- go body
                        tell $ "p" ++ show ident ++ " -> p" ++ show bodyid ++ ";\n"
                        varid <- go var
                        tell $ "p" ++ show ident ++ " -> p" ++ show varid ++ " [weight=0,color=blue];\n"
                    VarNode -> do
                        tell $ "p" ++ show ident ++ " [label=\"x\"];\n"
                return ident

runGraphviz :: NodeRef -> IO ()
runGraphviz node = do
    writeFile "graph.dot" =<< graphviz node
    system "dot -T png -o graph.png graph.dot"
    system "eog graph.png"
    return ()

app :: NodeRef -> NodeRef -> IO NodeRef
app left right = do
    newref <- newNodeRef $ AppNode left right
    modifyIORef left (addUplink (UplinkAppL, newref))
    modifyIORef right (addUplink (UplinkAppR, newref))
    return newref

fun :: (NodeRef -> IO NodeRef) -> IO NodeRef
fun bodyf = do
    var <- newNodeRef $ VarNode
    body <- bodyf var
    newref <- newNodeRef $ LambdaNode var body
    modifyIORef body (addUplink (UplinkLambda, newref))
    return newref

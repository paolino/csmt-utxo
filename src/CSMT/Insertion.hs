{-# LANGUAGE StrictData #-}

module CSMT.Insertion
    ( inserting
    )
where

import CSMT.Interface
    ( CSMT (..)
    , Direction (..)
    , Indirect (..)
    , Key
    , Query
    , compareKeys
    , opposite
    )

-- A binary tree with a Key at each node
data Compose a
    = Compose Key (Compose a) (Compose a)
    | Leaf Key a
    deriving (Show, Eq)

-- Construct a Compose node composed of two subtrees
compose :: Direction -> Key -> Compose a -> Compose a -> Compose a
compose L j left right = Compose j left right
compose R j left right = Compose j right left

-- | Insert a value into a CSMT
inserting
    :: Monad m
    => CSMT m a
    -- ^ Backend interface of the CSMT
    -> (a -> a -> a)
    -- ^ Hash composition function
    -> Key
    -- ^ Key to insert at
    -> a
    -- ^ Hash to insert
    -> m ()
inserting (CSMT i q) add key value = do
    c <- mkCompose q key value
    i $ snd $ scanCompose add c

-- Scan a Compose tree and produce the resulting hash and list of inserts
scanCompose :: (a -> a -> a) -> Compose a -> (a, [(Key, Indirect a)])
scanCompose add = go []
  where
    go k (Leaf j h) = (h, [(k, Indirect{jump = j, value = h})])
    go k (Compose j left right) =
        let k' = k <> j
            (hl, ls) = go (k' <> [L]) left
            (hr, rs) = go (k' <> [R]) right
            h = add hl hr
        in  (h, ls <> rs <> [(k, Indirect j h)])

-- Build a Compose tree for inserting a value at a given key
mkCompose
    :: forall a m. Monad m => Query m a -> Key -> a -> m (Compose a)
mkCompose get key h = go key [] pure
  where
    go :: Key -> Key -> (Compose a -> m (Compose a)) -> m (Compose a)
    go [] _ cont = cont $ Leaf [] h
    go target current cont = do
        mi <- get current
        case mi of
            Nothing -> cont $ Leaf target h
            Just Indirect{jump, value} -> do
                let (common, other, us) = compareKeys jump target
                case (other, us) of
                    ([], []) -> cont $ Leaf common h
                    ([], z : zs) -> do
                        mov <- get (current <> common <> [opposite z])
                        case mov of
                            Nothing -> error "a jump pointed to a non-existing node"
                            Just Indirect{jump = oj, value = ov} ->
                                go zs (current <> common <> [z]) $ \c ->
                                    cont $ compose z common c $ Leaf oj ov
                    (_ : os, z : zs) -> do
                        go zs (current <> common <> [z]) $ \c ->
                            cont $ compose z common c $ Leaf os value
                    _ ->
                        error
                            "there is at least on key longer than the requested key to insert"

module CSMT.Deletion
    ( deleting
    )
where

import CSMT.Interface
    ( CSMT (..)
    , Indirect (..)
    , Key
    , Op (..)
    , Query
    )

-- Path to the keys to delete
data Compose a
    = Compose Key a (Compose a)
    | Leaf Key Key
    deriving (Show, Eq)

-- | Change a value into a CSMT
deleting
    :: Monad m
    => CSMT m a
    -- ^ Backend interface of the CSMT
    -> Key
    -- ^ Key to delete at
    -> m ()
deleting (CSMT i q) key = do
    c <- mkCompose q key
    i $ scanCompose c

-- Scan a Compose tree and produce the resulting hash and list of inserts and one deletion
scanCompose :: Compose a -> [Op a]
scanCompose = go []
  where
    go k (Leaf l r) = [Delete $ k <> l, Delete $ k <> r]
    go k (Compose j h down) =
        let k' = k <> j
            rs = go k' down
        in  Insert k (Indirect{jump = j, value = h}) : rs

-- Build a Compose tree for inserting a value at a given key
mkCompose
    :: forall a m
     . Query m a
    -> Key
    -> m (Compose a)
mkCompose _get key = go key []
  where
    go = undefined

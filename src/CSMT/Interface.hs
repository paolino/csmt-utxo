{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module CSMT.Interface
    ( -- * Keys
      Direction (..)
    , Key
    , compareKeys
    , opposite

      -- * Interface Types
    , Indirect (..)
    , Op (..)
    , Change
    , Query
    , CSMT (..)
    , fromBool
    , toBool
    , root
    )
where

-- | Key segment
data Direction = L | R deriving (Show, Eq, Ord)

-- | Convert Bool to Direction
fromBool :: Bool -> Direction
fromBool True = R
fromBool False = L

-- Convert Direction to its Bool representation
toBool :: Direction -> Bool
toBool L = False
toBool R = True

-- | Get the opposite direction
opposite :: Direction -> Direction
opposite L = R
opposite R = L

-- | Key type
type Key = [Direction]

-- | An indirect reference to a value stored at a given Key from a node
-- If the 'jump' key is empty then the value is stored at the current node
-- If the 'jump' key is non-empty then the value is stored at a descendant node
-- reachable by following the 'jump' key from the current node
data Indirect a = Indirect
    { jump :: Key
    , value :: a
    }
    deriving (Show, Eq, Functor)

data Op a
    = Insert Key (Indirect a)
    | Delete Key
    deriving (Show, Eq)

-- | Type alias for a change function in some monad m. It support batch inserts.
type Change m a = [Op a] -> m ()

-- | Type alias for a query function in some monad m.
type Query m a = Key -> m (Maybe (Indirect a))

-- | The backend interface for a CSMT in some monad m.
data CSMT m a = CSMT
    { change :: Change m a
    , query :: Query m a
    }

-- | Compare two keys and return their common prefix and the remaining suffixes
-- of each key after the common prefix.
compareKeys :: Key -> Key -> (Key, Key, Key)
compareKeys [] ys = ([], [], ys)
compareKeys xs [] = ([], xs, [])
compareKeys (x : xs) (y : ys)
    | x == y =
        let (j, o, r) = compareKeys xs ys
        in  (x : j, o, r)
    | otherwise = ([], x : xs, y : ys)

root :: Monad m => CSMT m a -> m (Maybe a)
root csmt = do
    mi <- query csmt []
    case mi of
        Nothing -> return Nothing
        Just Indirect{value} -> return (Just value)

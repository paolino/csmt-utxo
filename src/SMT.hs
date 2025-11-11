{-# LANGUAGE StrictData #-}

module SMT
    ( Direction (..)
    , Indirect (..)
    , Key
    , Proof
    , Pure
    , inserting
    -- , mkProof
    , pureInsert
    , pureQuery
    , runPure
    -- , verifyProof
    , compareKeys
    )
where

import Control.Monad.State
    ( State
    , gets
    , modify'
    , runState
    )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Key = [Direction]

data Direction = L | R deriving (Show, Eq, Ord)

opposite :: Direction -> Direction
opposite L = R
opposite R = L

data Compose a
    = Compose Key (Compose a) (Compose a)
    | Leaf Key a
    deriving (Show, Eq)

compose :: Direction -> Key -> Compose a -> Compose a -> Compose a
compose L j left right = Compose j left right
compose R j left right = Compose j right left

data Indirect a = Indirect
    { jump :: Key
    , value :: a
    }
    deriving (Show, Eq)

type Insert m a = [(Key, Indirect a)] -> m ()
type Query m a = (Key -> m (Maybe (Indirect a)))

inserting
    :: Monad m
    => Query m a
    -> Insert m a
    -> (a -> a -> a)
    -> Key
    -> a
    -> m ()
inserting q i add key value = do
    c <- mkCompose q key value
    i
        $ snd
        $ scanCompose add c

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

compareKeys :: Key -> Key -> (Key, Key, Key)
compareKeys [] ys = ([], [], ys)
compareKeys xs [] = ([], xs, [])
compareKeys (x : xs) (y : ys)
    | x == y =
        let (j, o, r) = compareKeys xs ys
        in  (x : j, o, r)
    | otherwise = ([], x : xs, y : ys)

type Proof a = [(Direction, a)]

-- mkProof :: Monad m => Query m a -> Key -> m (Maybe (Proof a))
-- mkProof (def, find) key = go [] key []
--   where
--     go _ [] rs = pure $ Just rs
--     go u (k : ks) rs = do
--         mr <- find (u <> [k])
--         case mr of
--             Nothing -> pure Nothing
--             Just _ -> do
--                 o <- fromMaybe def <$> find (u <> [opposite k])
--                 go
--                     (u <> [k])
--                     ks
--                     ((k, o) : rs)

-- foldProof :: (a -> a -> a) -> a -> Proof a -> a
-- foldProof add = foldl' step
--   where
--     step acc (L, h) = add acc h
--     step acc (R, h) = add h acc

-- verifyProof
--     :: (Eq a, Monad m)
--     => Query m a
--     -> (a -> a -> a)
--     -> a
--     -> Proof a
--     -> m Bool
-- verifyProof (_, find) add value proof = do
--     mv <- find []
--     pure $ case mv of
--         Just rootHash -> rootHash == foldProof add value proof
--         Nothing -> False

--------------- Pure as a Map in the State ---------------

type Pure a = State (Map Key (Indirect a))

pureQuery :: Query (Pure a) a
pureQuery = gets . Map.lookup

pureInsert :: Insert (Pure a) a
pureInsert kvs = modify' $ \m -> Map.fromList kvs <> m

runPure
    :: Map Key (Indirect a) -> Pure a b -> (b, Map Key (Indirect a))
runPure = flip runState

------------- Hash-based SMT --------------

-- type Hash = ByteString
-- type SMTMap = Map Key Hash

-- fromBool :: Bool -> Direction
-- fromBool True = L
-- fromBool False = R

-- mkHash :: ByteString -> Hash
-- mkHash = convert . hash @ByteString @SHA256

-- defaultHash :: Hash
-- defaultHash = mkHash ""

-- root :: SMTMap -> Maybe Hash
-- root = Map.lookup []

-- mkKey :: ByteString -> Key
-- mkKey bs =
--     concatMap byteToKey (ByteString.unpack $ mkHash bs)
--   where
--     byteToKey w = [0 .. 7] <&> fromBool . testBit w

-- composeHash :: Hash -> Hash -> Hash
-- composeHash left right = mkHash (left <> right)

-- insert :: ByteString -> ByteString -> SMTMap -> SMTMap
-- insert key value = inserting composeHash defaultHash (mkKey key) (mkHash value)

{-# LANGUAGE StrictData #-}

module CSMT.Backend.Pure
    ( InMemoryDB
    , Pure
    , runPure
    , pureCSMT
    )
where

import CSMT.Interface
    ( CSMT (..)
    , Indirect
    , Insert
    , Key
    , Query
    )
import Control.Monad.State
    ( State
    , gets
    , modify'
    , runState
    )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | In-memory database type from keys to indirect values
type InMemoryDB a = Map Key (Indirect a)

-- | Pure monad for CSMT operations
type Pure a = State (InMemoryDB a)

pureQuery :: Query (Pure a) a
pureQuery = gets . Map.lookup

pureInsert :: Insert (Pure a) a
pureInsert kvs = modify' $ \m -> Map.fromList kvs <> m

runPure :: InMemoryDB a -> State (InMemoryDB a) b -> (b, InMemoryDB a)
runPure = flip runState

pureCSMT :: CSMT (Pure a) a
pureCSMT =
    CSMT
        { insert = pureInsert
        , query = pureQuery
        }

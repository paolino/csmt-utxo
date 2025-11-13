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
    , Key
    , Op (..)
    )
import Control.Monad.Trans.State.Strict
    ( State
    , gets
    , modify'
    , runState
    )
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | In-memory database type from keys to indirect values
type InMemoryDB a = Map Key (Indirect a)

-- | Pure monad for CSMT operations
type Pure a = State (InMemoryDB a)

runPure :: InMemoryDB a -> State (InMemoryDB a) b -> (b, InMemoryDB a)
runPure = flip runState

pureChange :: InMemoryDB a -> Op a -> InMemoryDB a
pureChange m (Insert k v) = Map.insert k v m
pureChange m (Delete k) = Map.delete k m

pureCSMT :: CSMT (Pure a) a
pureCSMT =
    CSMT
        { change = \kvs -> modify' $ \m -> foldl' pureChange m kvs
        , query = gets . Map.lookup
        }

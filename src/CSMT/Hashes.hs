{-# LANGUAGE StrictData #-}

module CSMT.Hashes
    ( mkHash
    , addHash
    )
where

import Crypto.Hash (Keccak_256, hash)
import Data.ByteArray (ByteArray, ByteArrayAccess, convert)
import Data.ByteString (ByteString)

-- | A simple wrapper around Keccak-256 hashes, with a combining function.
newtype Hash = Hash ByteString
    deriving
        (Eq, Ord, Show, Semigroup, Monoid, ByteArrayAccess, ByteArray)

-- | Create a Keccak-256 hash from a ByteString.
mkHash :: ByteString -> Hash
mkHash = convert . hash @ByteString @Keccak_256

-- | Combine two hashes by concatenating their ByteString representations
--   and hashing the result.
addHash :: Hash -> Hash -> Hash
addHash (Hash h1) (Hash h2) = mkHash (h1 <> h2)

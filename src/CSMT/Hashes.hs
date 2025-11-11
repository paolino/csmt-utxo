{-# LANGUAGE StrictData #-}

module CSMT.Hashes
    (
    )
where

-- type Hash = ByteString
-- type SMTMap = Map Key Hash

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

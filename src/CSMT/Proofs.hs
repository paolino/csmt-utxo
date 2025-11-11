{-# LANGUAGE StrictData #-}

module CSMT.Proofs
    ( -- , mkProof
        Proof
    )
where

import CSMT.Interface (Direction)

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

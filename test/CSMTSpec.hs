{-# LANGUAGE OverloadedLists #-}

module CSMTSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , InMemoryDB
    , Indirect (..)
    , Key
    , Proof
    , Pure
    , compareKeys
    , inserting
    , mkInclusionProof
    , pureCSMT
    , runPure
    , verifyInclusionProof
    )
import CSMT.Hashes (Hash, addHash, mkHash)
import Data.Foldable (Foldable (..), foldl', forM_)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, nub)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.String (IsString (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( Property
    , forAll
    , listOf
    , listOf1
    , shuffle
    )
import Test.QuickCheck.Gen (Gen, elements)

mk :: (a -> a -> a) -> InMemoryDB a -> Key -> a -> InMemoryDB a
mk a m k v = snd $ runPure m $ mkM a k v

mkInt :: InMemoryDB Int -> Key -> Int -> InMemoryDB Int
mkInt = mk (+)

mkM :: (a -> a -> a) -> Key -> a -> Pure a ()
mkM = inserting pureCSMT

mkMInt :: Key -> Int -> Pure Int ()
mkMInt = mkM (+)

pfM :: Key -> Pure a (Maybe (Proof a))
pfM = mkInclusionProof pureCSMT

vpfM :: Eq a => (a -> a -> a) -> Key -> a -> Pure a Bool
vpfM a k v = do
    mp <- pfM k
    case mp of
        Nothing -> pure False
        Just p -> verifyInclusionProof pureCSMT a v p

vpfMInt :: Key -> Int -> Pure Int Bool
vpfMInt = vpfM (+)

vpfMHash :: Key -> Hash -> Pure Hash Bool
vpfMHash = vpfM addHash
i :: Key -> a -> Indirect a
i p v = Indirect{jump = p, value = v}

spec :: Spec
spec = do
    describe "compareKeys" $ do
        it "handles empty keys"
            $ compareKeys [] []
            `shouldBe` ([], [], [])
        it "handles identical keys"
            $ compareKeys [L, R, L] [L, R, L]
            `shouldBe` ([L, R, L], [], [])
        it "handles common prefixes"
            $ compareKeys [L, R, R, R] [L, R, L, R]
            `shouldBe` ([L, R], [R, R], [L, R])
    describe "inserting" $ do
        it "inserts 1 key L"
            $ let
                rs = mkInt [] [L] (1 :: Int)
              in
                rs `shouldBe` [([], i [L] 1)]
        it "inserts 1 key R"
            $ let
                rs = mkInt [] [R] (1 :: Int)
              in
                rs `shouldBe` [([], i [R] 1)]
        it "inserts 1 key LL"
            $ let
                rs = mkInt [] [L, L] (1 :: Int)
              in
                rs `shouldBe` [([], i [L, L] 1)]
        it "inserts 1 key LR"
            $ let
                rs = mkInt [] [R, R] (1 :: Int)
              in
                rs `shouldBe` [([], i [R, R] 1)]
        it "inserts 2 keys R and L"
            $ let
                rs0 = mkInt [] [L] (1 :: Int)
                rs1 = mkInt rs0 [R] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], i [] 3)
                               , ([L], i [] 1)
                               , ([R], i [] 2)
                               ]
        it "inserts 2 keys L and R"
            $ let
                rs0 = mkInt [] [R] (2 :: Int)
                rs1 = mkInt rs0 [L] (1 :: Int)
              in
                rs1
                    `shouldBe` [ ([], i [] 3)
                               , ([L], i [] 1)
                               , ([R], i [] 2)
                               ]
        it "inserts 2 keys LL and LR"
            $ let
                rs0 = mkInt [] [L, L] (1 :: Int)
                rs1 = mkInt rs0 [L, R] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], i [L] 3)
                               , ([L, L], i [] 1)
                               , ([L, R], i [] 2)
                               ]

        it "inserts 2 keys RR and LL"
            $ let
                rs0 = mkInt [] [L, L] (1 :: Int)
                rs1 = mkInt rs0 [R, R] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], i [] 3)
                               , ([L], i [L] 1)
                               , ([R], i [R] 2)
                               ]
        it "inserts 2 keys LR and RL"
            $ let
                rs0 = mkInt [] [L, R] (1 :: Int)
                rs1 = mkInt rs0 [R, L] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], i [] 3)
                               , ([L], i [R] 1)
                               , ([R], i [L] 2)
                               ]
        it "inserts 3 keys LL, RL, LR"
            $ let
                rs0 = mkInt [] [L, L] (1 :: Int)
                rs1 = seq rs0 $ mkInt rs0 [R, L] (2 :: Int)
                rs2 = seq rs1 $ mkInt rs1 [L, R] (3 :: Int)
              in
                rs2
                    `shouldBe` [ ([], i [] 6)
                               , ([L], i [] 4)
                               , ([L, L], i [] 1)
                               , ([L, R], i [] 3)
                               , ([R], i [L] 2)
                               ]

        it "inserts 3 keys LL, LR, RL"
            $ let
                rs0 = mkInt [] [L, L] (1 :: Int)
                rs1 = mkInt rs0 [L, R] (2 :: Int)
                rs2 = mkInt rs1 [R, L] (3 :: Int)
              in
                rs2
                    `shouldBe` [ ([], i [] 6)
                               , ([L], i [] 3)
                               , ([R], i [L] 3)
                               , ([L, L], i [] 1)
                               , ([L, R], i [] 2)
                               ]

        it "inserting all leaves populates the full tree"
            $ forAll (elements [1 .. 10])
            $ \n -> forAll (genPaths n) $ \keys -> do
                let kvs = zip keys [1 :: Int .. 2 ^ n]
                inserted (+) kvs `shouldBe` summed n kvs

    describe "proving inclusion" $ do
        it "verifies a simple fact"
            $ let (r, _m) = runPure [] $ do
                    mkMInt [L] (1 :: Int)
                    vpfMInt [L] 1
              in  r `shouldBe` True
        it "verifies a deeper fact"
            $ let (r, _m) = runPure [] $ do
                    mkMInt [L, R, L] (42 :: Int)
                    vpfMInt [L, R, L] 42
              in  r `shouldBe` True
        it "verifies a fact with siblings"
            $ let (r, _m) = runPure [] $ do
                    mkMInt [L] (10 :: Int)
                    mkMInt [R] (20 :: Int)
                    vpfMInt [L] 10
              in  r `shouldBe` True
        it "verifies another fact with siblings"
            $ let (r, _m) = runPure [] $ do
                    mkMInt [L, L] (5 :: Int)
                    mkMInt [L, R] (15 :: Int)
                    mkMInt [R, L] (25 :: Int)
                    mkMInt [R, R] (35 :: Int)
                    vpfMInt [R, L] 25
              in  r `shouldBe` True
        let testRandomFactsInAFullTree
                :: (Int -> a) -> (a -> a -> a) -> (Key -> a -> Pure a Bool) -> Property
            testRandomFactsInAFullTree h a vpf = forAll (elements [1 .. 14])
                $ \n -> forAll (listOf $ elements [0 .. 2 ^ n - 1]) $ \ms ->
                    forAll (genPaths n) $ \keys -> forM_ ms $ \m -> do
                        let kvs = zip keys $ h <$> [1 .. 2 ^ n]
                            (testKey, testValue) = kvs !! m
                            (r, _m) = runPure (inserted a kvs) $ vpf testKey testValue
                        r `shouldBe` True
        it "verifies random facts in a full tree"
            $ testRandomFactsInAFullTree id (+) vpfMInt
        it "verifies random hash facts in a full tree"
            $ testRandomFactsInAFullTree intHash addHash vpfMHash
        let testRandomFactsInASparseTree
                :: (Int -> a) -> (a -> a -> a) -> (Key -> a -> Pure a Bool) -> Property
            testRandomFactsInASparseTree h a vpf = forAll (elements [128 .. 256])
                $ \n ->
                    forAll (genSomePaths n) $ \keys ->
                        forAll (listOf $ elements [0 .. length keys - 1]) $ \ks ->
                            forM_ ks $ \m -> do
                                let kvs = zip keys $ h <$> [1 ..]
                                    (testKey, testValue) = kvs !! m
                                    (r, _m) =
                                        runPure (inserted a kvs)
                                            $ vpf testKey testValue
                                r `shouldBe` True
        it "verifies random facts in a sparse tree"
            $ testRandomFactsInASparseTree id (+) vpfMInt
        it "verifies random hash facts in a sparse tree"
            $ testRandomFactsInASparseTree intHash addHash vpfMHash

intHash :: Int -> Hash
intHash = mkHash . fromString . show

inserted :: Foldable t => (a -> a -> a) -> t (Key, a) -> InMemoryDB a
inserted a = foldl' (\m (k, v) -> mk a m k v) []

summed :: Int -> [(Key, Int)] -> Map Key (Indirect Int)
summed n kvs =
    Map.fromList $ allInits n <&> \x ->
        let
            w = (Map.fromList (toList kvs) Map.!)
        in
            ( x
            , i []
                $ foldl' (+) 0
                $ NE.fromList
                $ fmap w
                $ filter (isPrefixOf x)
                $ allPaths n
            )
  where
    allInits :: Int -> [Key]
    allInits 0 = [[]]
    allInits c =
        allInits (c - 1) <> do
            p <- allInits (c - 1)
            [p, L : p, R : p]

allPaths :: Int -> [Key]
allPaths 0 = [[]]
allPaths c = do
    p <- allPaths (c - 1)
    [L : p, R : p]

genPaths :: Int -> Gen [Key]
genPaths n = shuffle (allPaths n)

genSomePaths :: Int -> Gen [Key]
genSomePaths n = fmap nub <$> listOf1 $ do
    let go 0 = return []
        go c = do
            d <- elements [L, R]
            ds <- go (c - 1)
            return (d : ds)
    go n

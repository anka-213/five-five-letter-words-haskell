-- {-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O1 #-}
module Main where

import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.IntSet (IntSet)
import Debug.Trace (traceM, traceShowId)
import Control.Monad (when, guard)
import System.Environment (getArgs)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap (IntMap)
import Data.Char (ord, toLower)
import GHC.Char (chr)
import Control.Applicative (Alternative((<|>)))

type SomeWord = IntSet

findThings :: IntSet -> IntMap (Set.Set SomeWord) -> [[IntSet]]
findThings usedLetters remaining | IntSet.size usedLetters >= 5*5 = pure []
findThings usedLetters remaining = do
    let relevantParts = IntMap.filterWithKey (\k v -> k `IntSet.notMember` usedLetters) remaining
    -- when (IntSet.size usedLetters >= 5*4) $ do
    --     traceM $ show usedLetters
    --     mapM_ (traceM . take 200 . show) $ IntMap.toList relevantParts
    --     traceM (relevantParts `seq` "")
    Just ((k, firstUnused), otherUnused) <- pure $ IntMap.minViewWithKey relevantParts
    (usedLetters', firstOrSecondUnused) <-
        pure (usedLetters, firstUnused) <|> do
            guard $ IntSet.size usedLetters `mod` 5 == 0 -- If we haven't skipped any letters so far
            -- traceM $ "skipping " ++ show k
            Just (secondUnused, otherUnused') <- pure $ IntMap.minView otherUnused
            pure (IntSet.insert k usedLetters, secondUnused)
    attempt <- Set.toList firstOrSecondUnused
    guard $ IntSet.disjoint attempt usedLetters'

    -- when (IntSet.size usedLetters' `mod` 5 /= 0) $ traceM $ show (usedLetters', aStr)
    -- when (IntSet.size usedLetters >= 5*3) $
    --   traceM $ show (usedLetters, aStr)
    rest <- findThings (IntSet.union attempt usedLetters') otherUnused -- Should only use later than attempt
    return $ attempt : rest

charToInt :: Char -> Int
charToInt c
    | Just n <- IntMap.lookup (fromEnum c) reverseLetterFrequency = n
    | otherwise = error $ "invalid char:" ++ show c
-- charToInt = (reverseLetterFrequency IntMap.!) . fromEnum
-- charToInt = fromEnum

reverseLetterFrequency :: IntMap Int
reverseLetterFrequency = IntMap.fromList $ zip (fmap ord "qxjzvfwbkgpmhdcytlnuroisea") [0..]
-- reverseLetterFrequency = IntMap.fromAscList $ zip [0..] $ fmap ord "QXJZVFWBKGPMHDCYTLNUROISEA"

main :: IO ()
main = do
    -- [nr] <- fmap read <$> getArgs
    -- words <- take nr . fmap init . lines <$> readFile "words_alpha.txt"
    words <- fmap init . lines <$> readFile "words_alpha.txt"
    -- words <- take 40000 . fmap init . lines <$> readFile "words_alpha.txt"
    -- let words5 = filter ((==5) . length . fst) $ [(IntSet.fromList w, w) | w <- words]
    let words5 =
            [ (ws, w)
            | w <- words , length w == 5
            , let ws = IntSet.fromList (map charToInt w) , IntSet.size ws == 5
            ]
    let words5Map = IntMap.fromListWith Set.union
            [ (leastCommonLetter, Set.singleton ws)
            | (ws, w) <- words5
            , let Just (leastCommonLetter, _) = IntSet.minView ws]
    let reverseMap = Map.fromListWith (++) $ fmap (fmap pure) words5
    let result = traverse (reverseMap Map.!) =<< findThings IntSet.empty words5Map
    mapM_ print result
    print $ length result
    -- print $ last $ take 1000000 result

    -- print $ take 10 words5

-- >>> words <- fmap init . lines <$> readFile "words_alpha.txt"
-- >>> let words5 = filter ((==5) . length) $ map IntSet.fromList words
-- >>> take 10 words5
-- [fromList "aghin",fromList "adkrv",fromList "abdeh",fromList "abcet",fromList "abcix",fromList "abcis",fromList "abcil",fromList "abcot",fromList "abcsu",fromList "abdno"]


-- >>> length $ Set.toList $ Set.fromList $ concat ["whamp","bucks","fldxt","vejoz","ringy"]
-- 25

-- >>> length $ Set.toList $ Set.fromList $ concat ["quawk","fldxt","vejoz","brigs","nymph"]
-- 25

-- >>> length $ Set.toList $ Set.fromList $ ['a'..'z']
-- 26

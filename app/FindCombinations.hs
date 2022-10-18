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
import qualified BitMask
import BitMask (Bitmask32)
import Data.List (uncons)
-- import Data.IntSet (showTree)

findThings :: Bitmask32 -> [(Int,IntSet)] -> [[Bitmask32]]
findThings usedLetters remaining | BitMask.size usedLetters >= 5*5 = pure []
findThings usedLetters remaining = do
    let getRelevantParts = dropWhile (\k -> fst k `BitMask.member` usedLetters)
    -- let relevantParts = fmap (BitMask.restrictIntSet usedLetters) <$> getRelevantParts remaining
    let relevantParts = getRelevantParts remaining
    Just ((k, firstUnused), otherUnused) <- pure $ uncons relevantParts
    (usedLetters', firstOrSecondUnused) <-
        pure (usedLetters, firstUnused) <|> do
            guard $ BitMask.size usedLetters `mod` 5 == 0 -- If we haven't skipped any letters so far
            Just ((_, secondUnused), otherUnused') <- pure $ uncons $ getRelevantParts otherUnused
            pure (BitMask.insert k usedLetters, secondUnused)
    attempt <- BitMask.fromIntSet $ BitMask.restrictIntSet usedLetters' firstOrSecondUnused
    -- guard $ BitMask.disjoint attempt usedLetters'

    rest <- findThings (BitMask.union attempt usedLetters') otherUnused -- Should only use later than attempt
    return $ attempt : rest

charToInt :: Char -> Int
charToInt c
    | Just n <- IntMap.lookup (fromEnum c) reverseLetterFrequency = n
    | otherwise = error $ "invalid char:" ++ show c

reverseLetterFrequency :: IntMap Int
reverseLetterFrequency = IntMap.fromList $ zip (fmap ord "qxjzvfwbkgpmhdcytlnuroisea") [0..]

main :: IO ()
main = do
    words <- fmap init . lines <$> readFile "words_alpha.txt"
    let words5 =
            [ (ws, w)
            | w <- words , length w == 5
            , let ws = BitMask.fromList (map charToInt w) , BitMask.size ws == 5
            ]
    let words5Map = IntMap.toList $ IntMap.fromListWith IntSet.union
            [ (leastCommonLetter, IntSet.singleton $ BitMask.unBM ws)
            | (ws, w) <- words5
            , let Just leastCommonLetter = BitMask.minimum ws]
    let reverseMap = Map.fromListWith (++) $ fmap (fmap pure) words5
    -- mapM_ (putStrLn . take 1000 . showTree . snd) $ take 10 words5Map
    let result = traverse (reverseMap Map.!) =<< findThings BitMask.empty words5Map
    mapM_ print result
    print $ length result

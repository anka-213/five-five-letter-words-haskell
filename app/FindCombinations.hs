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

findThings :: Bitmask32 -> IntMap [Bitmask32] -> [[Bitmask32]]
findThings usedLetters remaining | BitMask.size usedLetters >= 5*5 = pure []
findThings usedLetters remaining = do
    let relevantParts = IntMap.filterWithKey (\k v -> k `BitMask.notMember` usedLetters) remaining
    Just ((k, firstUnused), otherUnused) <- pure $ IntMap.minViewWithKey relevantParts
    (usedLetters', firstOrSecondUnused) <-
        pure (usedLetters, firstUnused) <|> do
            guard $ BitMask.size usedLetters `mod` 5 == 0 -- If we haven't skipped any letters so far
            Just (secondUnused, otherUnused') <- pure $ IntMap.minView otherUnused
            pure (BitMask.insert k usedLetters, secondUnused)
    attempt <- firstOrSecondUnused
    guard $ BitMask.disjoint attempt usedLetters'

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
    let words5Map = Set.toList <$> IntMap.fromListWith Set.union
            [ (leastCommonLetter, Set.singleton ws)
            | (ws, w) <- words5
            , let Just leastCommonLetter = BitMask.minimum ws]
    let reverseMap = Map.fromListWith (++) $ fmap (fmap pure) words5
    let result = traverse (reverseMap Map.!) =<< findThings BitMask.empty words5Map
    mapM_ print result
    print $ length result

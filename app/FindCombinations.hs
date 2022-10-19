{-# OPTIONS_GHC -fprof-auto-calls #-}
module Main where

import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.IntSet (IntSet)
import Control.Monad (guard)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap (IntMap)
import Data.Char (ord)
import Control.Applicative (Alternative((<|>)))
import qualified BitMask
import BitMask (Bitmask32)
import Data.List (uncons)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

findThings :: Bitmask32 -> [(Int,IntSet)] -> [[Bitmask32]]
findThings usedLetters _remaining | BitMask.size usedLetters >= 5*5 = pure []
findThings usedLetters remaining = do
    let getRelevantParts = dropWhile (\k -> fst k `BitMask.member` usedLetters)
    let relevantParts = getRelevantParts remaining
    Just ((k, firstUnused), otherUnused) <- pure $ uncons relevantParts
    (usedLetters', firstOrSecondUnused) <-
        pure (usedLetters, firstUnused) <|> do
            guard $ BitMask.size usedLetters `mod` 5 == 0 -- If we haven't skipped any letters so far
            Just ((_, secondUnused), _otherUnused') <- pure $ uncons $ getRelevantParts otherUnused
            pure (BitMask.insert k usedLetters, secondUnused)
    attempt <- BitMask.fromIntSet $ BitMask.restrictIntSet usedLetters' firstOrSecondUnused

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
    wordList <- fmap BS8.strip . BS8.lines <$> BS.readFile "words_alpha.txt"
    let words5 =
            [ (ws, w)
            | w <- wordList , BS.length w == 5 -- All words of length 5
            , let ws = BitMask.fromList (map charToInt $ BS8.unpack w) , BitMask.size ws == 5 -- with five unique characters
            ]
    let words5Map = IntMap.toList $ IntMap.fromListWith IntSet.union -- a map from least common letter to set of words
            [ (leastCommonLetter, IntSet.singleton $ BitMask.unBM ws)
            | (ws, _w) <- words5
            , let Just leastCommonLetter = BitMask.minimum ws]
    let reverseMap = Map.fromListWith (++) $ fmap (fmap pure) words5
    let result = traverse (reverseMap Map.!) =<< findThings BitMask.empty words5Map
    mapM_ print result
    print $ length result

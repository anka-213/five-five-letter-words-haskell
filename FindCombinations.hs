module Main where

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Debug.Trace (traceM)
import Control.Monad (when)

type SomeWord = (IntSet, String)

findThings :: IntSet -> [SomeWord] -> [[String]]
findThings soFar remaining | IntSet.size soFar >= 5*4 = [[]]
findThings soFar remaining = do
    let nonOverlapping = filter (IntSet.disjoint soFar . fst) remaining
    (attempt, aStr) <- nonOverlapping
    -- when (IntSet.size soFar >= 5*3) $
    --   traceM $ show (soFar, aStr)
    rest <- findThings (IntSet.union attempt soFar) nonOverlapping -- Should only use later than attempt
    return $ aStr : rest

charToInt :: Char -> Int
charToInt = fromEnum
-- TODO: Use this: QXJZVFWBKGPMHDCYTLNUROISEA

main :: IO ()
main = do
    words <- fmap init . lines <$> readFile "words_alpha.txt"
    -- let words5 = filter ((==5) . length . fst) $ [(IntSet.fromList w, w) | w <- words]
    let words5 = [(ws, w)
                 | w <- words , length w == 5
                 , let ws = IntSet.fromList (map charToInt w) , IntSet.size ws == 5 ]
    let result = findThings IntSet.empty words5
    -- mapM_ print result
    print $ last $ take 1000000 result

    -- print $ take 10 words5

-- >>> words <- fmap init . lines <$> readFile "words_alpha.txt"
-- >>> let words5 = filter ((==5) . length) $ map IntSet.fromList words
-- >>> take 10 words5
-- [fromList "aghin",fromList "adkrv",fromList "abdeh",fromList "abcet",fromList "abcix",fromList "abcis",fromList "abcil",fromList "abcot",fromList "abcsu",fromList "abdno"]

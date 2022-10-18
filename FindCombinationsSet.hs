module Main where

import qualified Data.Set as Set
import Data.Set (Set)
import Debug.Trace (traceM)
import Control.Monad (when)

type SomeWord = (Set Char, String)

findThings :: Set Char -> [SomeWord] -> [[String]]
findThings soFar remaining | Set.size soFar >= 5*4 = [[]]
findThings soFar remaining = do
    let nonOverlapping = filter (Set.disjoint soFar . fst) remaining
    (attempt, aStr) <- nonOverlapping
    -- when (length soFar >= 5*3) $
    --   traceM $ show (soFar, aStr)
    rest <- findThings (Set.union attempt soFar) nonOverlapping -- Should only use later than attempt
    return $ aStr : rest

main :: IO ()
main = do
    words <- fmap init . lines <$> readFile "words_alpha.txt"
    -- let words5 = filter ((==5) . length . fst) $ [(Set.fromList w, w) | w <- words]
    let words5 = [(Set.fromList w, w)
                 | w <- words , length w == 5
                 , let ws = Set.fromList w , length ws == 5 ]
    print $ last $ take 100000 $ findThings Set.empty words5
    -- mapM_ print $ findThings Set.empty words5

    -- print $ take 10 words5

-- >>> words <- fmap init . lines <$> readFile "words_alpha.txt"
-- >>> let words5 = filter ((==5) . length) $ map Set.fromList words
-- >>> take 10 words5
-- [fromList "aghin",fromList "adkrv",fromList "abdeh",fromList "abcet",fromList "abcix",fromList "abcis",fromList "abcil",fromList "abcot",fromList "abcsu",fromList "abdno"]

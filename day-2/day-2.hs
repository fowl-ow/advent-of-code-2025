-- import System.Process.Internals (ProcessHandle (waitpidLock))
import Data.List.Split (splitOn)
import Numeric (readInt)

extractTextRanges :: String -> [String]
extractTextRanges = splitOn ","

extractRanges :: String -> [(Int, Int)]
extractRanges = map toPair . extractTextRanges
  where
    toPair s =
        case splitOn "-" s of
            [a, b] -> (read a, read b)
            _ -> error "invalid range"

catAndSum :: [[Int]] -> Int
catAndSum = sum . concat

----------------------------
---------- Part 1 ----------
----------------------------

-- main :: IO ()
-- main = do
--     contents <- readFile "input.txt"
--     print $ catAndSum $ map invalidNums $ extractRanges contents

invalidNums :: (Int, Int) -> [Int]
invalidNums (from, to) = [x | x <- [from .. to], invalid x]

invalid :: Int -> Bool
invalid x = front == back
  where
    s = show x
    (front, back) = splitAt (length s `div` 2) s

----------------------------
---------- Part 2 ----------
----------------------------

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ catAndSum $ map generalInvalidNums $ extractRanges contents

generalInvalidNums :: (Int, Int) -> [Int]
generalInvalidNums (from, to) = [x | x <- [from .. to], generalInvalid x]

generalInvalid :: Int -> Bool
generalInvalid x = foldr ((||) . allEqual) False lol
  where
    s = show x
    lol = map (`chunksOf` s) $ sublistSizes $ length s

sublistSizes :: Int -> [Int]
sublistSizes n = [x | x <- [1 .. n `div` 2], n `mod` x == 0]

chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

allEqual :: [String] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (a : as) = fst $ foldr (\x (okSoFar, ref) -> (okSoFar && x == ref, ref)) (True, a) as

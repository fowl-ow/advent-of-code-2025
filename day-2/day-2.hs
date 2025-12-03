-- import System.Process.Internals (ProcessHandle (waitpidLock))
import Data.List.Split (splitOn)
import Numeric (readInt)

----------------------------
---------- Part 1 ----------
----------------------------

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ show $ catAndSum $ map invalidNums $ extractRanges contents

-- evens n = [x | x <- [1 .. n], even x]
extractTextRanges :: String -> [String]
extractTextRanges = splitOn ","

extractRanges :: String -> [(Int, Int)]
extractRanges = map toPair . extractTextRanges
  where
    toPair s =
        case splitOn "-" s of
            [a, b] -> (read a, read b)
            _ -> error "invalid range"

invalidNums :: (Int, Int) -> [Int]
invalidNums (from, to) = [x | x <- [from .. to], invalid x]

invalid :: Int -> Bool
invalid x = front == back
  where
    s = show x
    (front, back) = splitAt (length s `div` 2) s

catAndSum :: [[Int]] -> Int
catAndSum = sum . concat

----------------------------
---------- Part 2 ----------
----------------------------

-- main :: IO ()
-- main = do
--     contents <- readFile "input.txt"
--     print $ show $ catAndSum $ map invalidNums $ extractRanges contents

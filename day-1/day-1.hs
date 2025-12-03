import System.Process.Internals (ProcessHandle (waitpidLock))

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    print (process ls)

data Dir = L | R

process :: [String] -> Int
process = snd . foldl' step (50, 0) . map unpack

unpack :: String -> (Dir, Int)
unpack [] = (R, 0)
unpack (x : xs) = (dir, n)
  where
    dir = if x == 'L' then L else R
    n = read xs :: Int

step :: (Int, Int) -> (Dir, Int) -> (Int, Int)
step (current, zeroes) (dir, x) = (newCurrent, newZeroes)
  where
    newCurrent = calc current (dir, x)
    newZeroes = if newCurrent == 0 then zeroes + 1 else zeroes

calc :: Int -> (Dir, Int) -> Int
calc n (L, x) = (n - x) `mod` 100
calc n (R, x) = (n + x) `mod` 100

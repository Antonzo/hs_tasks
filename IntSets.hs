
import qualified Data.Set as Set
import System.Environment

readNumFile :: FilePath -> IO [Int]
readNumFile path = do
  nums <- readFile path
  return $ map (read) $ words nums

readAllFiles :: [FilePath] -> IO [[Int]]
readAllFiles = mapM readNumFile

--Создание списка множеств
createLSet :: [[Int]] -> [Set.Set Int]
createLSet = foldl(\res l -> res ++ [Set.fromList l]) []

solveA, solveB :: [[Int]] -> (Int, [Int])
solveA [] = (0, [])
solveA (x:xs) = (length $ help (x:xs), help (x:xs))
  where
    help (x:xs) = foldl(\res lst -> Set.elems $ Set.intersection (Set.fromList res) (Set.fromList lst)) x xs


solveB list = (length $ help list, help list)
  where
    help = foldl(\res lst -> Set.elems $ Set.union (Set.fromList res) (Set.fromList lst)) []

main = do
  args <- getArgs
  xss <- readAllFiles args
  print $ solveA xss
  print $ solveB xss

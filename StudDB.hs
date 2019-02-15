module StudDB where
import Data.List
import System.Environment

--Студент
data Student = Student String Int Int Int deriving Show

--Аргументы командной строки
data Args = Args FilePath Int (Maybe (Int, Int))

--Создание студента из списка с информацией
createSt :: [String] -> Student
createSt (fio: age: course: group: []) = Student fio (read age) (read course) (read group)
createSt _ = error "Incorrect student"

--Разбиение строки по заданному символу
wordsBySym :: (Char -> Bool) -> String -> [String]
wordsBySym pred s = case dropWhile pred s of
                       "" -> []
                       s' -> w: wordsBySym pred s''
					     where
						   (w, s'') = break pred s''
						   
--Загрузка информации из файла
loadData :: FilePath -> IO[Student]
loadData name = do
  students <- readFile name
  return $ foldl (\res x -> res ++ [createSt x]) [] $ map (\str -> wordsBySym ( == ';') str) $ lines students

--Загрузка данных из строки
loadInform :: [String] -> Args
loadInform (name:num:[]) = Args name (read num) Nothing
loadInform (name:num:course:group:[]) = Args name (read num) (Just (read course, read group))
  
--Средний возраст
middleAge :: (Real a) => Int -> Int -> IO [Student] -> a
middleAge c g list = undefined{-= (foldl(\sum x -> sum + x) 0 (help c g list)) / (length (help c g list))
  where
    help :: Int -> Int -> IO [Student] -> [Int]
    help c g lst = foldl (\res (Student _ age course group) -> if course == c && group == g then res ++ [age] else res) [] lst
  -}
--Количество студентов в каждой группе каждого курса
countInEvGroup :: IO [Student] -> [((Int, Int),  Int)]
countInEvGroup = undefined {-$ groupBy(\(Student _ _ course1 group1) (Student _ _ course2 group2) -> compare course1 course2 && compare group1 group2) -}
    
--Файлы со списками студентов	
listOfGroup :: IO [Student] -> IO ()
listOfGroup = undefined	
	
--Выбор действия  
choiceAction :: Args -> IO ()

choiceAction (Args name 1 (Just (course, group)) ) = do
  putStrLn $ show $ middleAge course group $ loadData name
  
choiceAction (Args name act Nothing) = do
  case act of

     2 -> putStrLn $ show $ countInEvGroup $ loadData name
     3 -> listOfGroup $ loadData name
     _ -> error "Incorrect action"
  
choiceAction _ = error "Incorrect format"
 
main = do
  args <- getArgs
  choiceAction $ loadInform args
  
  

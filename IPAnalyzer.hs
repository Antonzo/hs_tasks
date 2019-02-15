{-# LANGUAGE EmptyDataDecls #-}

import System.Environment
import Data.List
{-
  Разработайте тип данных Host для хранения следующей информации:
   * IP-адрес (строка)
   * Имя хоста
   * Страна
-}

data Host = Host String String String deriving Show

{-
   Реализуйте функцию, которая по заданной строке возвращает значение типа Host.
   Формат строки: IP-АДРЕС ИМЯ СТРАНА
   Название страны может состоять из более чем одного слова.
-}

str2host :: String -> Host
str2host str = Host ip name (unwords country)
  where
    (ip: name: country) = words str

{-
> str2host "123-53 tutu Germany"
Host "123-53" "tutu" "Germany"

> str2host "123-53 tutu Bosnia & Gercogovina"
Host "123-53" "tutu" "Bosnia & Gercogovina"
-}	
	
{-
   Реализуйте функцию, которая загружает из файла с заданным именем
   список пользователей. Для проверки используйте файл hosts.txt.
-}

loadData :: FilePath -> IO [Host]
loadData filename = do
  fc <- readFile filename
  return $ map(str2host) $ lines fc


{-
   Напишите функции, которые по заданному списку хостов находят следующую
   информацию:
   * Отчёт 1: количество хостов из страны с заданным названием
   * Отчёт 2: количество различных стран (может оказаться полезной функция nub)
   * Отчёт 3: список хостов с именами максимальной длины
   * Отчёт 4: страна с наибольшим количеством хостов

   Явная рекурсия в отчётах не допускается, следует использовать функции
   высших порядков. 
-}

report1 :: [Host] -> String -> Int
report1 list cntr = foldl(\cnt (Host ip name country) -> if country == cntr then (cnt + 1)
                                                         else cnt) 0 list

{-
> report1 ( map(str2host) ["198.171.254.224 macws16 Aruba", "165.184.98.143 arago Paraguay", "183.1.9.2 lll-freedom Paraguay"]) "Paraguay"
2
-}
														 
-- Отчёты 2--4

report2 :: [Host] -> Int
report2 = length . nub . map (\(Host ip name country) -> country)

{-
> report2 $ map(str2host) ["198.171.254.224 macws16 Aruba", "165.184.98.143 arago Paraguay", "183.1.9.2 lll-freedom Paraguay"]
2
-}

report3 :: [Host] -> [Host]
report3 lst = fst $ foldl(\(list, len) (Host ip name country)->
                                 if (length name) == len then (list ++ [Host ip name country], len)
								 else (list, len)) ([], maxLen $ maxHost lst) lst
  where
    maxHost = maximumBy(\(Host ip1 name1 country1) (Host ip2 name2 country2) -> compare (length name1) (length name2))
    maxLen (Host ip1 name1 country1) = length name1
	
{-
> report3 $ map(str2host) ["198.171.254.224 macws16 Aruba", "165.184.98.143 arago Paraguay", "183.1.9.2 freedom Paraguay"]
[Host "198.171.254.224" "macws16" "Aruba",Host "183.1.9.2" "freedom" "Paraguay"]
-}

report4 :: [Host] -> String
report4 list = fst $ foldl(\(res, cnt) (x:xs) -> if length (x:xs) > cnt then (x, length (x:xs))
                                      else (res, cnt)) ("", 0) $ group $ sort
                       $ map (\(Host ip name country) -> country) list
{-
   Напишите основную программу, которая читает параметр командной строки --- имя файла,
   загружает из него список хостов и печатает отчёты 1-4.
-}

main = do
  [arg] <- getArgs
  hosts <- loadData arg
  print "Count of hosts in Russia ="
  print $ report1 hosts "Russia"
  
  print "Count of different countries ="
  print $ report2 hosts
  
  print "List of hosts with max length of its name ="
  print $ report3 hosts
  
  print "Country with max count of hosts:"
  print $ report4 hosts
  
  {-
> :main hosts.txt
"Count of hosts in Russia ="
56
"Count of different countries ="
250
"List of hosts with max length of its name ="
[Host "167.116.247.147" "navshipyd-pearl-harbor" "Slovenia"]
"Country with max count of hosts:"
"Belize"
  -}
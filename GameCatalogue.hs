import Data.List
import Data.Char
import System.Directory
import qualified Data.List.Split as S

fileData :: IO String
fileData = readFile "Catalogue.csv"

--returns with lines
getData :: IO String -> IO [String]
getData x = x >>= return . lines

writeData :: IO [[String]]
writeData = getData fileData  >>= return . map (\x -> S.splitOn (",") x)

compareData :: IO [[String]]
compareData = writeData >>= return . tail

--sorts by most expensive order
sortByExpensive :: [[String]] -> [[String]]
sortByExpensive x = reverse $ sortBy (\x y -> compare (read (x !! 4) :: Double) (read (y !! 4) :: Double)) x

--sorts by top selling order
sortBySelling :: [[String]] -> [[String]]
sortBySelling x = reverse $ sortBy (\x y -> compare (read (x !! 6) :: Int) (read (y !! 6) :: Int)) x

--adds comma and space to the list of Strings
addComma :: [String] -> String
addComma (x:xs) | length xs > 0 = x ++ ", " ++ addComma xs
                | otherwise = x ++ "\n\n"

firstLine :: IO String
firstLine = writeData >>= return . addComma . head

--outputs the data from the file in a displayable manner
showCatalogue :: [[String]] -> IO ()
showCatalogue [] = putStr ""
showCatalogue (x:xs) | length x == 3 = putStr "$" >> (putStr . head) x >> putStr ", " >> showCatalogue ([tail x] ++ xs) 
                     | length x > 1 = (putStr . head) x >> putStr ", " >> showCatalogue ([tail x] ++ xs) 
                     | otherwise = (putStr . head) x >> putStrLn "\n" >> showCatalogue xs  

--finds whether a given ID exists in the database
findID :: String -> [[String]] -> String
findID _ [] = ""
findID x (y:ys) | (y !! 0) == x = x
                | otherwise = findID x ys

viewMenu = putStrLn "\n1. Chronological\n2. Price Hight to Low\n3. Top Selling\n4. Back to Menu" >> getLine >>= displayList

createMenu = putStrLn "\nPlease enter ID, name, genre, platfrom, price, how many in stock, and sold numbers of the game separated by commas\n"

updateMenu1 = putStr "\nPlease enter ID of the record you want to update: " >> getLine
updateMenu2 = putStrLn "\nPlease enter new name, genre, platfrom, price, how many in stock, and sold numbers of the game separated by commas\n"

removeMenu = putStr "\nPlease enter ID of the record you want to remove: " >> getLine

--the record from the input where all leading and ending spaces are removed and the data is split by commas
newRec :: IO [String]
newRec = getLine >>= return . map (reverse) . map (dropWhile (\x -> x == ' ') . reverse) . map (dropWhile (\x -> x == ' ')) . S.splitOn(",")

--updates the given record
updateElem :: Eq t => t -> [t] -> [[t]] -> [[t]]
updateElem _ _ [] = [] 
updateElem x y (z:zs) | (z !! 0) == x = (x : y) : updateElem x y zs
                      | otherwise = z : updateElem x y zs

--removes the given record
removeElem :: Eq t => t -> [[t]] -> [[t]]
removeElem _ []  = []
removeElem x (y:ys) | (y !! 0) == x = removeElem x ys
                    | otherwise = y : removeElem x ys         

--parses the list of lists to a complete String for writing back to the file
parseToStr :: [[String]] -> String
parseToStr [] = ""
parseToStr (x:xs) | length x > 1 = (head x) ++ "," ++ parseToStr ([tail x] ++ xs)
                  | otherwise = (head x) ++ "\n" ++ parseToStr xs

--special check function for adding
checkAdd :: String -> [String] -> [[String]] -> String
checkAdd "" xs xss | map toLower (xs !! 0) == "id" = "id" | length xs /= 7 = "*" | otherwise = parseToStr $ xss ++ [xs]
checkAdd _ _ _ = "#"

--special check function for updating
checkUpd :: String -> [String] -> [[String]] -> String
checkUpd x xs xss | length xs == 6 = parseToStr $ updateElem x xs xss | otherwise = "*"

--needed for the new record when updating, all of these are broadly explained in the report
handleUpd :: String -> [[String]] -> IO ()
handleUpd "" _ = updateFile ""
handleUpd x xss = updateMenu2 >> newRec >>= \a -> updateFile $ checkUpd x a xss

--special check function for removing
checkRem :: String -> [[String]] -> String
checkRem "" _ = ""
checkRem x xss = parseToStr $ removeElem x xss

--best explanation in report, but basically writes the data back to the file
updateFile :: String -> IO ()
updateFile "" = putStrLn "\nID not found\n"
updateFile "*" = putStrLn "\nInvalid number of inputs\n"
updateFile "id" = putStrLn "\nID cannot be id\n"
updateFile "#" = putStrLn "\nID already exists\n"
updateFile x = writeFile "Reserve.csv" x >> (readFile "Reserve.csv") >>= writeFile "Catalogue.csv" >> removeFile "Reserve.csv" >> putStrLn "\nChanges are made successfully\n"

--used for view catalogue
displayList :: String -> IO ()
displayList "1" = putStrLn "" >> firstLine >>= putStr >> compareData >>= showCatalogue >> viewMenu
displayList "2" = putStrLn "" >> firstLine >>= putStr >> compareData >>= return . sortByExpensive >>= showCatalogue >> viewMenu
displayList "3" = putStrLn "" >> firstLine >>= putStr >> compareData >>= return . sortBySelling >>= showCatalogue >> viewMenu
displayList "4" = main
displayList _ = putStrLn "\nInvalid Input" >> viewMenu

--very first actions of the program
process :: String -> IO ()
process "1" = viewMenu
process "2" = (createMenu >> newRec >>= \x -> writeData >>= \y -> updateFile $ checkAdd (findID (x !! 0) y) x y) >> main
process "3" = (updateMenu1 >>= \x -> writeData >>= \y -> handleUpd (findID x (tail y)) y) >> main
process "4" = (removeMenu >>= \x -> writeData >>= \y -> updateFile $ checkRem (findID x (tail y)) y) >> main
process "5" = putStrLn "\nProgram Closed"
process _ = putStrLn "\nInvalid Input" >> main

menu = putStrLn "==========================" >>
       putStrLn "| 1. View Catalogue      |" >>
       putStrLn "| 2. Create New Record   |" >>
       putStrLn "| 3. Update Record       |" >>
       putStrLn "| 4. Remove Record       |" >>
       putStrLn "| 5. Exit                |" >>
       putStrLn "==========================" >>
       getLine

main = menu >>= process
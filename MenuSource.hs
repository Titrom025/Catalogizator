module MenuSource where 

import System.IO
import FileHandler
import System.Directory
import System.Console.ANSI

getFilesInfo :: IO ()
getFilesInfo = do
    existense <- doesFileExist ".system/.files.csv"
    if existense
        then do
            removeFile ".system/.files.csv"
        else do
            return ()

    existense <- doesFileExist ".system/.dirs.csv"
    if existense
        then do
            removeFile ".system/.dirs.csv"
        else do
            return ()

    scan_dir "."

print_menu :: IO ()
print_menu = do
    putStr $ "##################################################" ++ "\n"
    putStr $ "#                                                #" ++ "\n"
    putStr $ "#          Insert one of these options:          #" ++ "\n"
    putStr $ "#                                                #" ++ "\n"
    putStr $ "#       1 - rescan current dir                   #" ++ "\n"
    putStr $ "#       2 - print file tree                      #" ++ "\n"
    putStr $ "#       3 + filename - find doubles by name      #" ++ "\n"
    putStr $ "#                                                #" ++ "\n"
    putStr $ "##################################################" ++ "\n\n"
 

io_handler :: IO b
io_handler = do
    putStr "Option: "
    hFlush stdout
    command <- getLine
 
    case command of 
        "1" -> do 
            clearScreen
            setSGR [SetColor Foreground Vivid Green]
            putStrLn "Rescan complete.\n"
            setSGR [SetColor Foreground Vivid Yellow]
            getFilesInfo
            print_menu

        "2" -> do
            clearScreen
            setSGR [SetColor Foreground Vivid Blue]
            currDir <- getCurrentDirectory
            putStrLn $ currDir 
            printFiles "." "." " " 
            setSGR [SetColor Foreground Vivid Yellow]
        
        "3" -> do 
            putStr "\nInsert filename: "
            hFlush stdout
            filename <- getLine
            findDoublesByName filename

        otherwise -> do
            clearScreen 
            setSGR [SetColor Foreground Vivid Red]
            putStr "Wrong option.\n\n"
            setSGR [SetColor Foreground Vivid Yellow]
            print_menu

    io_handler

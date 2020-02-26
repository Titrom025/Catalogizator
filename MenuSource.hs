module MenuSource where 

import FileHandler
import System.Directory
import System.Console.ANSI
import System.IO

getFilesInfo :: IO ()
getFilesInfo = do
    existense <- doesFileExist ".files.csv"
    if existense
        then do
            removeFile ".files.csv"
        else do
            return ()

    scan_dir "."

print_menu :: IO ()
print_menu = do
    putStr $ "#########################################" ++ "\n"
    putStr $ "#                                       #" ++ "\n"
    putStr $ "#     Insert one of these options:      #" ++ "\n"
    putStr $ "#                                       #" ++ "\n"
    putStr $ "#  1 - rescan current dir               #" ++ "\n"
    putStr $ "#  2 - print file tree                  #" ++ "\n"
    putStr $ "#  3 + filename - find doubles by name  #" ++ "\n"
    putStr $ "#                                       #" ++ "\n"
    putStr $ "#########################################" ++ "\n\n"
 

io_handler :: IO b
io_handler = do
    putStr "Option: "
    hFlush stdout
    command <- getLine
 
    case command of 
        "1" -> do 
            clearScreen
            setSGR [SetColor Foreground Vivid Green]
            putStr "Rescan complete.\n\n"
            setSGR [SetColor Foreground Vivid Yellow]
            getFilesInfo
            print_menu

        "2" -> printTree "."
        
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

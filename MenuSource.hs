module MenuSource where 

import Control.Concurrent.Thread.Delay
import System.IO
import FileHandler
import System.Directory
import System.Console.ANSI
import Data.Maybe


isSizeOfTerminalRight = do
    size <- getTerminalSize
    let width = maybe 8 snd size
    if width < 70
        then do
            clearScreen
            setSGR [SetColor Foreground Vivid Red]
            putStrLn $ "\nMake the terminal wider\n"
            setSGR [SetColor Foreground Vivid Yellow]
            checkWindowSize
        else do
            return ()

checkWindowSize = do
    size <- getTerminalSize
    let width = maybe 0 snd size
    if width < 70
        then do
            delay 500000
            checkWindowSize
        else do
            clearScreen
            print_menu



getFilesInfo :: IO ()
getFilesInfo = do
    existense <- doesFileExist (".system/.dirs.csv")
    if existense
        then do
            removeFile (".system/.dirs.csv") 
        else do
            return ()

    existense <- doesFileExist (".system/.files.csv") 
    if existense
        then do
            removeFile (".system/.files.csv") 
        else do
            return ()

    scan_dir "."

print_menu :: IO ()
print_menu = do
    putStr $ "######################################################################" ++ "\n"
    putStr $ "#                                                                    #" ++ "\n"
    putStr $ "#                    Insert one of these options:                    #" ++ "\n"
    putStr $ "#                                                                    #" ++ "\n"
    putStr $ "#                 1 - rescan current dir                             #" ++ "\n"
    putStr $ "#                 2 - print file tree                                #" ++ "\n"
    putStr $ "#                 3 + filename - find doubles by name                #" ++ "\n"
    putStr $ "#                                                                    #" ++ "\n"
    putStr $ "######################################################################" ++ "\n\n"
 

io_handler :: IO b
io_handler = do
    isSizeOfTerminalRight
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
            putStr "Enter path to directory to overlook: \n"
            currDir <- getLine
    
            printFiles currDir currDir currDir " "
            setSGR [SetColor Foreground Vivid  Cyan]
            putStr "\nTo call menu press Enter"
            getLine
            setSGR [SetColor Foreground Vivid Yellow]
            clearScreen
            print_menu

        "3" -> do
            clearScreen
            setSGR [SetColor Foreground Vivid Blue]
            currDir <- getCurrentDirectory
            putStrLn $ currDir 
            printFiles "." "." "." " "
            setSGR [SetColor Foreground Vivid  Cyan]
            putStr "\nTo call menu press Enter"
            getLine
            setSGR [SetColor Foreground Vivid Yellow]
            clearScreen
            print_menu
        
        
        "4" -> do 
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

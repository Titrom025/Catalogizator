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
    existenseDir <- doesFileExist (".system/.dirs.csv")
    existenseFiles <- doesFileExist (".system/.files.csv") 

    if existenseDir
        then do
            if existenseFiles
                then do
                    return ()
                else do
                    removeFile (".system/.dirs.csv")
                    scan_dir "."
        else do
            if existenseFiles
                then do
                    removeFile (".system/.files.csv")
                    scan_dir "."
                else do
                    scan_dir "."

    
    


print_menu :: IO ()
print_menu = do
    putStr $ "######################################################################" ++ "\n"
    putStr $ "#                                                                    #" ++ "\n"
    putStr $ "#                    Insert one of these options:                    #" ++ "\n"
    putStr $ "#                                                                    #" ++ "\n"
    putStr $ "#                 1 - rescan root dir                                #" ++ "\n"
    putStr $ "#                 2 - print file tree for root                       #" ++ "\n"
    putStr $ "#                 3 - print file tree for directory by path          #" ++ "\n"
    putStr $ "#                 4 + filename - find doubles by name                #" ++ "\n"
    putStr $ "#                                                                    #" ++ "\n"
    putStr $ "######################################################################" ++ "\n\n"
 

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
            removeFile (".system/.dirs.csv")
            removeFile (".system/.files.csv")
            getFilesInfo
            print_menu
            io_handler

        "2" -> do
            clearScreen
            getFilesInfo
            setSGR [SetColor Foreground Vivid Blue]
            currDir <- getCurrentDirectory
            putStrLn $ currDir 
            printFiles "." "." " "
            setSGR [SetColor Foreground Vivid  Cyan]
            putStr "\nTo call menu press Enter"
            hFlush stdout
            getLine
            setSGR [SetColor Foreground Vivid Yellow]
            clearScreen
            print_menu
            io_handler

        "3" -> do
            clearScreen
            setSGR [SetColor Foreground Vivid Blue]
            printListOfDirectories
            putStrLn "\nEnter path to directory to overlook:"
            currDirRaw <- getLine 
            let currDir = "./" ++ currDirRaw
    
            printFiles currDir currDir " "
            setSGR [SetColor Foreground Vivid  Cyan]
            putStr "\nTo call menu press Enter"
            hFlush stdout
            getLine
            setSGR [SetColor Foreground Vivid Yellow]
            clearScreen
            print_menu    
            io_handler
        
        "4" -> do 
            putStr "\nInsert filename: "
            hFlush stdout
            filename <- getLine
            findDoublesByName filename
            io_handler

        "5" -> do
            clearScreen
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Catalogizator stopped"
            setSGR [SetColor Foreground Vivid White]
            removeDirectoryRecursive ".system"
            return ()

        otherwise -> do
            clearScreen 
            setSGR [SetColor Foreground Vivid Red]
            putStr "Wrong option.\n\n"
            setSGR [SetColor Foreground Vivid Yellow]
            print_menu
            io_handler



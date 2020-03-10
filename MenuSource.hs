module MenuSource where 

import FileHandler
import System.FilePath ((</>))
import Control.Concurrent.Thread.Delay (delay)
import System.IO (hFlush, stdout)
import System.Directory (getCurrentDirectory, removeDirectoryRecursive, removeFile, doesFileExist, doesDirectoryExist)
import System.Console.ANSI (getTerminalSize, SGR(SetColor), setSGR, clearScreen, 
    ConsoleLayer(Foreground), ColorIntensity(Vivid), Color(Yellow, Red, Green, Blue, Cyan, White))

----- Get terminal size and check for correctness (greater then 70) -----
isSizeOfTerminalRight :: IO ()
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

----- Check teminal size in loop -----
checkWindowSize :: IO ()
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

----- Collect dirs and files info from working directory -----
getFilesInfo :: IO ()
getFilesInfo = do
    currDir <- getCurrentDirectory
    existenseDir <- doesFileExist (currDir  </> ".system" </> ".dirs.csv")
    existenseFiles <- doesFileExist (currDir  </> ".system" </> ".files.csv") 

    if existenseDir
        then do
            if existenseFiles
                then do
                    return ()
                else do
                    removeFile (currDir  </> ".system" </> ".dirs.csv")
                    scan_dir currDir currDir
        else do
            if existenseFiles
                then do
                    removeFile (currDir  </> ".system" </> ".files.csv")
                    scan_dir currDir currDir
                else do
                    scan_dir currDir currDir


print_menu :: IO ()
print_menu = do
    putStr $ "######################################################################" ++ "\n"
    putStr $ "#                                                                    #" ++ "\n"
    putStr $ "#                    Insert one of these options:                    #" ++ "\n"
    putStr $ "#                                                                    #" ++ "\n"
    putStr $ "#                 1 - rescan root dir                                #" ++ "\n"
    putStr $ "#                 2 - print file tree for root                       #" ++ "\n"
    putStr $ "#                 3 + dirpath print file tree for directory by path  #" ++ "\n"
    putStr $ "#                 4 + filename - find doubles by name                #" ++ "\n"
    putStr $ "#                 5 + filepath - find doubles by content             #" ++ "\n"
    putStr $ "#                 6 - stop Catalogizator                             #" ++ "\n"
    putStr $ "#                                                                    #" ++ "\n"
    putStr $ "######################################################################" ++ "\n\n"
 

----- Main cycle of program: scan for op codes -----
io_handler :: IO ()
io_handler = do
    isSizeOfTerminalRight
    putStr "Option: "
    hFlush stdout
    command <- getLine
    currDir <- getCurrentDirectory
 
    case command of 
        "1" -> do 
            clearScreen
            setSGR [SetColor Foreground Vivid Green]
            putStrLn "Rescan complete.\n"
            setSGR [SetColor Foreground Vivid Yellow]
            removeFile (currDir </> ".system" </> ".dirs.csv")
            removeFile (currDir </> ".system" </> ".files.csv")
            getFilesInfo
            print_menu
            io_handler

        "2" -> do
            clearScreen
            getFilesInfo
            setSGR [SetColor Foreground Vivid Blue]
            currDir <- getCurrentDirectory
            putStrLn $ currDir 
            printFiles currDir currDir " " currDir
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
            printListOfDirectories currDir
            setSGR [SetColor Foreground Vivid Cyan]
            putStrLn "\nEnter path to directory to overlook:"
            setSGR [SetColor Foreground Vivid Blue]
            currDirRaw <- getLine 
            let currDirNew = currDir </> currDirRaw

            existanse <- doesDirectoryExist currDirNew
            if existanse && currDirRaw /= ""
                then do
                    putStrLn $ currDirNew
                    if currDirRaw == "." 
                        then do
                            printFiles currDir currDir " " currDir
                        else do 
                            printFiles currDirNew currDirNew " " currDir
                else do
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn "\nEnter correct directory path"

            setSGR [SetColor Foreground Vivid Cyan]
            putStr "\nTo call menu press Enter"
            hFlush stdout
            getLine
            setSGR [SetColor Foreground Vivid Yellow]
            clearScreen
            print_menu    
            io_handler
        
        "4" -> do 
            setSGR [SetColor Foreground Vivid Cyan]
            putStr "\nInsert filename: "
            hFlush stdout
            setSGR [SetColor Foreground Vivid Blue]
            filename <- getLine
            existanse <- doesFileExist filename
            if existanse
                then do
                    findDoublesByName filename currDir
                    putStrLn ""
                else do
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn "\nInsert correct path to file\n"

            setSGR [SetColor Foreground Vivid Cyan]
            putStr "To call menu press Enter"
            hFlush stdout
            getLine
            setSGR [SetColor Foreground Vivid Yellow]
            clearScreen
            print_menu   
            io_handler

        "5" -> do
            setSGR [SetColor Foreground Vivid Cyan]
            putStr "\nInsert path to file: "
            hFlush stdout
            setSGR [SetColor Foreground Vivid Blue]
            path <- getLine
            existanse <- doesFileExist path
            if existanse
                then do
                    hash <- getHash path
                    findDoublesByContent hash currDir
                else do
                    setSGR [SetColor Foreground Vivid Red]
                    putStrLn "\nInsert correct path to file"
                
            setSGR [SetColor Foreground Vivid Cyan]
            putStr "\nTo call menu press Enter"
            hFlush stdout
            getLine
            setSGR [SetColor Foreground Vivid Yellow]
            clearScreen
            print_menu   
            io_handler

        "6" -> do
            clearScreen
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Catalogizator stopped"
            setSGR [SetColor Foreground Vivid White]
            removeDirectoryRecursive $ currDir </> ".system"
            return ()

        otherwise -> do
            clearScreen 
            setSGR [SetColor Foreground Vivid Red]
            putStr "Wrong option.\n\n"
            setSGR [SetColor Foreground Vivid Yellow]
            print_menu
            io_handler



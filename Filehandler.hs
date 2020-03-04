{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, BangPatterns #-}

module FileHandler where

--------- Dir walk ---------
import System.FilePath
import System.Directory

---- Collect files info ----
--import System.Posix.Files
import Data.Time.Clock.POSIX
import Data.Digest.Pure.CRC32

------ Csv read/wrire ------
import Data.Csv
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as LB


import System.IO
----- Create data types ----

data FileInfo = FileInfo FilePath FilePath String deriving (Generic, Eq) 

data DirInfo = DirInfo FilePath FilePath deriving (Generic, Eq)

instance FromRecord FileInfo
instance ToRecord FileInfo

instance FromRecord DirInfo
instance ToRecord DirInfo


getHash :: FilePath -> IO String
getHash file = do
    fileContent <- LB.readFile file
    let crc32Digest = crc32 fileContent
    return $ show crc32Digest
------------------------------

remove2simv (x:y:xs) = "-> " ++ xs


--scan_dir :: FilePath -> IO ()
scan_dir path = do
    isSystemExist <- doesDirectoryExist ".system"

    if isSystemExist
        then do
            return ()
        else do
            createDirectory ".system"


    isDirectory <- doesDirectoryExist path
    if isDirectory
        then do
            files <- getDirectoryContents path
            let list = [".", "..", ".DS_Store", ".git", ".gitignore", ".files.csv", ".system"]
            let nonDotFiles = filter (not . (`elem` list)) files

            LB.appendFile (".system/.dirs.csv") $ encode [DirInfo (takeFileName path) (takeDirectory path)]
            mapM (\file -> scan_dir (path </> file)) nonDotFiles
        
            return ()

        else do
            hash <- getHash path
            LB.appendFile (".system/.files.csv") $ encode [FileInfo (takeFileName path) (takeDirectory path) hash]


collectInfo dirPath dirName = do
    csvFilesData <- LB.readFile (".system/.files.csv")
    csvDirData <- LB.readFile (".system/.dirs.csv")
    
    LB.writeFile (".system/Dirsinfo-" ++ takeFileName dirName ++ ".csv") $  encode [DirInfo "dirName" "dirPath"]
    LB.writeFile (".system/Fileinfo-" ++ takeFileName dirName ++ ".csv") $  encode [FileInfo "fileName" "filePath" "hash"]


    case decode NoHeader csvFilesData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (FileInfo fName fDir hash) ->
            if dirPath == fDir
                then do
                    LB.appendFile (".system/Fileinfo-" ++ takeFileName dirName ++ ".csv") $ encode [FileInfo fName fDir hash]
                else do
                    return ()

    case decode NoHeader csvDirData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (DirInfo dName dDir) ->
            if dirPath == dDir && dirName /= dName 
                then do
                    LB.appendFile (".system/Dirsinfo-" ++ takeFileName dirName ++ ".csv") $  encode [DirInfo dName dDir]
                    LB.appendFile (".system/Fileinfo-" ++ takeFileName dirName ++ ".csv") $  encode [FileInfo dName dDir "0"]
                else do
                    return ()



--printFiles :: [Char] -> FilePath -> [Char] -> IO ()
printFiles dirPath dirName strPath = do
    collectInfo dirPath dirName


    cvsFileInfo <- LB.readFile (".system/Fileinfo-" ++ takeFileName dirName ++ ".csv")
    removeFile (".system/Fileinfo-" ++ takeFileName dirName ++ ".csv")
    case decode NoHeader cvsFileInfo of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ curr@(FileInfo fName fDir hash) ->
            if fDir == (takeDirectory dirPath ++ "/" ++ takeFileName dirName)
                then do 
                    isDir <- doesDirectoryExist (dirPath ++ "/" ++ fName)
                    if isDir
                        then do
                            return ()
                        else 
                            if curr == (V.last v)
                                then do
                                    putStrLn $ strPath ++ "└─── " ++ fName
                                    putStrLn $ strPath
                                else do
                                    putStrLn $ strPath ++ "├─── " ++ fName
                else do
                    return ()

    csvDirInfo <- LB.readFile (".system/Dirsinfo-" ++ takeFileName dirName ++ ".csv")
    removeFile (".system/Dirsinfo-" ++ takeFileName dirName ++ ".csv")

    case decode NoHeader csvDirInfo of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ curr@(DirInfo dName dDir) ->
            if dirPath == dDir && dirName /= dName 
                then do
                    if curr == (V.last v)
                        then do
                            collectInfo (dDir ++ "/" ++ dName) dName
                            cvsFileInfo <- LB.readFile (".system/Fileinfo-" ++ dName ++ ".csv")
                            removeFile (".system/Fileinfo-" ++ dName ++ ".csv")

                            if show cvsFileInfo == "\"fileName,filePath,hash\\r\\n\""
                                then do
                                    putStrLn $ strPath ++ "└─── " ++ dName
                                else do
                                    putStrLn $ strPath ++ "└──┬ " ++ dName

                            printFiles (dDir ++ "/" ++ dName) dName (strPath ++ "   ")
                        else do
                            putStrLn $ strPath ++ "├──┬ " ++ dName
                            printFiles (dDir ++ "/" ++ dName) dName (strPath ++ "│  ")
                else do
                    return ()
   


printListOfDirectories = do
    csvData <- LB.readFile ".system/.dirs.csv"
    putStrLn "Available dirs:"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (DirInfo name path) ->
            putStrLn $ remove2simv $ path </> name

                


findDoublesByName :: FilePath -> IO ()
findDoublesByName search_name = do
    csvData <- LB.readFile ".system/.files.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (FileInfo name dir hash) ->
            if name == search_name
                then do
                    putStrLn $ "File: " ++ name ++ ", Dir: " ++ dir ++ ", Hash: " ++ hash
                else do
                    return ()



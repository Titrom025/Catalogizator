{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

module FileHandler where

--------- Dir walk ---------
import System.FilePath
import System.Directory

---- Collect files info ----
import System.Posix.Files
import Data.Time.Clock.POSIX
import Data.Digest.Pure.CRC32

------ Csv read/wrire ------
import Data.Csv
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as LB

----- Create data types ----

data FileInfo = FileInfo FilePath FilePath String deriving (Generic, Eq) 

data DirInfo = DirInfo FilePath FilePath deriving (Generic, Eq)

instance FromRecord FileInfo
instance ToRecord FileInfo

instance FromRecord DirInfo
instance ToRecord DirInfo

--buildGraph path = foldl (\(x:xs) y -> [x] ++ "    " ++ xs) "1|-- " (init $ splitDirectories path)

--graphConvertor path file = buildGraph path ++ file

--deleteSimv (x:y:path) = [x] ++ path


--remove2simv (x:y:xs) = xs
--remove2simv (x:_) = [x] 


------- Grab file info -------
getFSize :: FilePath -> IO String
getFSize path = getFileStatus path >>= \s -> return $ show $ fileSize s

getFTime :: FilePath -> IO String
getFTime path = getFileStatus path >>= \s -> return $ show $ posixSecondsToUTCTime $ modificationTimeHiRes s

getHash :: FilePath -> IO String
getHash file = do
    fileContent <- LB.readFile file
    let crc32Digest = crc32 fileContent
    return $ show crc32Digest
------------------------------



scan_dir :: FilePath -> IO ()
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

            LB.appendFile ".system/.dirs.csv" $ encode [DirInfo (takeFileName path) (takeDirectory path)]
            mapM (\file -> scan_dir (path </> file)) nonDotFiles
        
            return ()

        else do
            hash <- getHash path
            LB.appendFile ".system/.files.csv" $ encode [FileInfo (takeFileName path) (takeDirectory path) hash]

printFiles :: [Char] -> FilePath -> [Char] -> IO ()
printFiles dirPath dirName strPath = do
    csvFilesData <- LB.readFile ".system/.files.csv"
    csvDirData <- LB.readFile ".system/.dirs.csv"
    
    LB.writeFile (".system/Dirsinfo-" ++ dirName ++ ".csv") $  encode [DirInfo "dirName" "dirPath"]
    LB.writeFile (".system/Fileinfo-" ++ dirName ++ ".csv") $  encode [FileInfo "fileName" "filePath" "hash"]

    case decode NoHeader csvFilesData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (FileInfo fName fDir hash) ->
            if dirPath == fDir
                then do
                    LB.appendFile (".system/Fileinfo-" ++ dirName ++ ".csv") $ encode [FileInfo fName fDir hash]
                else do
                    return ()

    case decode NoHeader csvDirData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (DirInfo dName dDir) ->
            if dirPath == dDir && dirName /= dName 
                then do
                    LB.appendFile (".system/Dirsinfo-" ++ dirName ++ ".csv") $  encode [DirInfo dName dDir]
                    LB.appendFile (".system/Fileinfo-" ++ dirName ++ ".csv") $  encode [FileInfo dName dDir "0"]
                else do
                    return ()

    cvsFileInfo <- LB.readFile (".system/Fileinfo-" ++ dirName ++ ".csv")
    removeFile (".system/Fileinfo-" ++ dirName ++ ".csv")

    case decode NoHeader cvsFileInfo of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ curr@(FileInfo fName fDir hash) ->
            if fDir == (takeDirectory dirPath ++ "/" ++ dirName)
                then do 
                    isDir <- doesDirectoryExist (dirPath ++ "/" ++ fName)
                    if isDir
                        then do
                            return ()
                        else 
                            if curr == (V.last v)
                                then do
                                    putStrLn $ strPath ++ "└── " ++ fName
                                    putStrLn $ strPath
                                else do
                                    putStrLn $ strPath ++ "├── " ++ fName
                else do
                    return ()

    csvDirInfo <- LB.readFile (".system/Dirsinfo-" ++ dirName ++ ".csv")
    removeFile (".system/Dirsinfo-" ++ dirName ++ ".csv")

    case decode NoHeader csvDirInfo of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ curr@(DirInfo dName dDir) ->
            if dirPath == dDir && dirName /= dName 
                then do
                    if curr == (V.last v)
                        then do
                            putStrLn $ strPath ++ "└── " ++ dName
                            printFiles (dDir ++ "/" ++ dName) dName (strPath ++ "    ")
                        else do
                            putStrLn $ strPath ++ "├── " ++ dName
                            printFiles (dDir ++ "/" ++ dName) dName (strPath ++ "│   ")
                else do
                    return ()
   

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



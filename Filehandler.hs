{-# LANGUAGE DeriveGeneric #-}

module FileHandler where

----- Dir walk -----
import System.FilePath (takeFileName, takeDirectory, (</>))
import System.Directory (removeFile, doesDirectoryExist, doesDirectoryExist,
    getFileSize, createDirectory, getDirectoryContents)

------ Csv read/wrire ------
import System.IO (openFile, IOMode(ReadMode))
import qualified Data.ByteString as B (append, hGetSome, ByteString, readFile)
import qualified Data.ByteString.Lazy as LB (readFile, appendFile, writeFile)

----- Work with .csv file info -----
import GHC.Generics (Generic)
import qualified Data.Vector as V (forM_, last)
import Data.Csv (FromRecord, ToRecord, encode, decode, HasHeader(NoHeader))

----- Hash calculation -----
import Crypto.Hash (SHA1, Digest, hash)
import GHC.IO.Handle (HandlePosn(HandlePosn), HandlePosn, hSetPosn, hFileSize)

import System.Console.ANSI (SGR(SetColor), setSGR, 
    ConsoleLayer(Foreground), ColorIntensity(Vivid), Color(Blue, Cyan))


----- Create data types ----
data FileInfo = FileInfo FilePath FilePath String Integer deriving (Generic, Eq) 
data DirInfo = DirInfo FilePath FilePath deriving (Generic, Eq)

instance FromRecord FileInfo
instance ToRecord FileInfo

instance FromRecord DirInfo
instance ToRecord DirInfo


----- Get file hash -----
sha1 :: B.ByteString -> Digest SHA1
sha1 = hash


getHash :: FilePath -> IO String
getHash file = do
    fileContent <- LB.readFile file
    fileHandle <- openFile file ReadMode
    fileSize <- hFileSize fileHandle
    if fileSize < 10000
        then do
            fileContent <- B.readFile file
            return $ show $ sha1 fileContent
        else do
            fileBeg <- B.hGetSome fileHandle 4096
            handleSize <- hFileSize fileHandle
            hSetPosn (HandlePosn fileHandle (handleSize - 4096))
            fileEnd <- B.hGetSome fileHandle 4096
            return $ show $ sha1 (B.append fileBeg fileEnd)


----- change "./" to "-> " in path -----
remove2simv :: [Char] -> [Char]
remove2simv (x:y:xs) = "-> " ++ xs
remove2simv (x:_) = "-> " ++ [x]


----- Replace absolute path to relative -----
replacePath :: Foldable t => t a -> [Char] -> [Char]
replacePath removeDir dir = "." ++ drop (length removeDir) dir


----- Scan directory recursive and get list of dirs and files -----
scan_dir :: FilePath -> FilePath -> IO ()
scan_dir path currDir = do
    isDirectory <- doesDirectoryExist path
    if isDirectory
        then do
            files <- getDirectoryContents path
            let list = [".", "..", ".DS_Store", ".git", ".gitignore", ".files.csv", ".system"]
            let nonDotFiles = filter (not . (`elem` list)) files
            LB.appendFile (currDir </> ".system" </> ".dirs.csv") $ encode [DirInfo (takeFileName path) (takeDirectory path)]
            mapM (\file -> scan_dir (path </> file) currDir) nonDotFiles
            return ()
        else do
            hash <- getHash path
            size <- getFileSize path
            LB.appendFile (currDir </> ".system" </> ".files.csv") $ encode [FileInfo (takeFileName path) (takeDirectory path) hash size]


----- Get content list of directory from .system files----- 
collectInfo :: FilePath -> [Char] -> FilePath -> IO ()
collectInfo dirPath dirName currDir = do
    csvFilesData <- LB.readFile (currDir </> ".system" </> ".files.csv")
    csvDirData <- LB.readFile (currDir </> ".system" </> ".dirs.csv")
    LB.writeFile (currDir </> ".system" </> "Dirsinfo-" ++ dirName ++ ".csv") $  encode [DirInfo "dirName" "dirPath"]
    LB.writeFile (currDir </> ".system" </> "Fileinfo-" ++ dirName ++ ".csv") $  encode [FileInfo "fileName" "filePath" "hash" 0]

    case decode NoHeader csvFilesData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (FileInfo fName fDir hash size) ->
            if dirPath == fDir
                then do
                    LB.appendFile (currDir </> ".system" </> "Fileinfo-" ++ dirName ++ ".csv") $ encode [FileInfo fName fDir hash size]
                else do
                    return ()

    case decode NoHeader csvDirData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (DirInfo dName dDir) ->
            if dirPath == dDir && dirName /= dName 
                then do
                    LB.appendFile (currDir </> ".system" </> "Dirsinfo-" ++ dirName ++ ".csv") $  encode [DirInfo dName dDir]
                    LB.appendFile (currDir </> ".system" </> "Fileinfo-" ++ dirName ++ ".csv") $  encode [FileInfo dName dDir "0" 0]
                else do
                    return ()


----- Print recursive file tree of directory -----
printFiles :: FilePath -> FilePath -> [Char] -> FilePath -> IO ()
printFiles dirPath dirName strPath currDir = do
    collectInfo dirPath (takeFileName dirName) currDir

    cvsFileInfo <- LB.readFile (currDir </> ".system" </> "Fileinfo-" ++ takeFileName dirName ++ ".csv")
    removeFile (currDir </> ".system" </> "Fileinfo-" ++ takeFileName dirName ++ ".csv")
    
    case decode NoHeader cvsFileInfo of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ curr@(FileInfo fName fDir hash size) ->
            if fDir == (takeDirectory dirPath </> takeFileName dirName) || fDir == dirPath
                then do 
                    isDir <- doesDirectoryExist (dirPath </> fName)
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

    csvDirInfo <- LB.readFile (currDir </> ".system" </> "Dirsinfo-" ++ takeFileName dirName ++ ".csv")
    removeFile (currDir </> ".system" </> "Dirsinfo-" ++ takeFileName dirName ++ ".csv")

    case decode NoHeader csvDirInfo of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ curr@(DirInfo dName dDir) ->
            if dirPath == dDir && takeFileName dirName /= dName 
                then do
                    collectInfo (dDir </> dName) (takeFileName dName) currDir
                    cvsFileInfo <- LB.readFile (currDir </> ".system" </> "Fileinfo-" ++ takeFileName dName ++ ".csv")
                    removeFile (currDir </> ".system" </> "Fileinfo-" ++ takeFileName dName ++ ".csv")
                    if curr == (V.last v)
                        then do
                            if show cvsFileInfo == "\"fileName,filePath,hash,0\\r\\n\""
                                then do
                                    putStrLn $ strPath ++ "└──< " ++ dName
                                else do
                                    putStrLn $ strPath ++ "└──┬ " ++ dName

                            printFiles (dDir </> dName) dName (strPath ++ "   ") currDir
                        else do
                            if show cvsFileInfo == "\"fileName,filePath,hash,0\\r\\n\""
                                then do
                                    putStrLn $ strPath ++ "├──< " ++ dName
                                    printFiles (dDir </> dName) dName (strPath ++ "│  ") currDir
                                else do
                                    putStrLn $ strPath ++ "├──┬ " ++ dName
                                    printFiles (dDir </> dName) dName (strPath ++ "│  ") currDir
                else do
                    return ()
   

printListOfDirectories :: FilePath -> IO ()
printListOfDirectories currDir = do
    csvData <- LB.readFile $ currDir </> ".system" </> ".dirs.csv"
    setSGR [SetColor Foreground Vivid Cyan]
    putStrLn "Available dirs:\n"
    setSGR [SetColor Foreground Vivid Blue]
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (DirInfo name path) ->
            putStrLn $ remove2simv (replacePath currDir (path </> name))
      

findDoublesByName :: FilePath -> FilePath -> IO ()
findDoublesByName search_name currDir = do
    csvData <- LB.readFile $ currDir </> ".system" </> ".files.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (FileInfo name dir hash size) ->
            if name == search_name
                then do
                    putStrLn $ "Dir: \""  ++ (replacePath currDir dir) ++ "\", Size: " ++ (show size)
                else do
                    return ()


findDoublesByContent :: String -> FilePath -> IO ()
findDoublesByContent soughtHash currDir = do
    csvData <- LB.readFile $ currDir </> ".system" </> ".files.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (FileInfo name dir hash size) ->
            if soughtHash == hash
                then do
                    putStrLn $ "Name: " ++ name ++ "\", Dir: \""  ++ (replacePath currDir dir)
                else do
                    return ()
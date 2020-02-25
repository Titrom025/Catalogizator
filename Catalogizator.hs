{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

module FileHandler where

------
import System.IO
import System.FilePath
import System.Directory
import System.Posix.Files
import Data.Time.Clock.POSIX
------
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.CRC32
------
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import GHC.Generics
------

data FileInfo = FileInfo FilePath FilePath String deriving Generic 

instance FromRecord FileInfo
instance ToRecord FileInfo


changePathToGraph path = foldl (\(x:xs) y -> [x] ++ " " ++ xs) "||-- " (init path)

checkIfInRoot (x:y:path) = if y == '|'
                            then [x] ++ path
                            else [x] ++ [y] ++ path

remove2simv (x:y:xs) = xs
remove2simv (x:_) = [x] 

fileConverter path = (changePathToGraph $ takeDirectory path) ++ takeFileName path


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
    isDirectory <- doesDirectoryExist path
    if isDirectory
        then do
            files <- getDirectoryContents path
            let list = [".", "..", ".DS_Store", ".git", ".gitignore", "files.csv"]
            let nonDotFiles = filter (not . (`elem` list)) files
            --putStrLn $ checkIfInRoot $ fileConverter path
            mapM (\file -> scan_dir (path </> file)) nonDotFiles
            return ()

        else do
            hash <- getHash path
            BL.appendFile "files.csv" $ encode [FileInfo (takeFileName path) (takeDirectory path) hash]
            --putStrLn $ checkIfInRoot $ fileConverter path

findDoublesByName :: FilePath -> IO ()
findDoublesByName search_name = do
    csvData <- BL.readFile "files.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (FileInfo name dir hash) ->
            if name == search_name
                then do
                    putStrLn $ "File: " ++ name ++ ", Dir: " ++ dir ++ ", Hash: " ++ hash
                else do
                    return ()



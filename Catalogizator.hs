-- import Path.IO
import System.FilePath
import System.Directory
import System.Posix.Files
import Data.Time.Clock.POSIX

import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.CRC32

--findFile

changePathToGraph path = foldl (\(x:xs) y -> [x] ++ " " ++ xs) "||-- " (init path)

checkIfInRoot (x:y:path) = if y == '|'
							then [x] ++ path
							else [x] ++ [y] ++ path

remove2simv (x:y:xs) = xs
remove2simv (x:_) = [x] 

fileConverter path = (changePathToGraph $ takeDirectory path) ++ takeFileName path


------- Grab file info -------

getFSize path = getFileStatus path >>= \s -> return $ fileSize s

getFTime path = getFileStatus path >>= \s -> return $ posixSecondsToUTCTime $ modificationTimeHiRes s

getHash file = do
    fileContent <- LB.readFile file
    let crc32Digest = crc32 fileContent
    return crc32Digest

buildFileInfo file = do
	hash <- getHash file
	size <- getFSize file
	time <- getFTime file
	return (takeFileName file, hash, size, time)

------------------------------

dir_walk path filefunc dirfunc = do
  isDirectory <- doesDirectoryExist path
  
  if isDirectory
    then do
      files <- getDirectoryContents path
      let nonDotFiles = filter (not . (`elem` [".", "..", ".DS_Store"])) files

      putStrLn $ checkIfInRoot $ fileConverter path

      results <- mapM (\file -> dir_walk (path </> file) filefunc dirfunc) nonDotFiles

      putStr "|\n"

    else do
      hash <- getHash path

      info <- buildFileInfo path
      --print info
      putStrLn $ checkIfInRoot $ fileConverter path ++ "   / Hash: " ++ show hash 



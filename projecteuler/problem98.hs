
import Data.Char (digitToInt)
import System.IO

-- Read file with words
readLines = do
        handle <- openFile "words.txt" ReadMode
        list <- readWords handle []
        hClose handle
		

readWords :: Handle -> [String] -> IO [String]
readWords handle list = do
        isEOF <- hIsEOF handle
        if isEOF
        then return(list)
        else do
                line <- hGetLine handle
                readWords handle (line:list)


-- Encode characters with numbers

-- 

main = do
	readLines


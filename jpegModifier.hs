import Data.Char
import System.Environment
import System.IO

checkSOI (x:[]) = False
checkSOI (x:x':_) = x == 0xFF && x' == 0xD8

checkEOI (x:[]) = False
checkEOI xs = checkEOI' $ reverse xs
    where
        checkEOI' (x:x':_) = x == 0xD9 && x' == 0xFF

jpgCheck xs = checkSOI xs && checkEOI xs

main = do
        args <- getArgs
        if null args
            then putStrLn "Error! Usage : jpgCheck filename"
            else do
                h <- openBinaryFile (head args) ReadMode
                cs <- hGetContents h
                print $ jpgCheck $ map ord cs
                if jpgCheck $ map ord cs
import qualified Data.ByteString as B
import System.Environment
import System.IO

main = do
  args <- getArgs
  if null args
    then putStrLn "Error! Usage : jpgCheck filename"
    else do
    contents <- B.readFile (head args)
    if jpgCheck $ B.unpack contents then
      putStrLn "input jpeg file was valid!"
      else do
      B.writeFile ((head args) ++ ".modified.jpeg") $ (if checkSOI $ B.unpack contents then \x->x else addSOI) ( ( if checkEOI $ B.unpack contents then \x->x else addEOI ) contents)
      putStrLn $ "output as" ++  ((head args) ++ ".modified.jpeg")

checkSOI (x:[]) = False
checkSOI (x:x':_) = x == 0xFF && x' == 0xD8

checkEOI (x:[]) = False
checkEOI xs = checkEOI' $ reverse xs
    where
        checkEOI' (x:x':_) = x == 0xD9 && x' == 0xFF

jpgCheck xs = checkSOI xs && checkEOI xs

addSOI xs = B.cons 0xFF $ B.cons 0xD8 xs

addEOI xs = B.snoc (B.snoc xs  0xFF) 0xD9

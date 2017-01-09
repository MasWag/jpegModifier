import qualified Data.ByteString as B
import System.Environment
import System.IO

main = do
  args <- getArgs
  if null args
    then putStrLn "Error! Usage : jpgCheck filename"
    else do
    contents <- B.readFile (head args)
    if jpgCheck contents then
      putStrLn "input jpeg file was valid!"
      else do
      B.writeFile ((head args) ++ ".modified.jpeg") $ (if checkSOI contents then \x->x else addSOI) ( ( if checkEOI contents then \x->x else addEOI ) contents)
      putStrLn $ "output as " ++  ((head args) ++ ".modified.jpeg")

checkSOI xs = ( B.head xs ) == 0xFF && (B.head ( B.tail xs)) == 0xD8
checkEOI xs = ( B.last xs ) == 0xD9 && (B.last ( B.init xs)) == 0xFF

jpgCheck xs = checkSOI xs && checkEOI xs

addSOI xs = B.cons 0xFF $ B.cons 0xD8 xs

addEOI xs = B.snoc (B.snoc xs  0xFF) 0xD9

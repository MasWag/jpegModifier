import qualified Data.ByteString as B
import System.Environment

main :: IO()
main = do
  args <- getArgs
  if null args then
    putStrLn "Error! Usage : jpgCheck filename"
  else do
    contents <- B.readFile (head args)
    if jpgCheck contents then
      putStrLn "input jpeg file was valid!"
    else do
      B.writeFile ((head args) ++ ".modified.jpeg") $ (ifFunc checkSOI addSOI) $ (ifFunc checkEOI addEOI) contents
      putStrLn $ "output as " ++  ((head args) ++ ".modified.jpeg")

ifFunc :: (a -> Bool) -> (a -> a) -> a -> a
ifFunc cond morph src 
    | cond src = morph src
    | otherwise = src
                        

checkSOI :: B.ByteString -> Bool
checkSOI xs = ( B.head xs ) == 0xFF && (B.head ( B.tail xs)) == 0xD8

checkEOI :: B.ByteString -> Bool
checkEOI xs = ( B.last xs ) == 0xD9 && (B.last ( B.init xs)) == 0xFF

jpgCheck :: B.ByteString -> Bool
jpgCheck xs = checkSOI xs && checkEOI xs

addSOI :: B.ByteString -> B.ByteString
addSOI xs = B.cons 0xFF $ B.cons 0xD8 xs

addEOI :: B.ByteString -> B.ByteString
addEOI xs = B.snoc (B.snoc xs  0xFF) 0xD9

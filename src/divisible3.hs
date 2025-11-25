import Text.Read (readMaybe)

main :: IO Bool
main = readInteger >>= divisible3

readInteger :: IO Int
readInteger = do
    line <- getLine
    let maybeNum = readMaybe line
    maybe readInteger return maybeNum

divisible3 :: Int -> IO Bool
divisible3 num = do 
    if num `mod` 3 == 0 
    then print True >> return True 
    else print False >> return False



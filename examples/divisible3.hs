-- reads an integer from stdin and prints whether it is divisible by 3.
-- this is the system learned by `io.hs`. build it from the project root with
--   ghc examples/divisible3.hs
main :: IO ()
main = do
    n <- readLn :: IO Integer
    print (n `mod` 3 == 0)

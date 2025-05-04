module agda.BlackBox where

class SUL i o where
    step :: SUL i o -> i -> (SUL i o, o)
    reset :: SUL i o -> SUL i o


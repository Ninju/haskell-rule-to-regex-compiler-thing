module Main where
import PatternParser (parsePattern)
import Control.Monad (forM_)

main :: IO ()
main = forM_ patterns $ \pattern -> putStrLn $ pattern ++ " -> " ++ show (parsePattern pattern)

patterns :: [String]
patterns = [ "AAAA",
             "AAA_#####",
             "AAA_#######_##",
             "A####_@@",
             "??_AAA#A_#",
             "@@@@_####_????" ]

module PatternParser (parsePattern) where

type Flags = String
type RegexPattern = String
data Regex = Regex RegexPattern Flags

instance Show Regex where
  show (Regex content flags) = "/" ++ content ++ "/" ++ flags

parsePattern :: String -> Regex
parsePattern xs = Regex (parsePatternChars xs) "gi"

parsePatternChars :: String -> RegexPattern
parsePatternChars []     = ""
parsePatternChars (x:xs) = regexPattern ++ parsePatternChars rest
                           where
                           (matches, rest) = span (x==) xs
                           regexPattern = reducedRegexPattern (parsePatternChar x) (length matches + 1)

parsePatternChar :: Char -> RegexPattern
parsePatternChar c = case c of
                      'A' -> "[a-z]"
                      '#' -> "\\d"
                      '@' -> "[a-z0-9]"
                      '?' -> "."
                      '_' -> "[\\s_-]"
                      _   -> [c]

reducedRegexPattern :: RegexPattern -> Int -> RegexPattern
reducedRegexPattern regexPattern 1 = regexPattern
reducedRegexPattern regexPattern n = regexPattern ++ "{" ++ show n ++ "," ++ show n ++ "}"


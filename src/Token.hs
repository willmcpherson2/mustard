module Token (Token(..), mkTokens) where

data Token
  = Space
  | Newline
  | Open
  | Close
  | Other String
  deriving (Show)

mkTokens :: String -> [Token]
mkTokens = joinOthers . mapTokens

mapTokens :: String -> [Token]
mapTokens = map mapToken
  where
    mapToken ch = case ch of
      ' ' -> Space
      '\t' -> Space
      '\n' -> Newline
      '\r' -> Newline
      '(' -> Open
      ')' -> Close
      ch -> Other [ch]

joinOthers :: [Token] -> [Token]
joinOthers = foldr join []
  where
    join (Other l) (Other r : tokens) = Other (l ++ r) : tokens
    join token tokens = token : tokens

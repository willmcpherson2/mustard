module Error (Error(..), Fallible) where

data Error
  = UnexpectedOp
  | ExpectedItem
  | ExpectedVal
  | ExpectedType
  | ExpectedCtor
  | ExpectedLam
  | ExpectedPat
  | ExpectedLower
  | ExpectedUpper
  | InvalidPath
  deriving Show

type Fallible a = Either Error a

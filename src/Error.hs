module Error (Error(..), Fallible) where

data Error
  = InvalidName
  | UnexpectedOp
  | ExpectedItem
  | ExpectedBinder
  | ExpectedVal
  | ExpectedType
  | ExpectedCtor
  | ExpectedAlt
  | ExpectedPat
  | ExpectedLower
  | ExpectedUpper
  deriving Show

type Fallible a = Either Error a

module Error (Error(..), Fallible) where

data Error
  = UnexpectedOp
  | ExpectedItem
  | ExpectedBinder
  | ExpectedVal
  | ExpectedType
  | ExpectedCtor
  | ExpectedLam
  | ExpectedPat
  | ExpectedLower
  | ExpectedUpper
  | ExpectedLowerBinder
  | ExpectedUpperBinder
  deriving Show

type Fallible a = Either Error a

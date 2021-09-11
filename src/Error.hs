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
  | TypeConflict
  deriving (Show, Eq)

type Fallible a = Either Error a

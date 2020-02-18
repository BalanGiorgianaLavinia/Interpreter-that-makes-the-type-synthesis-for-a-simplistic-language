module InferenceDataType
where

-- TDA-ul utiliat pentru a reprezenta o expresie
data Expr = Va String | FCall String String [Expr] deriving (Show)

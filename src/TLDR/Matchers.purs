module TLDR.Matchers where

data Any

data Literal :: Symbol -> Type
data Literal literal

data MatchAZ
data Match09
data Matchaz
data MatchAlpha
data MatchAlphanumeric
data MatchAZ09
data Matchaz09
data MatchHex
data MatchWhitespace

data Match2 :: Type -> Type
data Match2 match

data Match3 :: Type -> Type
data Match3 match

data Match4 :: Type -> Type
data Match4 match

data Match5 :: Type -> Type
data Match5 match

data Match6 :: Type -> Type
data Match6 match

data Match7 :: Type -> Type
data Match7 match

data Match8 :: Type -> Type
data Match8 match

data Match9 :: Type -> Type
data Match9 match

data Some :: Type -> Type
data Some match

data Many :: Type -> Type
data Many match

-- no yes
data Except :: Type -> Type -> Type
data Except disallowed allowed

data Or :: Type -> Type -> Type
data Or left right

infixr 2 type Or as |||

data And :: Type -> Type -> Type
data And left right

infixr 2 type And as &&&

data Noop

data EOF

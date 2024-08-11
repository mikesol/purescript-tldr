module TLDR.List where

data Cons :: Type -> Type -> Type
data Cons head tail

data Nil

class Append :: Type -> Type -> Type -> Constraint
class Append l1 l2 l3 | l1 l2 -> l3

instance Append Nil l l
else instance Append l Nil l
else instance (Append tail l3 l4) => Append (Cons head tail) l3 (Cons head l4)

infixr 6 type Cons as :

module TLDR.Combinators where

import TLDR.List (Cons, Nil)
import TLDR.Matchers (Noop)

data IfThen :: Type -> Type -> Type
data IfThen condition action

data ModifyStateAfterSuccessOnConstant :: Type -> Type -> Type
data ModifyStateAfterSuccessOnConstant constant cont

data ModifyStateAfterSuccessWithResult :: (Type -> Type) -> Type -> Type
data ModifyStateAfterSuccessWithResult functor cont

data BranchOnState :: Type -> Type -> Type
data BranchOnState branches otherwise

--

data Or :: Type -> Type -> Type
data Or left right

infixr 2 type Or as ||

data IgnoreAndThenParse :: Type -> Type -> Type
data IgnoreAndThenParse ignore parse

infixl 4 type IgnoreAndThenParse as $>

data ParseAndThenIgnore :: Type -> Type -> Type
data ParseAndThenIgnore parse ignore

infixl 4 type ParseAndThenIgnore as <$

data Some :: Type -> Type
data Some combinator

data Many :: Type -> Type
data Many combinator

data Const :: Type -> Type
data Const tp

data Fix :: Row Type -> Type
data Fix selfs

type SepBy1 a sep = Cons a (Many (IgnoreAndThenParse sep a))
type SepBy a sep = Or (SepBy1 a sep) (Const Nil)
type SepEndBy1 a sep = (ParseAndThenIgnore (SepBy1 a sep) (Or sep Noop))
type SepEndBy a sep = (ParseAndThenIgnore (SepBy a sep) (Or sep Noop))
type EndBy1 a sep = (ParseAndThenIgnore (SepBy1 a sep) sep)
type EndBy a sep = (ParseAndThenIgnore (SepBy a sep) sep)

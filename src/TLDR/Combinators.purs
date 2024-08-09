module TLDR.Combinators where

import TLDR.List (Cons, Nil)
import TLDR.Matchers (Noop)

data IfThen :: Type -> Type -> Type
data IfThen condition action

data ModifyStateBeforeOnConstant :: Type -> Type -> Type
data ModifyStateBeforeOnConstant constant cont

data ModifyStateAfterSuccessOnConstant :: Type -> Type -> Type
data ModifyStateAfterSuccessOnConstant constant cont

data ModifyStateAfterSuccessWithResult :: (Type -> Type) -> Type -> Type
data ModifyStateAfterSuccessWithResult f cont

data BranchOnState :: Type -> Type -> Type
data BranchOnState branches otherwise

--

data Or :: Type -> Type -> Type
data Or left right

data IgnoreAndThenParse :: Type -> Type -> Type
data IgnoreAndThenParse ignore parse

data ParseAndThenIgnore :: Type -> Type -> Type
data ParseAndThenIgnore parse ignore

data Some :: Type -> Type
data Some combinator

data Many :: Type -> Type
data Many combinator

data Const :: Type -> Type
data Const tp

data Fix :: Type -> Type -> Type
data Fix self combinator
type SepBy1 a sep = Cons a (Many (IgnoreAndThenParse sep a))
type SepBy a sep = Or (SepBy1 a sep) (Const Nil)
type SepEndBy1 a sep = (ParseAndThenIgnore (SepBy1 a sep) (Or sep Noop))
type SepEndBy a sep = (ParseAndThenIgnore (SepBy a sep) (Or sep Noop))
type EndBy1 a sep = (ParseAndThenIgnore (SepBy1 a sep) sep)
type EndBy a sep = (ParseAndThenIgnore (SepBy a sep) sep)

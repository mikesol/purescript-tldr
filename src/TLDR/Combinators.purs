module TLDR.Combinators where

data IfThen :: Type -> Type -> Type
data IfThen condition action

data ModifyStateBeforeOnConstant :: Type -> Type -> Type -> Type
data ModifyStateBeforeOnConstant constant  state  cont

data ModifyStateAfterSuccessOnConstant :: Type -> Type -> Type -> Type
data ModifyStateAfterSuccessOnConstant  constant state cont

data ModifyStateAfterFailureOnConstant :: Type -> Type -> Type -> Type
data ModifyStateAfterFailureOnConstant constant state cont

data ModifyStateAfterWithResult :: Type -> Type -> Type -> Type
data ModifyStateAfterWithResult result state cont

data BranchOnState :: Type -> Type
data BranchOnState branches

data Or :: Type -> Type -> Type
data Or left right

data IgnoreAndThenParse :: Type -> Type -> Type
data IgnoreAndThenParse ignore parse

data ParseAndThenIgnore :: Type -> Type -> Type
data ParseAndThenIgnore parse ignore

data NoConstructorMatch :: Type -> Type -> Type
data NoConstructorMatch match representation

data Some :: Type -> Type
data Some combinator

data Many :: Type -> Type
data Many combinator

data SomeWithSeparator :: Type -> Type -> Type
data SomeWithSeparator sep combinator

data ManyWithSeparator :: Type -> Type -> Type
data ManyWithSeparator sep combinator

data Const :: Type -> Type
data Const tp

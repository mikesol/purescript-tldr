module TLDR.Result
  ( SingleFailure
  , Success
  , Failure
  ) where

import Prim.TypeError (Doc)

-- use this level of indirection in case we for whatever
-- reason want to wrap failure in something or extend it to multiple
-- failures
type SingleFailure :: Doc -> Doc
type SingleFailure a = a

data Success :: forall k. k -> Symbol -> Type
data Success parsed remaining

data Failure :: Doc -> Type
data Failure a
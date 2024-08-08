module Test.Readme where

import Prelude

import TLDR.Combinators (IgnoreAndThenParse)
import TLDR.Combinators.Class (class Parse)
import TLDR.Matchers (Match2, Match3, MatchWhitespace, Matchaz, Some)

data FooBarBazQux :: Type -> Type -> Type -> Type -> Type
data FooBarBazQux foo bar baz qux

u
  :: forall @a
   . Parse "hello world"
       ( FooBarBazQux
           (Match3 Matchaz)
           (Match2 Matchaz)
           (IgnoreAndThenParse (Some MatchWhitespace) (Match2 Matchaz))
           (Match3 Matchaz)
       )
       Unit
       a
       Unit
  => Unit
u = unit

-- z = u @(Failure _) :: Unit
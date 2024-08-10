module Test.Readme where

import Prelude

import TLDR.Combinators (IgnoreAndThenParse)
import TLDR.Combinators.Class (class FailOnFail, class Parse, class ShowParser, SP4)
import TLDR.Matchers (Match2, Match3, MatchWhitespace, Matchaz, Some)

data FooBarBazQux :: Type -> Type -> Type -> Type -> Type
data FooBarBazQux foo bar baz qux

instance
  ShowParser (SP4 "FooBarBazQux" foo bar baz qux) doc =>
  ShowParser (FooBarBazQux foo bar baz qux) doc

x
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
  => Unit -> Unit
x _ = unit

-- y _ = x @(Failure _)

a
  :: forall @a
   . Parse "hell0 world"
       ( FooBarBazQux
           (Match3 Matchaz)
           (Match2 Matchaz)
           (IgnoreAndThenParse (Some MatchWhitespace) (Match2 Matchaz))
           (Match3 Matchaz)
       )
       Unit
       a
       Unit
  => FailOnFail a
  => Unit -> Unit
a _ = unit

-- b _ = a

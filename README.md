# purescript-tldr

A parser of type-level domain representations.

## Why?

As LLMs get better at writing in DSLs, from HTML to SQL to GraphQL, they've become a fast and relatively error-free way to develop software.

Traditional ORMs, on the other hand, act as a typing layer between a DSL and an application.

This library aims to bridge that gap, providing the copy-and-paste-ability of LLM-generated DSL code with the type-safety of an ORM.

## How it works

Let's write a parser for a subset of JSON, called MySON.

The full example is below.

```purescript
module Test.Readme where

import Prelude

import Prim.TypeError (Text)
import TLDR.Combinators (ParseAndThenIgnore)
import TLDR.Combinators as C
import TLDR.Combinators.Class (class FailOnFail, class Parse, class ShowParser, SP1, SP2, SP4)
import TLDR.Matchers as M
import TLDR.Sugar (Bracket, DQ, L4, L5, WS, WSM, DQM)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

data MySONFix

instance ShowParser MySONFix (Text "MySONFix")

data MyInt a

instance ShowParser (SP1 "MyInt" a) doc => ShowParser (MyInt a) doc

data MyString a

instance ShowParser (SP1 "MyString" a) doc => ShowParser (MyString a) doc

data MyBoolean a

instance ShowParser (SP1 "MyBoolean" a) doc => ShowParser (MyBoolean a) doc

data MyArray a

instance ShowParser (SP1 "MyArray" a) doc => ShowParser (MyArray a) doc

data MyObjectEntry k v

instance ShowParser (SP2 "MyObjectEntry" a b) doc => ShowParser (MyObjectEntry a b) doc

data MyObject a

instance ShowParser (SP1 "MyObject" a) doc => ShowParser (MyObject a) doc

type ParseInt = WS (MyInt (M.Some M.Match09))

type ParseString = WS (DQ (MyString (M.Some M.MatchAlphanumeric)))

type ParseBoolean = WS
  $ MyBoolean
  $ M.Or (L4 "t" "r" "u" "e") (L5 "f" "a" "l" "s" "e")

type ParseArray a = WS
  $ Bracket (M.Literal "[")
      (MyArray (C.SepBy a (WSM (M.Literal ","))))
      (M.Literal "]")

type ObjectPair (a :: Type) = WS
  $ MyObjectEntry
      ( ParseAndThenIgnore
          (WS (DQ (M.Some M.MatchAlphanumeric)))
          (M.Literal ":")
      )
      a

type ParseObject a = WS
  $ Bracket (M.Literal "{")
      (MyObject (C.SepBy (ObjectPair a) (WSM (M.Literal ","))))
      (M.Literal "}")

type MySON = WS
  $ C.Fix MySONFix
  $ C.Or ParseInt
  $ C.Or ParseString
  $ C.Or ParseBoolean
  $ C.Or (ParseArray MySONFix) (ParseObject MySONFix)

myson0' :: forall a. Parse "1" MySON Unit a Unit => Unit -> Proxy a
myson0' _ = Proxy

myson0 = myson0' unit

myson1'
  :: forall a
   . Parse
       """[1,"true",false]"""
       MySON
       Unit
       a
       Unit
  => Unit
  -> Proxy a
myson1' _ = Proxy

myson1 = myson1' unit

myson2'
  :: forall a
   . Parse
       """
  {"username":"mikesol", "pw":1234, "tags":["bored", "tired"] }
  """
       MySON
       Unit
       a
       Unit
  => Unit
  -> Proxy a
myson2' _ = Proxy

myson2 = myson2' unit
```

Now, when we hover over the ide for the definitions of `myson0`, `myson1`, and `myson2`, we get the following:

```bash
Proxy (Success (MyInt (Proxy "1")) "")

Proxy (Success (MyArray (Cons (MyInt (Proxy "1")) (Cons (MyString (Proxy "true")) (Cons (MyBoolean (Proxy "false")) Nil)))) "")

Proxy (Success (MyObject (Cons (MyObjectEntry (Proxy "username") (MyString (Proxy "mikesol"))) (Cons (MyObjectEntry (Proxy "pw") (MyInt (Proxy "1234"))) (Cons (MyObjectEntry (Proxy "tags") (MyArray (Cons (MyString (Proxy "bored")) (Cons (MyString (Proxy "tired")) Nil)))) Nil)))) "")
```

From there, you can consume your typelevel ADT in downstream constraints.

## Errors

When parsing fails, we can reasonably nice error messages using the `FailOnFail` class. Otherwise, the type variable `a` can carry the failure forward for downstream processing.

Take for example:

```purescript
mysonFail'
  :: forall a
   . Parse
       """{1,"true",false}"""
       MySON
       Unit
       a
       Unit
  => FailOnFail a
  => Unit
  -> Proxy a
mysonFail' _ = Proxy

mysonFail = mysonFail' unit
```

You get:

```bash
  Custom error:

    Could not match } against 1
```
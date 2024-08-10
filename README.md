# purescript-tldr

A parser of type-level domain representations.

## Why?

As LLMs get better at writing in DSLs, from JSON to HTML to SQL to GraphQL, they've become a fast and relatively error-free way to develop software.

Many libraries, like ORMs, act as a typing layer between a DSL and an application.

This library aims to bridge that gap, providing the copy-and-paste-ability of LLM-generated DSL code with the type-safety of an ORM.

## How it works

Let's examine how `purescript-tldr` works by creating a parser for a subset of JSON, called MySON. It's like JSON, but has no floats and strings can only be alphanumeric.

### Data types

We'll first create `data` types that our data will parse to. For example, `1` and `42` will parse to `MyInt (Proxy "1")` and `MyInt (Proxy "42")`.

```purescript
data MyInt a

instance ShowParser (SP1 "MyInt" a) doc => ShowParser (MyInt a) doc
```

### ShowParser

In the `MyInt`, we saw that `ShowParser` is used to document our parser. This must be done for every data type to which you parse, otherwise parsing will fail. That's because this library is intended to provide the most helpful parsing errors possible, and that can't be done without rich error messages for any `data` it parses.

A convenient way to do this is to use `SP1`, `SP2` ... based on the arity of the type, which will generate a sensible automatic representation.

### Matching

After defining our data types, we match them against whatever we want to parse. In the example below, we use `WS` to ignore optional whitespace around an int, and then match against at least one value between 0 and 9.

```purescript
type ParseInt = WS (MyInt (M.Some M.Match09))
```

We'll go over all of the matchers and combinators in the library later.

### Full example

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

## Visualizing errors

When parsing fails, we can print a reasonably nice error messages using the `FailOnFail` class. Otherwise, the type variable `a` can carry the failure forward for downstream processing.

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

## API

The generated documentation for type-level libraries on [Pursuit](https://pursuit.purescript.org) is notoriously difficult to read. So here's the API. It's split into `Parse`, matchers, combinators, and sugar.

- `class Parse` is how you parse a typelevel string.
- `TLDR.Matchers` constains roughly the same stuff as `Parsing.String`.
- `TLDR.Combinators` contains roughyl the same stuff as `Parsing.Combinators`.
- `TLDR.Sugar` contains some stuff I was too lazy to write out by hand over and over.

### Parse

As seen in the MySON example, the signature of `Parse` is:

```purescript
Parse sym combinator inState result outState
```

We'll see how to use state in a bit. In the MySON example, it's just `Unit` and it isn't touched.

### Matchers

| Matcher       | What it does  |
| ------------- | ------------- |
| `Match09` | Matches a char in the range 0-9  |
| `Matchaz` | Matches a char in the range a-z  |
| `MatchAZ` | Matches a char in the range A-Z |
| `MatchAlpha` | Matches a char in the range a-zA-Z |
| `MatchAlphanumeric` | Matches a char in the range a-zA-Z0-0 |
| `Matchaz09` | Matches a char in the range a-z0-9 |
| `MatchAZ09` | Matches a char in the range A-Z0-9 |
| `MatchHex` | Matches a char in the range a-f0-9 |
| `MatchWhitespace` | Matches a whitespace char |
| `Literal char` | Matches a literal. Will fail on anything longer than 1 char! |
| `Match2 match` | Matches two consective chars matching `match` |
| `Match3 match` | Matches three consective chars matching `match` |
| `Match4 match` | Matches four consective chars matching `match` |
| `Match5 match` | Matches five consective chars matching `match` |
| `Match6 match` | Matches six consective chars matching `match` |
| `Match7 match` | Matches seven consective chars matching `match` |
| `Match8 match` | Matches eight consective chars matching `match` |
| `Match9 match` | Matches nine consective chars matching `match` |
| `Many match` | Matches 0 or more `match` |
| `Some match` | Matches 1 or more `match` |
| `Except no yes` | Matches `yes` _except_ anything matching to `no` |
| `Or left right` | Matches either `left` or `right` |
| `And left right` | Consumes `left`, then `right`, and concatenates them |
| `Noop` | Doesn't match anything, no input is consumed |
| `EOF` | Matches `""` |
| `Any` | Matches any single char |


Note that PureScript typelevel strings don't have a proper `Char` representation, so when I use "char" above, I mean any `head` that is produced by `Symbol.Cons head tail string`. For example, `Symbol.Cons head tail "hello"` produces `"h"` for `head`.

### Combinators

Combinators take matchers and use them to populate our data types.

At the most basic level, any data type that accepts `Type` arguments and with a `ShowParser` instance can be a combinator. For example, `MyBoolean` is a combinator below.

```purescript
 type ParseBoolean = MyBoolean $ M.Or (L4 "t" "r" "u" "e") (L5 "f" "a" "l" "s" "e")
```

In a term-level parsing library, you'd achieve a similar result with mapping, ie:

```purescript
parseBoolean = MyBoolean <$> or (l4 "t" "r" "u" "e") (l5 "f" "a" "l" "s" "e")
```

Here are the combinators. `Fix` is a mind-melt, so there'll be a whole section on that in a bit.


| Combinator       | What it does  |
| ------------- | ------------- |
| `IgnoreAndThenParse ignore parse` | Ignore _matcher_ `ignore` and then parse _combinator_ `parse`. |
| `ParseAndThenIgnore parse ignore` | Parse _combinator_ `parse` and then ignore _matcher_ `ignore`. |
| `Many a` | Accumulates 0 or more of combinator `a` into a typelevel list. |
| `Some a` | Accumulates 1 or more of combinator `a` into a typelevel list. |
| `Const a` | Consume no input and output `a`. |
| `Or left right` | Parses to either `left` or `right` |
| `SepBy a sep` | Parse phrases delimited by a separator. |
| `SepBy1 a sep` | Parse phrases delimited by a separator, requiring at least one match. |
| `SepEndBy a sep` | Parse phrases delimited and optionally terminated by a separator. |
| `SepEndBy1 a sep` | Parse phrases delimited and optionally terminated by a separator, requiring at least one match. |
| `EndBy a sep` | Parse phrases delimited and terminated by a separator. |
| `EndBy1 a sep` | Parse phrases delimited and terminated by a separator, requiring at least one match. |
| `Fix self a` | A fixed point. See below. |

#### Fix

Fixed points are essential in parsing because they allow you to parse arbitrarily nested structures. The MySON example above uses one, and most parsing tasks eventually requires a fixed point.

The way `Fix self a` works is that you define a type for `self`. In the MySON example, it's `MySONFix`. Then, in `a`, you refer to `MySONFix` whenever you want to parse `a`. This is how objects and arrays are expressed in the `MySON` example.

#### State management

There are three state-management combinators for keeping and using an internal state.

| Combinator       | What it does  |
| ------------- | ------------- |
| `ModifyStateAfterSuccessOnConstant constant combinator` | Modify the state upon a successful parsing outcome with a constant value. |
| `ModifyStateAfterSuccessWithResult functor combinator` | Modify the state upon a successful parsing outcome with the result of the parsing operation as the argument to `functor`. |
| `BranchOnState branches otherwise` | Branch to different parsers depending on the internal state. |

There are examples of all four of these in the [tests](./test/Test/Parser.purs).

In general, state management should be considered an advanced feature. Most often, you can do it in a second step. For example, in XML parsing, you can use state management to determine if open tags are closed. But you can also do this in a second step after successfully parsing to a data type. While the latter is slower, it's easier to read and implement.

#### Gotchyas

1. Some combinators have the same names as data types. For example, `Or` in matcher-land matches either of two symbols, whereas `Or` in combinator-land matches either of two data types.
2. Some combinators take other combinators, some take matchers, and some like `SepBy` take both - a combinator for the thing to match and a matcher for the separator. Getting this wrong can lead to funky error messages.

### Sugar

What fun would life be without syntactic sugar? Probably healthier and cavity-free. But still, we can indulge ourselves from time to time with these useful, delectable tidbits.

| Sugar       | What it does  |
| ------------- | ------------- |
| `WS combinator` | Allows for optional whitespace before and after a combinator. |
| `WSM match` | Allows for optional whitespace before and after a match. |
| `DQ combinator` | Puts a combinator in double quotes. |
| `DQM match` | Puts a match in double quotes. |
| `Bracket left combinator right` | Brackets a combinator between two ignorable matches. |
| `L1` ... `L10` | Matches a literal with 1 to 10 arguments, ie `L4 "t" "r" "u" "e"`. |
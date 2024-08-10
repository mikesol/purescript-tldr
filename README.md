# purescript-tldr

A parser of type-level domain representations.

Let's say you do

```purescript
data FooBarBazQux :: Type -> Type -> Type -> Type -> Type
data FooBarBazQux foo bar baz qux

instance
  ShowParser (SP4 "FooBarBazQux" foo bar baz qux) doc =>
  ShowParser (FooBarBazQux foo bar baz qux) doc

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

y _ = x @(Failure _)
```

You'll get the following error:

```bash
  Could not match type

    Success (FooBarBazQux (Proxy "hel") (Proxy "lo") (Proxy "wo") (Proxy "rld"))

  with type

    Failure
```

The library generates success responses for consumption in downstream constraints.

## Error messages

The library prints formatted error messages showing exactly where the parsing went wrong. You can print this with the `FailOnFail` class.

The parsing would likely fail anyway because of a failed match against `Success` in your downstream constraint, but by using `FailOnFail`, you get the nice error message.

```purescript
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

b _ = a
```

This yields:

```bash
  Custom error:

     FooBarBazQux
      hel
       Failed to match on matcher And a-za-z
         IgnoreAndThenParse
          Some Whitespace
          Two a-z
        Three a-z


while solving type class constraint

  TLDR.Combinators.Class.FailOnFail t0
```

We can see that it parses `hel` but then fails on the second matcher because of the `0` and does not get to the third.
# purescript-tldr

A parser of type-level domain representations.

Let's say you do

```purescript
data FooBarBazQux :: Type -> Type -> Type -> Type -> Type
data FooBarBazQux foo bar baz qux

u
  :: forall @a
   . Parse "hello world"
       ( FooBarBazQux
           (Match3 Matchaz)
           (Match2 Matchaz)
           (IgnoreAndThenParse
             (Some MatchWhitespace)
             (Match2 Matchaz)
           )
           (Match3 Matchaz)
       )
       Unit
       a
       Unit
  => Unit
u = unit

z = u @(Failure _) :: Unit
```

You'll get the following error:

```bash
  Could not match type

    Success (FooBarBazQux (Proxy "hel") (Proxy "lo") (Proxy "wo") (Proxy "rld"))

  with type

    Failure
```

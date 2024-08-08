module Test.Parser where

import Prelude

import Prim.TypeError (Text)
import TLDR.Combinators (IgnoreAndThenParse)
import TLDR.Combinators as C
import TLDR.Combinators.Class (class Parse, class ShowParser, SP2, SP4)
import TLDR.Matchers (Literal, Match09, Match3, MatchAZ, Matchaz, Or, Some)
import TLDR.Result (Failure, Success)
import Type.Proxy (Proxy)

data Qux

instance ShowParser Qux (Text "Qux")

data FooBar :: Type -> Type -> Type
data FooBar foo bar

instance ShowParser (SP2 "FooBar" a b) doc => ShowParser (FooBar a b) doc

testMatchFooBarSuccess :: forall @toParse @h @t. Parse toParse (FooBar (MatchAZ) (Match09)) Unit (Success h t) Unit => Unit
testMatchFooBarSuccess = unit

testMatchFooBarFailure
  :: forall @toParse
   . Parse toParse (FooBar (MatchAZ) (Match09)) Unit
       ( Failure _
       )
       Unit
  => Unit
testMatchFooBarFailure = unit

testMatchFooBarSuccess0 = testMatchFooBarSuccess @"F1a" @(FooBar (Proxy "F") (Proxy "1")) @"a" :: Unit
testMatchFooBarFailure0 = testMatchFooBarFailure @"q1a" :: Unit

-- these tests below should fail to compile, check periodically
-- testMatchFooBarSuccess0' = testMatchFooBarSuccess @"q1a" @(FooBar (Proxy "F") (Proxy "1")) @"a" :: Unit
-- testMatchFooBarFailure0' = testMatchFooBarFailure @"F1a" :: Unit

data FooBarBazQux :: Type -> Type -> Type -> Type -> Type
data FooBarBazQux foo bar baz qux

instance ShowParser (SP4 "FooBarBazQux" a b c d) doc => ShowParser (FooBarBazQux a b c d) doc

testMatchFooBarBazQuxSuccess :: forall @toParse @h @t. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (Match3 Matchaz) (Or (Some Match09) (Some Matchaz)) (Literal "M")) Unit (Success h t) Unit => Unit
testMatchFooBarBazQuxSuccess = unit

testMatchFooBarBazQuxFailure
  :: forall @toParse
   . Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (Match3 Matchaz) (Or (Some Match09) (Some Matchaz)) (Literal "M")) Unit
       ( Failure _
       )
       Unit
  => Unit
testMatchFooBarBazQuxFailure = unit

testMatchFooBarBazQuxSuccess0 = testMatchFooBarBazQuxSuccess @"F1zqr12M" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Proxy "zqr") (Proxy "12") (Proxy "M")) @"" :: Unit
testMatchFooBarBazQuxSuccess1 = testMatchFooBarBazQuxSuccess @"I2mmmarfM" @(FooBarBazQux (FooBar (Proxy "I") (Proxy "2")) (Proxy "mmm") (Proxy "arf") (Proxy "M")) @"" :: Unit
testMatchFooBarBazQuxFailure0 = testMatchFooBarBazQuxFailure @"n2mmmarfM" :: Unit
testMatchFooBarBazQuxFailure1 = testMatchFooBarBazQuxFailure @"F1zqr12N" :: Unit

-- these tests below should fail to compile, check periodically
-- testMatchFooBarBazQuxSuccess0' = testMatchFooBarBazQuxSuccess @"F1zqr12N" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Proxy "zqr") (Proxy "12") (Proxy "M")) @"" :: Unit
-- testMatchFooBarBazQuxSuccess1' = testMatchFooBarBazQuxSuccess @"n2mmmarfM" @(FooBarBazQux (FooBar (Proxy "I") (Proxy "2")) (Proxy "mmm") (Proxy "arf") (Proxy "M")) @"" :: Unit
-- testMatchFooBarBazQuxFailure0' = testMatchFooBarBazQuxFailure @"F1zqr12M" :: Unit
-- testMatchFooBarBazQuxFailure1' = testMatchFooBarBazQuxFailure @"I2mmmarfM" :: Unit

testMatchFooBarBazQuxWithIgnoreSuccess :: forall @toParse @h @t. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (IgnoreAndThenParse (Some MatchAZ) (Match3 Matchaz)) (Or (Some Match09) (Some Matchaz)) (Literal "M")) Unit (Success h t) Unit => Unit
testMatchFooBarBazQuxWithIgnoreSuccess = unit

testMatchFooBarBazQuxWithIgnoreFailure
  :: forall @toParse
   . Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (IgnoreAndThenParse (Some MatchAZ) (Match3 Matchaz)) (Or (Some Match09) (Some Matchaz)) (Literal "M")) Unit
       ( Failure _
       )
       Unit
  => Unit
testMatchFooBarBazQuxWithIgnoreFailure = unit

testMatchFooBarBazQuxWithIgnoreSuccess0 = testMatchFooBarBazQuxWithIgnoreSuccess @"F1IGNOREzqr12M" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Proxy "zqr") (Proxy "12") (Proxy "M")) @"" :: Unit
testMatchFooBarBazQuxWithIgnoreSuccess1 = testMatchFooBarBazQuxWithIgnoreSuccess @"I2HELLOmmmarfM" @(FooBarBazQux (FooBar (Proxy "I") (Proxy "2")) (Proxy "mmm") (Proxy "arf") (Proxy "M")) @"" :: Unit
testMatchFooBarBazQuxWithIgnoreFailure0 = testMatchFooBarBazQuxWithIgnoreFailure @"I2mmmarfM" :: Unit
testMatchFooBarBazQuxWithIgnoreFailure1 = testMatchFooBarBazQuxWithIgnoreFailure @"F11zqr12M" :: Unit

testMatchOrSuccess :: forall @toParse @h @t. Parse toParse (C.Or (FooBarBazQux (FooBar (MatchAZ) (Match09)) (Match3 Matchaz) (Or (Some Match09) (Some Matchaz)) (Literal "M")) (FooBar Match09 Matchaz)) Unit (Success h t) Unit => Unit
testMatchOrSuccess = unit

testMatchOrFailure :: forall @toParse. Parse toParse (C.Or (FooBarBazQux (FooBar (MatchAZ) (Match09)) (Match3 Matchaz) (Or (Some Match09) (Some Matchaz)) (Literal "M")) (FooBar Match09 Matchaz)) Unit (Failure _) Unit => Unit
testMatchOrFailure = unit

testMatchOrSuccess0 = testMatchOrSuccess @"0za" @(FooBar (Proxy "0") (Proxy "z")) @"a" :: Unit
testMatchOrSuccess1 = testMatchOrSuccess @"F1zqr12M" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Proxy "zqr") (Proxy "12") (Proxy "M")) @"" :: Unit
testMatchOrFailure0 = testMatchOrFailure @"zza" :: Unit
testMatchOrFailure1 = testMatchOrFailure @"F1zqr12Q" :: Unit

-- these tests below should fail to compile, check periodically
-- testMatchOrSuccess0' = testMatchOrSuccess @"F1zqr12Q" @(FooBar (Proxy "0") (Proxy "z")) @"a" :: Unit
-- testMatchOrSuccess1' = testMatchOrSuccess @"zza" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Proxy "zqr") (Proxy "12") (Proxy "M")) @"" :: Unit
-- testMatchOrFailure0' = testMatchOrFailure  @"F1zqr12M" :: Unit
-- testMatchOrFailure1' = testMatchOrFailure @"0za" :: Unit

testMatchConstSuccess :: forall @toParse @h @t. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (C.Const Qux) (Or (Some Match09) (Some Matchaz)) (Literal "M")) Unit (Success h t) Unit => Unit
testMatchConstSuccess = unit

testMatchConstFailure :: forall @toParse. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (C.Const Qux) (Or (Some Match09) (Some Matchaz)) (Literal "M")) Unit (Failure _) Unit => Unit
testMatchConstFailure = unit

testMatchConstSuccess0 = testMatchConstSuccess @"F1zzM" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) Qux (Proxy "zz") (Proxy "M")) @"" :: Unit
testMatchConstSuccess1 = testMatchConstSuccess @"I23333M" @(FooBarBazQux (FooBar (Proxy "I") (Proxy "2")) Qux (Proxy "3333") (Proxy "M")) @"" :: Unit
testMatchConstFailure0 = testMatchConstFailure @"F10zzM" :: Unit
testMatchConstFailure1 = testMatchConstFailure @"I23x33M" :: Unit
-- these tests below should fail to compile, check periodically
-- testMatchConstSuccess0' = testMatchConstSuccess @"I23x33M" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) Qux (Proxy "zz") (Proxy "M")) @"" :: Unit
-- testMatchConstSuccess1' = testMatchConstSuccess @"F10zzM" @(FooBarBazQux (FooBar (Proxy "I") (Proxy "2")) Qux (Proxy "3333") (Proxy "M")) @"" :: Unit
-- testMatchConstFailure0' = testMatchConstFailure @"I23333M" :: Unit
-- testMatchConstFailure1' = testMatchConstFailure @"F1zzM" :: Unit

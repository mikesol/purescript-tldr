module Test.Parser where

import Prelude

import Prim.TypeError (Text)
import TLDR.Combinators (IgnoreAndThenParse, ModifyStateAfterSuccessOnConstant, ModifyStateAfterSuccessWithResult, ParseAndThenIgnore)
import TLDR.Combinators as C
import TLDR.Combinators.Class (class ModifyState, class Parse, class ShowParser, SP1, SP2, SP4)
import TLDR.List (Cons, Nil)
import TLDR.List as L
import TLDR.Matchers (Literal, Match09, Match3, MatchAZ, MatchWhitespace, Matchaz, Or, Some)
import TLDR.Matchers as M
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

testMatchFooBarBazQuxWithIgnore'Success :: forall @toParse @h @t. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (ParseAndThenIgnore (Match3 Matchaz) (Some MatchAZ)) (Or (Some Match09) (Some Matchaz)) (Literal "M")) Unit (Success h t) Unit => Unit
testMatchFooBarBazQuxWithIgnore'Success = unit

testMatchFooBarBazQuxWithIgnore'Failure
  :: forall @toParse
   . Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (ParseAndThenIgnore (Match3 Matchaz) (Some MatchAZ)) (Or (Some Match09) (Some Matchaz)) (Literal "M")) Unit
       ( Failure _
       )
       Unit
  => Unit
testMatchFooBarBazQuxWithIgnore'Failure = unit

testMatchFooBarBazQuxWithIgnore'Success0 = testMatchFooBarBazQuxWithIgnore'Success @"F1zqrIGNORE12M" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Proxy "zqr") (Proxy "12") (Proxy "M")) @"" :: Unit
testMatchFooBarBazQuxWithIgnore'Success1 = testMatchFooBarBazQuxWithIgnore'Success @"I2mmmHELLOarfM" @(FooBarBazQux (FooBar (Proxy "I") (Proxy "2")) (Proxy "mmm") (Proxy "arf") (Proxy "M")) @"" :: Unit
testMatchFooBarBazQuxWithIgnore'Failure0 = testMatchFooBarBazQuxWithIgnore'Failure @"I2mmmarfM" :: Unit
testMatchFooBarBazQuxWithIgnore'Failure1 = testMatchFooBarBazQuxWithIgnore'Failure @"F11zqr12M" :: Unit

-- testMatchFooBarBazQuxManySuccess :: forall @toParse @h @t. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (ParseAndThenIgnore  (IgnoreAndThenParse (Some MatchWhitespace) (C.Many (FooBar (Match3 Matchaz) (Some MatchAZ)))) (Some MatchWhitespace)) (Or (Some Match09) (Some Matchaz)) (Literal "Z")) Unit (Success h t) Unit => Unit
testMatchFooBarBazQuxManySuccess :: forall @toParse @h @t. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (C.Or (ParseAndThenIgnore (IgnoreAndThenParse (Some MatchWhitespace) (C.Many (FooBar (Match3 Matchaz) (Some MatchAZ)))) (Some MatchWhitespace)) (IgnoreAndThenParse (Some MatchWhitespace) (C.Const Nil))) (Or (Some Match09) (Some Matchaz)) (Literal "Z")) Unit (Success h t) Unit => Unit
testMatchFooBarBazQuxManySuccess = unit

testMatchFooBarBazQuxManyFailure :: forall @toParse. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (IgnoreAndThenParse MatchWhitespace (C.Many (FooBar (Match3 Matchaz) (Some MatchAZ)))) (IgnoreAndThenParse (Some MatchWhitespace) (Or (Some Match09) (Some Matchaz))) (Literal "Z")) Unit (Failure _) Unit => Unit
testMatchFooBarBazQuxManyFailure = unit

testMatchFooBarBazQuxManySuccess0 = testMatchFooBarBazQuxManySuccess @"F1 bbbAzbzAAqrsABCD 12Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Cons (FooBar (Proxy "bbb") (Proxy "A")) (Cons (FooBar (Proxy "zbz") (Proxy "AA")) (Cons (FooBar (Proxy "qrs") (Proxy "ABCD")) Nil))) (Proxy "12") (Proxy "Z")) @"" :: Unit
testMatchFooBarBazQuxManySuccess1 = testMatchFooBarBazQuxManySuccess @"F1 12Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) Nil (Proxy "12") (Proxy "Z")) @"" :: Unit
testMatchFooBarBazQuxManyFailure0 = testMatchFooBarBazQuxManyFailure @"F1 bbb 12Z" :: Unit
testMatchFooBarBazQuxManyFailure1 = testMatchFooBarBazQuxManyFailure @"F1 bbbAzbzAAqrs2 12Z" :: Unit

-- these tests below should fail to compile, check periodically
-- testMatchFooBarBazQuxManySuccess0' = testMatchFooBarBazQuxManySuccess @"F1 bbbAzbzAAqrs2 12Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Cons (FooBar (Proxy "bbb") (Proxy "A")) (Cons (FooBar (Proxy "zbz") (Proxy "AA")) (Cons (FooBar (Proxy "qrs") (Proxy "ABCD")) Nil))) (Proxy "12") (Proxy "Z")) @"" :: Unit
-- testMatchFooBarBazQuxManySuccess1' = testMatchFooBarBazQuxManySuccess @"F1 bbb 12Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) Nil (Proxy "12") (Proxy "Z")) @"" :: Unit
-- testMatchFooBarBazQuxManyFailure0' = testMatchFooBarBazQuxManyFailure @"F1  12Z" :: Unit
-- testMatchFooBarBazQuxManyFailure1' = testMatchFooBarBazQuxManyFailure @"F1 bbbAzbzAAqrsABCD 12Z" :: Unit

testMatchFooBarBazQuxSomeSuccess :: forall @toParse @h @t. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (IgnoreAndThenParse MatchWhitespace (C.Some (FooBar (Match3 Matchaz) (Some MatchAZ)))) (IgnoreAndThenParse (Some MatchWhitespace) (Or (Some Match09) (Some Matchaz))) (Literal "Z")) Unit (Success h t) Unit => Unit
testMatchFooBarBazQuxSomeSuccess = unit

testMatchFooBarBazQuxSomeFailure :: forall @toParse. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (IgnoreAndThenParse MatchWhitespace (C.Some (FooBar (Match3 Matchaz) (Some MatchAZ)))) (IgnoreAndThenParse (Some MatchWhitespace) (Or (Some Match09) (Some Matchaz))) (Literal "Z")) Unit (Failure _) Unit => Unit
testMatchFooBarBazQuxSomeFailure = unit

testMatchFooBarBazQuxSomeSuccess0 = testMatchFooBarBazQuxSomeSuccess @"F1 bbbAzbzAAqrsABCD 12Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Cons (FooBar (Proxy "bbb") (Proxy "A")) (Cons (FooBar (Proxy "zbz") (Proxy "AA")) (Cons (FooBar (Proxy "qrs") (Proxy "ABCD")) Nil))) (Proxy "12") (Proxy "Z")) @"" :: Unit
testMatchFooBarBazQuxSomeSuccess1 = testMatchFooBarBazQuxSomeSuccess @"F1 abcD 12Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Cons (FooBar (Proxy "abc") (Proxy "D")) Nil) (Proxy "12") (Proxy "Z")) @"" :: Unit
testMatchFooBarBazQuxSomeFailure0 = testMatchFooBarBazQuxSomeFailure @"F1 bbb 12Z" :: Unit
testMatchFooBarBazQuxSomeFailure1 = testMatchFooBarBazQuxSomeFailure @"F1 bbbAzbzAAqrs2 12Z" :: Unit

-- these tests below should fail to compile, check periodically
-- testMatchFooBarBazQuxSomeSuccess0' = testMatchFooBarBazQuxSomeSuccess @"F1 bbbAzbzAAqrs2 12Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Cons (FooBar (Proxy "bbb") (Proxy "A")) (Cons (FooBar (Proxy "zbz") (Proxy "AA")) (Cons (FooBar (Proxy "qrs") (Proxy "ABCD")) Nil))) (Proxy "12") (Proxy "Z")) @"" :: Unit
-- testMatchFooBarBazQuxSomeSuccess1' = testMatchFooBarBazQuxSomeSuccess @"F1 bbb 12Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) Nil (Proxy "12") (Proxy "Z")) @"" :: Unit
-- testMatchFooBarBazQuxSomeFailure0' = testMatchFooBarBazQuxSomeFailure @"F1 abcD 12Z" :: Unit
-- testMatchFooBarBazQuxSomeFailure1' = testMatchFooBarBazQuxSomeFailure @"F1 bbbAzbzAAqrsABCD 12Z" :: Unit

testMatchFooBarBazQuxSepBySuccess :: forall @toParse @h @t. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (C.Or (ParseAndThenIgnore (IgnoreAndThenParse (Some MatchWhitespace) (C.SepBy (FooBar (Match3 Matchaz) (Some MatchAZ)) (Literal ","))) (Some MatchWhitespace)) (IgnoreAndThenParse (Some MatchWhitespace) (C.Const Nil))) (Or (Some Match09) (Some Matchaz)) (Literal "Z")) Unit (Success h t) Unit => Unit
testMatchFooBarBazQuxSepBySuccess = unit

testMatchFooBarBazQuxSepByFailure :: forall @toParse. Parse toParse (FooBarBazQux (FooBar (MatchAZ) (Match09)) (C.Or (ParseAndThenIgnore (IgnoreAndThenParse (Some MatchWhitespace) (C.SepBy (FooBar (Match3 Matchaz) (Some MatchAZ)) (Literal ","))) (Some MatchWhitespace)) (IgnoreAndThenParse (Some MatchWhitespace) (C.Const Nil))) (Or (Some Match09) (Some Matchaz)) (Literal "Z")) Unit (Failure _) Unit => Unit
testMatchFooBarBazQuxSepByFailure = unit

testMatchFooBarBazQuxSepBySuccess0 = testMatchFooBarBazQuxSepBySuccess @"F1 bbbA,zbzAA,qrsABCD 12Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Cons (FooBar (Proxy "bbb") (Proxy "A")) (Cons (FooBar (Proxy "zbz") (Proxy "AA")) (Cons (FooBar (Proxy "qrs") (Proxy "ABCD")) Nil))) (Proxy "12") (Proxy "Z")) @"" :: Unit
testMatchFooBarBazQuxSepBySuccess1 = testMatchFooBarBazQuxSepBySuccess @"F1 12Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) Nil (Proxy "12") (Proxy "Z")) @"" :: Unit
testMatchFooBarBazQuxSepByFailure0 = testMatchFooBarBazQuxSepByFailure @"F1 bbbAzbzAAqrsABCD 12Z" :: Unit
testMatchFooBarBazQuxSepByFailure1 = testMatchFooBarBazQuxSepByFailure @"F112Z" :: Unit
-- these tests below should fail to compile, check periodically
-- testMatchFooBarBazQuxSepBySuccess0' = testMatchFooBarBazQuxSepBySuccess @"F112Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) (Cons (FooBar (Proxy "bbb") (Proxy "A")) (Cons (FooBar (Proxy "zbz") (Proxy "AA")) (Cons (FooBar (Proxy "qrs") (Proxy "ABCD")) Nil))) (Proxy "12") (Proxy "Z")) @"" :: Unit
-- testMatchFooBarBazQuxSepBySuccess1' = testMatchFooBarBazQuxSepBySuccess @"F1 bbbAzbzAAqrsABCD 12Z" @(FooBarBazQux (FooBar (Proxy "F") (Proxy "1")) Nil (Proxy "12") (Proxy "Z")) @"" :: Unit
-- testMatchFooBarBazQuxSepByFailure0' = testMatchFooBarBazQuxSepByFailure @"F1 12Z" :: Unit
-- testMatchFooBarBazQuxSepByFailure1' = testMatchFooBarBazQuxSepByFailure @"F1 bbbA,zbzAA,qrsABCD 12Z" :: Unit

data MyState0
data MyState1
data MyConst0

instance ModifyState sym MyConst0 MyState0 MyState1

--
testMatchModifyStateAfterSuccessOnConstantSuccess :: forall @toParse @h @t. Parse toParse (FooBar (ModifyStateAfterSuccessOnConstant MyConst0 (MatchAZ)) (Match09)) MyState0 (Success h t) MyState1 => Unit
testMatchModifyStateAfterSuccessOnConstantSuccess = unit

testMatchModifyStateAfterSuccessOnConstantFailure
  :: forall @toParse
   . Parse toParse (FooBar (ModifyStateAfterSuccessOnConstant MyConst0 (MatchAZ)) (Match09)) MyState0
       ( Failure _
       )
       MyState0
  => Unit
testMatchModifyStateAfterSuccessOnConstantFailure = unit

testMatchModifyStateAfterSuccessOnConstantSuccess0 = testMatchModifyStateAfterSuccessOnConstantSuccess @"F1a" @(FooBar (Proxy "F") (Proxy "1")) @"a" :: Unit

-- this will fail to compile, as it should
-- testMatchModifyStateAfterSuccessOnConstantFailure0 = testMatchModifyStateAfterSuccessOnConstantFailure @"q1a" :: Unit

--

data MyFunctor0 :: Type -> Type
data MyFunctor0 a

data MyFunctor1 :: Type -> Type
data MyFunctor1 a

instance ModifyState sym (MyFunctor0 (Proxy a)) MyState0 (MyFunctor1 (Proxy a))

testMatchModifyStateAfterSuccessWithResultSuccess :: forall @toParse @r @h @t. Parse toParse (FooBar (ModifyStateAfterSuccessWithResult MyFunctor0 (MatchAZ)) (Match09)) MyState0 (Success h t) (MyFunctor1 r) => Unit
testMatchModifyStateAfterSuccessWithResultSuccess = unit

testMatchModifyStateAfterSuccessWithResultFailure
  :: forall @toParse h t. Parse toParse (FooBar (ModifyStateAfterSuccessWithResult MyFunctor0 (MatchAZ)) (Match09)) MyState0 (Success h t) (MyFunctor1 (Proxy "Z")) => Unit
testMatchModifyStateAfterSuccessWithResultFailure = unit

testMatchModifyStateAfterSuccessWithResultSuccess0 = testMatchModifyStateAfterSuccessWithResultSuccess @"F1a" @(Proxy "F") @(FooBar (Proxy "F") (Proxy "1")) @"a" :: Unit

-- this will fail to compile, as it should
-- testMatchModifyStateAfterSuccessWithResultFailure0 = testMatchModifyStateAfterSuccessWithResultFailure @"F1a" :: Unit

---

testMatchBranchOnStateSuccess :: forall @toParse @h @t. Parse toParse (FooBar (ModifyStateAfterSuccessOnConstant MyConst0 (MatchAZ)) (C.BranchOnState (L.Cons (C.IfThen MyState1 Matchaz) L.Nil) Match09)) MyState0 (Success h t) MyState1 => Unit
testMatchBranchOnStateSuccess = unit

testMatchBranchOnStateFailure :: forall @toParse. Parse toParse (FooBar (ModifyStateAfterSuccessOnConstant MyConst0 (MatchAZ)) (C.BranchOnState (L.Cons (C.IfThen MyState1 Matchaz) L.Nil) Match09)) MyState0 (Failure _) MyState1 => Unit
testMatchBranchOnStateFailure = unit

testMatchBranchOnStateSuccess0 = testMatchBranchOnStateSuccess @"Fq" @(FooBar (Proxy "F") (Proxy "q")) @"" :: Unit
testMatchBranchOnStateFailure0 = testMatchBranchOnStateFailure @"F1" :: Unit

--

type LeftSide a = IgnoreAndThenParse (M.Many MatchWhitespace) (ParseAndThenIgnore a (M.Some MatchWhitespace))
type RightSide a = IgnoreAndThenParse (M.Some MatchWhitespace) (ParseAndThenIgnore a (M.Many MatchWhitespace))
type NoSide a = IgnoreAndThenParse (M.Many MatchWhitespace) (ParseAndThenIgnore a (M.Many MatchWhitespace))

data TAnd a b

instance ShowParser (SP2 "TAnd" a b) doc => ShowParser (TAnd a b) doc
data TOr a b

instance ShowParser (SP2 "TOr" a b) doc => ShowParser (TOr a b) doc
data TNot a

instance ShowParser (SP1 "TNot" a) doc => ShowParser (TNot a) doc

type ParseAnd c = ParseAndThenIgnore
  (IgnoreAndThenParse (Literal "(") (TAnd (ParseAndThenIgnore (LeftSide c) (M.And (Literal "A") (M.And (Literal "N") (Literal "D")))) (RightSide c)))
  (Literal ")")

type ParseOr c = ParseAndThenIgnore
  (IgnoreAndThenParse (Literal "(") (TOr (ParseAndThenIgnore (LeftSide c) (M.And (Literal "O") (Literal "R"))) (RightSide c)))
  (Literal ")")

type ParseNot c = ParseAndThenIgnore
  (IgnoreAndThenParse (Literal "(") (TNot (IgnoreAndThenParse (M.And (Literal "N") (M.And (Literal "O") (Literal "T"))) (NoSide c))))
  (Literal ")")

type ParseIdent = (M.Some Matchaz)

type ParseCond c = C.Or (ParseAnd c) (C.Or (ParseOr c) (C.Or (ParseNot c) ParseIdent))

testMatchRecursionSuccess :: forall @toParse @h @t. Parse toParse (C.Fix (condFix :: ParseCond (Proxy "condFix"))) Unit (Success h t) Unit => Unit
testMatchRecursionSuccess = unit

testMatchRecursionSuccess0 = testMatchRecursionSuccess @"(foo AND bar)" @(TAnd (Proxy "foo") (Proxy "bar")) @"" :: Unit
testMatchRecursionSuccess1 = testMatchRecursionSuccess @"(foo OR bar)" @(TOr (Proxy "foo") (Proxy "bar")) @"" :: Unit
testMatchRecursionSuccess2 = testMatchRecursionSuccess @"((baz OR qux) AND (foo OR bar))" @(TAnd (TOr (Proxy "baz") (Proxy "qux")) (TOr (Proxy "foo") (Proxy "bar"))) @"" :: Unit
testMatchRecursionSuccess3 = testMatchRecursionSuccess @"((baz OR qux) AND (NOT bar))" @(TAnd (TOr (Proxy "baz") (Proxy "qux")) (TNot (Proxy "bar"))) @"" :: Unit
testMatchRecursionSuccess4 = testMatchRecursionSuccess @"((baz OR qux) AND (NOT (bar OR baz)))" @(TAnd (TOr (Proxy "baz") (Proxy "qux")) (TNot (TOr (Proxy "bar") (Proxy "baz")))) @"" :: Unit
module Test.Matcher where

import Prelude

import TLDR.Matchers (And, Any, Except, Literal, Many, Match09, Match2, Match3, MatchAZ, MatchAlpha, MatchAlphanumeric, Matchaz, Or, Some)
import TLDR.Matchers.Class (class Match)
import TLDR.Result (Failure, Success)

testMatchAnySuccess :: forall @toMatch @h @t. Match toMatch Any (Success h t) => Unit
testMatchAnySuccess = unit

testMatchAnySuccess0 = testMatchAnySuccess @"abc" @"a" @"bc" :: Unit
testMatchAnySuccess1 = testMatchAnySuccess @"q" @"q" @"" :: Unit

testMatchAnyFailure :: forall @toMatch. Match toMatch Any (Failure _) => Unit
testMatchAnyFailure = unit

testMatchAnyFailure0 = testMatchAnyFailure @"" :: Unit

testMatchAZSuccess :: forall @toMatch @h @t. Match toMatch Matchaz (Success h t) => Unit
testMatchAZSuccess = unit

testMatchAZSuccess0 = testMatchAZSuccess @"abc" @"a" @"bc" :: Unit
testMatchAZSuccess1 = testMatchAZSuccess @"q" @"q" @"" :: Unit

testMatchAZFailure :: forall @toMatch. Match toMatch Matchaz (Failure _) => Unit
testMatchAZFailure = unit

testMatchAZFailure0 = testMatchAZFailure @"" :: Unit
testMatchAZFailure1 = testMatchAZFailure @"1" :: Unit

testMatchTwoSuccess :: forall @toMatch @h @t. Match toMatch (Match2 Matchaz) (Success h t) => Unit
testMatchTwoSuccess = unit

testMatchTwoSuccess0 = testMatchTwoSuccess @"abq" @"ab" @"q" :: Unit
testMatchTwoSuccess1 = testMatchTwoSuccess @"ab" @"ab" @"" :: Unit

testMatchTwoFailure :: forall @toMatch. Match toMatch (Match2 Matchaz) (Failure _) => Unit
testMatchTwoFailure = unit

testMatchTwoFailure0 = testMatchTwoFailure @"" :: Unit
testMatchTwoFailure1 = testMatchTwoFailure @"aZq" :: Unit

testMatchThreeSuccess :: forall @toMatch @h @t. Match toMatch (Match3 MatchAlpha) (Success h t) => Unit
testMatchThreeSuccess = unit

testMatchThreeSuccess0 = testMatchThreeSuccess @"aBc" @"aBc" @"" :: Unit
testMatchThreeSuccess1 = testMatchThreeSuccess @"DDxy" @"DDx" @"y" :: Unit

testMatchThreeFailure :: forall @toMatch. Match toMatch (Match3 MatchAlpha) (Failure _) => Unit
testMatchThreeFailure = unit

testMatchThreeFailure0 = testMatchThreeFailure @"" :: Unit
testMatchThreeFailure1 = testMatchThreeFailure @"aZ1w" :: Unit

testMatchLiteralSuccess :: forall @toMatch @s @h @t. Match toMatch (Literal s) (Success h t) => Unit
testMatchLiteralSuccess = unit

testMatchLiteralSuccess0 = testMatchLiteralSuccess @"abc" @"a" @"a" @"bc" :: Unit
testMatchLiteralSuccess1 = testMatchLiteralSuccess @"q" @"q" @"q" @"" :: Unit

testMatchLiteralFailure :: forall @toMatch @s. Match toMatch (Literal s) (Failure _) => Unit
testMatchLiteralFailure = unit

testMatchLiteralFailure0 = testMatchLiteralFailure @"abc" @"q" :: Unit
testMatchLiteralFailure1 = testMatchLiteralFailure @"" @"q" :: Unit

testMatchAndSuccess :: forall @toMatch @left @right @h @t. Match toMatch (And left right) (Success h t) => Unit
testMatchAndSuccess = unit

testMatchAndSuccess0 = testMatchAndSuccess @"qrstuv" @(Literal "q") @(Literal "r") @"qr" @"stuv" :: Unit
testMatchAndSuccess1 = testMatchAndSuccess @"qRstuv" @(Literal "q") @MatchAZ @"qR" @"stuv" :: Unit

testMatchAndFailure :: forall @toMatch @left @right. Match toMatch (And left right) (Failure _) => Unit
testMatchAndFailure = unit

testMatchAndFailure0 = testMatchAndFailure @"qrstuv" @(Literal "q") @(Literal "s") :: Unit
testMatchAndFailure1 = testMatchAndFailure @"qRstuv" @(Literal "q") @Matchaz :: Unit

testMatchOrSuccess :: forall @toMatch @left @right @h @t. Match toMatch (Or left right) (Success h t) => Unit
testMatchOrSuccess = unit

testMatchOrSuccess0 = testMatchOrSuccess @"qrstuv" @(Literal "q") @(Literal "f") @"q" @"rstuv" :: Unit
testMatchOrSuccess1 = testMatchOrSuccess @"frstuv" @(Literal "q") @(Literal "f") @"f" @"rstuv" :: Unit
testMatchOrSuccess2 = testMatchOrSuccess @"foobaz" @(And (Literal "f") (And (Literal "o") (Literal "o"))) @(And (Literal "b") (And (Literal "a") (Literal "r"))) @"foo" @"baz" :: Unit
testMatchOrSuccess3 = testMatchOrSuccess @"barbaz" @(And (Literal "f") (And (Literal "o") (Literal "o"))) @(And (Literal "b") (And (Literal "a") (Literal "r"))) @"bar" @"baz" :: Unit

testMatchOrFailure :: forall @toMatch @left @right. Match toMatch (Or left right) (Failure _) => Unit
testMatchOrFailure = unit

testMatchOrFailure0 = testMatchOrFailure @"bagbaz" @(And (Literal "f") (And (Literal "o") (Literal "o"))) @(And (Literal "b") (And (Literal "a") (Literal "r"))) :: Unit

testMatchExceptSuccess :: forall @toMatch @left @right @h @t. Match toMatch (Except left right) (Success h t) => Unit
testMatchExceptSuccess = unit

testMatchExceptSuccess0 = testMatchExceptSuccess @"9rstuv" @MatchAZ @Any @"9" @"rstuv" :: Unit
testMatchExceptSuccess1 = testMatchExceptSuccess @"漢rstuv" @MatchAZ @(Literal "漢") @"漢" @"rstuv" :: Unit

testMatchExceptFailure :: forall @toMatch @left @right. Match toMatch (Except left right) (Failure _) => Unit
testMatchExceptFailure = unit

testMatchExceptFailure0 = testMatchExceptFailure @"9rstuv" @Match09 @MatchAZ :: Unit
testMatchExceptFailure1 = testMatchExceptFailure @"9rstuv" @Match09 @MatchAlphanumeric :: Unit

testMatchManySuccess :: forall @toMatch @matcher @h @t. Match toMatch (Many matcher) (Success h t) => Unit
testMatchManySuccess = unit

testMatchManySuccess0 = testMatchManySuccess @"932rstuv" @Match09 @"932" @"rstuv" :: Unit
testMatchManySuccess1 = testMatchManySuccess @"932rstuv" @Matchaz @"" @"932rstuv" :: Unit
testMatchManySuccess2 = testMatchManySuccess @"932rstuv" @(And Match09 Match09) @"93" @"2rstuv" :: Unit

testMatchSomeSuccess :: forall @toMatch @matcher @h @t. Match toMatch (Some matcher) (Success h t) => Unit
testMatchSomeSuccess = unit

testMatchSomeSuccess0 = testMatchSomeSuccess @"932rstuv" @Match09 @"932" @"rstuv" :: Unit
testMatchSomeSuccess1 = testMatchSomeSuccess @"932rstuv" @(And Match09 Match09) @"93" @"2rstuv" :: Unit

testMatchSomeFailure :: forall @toMatch @matcher. Match toMatch (Some matcher) (Failure _) => Unit
testMatchSomeFailure = unit

testMatchSomeFailure0 = testMatchSomeFailure @"932rstuv" @Matchaz :: Unit
testMatchSomeFailure1 = testMatchSomeFailure @"932rstuv" @(And Match09 Matchaz) :: Unit
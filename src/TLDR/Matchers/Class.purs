module TLDR.Matchers.Class where

import Prim.Symbol as S
import Prim.TypeError (Beside, Text)
import TLDR.Matchers as Matchers
import TLDR.Matchers.Match09 as Match09
import TLDR.Matchers.MatchAlpha as MatchAlpha
import TLDR.Matchers.MatchAlphanumeric as MatchAlphanumeric
import TLDR.Matchers.MatchHex as MatchHex
import TLDR.Matchers.MatchLaz as Matchaz
import TLDR.Matchers.MatchLaz09 as Matchaz09
import TLDR.Matchers.MatchUaz as MatchAZ
import TLDR.Matchers.MatchUaz09 as MatchAZ09
import TLDR.Matchers.MatchWhitespace as MatchWhitespace
import TLDR.Matchers.Show (class ShowMatch)
import TLDR.Result (Failure, SingleFailure, Success)

class ReplaceFailureWith :: Type -> Type -> Type -> Constraint
class ReplaceFailureWith match resI resO | match resI -> resO

instance ShowMatch match o => ReplaceFailureWith match (Failure f) (Failure (SingleFailure (Beside (Text "Failed to match on matcher ") (Text o))))
instance ReplaceFailureWith match (Success a b) (Success a b)

class MatchLiteral :: Symbol -> Symbol -> Symbol -> Type -> Constraint
class MatchLiteral l h t res | l h t -> res

instance MatchLiteral l l t (Success l t)
else instance MatchLiteral l h t (Failure (SingleFailure (Beside (Text "Could not match ") (Beside (Text l) (Beside (Text " against ") (Text h))))))

class Match :: Symbol -> Type -> Type -> Constraint
class Match i m o | i m -> o

class SimpleSubResult :: Type -> Symbol -> Type -> Type -> Constraint
class SimpleSubResult i s m o | i s m -> o

instance SimpleSubResult (Success i s') s m (Success i s)
instance
  ShowMatch m o =>
  SimpleSubResult (Failure f)
    s
    m
    (Failure (SingleFailure (Beside (Text "Failed to match on matcher ") (Text o))))

class MergeMatch :: Type -> Symbol -> Type -> Type -> Constraint
class MergeMatch resI head failMatch resO | resI head failMatch -> resO

instance ShowMatch failMatch o => MergeMatch (Failure f) head failMatch (Failure (SingleFailure (Beside (Text "Failed to match on matcher ") (Text o))))
instance S.Append h head o => MergeMatch (Success head tail) h failMatch (Success o tail)

class ContinueOnSuccessOrFailWith :: Type -> Type -> Type -> Type -> Constraint
class ContinueOnSuccessOrFailWith match inRes failMatch outRes | match inRes failMatch -> outRes

instance ShowMatch failMatch o => ContinueOnSuccessOrFailWith match (Failure fail) failMatch (Failure (SingleFailure (Beside (Text "Failed to match on matcher ") (Text o))))
instance (Match tail match res, MergeMatch res head failMatch o) => ContinueOnSuccessOrFailWith match (Success head tail) failMatch o

class StopOnSuccessOrContinue :: Symbol -> Type -> Type -> Type -> Constraint
class StopOnSuccessOrContinue s match inRes outRes | match inRes -> outRes

instance Match s match res => StopOnSuccessOrContinue s match (Failure fail) res
instance StopOnSuccessOrContinue ignore match (Success a b) (Success a b)

class FailOnSuccessOrContinue :: Symbol -> Type -> Type -> Type -> Constraint
class FailOnSuccessOrContinue s match inRes outRes | match inRes -> outRes

instance Match s match res => FailOnSuccessOrContinue s match (Failure fail) res
instance FailOnSuccessOrContinue s match (Success a b) (Failure (SingleFailure (Beside (Text "Failed to match on matcher ") (Text s))))

class ManyLoop :: Symbol -> Type -> Type -> Type -> Constraint
class ManyLoop i inRes match outRes | i inRes match -> outRes

instance
  ( Match t match res'
  , ManyLoop t res' match (Success h' t')
  , S.Append h h' o
  ) =>
  ManyLoop i (Success h t) match (Success o t')

instance ManyLoop i (Failure ignore) match (Success "" i)

--
--
instance Match i Matchers.Noop (Success "" i)
else instance
  Match "" Matchers.EOF (Success "" "")
else instance
  Match "" (Matchers.Many anything) (Success "" "")
else instance
  ShowMatch m s =>
  Match "" m (Failure (SingleFailure (Beside (Text "Cannot match empty string against a matcher") (Text s))))

else instance
  ( S.Cons h t i
  ) =>
  Match i Matchers.Any (Success h t)

else instance
  ( S.Cons h t i
  , Match09.Match09 h res'
  , SimpleSubResult res' t Matchers.Match09 res
  ) =>
  Match i Matchers.Match09 res

else instance
  ( S.Cons h t i
  , MatchAlpha.MatchAlpha h res'
  , SimpleSubResult res' t Matchers.MatchAlpha res
  ) =>
  Match i Matchers.MatchAlpha res

else instance
  ( S.Cons h t i
  , MatchAlphanumeric.MatchAlphanumeric h res'
  , SimpleSubResult res' t Matchers.MatchAlphanumeric res
  ) =>
  Match i Matchers.MatchAlphanumeric res

else instance
  ( S.Cons h t i
  , MatchHex.MatchHex h res'
  , SimpleSubResult res' t Matchers.MatchHex res
  ) =>
  Match i Matchers.MatchHex res

else instance
  ( S.Cons h t i
  , Matchaz.Matchaz h res'
  , SimpleSubResult res' t Matchers.Matchaz res
  ) =>
  Match i Matchers.Matchaz res

else instance
  ( S.Cons h t i
  , Matchaz09.Matchaz09 h res'
  , SimpleSubResult res' t Matchers.Matchaz09 res
  ) =>
  Match i Matchers.Matchaz09 res

else instance
  ( S.Cons h t i
  , MatchAZ.MatchAZ h res'
  , SimpleSubResult res' t Matchers.MatchAZ res
  ) =>
  Match i Matchers.MatchAZ res

else instance
  ( S.Cons h t i
  , MatchAZ09.MatchAZ09 h res'
  , SimpleSubResult res' t Matchers.MatchAZ09 res
  ) =>
  Match i Matchers.MatchAZ09 res

else instance
  ( S.Cons h t i
  , MatchWhitespace.MatchWhitespace h res'
  , SimpleSubResult res' t Matchers.MatchWhitespace res
  ) =>
  Match i Matchers.MatchWhitespace res

else instance
  ( Match i (Matchers.And match match) res
  ) =>
  Match i (Matchers.Match2 match) res

else instance
  ( Match i match res'
  , ContinueOnSuccessOrFailWith (Matchers.Match2 match) res' (Matchers.Match2 match) res
  ) =>
  Match i (Matchers.Match3 match) res

else instance
  ( Match i match res'
  , ContinueOnSuccessOrFailWith (Matchers.Match3 match) res' (Matchers.Match2 match) res
  ) =>
  Match i (Matchers.Match4 match) res

else instance
  ( Match i match res'
  , ContinueOnSuccessOrFailWith (Matchers.Match4 match) res' (Matchers.Match2 match) res
  ) =>
  Match i (Matchers.Match5 match) res

else instance
  ( Match i match res'
  , ContinueOnSuccessOrFailWith (Matchers.Match5 match) res' (Matchers.Match2 match) res
  ) =>
  Match i (Matchers.Match6 match) res

else instance
  ( Match i match res'
  , ContinueOnSuccessOrFailWith (Matchers.Match6 match) res' (Matchers.Match2 match) res
  ) =>
  Match i (Matchers.Match7 match) res

else instance
  ( Match i match res'
  , ContinueOnSuccessOrFailWith (Matchers.Match7 match) res' (Matchers.Match2 match) res
  ) =>
  Match i (Matchers.Match8 match) res

else instance
  ( Match i match res'
  , ContinueOnSuccessOrFailWith (Matchers.Match8 match) res' (Matchers.Match2 match) res
  ) =>
  Match i (Matchers.Match9 match) res

else instance
  ( S.Cons h t i
  , MatchLiteral l h t res
  ) =>
  Match i (Matchers.Literal l) res

else instance
  ( Match i match1 res'
  , ContinueOnSuccessOrFailWith match2 res' (Matchers.And match1 match2) res
  ) =>
  Match i (Matchers.And match1 match2) res

else instance
  ( Match i match1 res'
  , StopOnSuccessOrContinue i match2 res' res''
  , ReplaceFailureWith (Matchers.Or match1 match2) res'' res
  ) =>
  Match i (Matchers.Or match1 match2) res

else instance
  ( Match i match1 res'
  , FailOnSuccessOrContinue i match2 res' res''
  , ReplaceFailureWith (Matchers.Except match1 match2) res'' res
  ) =>
  Match i (Matchers.Except match1 match2) res

else instance
  ( ManyLoop i (Success "" i) match res'
  , ReplaceFailureWith (Matchers.Many match) res' res
  ) =>
  Match i (Matchers.Many match) res

else instance
  ( Match i (Matchers.And match (Matchers.Many match)) res'
  , ReplaceFailureWith (Matchers.Some match) res' res
  ) =>
  Match i (Matchers.Some match) res

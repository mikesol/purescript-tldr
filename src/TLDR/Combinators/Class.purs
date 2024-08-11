module TLDR.Combinators.Class where

import TLDR.Matchers

import Prim.Row as Row
import Prim.RowList as RowList
import Prim.Symbol as S
import TLDR.Combinators as C
import TLDR.List as L
import TLDR.Matchers.Class as MatchClass
import Prim.TypeError (class Fail, Above, Beside, Doc, Quote, Text)
import TLDR.Matchers.Show (class ShowMatch)
import TLDR.Result (Failure, SingleFailure, Success)
import Type.Proxy (Proxy)

data Unit

data RCPair :: Type -> Type -> Type
data RCPair token combinator

class InternalFormat :: Symbol -> Doc -> Doc -> Constraint
class InternalFormat indent doc doc' | indent doc -> doc'

instance InternalFormat sym (Text t) (Beside (Text sym) (Text t))
instance InternalFormat sym (Quote q) (Beside (Text sym) (Quote q))
-- reset because we append
instance (InternalFormat "" a a', InternalFormat "" b b') => InternalFormat sym (Beside a b) (Beside (Text sym) (Beside a' b'))
instance (S.Append " " sym sym', InternalFormat sym' a a', InternalFormat sym' b b') => InternalFormat sym (Above a b) (Above a' b')

class Format :: Type -> Type -> Constraint
class Format i o | i -> o

instance Format (Success a b) (Success a b)
instance InternalFormat "" fail newFail => Format (Failure fail) (Failure newFail)

class ModifyState :: Symbol -> Type -> Type -> Type -> Constraint
class ModifyState remaining constant stateI stateO | remaining constant stateI -> stateO

class DoConstantStateModificationOnSuccess :: Symbol -> Type -> Type -> Type -> Type -> Constraint
class DoConstantStateModificationOnSuccess remaining constant res stateI stateO | constant res stateI -> stateO

instance DoConstantStateModificationOnSuccess remaining constant (Failure fail) stateI stateI
instance ModifyState remaining constant stateI stateO => DoConstantStateModificationOnSuccess remaining constant (Success a b) stateI stateO

class DoConstantStateModificationWithResult :: Symbol -> (Type -> Type) -> Type -> Type -> Type -> Constraint
class DoConstantStateModificationWithResult sym f res stateI stateO | f res stateI -> stateO

instance DoConstantStateModificationWithResult sym f (Failure fail) stateI stateI
instance ModifyState sym (f a) stateI stateO => DoConstantStateModificationWithResult sym f (Success a b) stateI stateO

class ShowParser :: forall k. k -> Doc -> Constraint
class ShowParser k doc | k -> doc

data SP10 :: Symbol -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data SP10 name a b c d e g h i j k

data SP9 :: Symbol -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data SP9 name a b c d e g h i j

data SP8 :: Symbol -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data SP8 name a b c d e g h i

data SP7 :: Symbol -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data SP7 name a b c d e g h

data SP6 :: Symbol -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data SP6 name a b c d e g

data SP5 :: Symbol -> Type -> Type -> Type -> Type -> Type -> Type
data SP5 name a b c d e

data SP4 :: Symbol -> Type -> Type -> Type -> Type -> Type
data SP4 name a b c d

data SP3 :: Symbol -> Type -> Type -> Type -> Type
data SP3 name a b c

data SP2 :: Symbol -> Type -> Type -> Type
data SP2 name a b

data SP1 :: Symbol -> Type -> Type
data SP1 name a

instance
  ( ShowParser a a'
  , ShowParser b b'
  , ShowParser c c'
  , ShowParser d d'
  , ShowParser e e'
  , ShowParser g g'
  , ShowParser h h'
  , ShowParser i i'
  , ShowParser j j'
  , ShowParser k k'
  ) =>
  ShowParser (SP10 name a b c d e g h i j k) (Above (Text name) (Above a' (Above b' (Above c' (Above d' (Above e' (Above g' (Above h' (Above i' (Above j' k'))))))))))

instance
  ( ShowParser a a'
  , ShowParser b b'
  , ShowParser c c'
  , ShowParser d d'
  , ShowParser e e'
  , ShowParser g g'
  , ShowParser h h'
  , ShowParser i i'
  , ShowParser j j'
  ) =>
  ShowParser (SP9 name a b c d e g h i j) (Above (Text name) (Above a' (Above b' (Above c' (Above d' (Above e' (Above g' (Above h' (Above i' j')))))))))

instance
  ( ShowParser a a'
  , ShowParser b b'
  , ShowParser c c'
  , ShowParser d d'
  , ShowParser e e'
  , ShowParser g g'
  , ShowParser h h'
  , ShowParser i i'
  ) =>
  ShowParser (SP8 name a b c d e g h i) (Above (Text name) (Above a' (Above b' (Above c' (Above d' (Above e' (Above g' (Above h' i'))))))))

instance
  ( ShowParser a a'
  , ShowParser b b'
  , ShowParser c c'
  , ShowParser d d'
  , ShowParser e e'
  , ShowParser g g'
  , ShowParser h h'
  ) =>
  ShowParser (SP7 name a b c d e g h) (Above (Text name) (Above a' (Above b' (Above c' (Above d' (Above e' (Above g' h')))))))

instance
  ( ShowParser a a'
  , ShowParser b b'
  , ShowParser c c'
  , ShowParser d d'
  , ShowParser e e'
  , ShowParser g g'
  ) =>
  ShowParser (SP6 name a b c d e g) (Above (Text name) (Above a' (Above b' (Above c' (Above d' (Above e' g'))))))

instance
  ( ShowParser a a'
  , ShowParser b b'
  , ShowParser c c'
  , ShowParser d d'
  , ShowParser e e'
  ) =>
  ShowParser (SP5 name a b c d e) (Above (Text name) (Above a' (Above b' (Above c' (Above d' e')))))

instance
  ( ShowParser a a'
  , ShowParser b b'
  , ShowParser c c'
  , ShowParser d d'
  ) =>
  ShowParser (SP4 name a b c d) (Above (Text name) (Above a' (Above b' (Above c' d'))))

instance
  ( ShowParser a a'
  , ShowParser b b'
  , ShowParser c c'
  ) =>
  ShowParser (SP3 name a b c) (Above (Text name) (Above a' (Above b' c')))

instance
  ( ShowParser a a'
  , ShowParser b b'
  ) =>
  ShowParser (SP2 name a b) (Above (Text name) (Above a' b'))

instance
  ( ShowParser a a'
  ) =>
  ShowParser (SP1 name a) (Above (Text name) a')

instance ShowParser (Text a) (Text a)
instance ShowParser (Quote a) (Quote a)
instance ShowParser (Above a b) (Above a b)
instance ShowParser (Beside a b) (Beside a b)
instance ShowParser (Failure fail) fail
instance ShowParser (Proxy s) (Text s)
instance ShowParser L.Nil (Text "Nil")
instance (ShowParser a a', ShowParser b b') => ShowParser (L.Cons a b) (Beside a' b')
instance (ShowParser ignore ignore', ShowParser match match') => ShowParser (C.IgnoreAndThenParse ignore match) (Above (Text "C.IgnoreAndThenParse") (Above ignore' match'))
instance (ShowParser ignore ignore', ShowParser match match') => ShowParser (C.ParseAndThenIgnore match ignore) (Above (Text "C.ParseAndThenIgnore") (Above match' ignore'))
instance (ShowParser match match') => ShowParser (C.Many match) (Above (Text "Many") match')
instance (ShowParser match match') => ShowParser (C.Some match) (Above (Text "Some") match')
instance (ShowParser match match') => ShowParser (C.Const match) (Above (Text "Const") match')
instance (ShowParser constant constant', ShowParser cont cont') => ShowParser (C.ModifyStateAfterSuccessOnConstant constant cont) (Above (Text "C.ModifyStateAfterSuccessOnConstant") (Above constant' cont'))
instance (ShowParser cont cont') => ShowParser (C.ModifyStateAfterSuccessWithResult f cont) (Above (Text "C.ModifyStateAfterSuccessWithResult") cont')
instance (ShowParser a a', ShowParser b b') => ShowParser (C.BranchOnState a b) (Above (Text "BranchOnState") (Above a' b'))
instance (ShowParser i i', ShowParser t t') => ShowParser (C.IfThen i t) (Above (Text "IfThen") (Above i' t'))
instance (ShowParser i i', ShowParser t t') => ShowParser (C.Or i t) (Above (Text "Or") (Above i' t'))

class ContinueIfMatch :: Row Type -> Type -> Type -> Type -> Type -> Type -> Constraint
class ContinueIfMatch rc res' parse stateI res stateO | rc res' parse stateI -> res stateO

instance ContinueIfMatch rc (Failure fail) parse stateI (Failure fail) stateI
instance (ParseRC rc b parse stateI res stateO) => ContinueIfMatch rc (Success ignore b) parse stateI res stateO

class GetDefault :: RowList.RowList Type -> Type -> Constraint
class GetDefault i o | i -> o

instance GetDefault (RowList.Cons k v rest) v

class ContinueIfParse :: Row Type -> Type -> Type -> Type -> Type -> Type -> Constraint
class ContinueIfParse rc res' match stateI res stateO | rc res' match stateI -> res stateO

instance ContinueIfParse rc (Failure fail) match stateI (Failure fail) stateI
instance (ParseRC rc b (C.IgnoreAndThenParse match (C.Const yay)) stateI res stateO) => ContinueIfParse rc (Success yay b) match stateI res stateO

class UseIfMatch :: Row Type -> Symbol -> Type -> Type -> Type -> Type -> Type -> Type -> Constraint
class UseIfMatch rc sym res' parse stateI stateI' res stateO | rc sym res' parse stateI stateI' -> res stateO

instance (ParseRC rc sym parse stateI res stateO) => UseIfMatch rc sym (Failure fail) parse stateI stateI' res stateO
instance UseIfMatch rc sym (Success h t) parse stateI stateI' (Success h t) stateI'

class ManyLoop :: Row Type -> Symbol -> Type -> Type -> Type -> Type -> Type -> Constraint
class ManyLoop rc i inRes inState parse outRes outState | rc i inRes inState parse -> outRes outState

instance
  ( ParseRC rc t parse inState res' outState'
  , ManyLoop rc t res' outState' parse (Success h' t') outState
  ) =>
  ManyLoop rc i (Success h t) inState parse (Success (L.Cons h h') t') outState

instance ManyLoop rc i (Failure ignore) inState parse (Success L.Nil i) inState

class UnUnit :: Type -> Type -> Constraint
class UnUnit i o | i -> o

instance UnUnit (Success (L.Cons Unit r) t) (Success r t)
else instance UnUnit a a

class FailOnNil :: Type -> Type -> Constraint
class FailOnNil i o | i -> o

instance FailOnNil (Success L.Nil i) (Failure (Text "Expected at least one match"))
instance FailOnNil (Success (L.Cons a b) c) (Success (L.Cons a b) c)
instance FailOnNil (Failure fail) (Failure fail)

class ContinueNary :: forall ctor. Row Type -> Type -> Type -> ctor -> Type -> Type -> Type -> Constraint
class ContinueNary rc resI stateI ctor args resO stateO | rc resI stateI ctor args -> resO stateO

instance ShowParser (ctor (Failure fail) b c d e g h i j k) doc => ContinueNary rc (Failure fail) stateI ctor (Args9 b c d e g h i j k) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d e g h i j) doc => ContinueNary rc (Failure fail) stateI ctor (Args8 b c d e g h i j) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d e g h i) doc => ContinueNary rc (Failure fail) stateI ctor (Args7 b c d e g h i) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d e g h) doc => ContinueNary rc (Failure fail) stateI ctor (Args6 b c d e g h) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d e g) doc => ContinueNary rc (Failure fail) stateI ctor (Args5 b c d e g) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d e) doc => ContinueNary rc (Failure fail) stateI ctor (Args4 b c d e) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d) doc => ContinueNary rc (Failure fail) stateI ctor (Args3 b c d) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c) doc => ContinueNary rc (Failure fail) stateI ctor (Args2 b c) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b) doc => ContinueNary rc (Failure fail) stateI ctor (Args1 b) (Failure (SingleFailure doc)) stateI

--
instance (ParseADT rc rest (ctor yay) (Args9 b c d e g h i j k) stateI res stateO) => ContinueNary rc (Success yay rest) stateI ctor (Args9 b c d e g h i j k) res stateO
else instance (ParseADT rc rest (ctor yay) (Args8 b c d e g h i j) stateI res stateO) => ContinueNary rc (Success yay rest) stateI ctor (Args8 b c d e g h i j) res stateO
else instance (ParseADT rc rest (ctor yay) (Args7 b c d e g h i) stateI res stateO) => ContinueNary rc (Success yay rest) stateI ctor (Args7 b c d e g h i) res stateO
else instance (ParseADT rc rest (ctor yay) (Args6 b c d e g h) stateI res stateO) => ContinueNary rc (Success yay rest) stateI ctor (Args6 b c d e g h) res stateO
else instance (ParseADT rc rest (ctor yay) (Args5 b c d e g) stateI res stateO) => ContinueNary rc (Success yay rest) stateI ctor (Args5 b c d e g) res stateO
else instance (ParseADT rc rest (ctor yay) (Args4 b c d e) stateI res stateO) => ContinueNary rc (Success yay rest) stateI ctor (Args4 b c d e) res stateO
else instance (ParseADT rc rest (ctor yay) (Args3 b c d) stateI res stateO) => ContinueNary rc (Success yay rest) stateI ctor (Args3 b c d) res stateO
else instance (ParseADT rc rest (ctor yay) (Args2 b c) stateI res stateO) => ContinueNary rc (Success yay rest) stateI ctor (Args2 b c) res stateO
else instance (ParseADT rc rest (ctor yay) (Args1 b) stateI res stateO) => ContinueNary rc (Success yay rest) stateI ctor (Args1 b) res stateO

-- else instance (Parse rest b stateI res stateO, Sequence2 (ctor yay) res o) => ContinueNary rc (Success yay rest) stateI ctor b res stateO

class RecastSuccessToProxy :: Type -> Type -> Constraint
class RecastSuccessToProxy i o | i -> o

instance RecastSuccessToProxy (Failure fail) (Failure fail)
instance RecastSuccessToProxy (Success a b) (Success (Proxy a) b)

class Sequence2 :: forall f. f -> Type -> Type -> Constraint
class Sequence2 f res o | f res -> o

instance Sequence2 f (Success a b) (Success (f a) b)
instance ShowParser (f (Failure fail)) o => Sequence2 f (Failure fail) (Failure (SingleFailure o))

class ParseRC :: Row Type -> Symbol -> Type -> Type -> Type -> Type -> Constraint
class ParseRC rc sym combinator stateI res stateO | rc sym combinator stateI -> res stateO

class ParseADT :: forall ctor. Row Type -> Symbol -> ctor -> Type -> Type -> Type -> Type -> Constraint
class ParseADT rc sym ctor args stateI res stateO | rc sym ctor args stateI -> res stateO

data Args10 :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data Args10 a b c d e g h i j k

data Args9 :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data Args9 a b c d e g h i j

data Args8 :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data Args8 a b c d e g h i

data Args7 :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
data Args7 a b c d e g h

data Args6 :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data Args6 a b c d e g

data Args5 :: Type -> Type -> Type -> Type -> Type -> Type
data Args5 a b c d e

data Args4 :: Type -> Type -> Type -> Type -> Type
data Args4 a b c d

data Args3 :: Type -> Type -> Type -> Type
data Args3 a b c

data Args2 :: Type -> Type -> Type
data Args2 a b

data Args1 :: Type -> Type
data Args1 a

-- matchers
instance ShowMatch Any o => ShowParser Any (Text o)
instance ShowMatch MatchAZ o => ShowParser MatchAZ (Text o)
instance ShowMatch Noop o => ShowParser Noop (Text o)
instance ShowMatch EOF o => ShowParser EOF (Text o)
instance ShowMatch Match09 o => ShowParser Match09 (Text o)
instance ShowMatch Matchaz o => ShowParser Matchaz (Text o)
instance ShowMatch MatchAlpha o => ShowParser MatchAlpha (Text o)
instance ShowMatch MatchAlphanumeric o => ShowParser MatchAlphanumeric (Text o)
instance ShowMatch MatchAZ09 o => ShowParser MatchAZ09 (Text o)
instance ShowMatch Matchaz09 o => ShowParser Matchaz09 (Text o)
instance ShowMatch MatchHex o => ShowParser MatchHex (Text o)
instance ShowMatch MatchWhitespace o => ShowParser MatchWhitespace (Text o)
instance ShowMatch (Literal m) o => ShowParser (Literal m) (Text o)
instance ShowMatch (Some m) o => ShowParser (Some m) (Text o)
instance ShowMatch (Many m) o => ShowParser (Many m) (Text o)
instance ShowMatch (Except m n) o => ShowParser (Except m n) (Text o)
instance ShowMatch (Or m n) o => ShowParser (Or m n) (Text o)
instance ShowMatch (And m n) o => ShowParser (And m n) (Text o)
instance ShowMatch (Match2 m) o => ShowParser (Match2 m) (Text o)
instance ShowMatch (Match3 m) o => ShowParser (Match3 m) (Text o)
instance ShowMatch (Match4 m) o => ShowParser (Match4 m) (Text o)
instance ShowMatch (Match5 m) o => ShowParser (Match5 m) (Text o)
instance ShowMatch (Match6 m) o => ShowParser (Match6 m) (Text o)
instance ShowMatch (Match7 m) o => ShowParser (Match7 m) (Text o)
instance ShowMatch (Match8 m) o => ShowParser (Match8 m) (Text o)
instance ShowMatch (Match9 m) o => ShowParser (Match9 m) (Text o)

-- adt

instance (ParseRC rc sym a stateI res' stateO', ContinueNary rc res' stateO' f (Args9 b c d e g h i j k) res stateO) => ParseADT rc sym f (Args10 a b c d e g h i j k) stateI res stateO
else instance (ParseRC rc sym a stateI res' stateO', ContinueNary rc res' stateO' f (Args8 b c d e g h i j) res stateO) => ParseADT rc sym f (Args9 a b c d e g h i j) stateI res stateO
else instance (ParseRC rc sym a stateI res' stateO', ContinueNary rc res' stateO' f (Args7 b c d e g h i) res stateO) => ParseADT rc sym f (Args8 a b c d e g h i) stateI res stateO
else instance (ParseRC rc sym a stateI res' stateO', ContinueNary rc res' stateO' f (Args6 b c d e g h) res stateO) => ParseADT rc sym f (Args7 a b c d e g h) stateI res stateO
else instance (ParseRC rc sym a stateI res' stateO', ContinueNary rc res' stateO' f (Args5 b c d e g) res stateO) => ParseADT rc sym f (Args6 a b c d e g) stateI res stateO
else instance (ParseRC rc sym a stateI res' stateO', ContinueNary rc res' stateO' f (Args4 b c d e) res stateO) => ParseADT rc sym f (Args5 a b c d e) stateI res stateO
else instance (ParseRC rc sym a stateI res' stateO', ContinueNary rc res' stateO' f (Args3 b c d) res stateO) => ParseADT rc sym f (Args4 a b c d) stateI res stateO
else instance (ParseRC rc sym a stateI res' stateO', ContinueNary rc res' stateO' f (Args2 b c) res stateO) => ParseADT rc sym f (Args3 a b c) stateI res stateO
else instance (ParseRC rc sym a stateI res' stateO', ContinueNary rc res' stateO' f (Args1 b) res stateO) => ParseADT rc sym f (Args2 a b) stateI res stateO
else instance (ParseRC rc sym a stateI res' stateO, Sequence2 f res' res) => ParseADT rc sym f (Args1 a) stateI res stateO

-- parsing with recursion
-- don't parse against Noop - it can only be in a matcher
-- don't parse against EOF - it can only be in a matcher
instance (MatchClass.Match sym Any res', RecastSuccessToProxy res' res) => ParseRC rc sym Any stateI res stateI
else instance (MatchClass.Match sym MatchAZ res', RecastSuccessToProxy res' res) => ParseRC rc sym MatchAZ stateI res stateI
else instance (MatchClass.Match sym Match09 res', RecastSuccessToProxy res' res) => ParseRC rc sym Match09 stateI res stateI
else instance (MatchClass.Match sym Matchaz res', RecastSuccessToProxy res' res) => ParseRC rc sym Matchaz stateI res stateI
else instance (MatchClass.Match sym MatchAlpha res', RecastSuccessToProxy res' res) => ParseRC rc sym MatchAlpha stateI res stateI
else instance (MatchClass.Match sym MatchAlphanumeric res', RecastSuccessToProxy res' res) => ParseRC rc sym MatchAlphanumeric stateI res stateI
else instance (MatchClass.Match sym MatchAZ09 res', RecastSuccessToProxy res' res) => ParseRC rc sym MatchAZ09 stateI res stateI
else instance (MatchClass.Match sym Matchaz09 res', RecastSuccessToProxy res' res) => ParseRC rc sym Matchaz09 stateI res stateI
else instance (MatchClass.Match sym MatchHex res', RecastSuccessToProxy res' res) => ParseRC rc sym MatchHex stateI res stateI
else instance (MatchClass.Match sym MatchWhitespace res', RecastSuccessToProxy res' res) => ParseRC rc sym MatchWhitespace stateI res stateI
else instance (MatchClass.Match sym (Literal m) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Literal m) stateI res stateI
else instance (MatchClass.Match sym (Some m) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Some m) stateI res stateI
else instance (MatchClass.Match sym (Many m) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Many m) stateI res stateI
else instance (MatchClass.Match sym (Except m n) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Except m n) stateI res stateI
else instance (MatchClass.Match sym (Or m n) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Or m n) stateI res stateI
else instance (MatchClass.Match sym (And m n) res', RecastSuccessToProxy res' res) => ParseRC rc sym (And m n) stateI res stateI
else instance (MatchClass.Match sym (Match2 m) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Match2 m) stateI res stateI
else instance (MatchClass.Match sym (Match3 m) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Match3 m) stateI res stateI
else instance (MatchClass.Match sym (Match4 m) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Match4 m) stateI res stateI
else instance (MatchClass.Match sym (Match5 m) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Match5 m) stateI res stateI
else instance (MatchClass.Match sym (Match6 m) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Match6 m) stateI res stateI
else instance (MatchClass.Match sym (Match7 m) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Match7 m) stateI res stateI
else instance (MatchClass.Match sym (Match8 m) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Match8 m) stateI res stateI
else instance (MatchClass.Match sym (Match9 m) res', RecastSuccessToProxy res' res) => ParseRC rc sym (Match9 m) stateI res stateI
--------
else instance (MatchClass.Match sym ignore res', ContinueIfMatch rc res' parse stateI res stateO) => ParseRC rc sym (C.IgnoreAndThenParse ignore parse) stateI res stateO
else instance (ParseRC rc sym parse stateI res' stateO', ContinueIfParse rc res' ignore stateO' res stateO) => ParseRC rc sym (C.ParseAndThenIgnore parse ignore) stateI res stateO
else instance (ParseRC rc sym left stateI res' stateO', UseIfMatch rc sym res' right stateI stateO' res stateO) => ParseRC rc sym (C.Or left right) stateI res stateO
else instance ParseRC rc sym (C.Const val) stateI (Success val sym) stateI
else instance (Row.Cons token combinator rc' rc, ParseRC rc sym combinator stateI res stateO) => ParseRC rc sym (Proxy token) stateI res stateO
else instance
  ( ParseRC rc sym cont stateI res stateO'
  , DoConstantStateModificationOnSuccess sym constant res stateO' stateO
  ) =>
  ParseRC rc sym (C.ModifyStateAfterSuccessOnConstant constant cont) stateI res stateO
else instance
  ( ParseRC rc sym cont stateI res stateO'
  , DoConstantStateModificationWithResult sym f res stateO' stateO
  ) =>
  ParseRC rc sym (C.ModifyStateAfterSuccessWithResult f cont) stateI res stateO
else instance
  ( ManyLoop rc i (Success Unit i) stateI parse res' stateO
  , UnUnit res' res
  ) =>
  ParseRC rc i (C.Many parse) stateI res stateO
else instance
  ( ManyLoop rc i (Success Unit i) stateI parse res' stateO
  , UnUnit res' res''
  , FailOnNil res'' res
  ) =>
  ParseRC rc i (C.Some parse) stateI res stateO
else instance
  ( ParseRC rc i cont stateI res stateO
  ) =>
  ParseRC rc i (C.BranchOnState (L.Cons (C.IfThen stateI cont) rest) otherwise) stateI res stateO
else instance
  ( ParseRC rc i (C.BranchOnState rest otherwise) stateI res stateO
  ) =>
  ParseRC rc i (C.BranchOnState (L.Cons (C.IfThen stateX cont) rest) otherwise) stateI res stateO
else instance
  ( ParseRC rc i otherwise stateI res stateO
  ) =>
  ParseRC rc i (C.BranchOnState L.Nil otherwise) stateI res stateO
else instance (Row.Union selfs rc newRC, RowList.RowToList selfs rcl, GetDefault rcl combinator, ParseRC newRC i combinator stateI res stateO) => ParseRC rc i (C.Fix selfs) stateI res stateO
else instance (ParseADT rc sym f (Args10 a b c d e g h i j k) stateI res stateO) => ParseRC rc sym (f a b c d e g h i j k) stateI res stateO
else instance (ParseADT rc sym f (Args9 a b c d e g h i j) stateI res stateO) => ParseRC rc sym (f a b c d e g h i j) stateI res stateO
else instance (ParseADT rc sym f (Args8 a b c d e g h i) stateI res stateO) => ParseRC rc sym (f a b c d e g h i) stateI res stateO
else instance (ParseADT rc sym f (Args7 a b c d e g h) stateI res stateO) => ParseRC rc sym (f a b c d e g h) stateI res stateO
else instance (ParseADT rc sym f (Args6 a b c d e g) stateI res stateO) => ParseRC rc sym (f a b c d e g) stateI res stateO
else instance (ParseADT rc sym f (Args5 a b c d e) stateI res stateO) => ParseRC rc sym (f a b c d e) stateI res stateO
else instance (ParseADT rc sym f (Args4 a b c d) stateI res stateO) => ParseRC rc sym (f a b c d) stateI res stateO
else instance (ParseADT rc sym f (Args3 a b c) stateI res stateO) => ParseRC rc sym (f a b c) stateI res stateO
else instance (ParseADT rc sym f (Args2 a b) stateI res stateO) => ParseRC rc sym (f a b) stateI res stateO
else instance (ParseADT rc sym f (Args1 a) stateI res stateO) => ParseRC rc sym (f a) stateI res stateO

-- user-facing parsing
class Parse :: Symbol -> Type -> Type -> Type -> Type -> Constraint
class Parse sym combinator stateI res stateO | sym combinator stateI -> res stateO

instance (ParseRC () sym Any stateI res' stateI, Format res' res) => Parse sym Any stateI res stateI
else instance (ParseRC () sym MatchAZ stateI res' stateI, Format res' res) => Parse sym MatchAZ stateI res stateI
else instance (ParseRC () sym Match09 stateI res' stateI, Format res' res) => Parse sym Match09 stateI res stateI
else instance (ParseRC () sym Matchaz stateI res' stateI, Format res' res) => Parse sym Matchaz stateI res stateI
else instance (ParseRC () sym MatchAlpha stateI res' stateI, Format res' res) => Parse sym MatchAlpha stateI res stateI
else instance (ParseRC () sym MatchAlphanumeric stateI res' stateI, Format res' res) => Parse sym MatchAlphanumeric stateI res stateI
else instance (ParseRC () sym MatchAZ09 stateI res' stateI, Format res' res) => Parse sym MatchAZ09 stateI res stateI
else instance (ParseRC () sym Matchaz09 stateI res' stateI, Format res' res) => Parse sym Matchaz09 stateI res stateI
else instance (ParseRC () sym MatchHex stateI res' stateI, Format res' res) => Parse sym MatchHex stateI res stateI
else instance (ParseRC () sym MatchWhitespace stateI res' stateI, Format res' res) => Parse sym MatchWhitespace stateI res stateI
else instance (ParseRC () sym (Literal m) stateI res' stateI, Format res' res) => Parse sym (Literal m) stateI res stateI
else instance (ParseRC () sym (Some m) stateI res' stateI, Format res' res) => Parse sym (Some m) stateI res stateI
else instance (ParseRC () sym (Many m) stateI res' stateI, Format res' res) => Parse sym (Many m) stateI res stateI
else instance (ParseRC () sym (Except m n) stateI res' stateI, Format res' res) => Parse sym (Except m n) stateI res stateI
else instance (ParseRC () sym (Or m n) stateI res' stateI, Format res' res) => Parse sym (Or m n) stateI res stateI
else instance (ParseRC () sym (And m n) stateI res' stateI, Format res' res) => Parse sym (And m n) stateI res stateI
else instance (ParseRC () sym (Match2 m) stateI res' stateI, Format res' res) => Parse sym (Match2 m) stateI res stateI
else instance (ParseRC () sym (Match3 m) stateI res' stateI, Format res' res) => Parse sym (Match3 m) stateI res stateI
else instance (ParseRC () sym (Match4 m) stateI res' stateI, Format res' res) => Parse sym (Match4 m) stateI res stateI
else instance (ParseRC () sym (Match5 m) stateI res' stateI, Format res' res) => Parse sym (Match5 m) stateI res stateI
else instance (ParseRC () sym (Match6 m) stateI res' stateI, Format res' res) => Parse sym (Match6 m) stateI res stateI
else instance (ParseRC () sym (Match7 m) stateI res' stateI, Format res' res) => Parse sym (Match7 m) stateI res stateI
else instance (ParseRC () sym (Match8 m) stateI res' stateI, Format res' res) => Parse sym (Match8 m) stateI res stateI
else instance (ParseRC () sym (Match9 m) stateI res' stateI, Format res' res) => Parse sym (Match9 m) stateI res stateI

else instance (ParseRC () sym (C.IgnoreAndThenParse ignore parse) stateI res' stateO, Format res' res) => Parse sym (C.IgnoreAndThenParse ignore parse) stateI res stateO
else instance (ParseRC () sym (C.ParseAndThenIgnore parse ignore) stateI res' stateO, Format res' res) => Parse sym (C.ParseAndThenIgnore parse ignore) stateI res stateO
else instance (ParseRC () sym (C.Or left right) stateI res' stateO, Format res' res) => Parse sym (C.Or left right) stateI res stateO
else instance ParseRC () sym (C.Const val) stateI (Success val sym) stateO => Parse sym (C.Const val) stateI (Success val sym) stateO
else instance (ParseRC () sym (C.ModifyStateAfterSuccessOnConstant constant cont) stateI res' stateO, Format res' res) => Parse sym (C.ModifyStateAfterSuccessOnConstant constant cont) stateI res stateO
else instance (ParseRC () sym (C.ModifyStateAfterSuccessWithResult f cont) stateI res' stateO, Format res' res) => Parse sym (C.ModifyStateAfterSuccessWithResult f cont) stateI res stateO
else instance (ParseRC () i (C.Many parse) stateI res' stateO, Format res' res) => Parse i (C.Many parse) stateI res stateO
else instance (ParseRC () i (C.Some parse) stateI res' stateO, Format res' res) => Parse i (C.Some parse) stateI res stateO
else instance (ParseRC () i (C.BranchOnState (L.Cons (C.IfThen stateI cont) rest) otherwise) stateI res' stateO, Format res' res) => Parse i (C.BranchOnState (L.Cons (C.IfThen stateI cont) rest) otherwise) stateI res stateO
else instance (ParseRC () i (C.BranchOnState (L.Cons (C.IfThen stateX cont) rest) otherwise) stateI res' stateO, Format res' res) => Parse i (C.BranchOnState (L.Cons (C.IfThen stateX cont) rest) otherwise) stateI res stateO
else instance (ParseRC () i (C.BranchOnState L.Nil otherwise) stateI res' stateO, Format res' res) => Parse i (C.BranchOnState L.Nil otherwise) stateI res stateO
else instance (RowList.RowToList selfs rcl, GetDefault rcl combinator, ParseRC selfs i combinator stateI res' stateO, Format res' res) => Parse i (C.Fix selfs) stateI res stateO
else instance (ParseRC () sym (f a b c d e g h i j k) stateI res' stateO, Format res' res) => Parse sym (f a b c d e g h i j k) stateI res stateO
else instance (ParseRC () sym (f a b c d e g h i j) stateI res' stateO, Format res' res) => Parse sym (f a b c d e g h i j) stateI res stateO
else instance (ParseRC () sym (f a b c d e g h i) stateI res' stateO, Format res' res) => Parse sym (f a b c d e g h i) stateI res stateO
else instance (ParseRC () sym (f a b c d e g h) stateI res' stateO, Format res' res) => Parse sym (f a b c d e g h) stateI res stateO
else instance (ParseRC () sym (f a b c d e g) stateI res' stateO, Format res' res) => Parse sym (f a b c d e g) stateI res stateO
else instance (ParseRC () sym (f a b c d e) stateI res' stateO, Format res' res) => Parse sym (f a b c d e) stateI res stateO
else instance (ParseRC () sym (f a b c d) stateI res' stateO, Format res' res) => Parse sym (f a b c d) stateI res stateO
else instance (ParseRC () sym (f a b c) stateI res' stateO, Format res' res) => Parse sym (f a b c) stateI res stateO
else instance (ParseRC () sym (f a b) stateI res' stateO, Format res' res) => Parse sym (f a b) stateI res stateO
else instance (ParseRC () sym (f a) stateI res' stateO, Format res' res) => Parse sym (f a) stateI res stateO

class FailOnFail :: Type -> Constraint
class FailOnFail res

instance Fail fail => FailOnFail (Failure fail)
instance FailOnFail (Success a b)

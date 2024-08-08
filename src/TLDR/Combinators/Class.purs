module TLDR.Combinators.Class where

import TLDR.Matchers

import Prim.TypeError (Above, Beside, Doc, Quote, Text)
import TLDR.Combinators (Const, IgnoreAndThenParse)
import TLDR.Combinators as C
import TLDR.Matchers.Class as MatchClass
import TLDR.Matchers.Show (class ShowMatch)
import TLDR.Result (Failure, SingleFailure, Success)
import Type.Proxy (Proxy)

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

class ContinueIfMatch :: Type -> Type -> Type -> Type -> Type -> Constraint
class ContinueIfMatch res' parse stateI res stateO | res' parse stateI -> res stateO

instance ContinueIfMatch (Failure fail) parse stateI (Failure fail) stateI
instance (Parse b parse stateI res stateO) => ContinueIfMatch (Success ignore b) parse stateI res stateO

class UseIfMatch :: Symbol -> Type -> Type -> Type -> Type -> Type -> Type  -> Constraint
class UseIfMatch sym res' parse stateI stateI' res stateO | sym res' parse stateI stateI' -> res stateO

instance (Parse sym parse stateI res stateO) => UseIfMatch sym (Failure fail) parse stateI stateI' res stateO
instance UseIfMatch sym (Success h t) parse stateI stateI' (Success h t) stateI'

class ContinueNary :: forall ctor. Type -> Type -> ctor -> Type -> Type -> Type -> Constraint
class ContinueNary resI stateI ctor args resO stateO | resI stateI ctor args -> resO stateO

instance ShowParser (ctor (Failure fail) b c d e g h i j k) doc => ContinueNary (Failure fail) stateI ctor (Args9 b c d e g h i j k) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d e g h i j) doc => ContinueNary (Failure fail) stateI ctor (Args8 b c d e g h i j) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d e g h i) doc => ContinueNary (Failure fail) stateI ctor (Args7 b c d e g h i) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d e g h) doc => ContinueNary (Failure fail) stateI ctor (Args6 b c d e g h) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d e g) doc => ContinueNary (Failure fail) stateI ctor (Args5 b c d e g) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d e) doc => ContinueNary (Failure fail) stateI ctor (Args4 b c d e) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c d) doc => ContinueNary (Failure fail) stateI ctor (Args3 b c d) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b c) doc => ContinueNary (Failure fail) stateI ctor (Args2 b c) (Failure (SingleFailure doc)) stateI
else instance ShowParser (ctor (Failure fail) b) doc => ContinueNary (Failure fail) stateI ctor (Args1 b) (Failure (SingleFailure doc)) stateI

--
instance (ParseADT rest (ctor yay) (Args9 b c d e g h i j k) stateI res stateO) => ContinueNary (Success yay rest) stateI ctor (Args9 b c d e g h i j k) res stateO
else instance (ParseADT rest (ctor yay) (Args8 b c d e g h i j) stateI res stateO) => ContinueNary (Success yay rest) stateI ctor (Args8 b c d e g h i j) res stateO
else instance (ParseADT rest (ctor yay) (Args7 b c d e g h i) stateI res stateO) => ContinueNary (Success yay rest) stateI ctor (Args7 b c d e g h i) res stateO
else instance (ParseADT rest (ctor yay) (Args6 b c d e g h) stateI res stateO) => ContinueNary (Success yay rest) stateI ctor (Args6 b c d e g h) res stateO
else instance (ParseADT rest (ctor yay) (Args5 b c d e g) stateI res stateO) => ContinueNary (Success yay rest) stateI ctor (Args5 b c d e g) res stateO
else instance (ParseADT rest (ctor yay) (Args4 b c d e) stateI res stateO) => ContinueNary (Success yay rest) stateI ctor (Args4 b c d e) res stateO
else instance (ParseADT rest (ctor yay) (Args3 b c d) stateI res stateO) => ContinueNary (Success yay rest) stateI ctor (Args3 b c d) res stateO
else instance (ParseADT rest (ctor yay) (Args2 b c) stateI res stateO) => ContinueNary (Success yay rest) stateI ctor (Args2 b c) res stateO
else instance (ParseADT rest (ctor yay) (Args1 b) stateI res stateO) => ContinueNary (Success yay rest) stateI ctor (Args1 b) res stateO

-- else instance (Parse rest b stateI res stateO, Sequence2 (ctor yay) res o) => ContinueNary (Success yay rest) stateI ctor b res stateO

class RecastSuccessToProxy :: Type -> Type -> Constraint
class RecastSuccessToProxy i o | i -> o

instance RecastSuccessToProxy (Failure fail) (Failure fail)
instance RecastSuccessToProxy (Success a b) (Success (Proxy a) b)

class Sequence2 :: forall f. f -> Type -> Type -> Constraint
class Sequence2 f res o | f res -> o

instance Sequence2 f (Success a b) (Success (f a) b)
instance ShowParser (f (Failure fail)) o => Sequence2 f (Failure fail) (Failure (SingleFailure o))

class Parse :: Symbol -> Type -> Type -> Type -> Type -> Constraint
class Parse sym combinator stateI res stateO | sym combinator stateI -> res stateO

class ParseADT :: forall ctor. Symbol -> ctor -> Type -> Type -> Type -> Type -> Constraint
class ParseADT sym ctor args stateI res stateO | sym ctor args stateI -> res stateO

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

instance (MatchClass.Match sym Any res', RecastSuccessToProxy res' res) => Parse sym Any stateI res stateI
else instance (MatchClass.Match sym MatchAZ res', RecastSuccessToProxy res' res) => Parse sym MatchAZ stateI res stateI
else instance (MatchClass.Match sym Match09 res', RecastSuccessToProxy res' res) => Parse sym Match09 stateI res stateI
else instance (MatchClass.Match sym Matchaz res', RecastSuccessToProxy res' res) => Parse sym Matchaz stateI res stateI
else instance (MatchClass.Match sym MatchAlpha res', RecastSuccessToProxy res' res) => Parse sym MatchAlpha stateI res stateI
else instance (MatchClass.Match sym MatchAlphanumeric res', RecastSuccessToProxy res' res) => Parse sym MatchAlphanumeric stateI res stateI
else instance (MatchClass.Match sym MatchAZ09 res', RecastSuccessToProxy res' res) => Parse sym MatchAZ09 stateI res stateI
else instance (MatchClass.Match sym Matchaz09 res', RecastSuccessToProxy res' res) => Parse sym Matchaz09 stateI res stateI
else instance (MatchClass.Match sym MatchHex res', RecastSuccessToProxy res' res) => Parse sym MatchHex stateI res stateI
else instance (MatchClass.Match sym MatchWhitespace res', RecastSuccessToProxy res' res) => Parse sym MatchWhitespace stateI res stateI
else instance (MatchClass.Match sym (Literal m) res', RecastSuccessToProxy res' res) => Parse sym (Literal m) stateI res stateI
else instance (MatchClass.Match sym (Some m) res', RecastSuccessToProxy res' res) => Parse sym (Some m) stateI res stateI
else instance (MatchClass.Match sym (Many m) res', RecastSuccessToProxy res' res) => Parse sym (Many m) stateI res stateI
else instance (MatchClass.Match sym (Except m n) res', RecastSuccessToProxy res' res) => Parse sym (Except m n) stateI res stateI
else instance (MatchClass.Match sym (Or m n) res', RecastSuccessToProxy res' res) => Parse sym (Or m n) stateI res stateI
else instance (MatchClass.Match sym (And m n) res', RecastSuccessToProxy res' res) => Parse sym (And m n) stateI res stateI
else instance (MatchClass.Match sym (Match2 m) res', RecastSuccessToProxy res' res) => Parse sym (Match2 m) stateI res stateI
else instance (MatchClass.Match sym (Match3 m) res', RecastSuccessToProxy res' res) => Parse sym (Match3 m) stateI res stateI
else instance (MatchClass.Match sym (Match4 m) res', RecastSuccessToProxy res' res) => Parse sym (Match4 m) stateI res stateI
else instance (MatchClass.Match sym (Match5 m) res', RecastSuccessToProxy res' res) => Parse sym (Match5 m) stateI res stateI
else instance (MatchClass.Match sym (Match6 m) res', RecastSuccessToProxy res' res) => Parse sym (Match6 m) stateI res stateI
else instance (MatchClass.Match sym (Match7 m) res', RecastSuccessToProxy res' res) => Parse sym (Match7 m) stateI res stateI
else instance (MatchClass.Match sym (Match8 m) res', RecastSuccessToProxy res' res) => Parse sym (Match8 m) stateI res stateI
else instance (MatchClass.Match sym (Match9 m) res', RecastSuccessToProxy res' res) => Parse sym (Match9 m) stateI res stateI
--------
else instance (MatchClass.Match sym ignore res', ContinueIfMatch res' parse stateI res stateO) => Parse sym (IgnoreAndThenParse ignore parse) stateI res stateO
else instance (Parse sym left stateI res' stateO', UseIfMatch sym res' right stateI stateI' res stateO) => Parse sym (C.Or left right) stateI res stateO
else instance Parse sym (Const val) stateI (Success val sym) stateO
else instance (ParseADT sym f (Args10 a b c d e g h i j k) stateI res stateO) => Parse sym (f a b c d e g h i j k) stateI res stateO
else instance (ParseADT sym f (Args9 a b c d e g h i j) stateI res stateO) => Parse sym (f a b c d e g h i j) stateI res stateO
else instance (ParseADT sym f (Args8 a b c d e g h i) stateI res stateO) => Parse sym (f a b c d e g h i) stateI res stateO
else instance (ParseADT sym f (Args7 a b c d e g h) stateI res stateO) => Parse sym (f a b c d e g h) stateI res stateO
else instance (ParseADT sym f (Args6 a b c d e g) stateI res stateO) => Parse sym (f a b c d e g) stateI res stateO
else instance (ParseADT sym f (Args5 a b c d e) stateI res stateO) => Parse sym (f a b c d e) stateI res stateO
else instance (ParseADT sym f (Args4 a b c d) stateI res stateO) => Parse sym (f a b c d) stateI res stateO
else instance (ParseADT sym f (Args3 a b c) stateI res stateO) => Parse sym (f a b c) stateI res stateO
else instance (ParseADT sym f (Args2 a b) stateI res stateO) => Parse sym (f a b) stateI res stateO
else instance (ParseADT sym f (Args1 a) stateI res stateO) => Parse sym (f a) stateI res stateO

instance (Parse sym a stateI res' stateO', ContinueNary res' stateO' f (Args9 b c d e g h i j k) res stateO) => ParseADT sym f (Args10 a b c d e g h i j k) stateI res stateO
else instance (Parse sym a stateI res' stateO', ContinueNary res' stateO' f (Args8 b c d e g h i j) res stateO) => ParseADT sym f (Args9 a b c d e g h i j) stateI res stateO
else instance (Parse sym a stateI res' stateO', ContinueNary res' stateO' f (Args7 b c d e g h i) res stateO) => ParseADT sym f (Args8 a b c d e g h i) stateI res stateO
else instance (Parse sym a stateI res' stateO', ContinueNary res' stateO' f (Args6 b c d e g h) res stateO) => ParseADT sym f (Args7 a b c d e g h) stateI res stateO
else instance (Parse sym a stateI res' stateO', ContinueNary res' stateO' f (Args5 b c d e g) res stateO) => ParseADT sym f (Args6 a b c d e g) stateI res stateO
else instance (Parse sym a stateI res' stateO', ContinueNary res' stateO' f (Args4 b c d e) res stateO) => ParseADT sym f (Args5 a b c d e) stateI res stateO
else instance (Parse sym a stateI res' stateO', ContinueNary res' stateO' f (Args3 b c d) res stateO) => ParseADT sym f (Args4 a b c d) stateI res stateO
else instance (Parse sym a stateI res' stateO', ContinueNary res' stateO' f (Args2 b c) res stateO) => ParseADT sym f (Args3 a b c) stateI res stateO
else instance (Parse sym a stateI res' stateO', ContinueNary res' stateO' f (Args1 b) res stateO) => ParseADT sym f (Args2 a b) stateI res stateO
else instance (Parse sym a stateI res' stateO, Sequence2 f res' res) => ParseADT sym f (Args1 a) stateI res stateO
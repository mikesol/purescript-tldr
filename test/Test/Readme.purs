module Test.Readme where

import Prelude

import Prim.TypeError (Text)
import TLDR.Combinators (ParseAndThenIgnore)
import TLDR.Combinators as C
import TLDR.Combinators.Class (class FailOnFail, class Parse, class ShowParser, SP1, SP2)
import TLDR.Matchers as M
import TLDR.Sugar (Bracket, DQ, L4, L5, WS, WSM)
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
  $ C.Fix
      ( mySONFix ::
          C.Or ParseInt
            $ C.Or ParseString
            $ C.Or ParseBoolean
            $ C.Or (ParseArray (Proxy "mySONFix")) (ParseObject (Proxy "mySONFix"))
      )

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

-- mysonFail = mysonFail' unit
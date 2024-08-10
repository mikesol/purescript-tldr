module Test.Sql1 where

import Prelude

import Prim.TypeError (Text)
import TLDR.Combinators as C
import TLDR.Combinators.Class (class Parse)
import TLDR.Combinators.Class as CC
import TLDR.List as L
import TLDR.Matchers as M
import TLDR.Result as R
import TLDR.Sugar (L3, L4, L5, L6, WS, WSM, Bracket)
import Type.Proxy (Proxy(..))

data Column a

instance CC.ShowParser (CC.SP1 "Column" a) doc => CC.ShowParser (Column a) doc

type ColumnP = Column (M.And M.MatchAlpha (M.Many (M.Or (M.Literal "_") M.MatchAlphanumeric)))

data SInt

instance CC.ShowParser SInt (Text "Int")

type IntP = C.IgnoreAndThenParse (L3 "i" "n" "t") (C.Const SInt)

data SText

instance CC.ShowParser SText (Text "Text")

type TextP = C.IgnoreAndThenParse (L4 "t" "e" "x" "t") (C.Const SText)

data ColumnDef k v

instance CC.ShowParser (CC.SP2 "ColumnDef" a b) doc => CC.ShowParser (ColumnDef a b) doc

type ColumnDefP = ColumnDef ColumnP (C.IgnoreAndThenParse (M.Some M.MatchWhitespace) (C.Or IntP TextP))

type ColumnDefs = C.SepBy ColumnDefP (WSM (M.Literal ","))

data TableDef n c

instance CC.ShowParser (CC.SP2 "TableDef" a b) doc => CC.ShowParser (TableDef a b) doc

type TableDefP = WS (C.ParseAndThenIgnore
  ( WS
      ( TableDef
          ( C.IgnoreAndThenParse
              ( M.And (L6 "C" "R" "E" "A" "T" "E")
                  ( M.And (M.Some M.MatchWhitespace)
                      ( M.And (L5 "T" "A" "B" "L" "E")
                          (M.Some M.MatchWhitespace)
                      )
                  )
              )
              (M.Some M.MatchAlpha)
          )
          ( WS
              ( Bracket (M.Literal "(")
                  (WS ColumnDefs)
                  (M.Literal ")")
              )
          )
      )
  )
  (M.Literal ";"))

mytable0' :: forall a. Parse """
    CREATE TABLE foo (id int, lastname text, firstname text);
    """ TableDefP Unit a Unit => Unit -> Proxy a
mytable0' _ = Proxy

mytable0 = mytable0' unit

type TableDefsP = C.Many TableDefP

mytables0' :: forall a. Parse """
    CREATE TABLE foo (id int, lastname text, firstname text);
    CREATE TABLE bar (id int, time_created int, nickname text);
    CREATE TABLE baz (id int, username text);
    CREATE TABLE qux (id int, user1 text, user2 text, user3 text, user4 text, user5 text);
    CREATE TABLE goo (id int, last_updated int);

    CREATE TABLE fooA (id int, lastname text, firstname text);
    CREATE TABLE barA (id int, time_created int, nickname text);
    CREATE TABLE bazA (id int, username text);
    CREATE TABLE quxA (id int, user1 text, user2 text);
    CREATE TABLE gooA (id int, last_updated int);

    """ TableDefsP Unit a Unit => Unit -> Proxy a
mytables0' _ = Proxy

mytables0 = mytables0' unit

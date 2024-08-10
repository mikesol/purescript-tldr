module Test.Sql0 where

import Prelude

import Prim.TypeError (Text)
import TLDR.Combinators as C
import TLDR.Combinators.Class as CC
import TLDR.List as L
import TLDR.Matchers as M
import TLDR.Result as R
import Type.Proxy (Proxy)

type IdentM = M.Some M.Matchaz

data Ident a

instance CC.ShowParser (CC.SP1 "Ident" a) doc => CC.ShowParser (Ident a) doc

data TabledIdent a b

instance CC.ShowParser (CC.SP2 "TabledIdent" a b) doc => CC.ShowParser (TabledIdent a b) doc

data Wildcard

instance CC.ShowParser Wildcard (Text "Wildcard")

type SingleColumn = C.Or (C.IgnoreAndThenParse (M.Literal "\"") (C.ParseAndThenIgnore (Ident IdentM) (M.Literal "\""))) (Ident IdentM)
type Column = C.Or (TabledIdent SingleColumn (C.IgnoreAndThenParse (M.Literal ".") SingleColumn)) SingleColumn
type Table = Column
type Columns = C.Or (C.SepBy1 Column (M.And (M.Many M.MatchWhitespace) (M.And (M.Literal ",") (M.Many M.MatchWhitespace)))) (C.IgnoreAndThenParse (M.Literal "*") (C.Const Wildcard))

data SelectStmt columns table next

instance CC.ShowParser (CC.SP3 "SelectStmt" a b c) doc => CC.ShowParser (SelectStmt a b c) doc

data SelectEnd

type Select = C.IgnoreAndThenParse
  (M.And (M.Literal "S") (M.And (M.Literal "E") (M.And (M.Literal "L") (M.And (M.Literal "E") (M.And (M.Literal "C") (M.Literal "T"))))))
  ( SelectStmt
      ( C.IgnoreAndThenParse
          (M.Some M.MatchWhitespace)
          (C.ParseAndThenIgnore Columns (M.Some M.MatchWhitespace))
      )
      ( C.IgnoreAndThenParse
          (M.And (M.Literal "F") (M.And (M.Literal "R") (M.And (M.Literal "O") (M.Literal "M"))))
          (C.IgnoreAndThenParse (M.Some M.MatchWhitespace) Table)
      )
      (C.IgnoreAndThenParse (M.Many M.MatchWhitespace) (C.IgnoreAndThenParse (M.Literal ";") (C.Const SelectEnd)))
  )

testColumn :: forall @toParse @h @t. CC.Parse toParse Column Unit (R.Success h t) Unit => Unit
testColumn = unit

testColumn0 = testColumn @"id" @(Ident (Proxy "id")) @""
testColumn1 = testColumn @"\"id\"" @(Ident (Proxy "id")) @""
testColumn2 = testColumn @"\"user\".\"id\"" @(TabledIdent (Ident (Proxy "user")) (Ident (Proxy "id"))) @""
testColumn3 = testColumn @"user.id" @(TabledIdent (Ident (Proxy "user")) (Ident (Proxy "id"))) @""

testColumns :: forall @toParse @h @t. CC.Parse toParse Columns Unit (R.Success h t) Unit => Unit
testColumns = unit

testColumns0 = testColumns @"id,email" @(L.Cons (Ident (Proxy "id")) (L.Cons (Ident (Proxy "email")) L.Nil)) @""
testColumns1 = testColumns @"id  , email" @(L.Cons (Ident (Proxy "id")) (L.Cons (Ident (Proxy "email")) L.Nil)) @""
testColumns2 = testColumns @"id" @(L.Cons (Ident (Proxy "id")) L.Nil) @""
testColumns3 = testColumns @"*" @Wildcard @""

testSelect :: forall @toParse @h @t. CC.Parse toParse Select Unit (R.Success h t) Unit => Unit
testSelect = unit

testSelect0 = testSelect @"SELECT id,email FROM user;" @(SelectStmt (L.Cons (Ident (Proxy "id")) (L.Cons (Ident (Proxy "email")) L.Nil)) (Ident (Proxy "user")) SelectEnd) @""

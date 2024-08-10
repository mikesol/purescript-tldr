module TLDR.Sugar where

import TLDR.Combinators as C
import TLDR.Matchers as M

type WS a = C.IgnoreAndThenParse (M.Many M.MatchWhitespace) (C.ParseAndThenIgnore a (M.Many M.MatchWhitespace))
type WSM a = M.And (M.Many M.MatchWhitespace) (M.And a (M.Many M.MatchWhitespace))
type DQ a = C.IgnoreAndThenParse (M.Literal "\"") (C.ParseAndThenIgnore a (M.Literal "\""))
type DQM a = M.And (M.Literal "\"") (M.And a (M.Literal "\""))
type Bracket a b c = C.IgnoreAndThenParse a (C.ParseAndThenIgnore b c)
type L1 a = M.Literal a
type L2 a b = M.And (M.Literal a) (M.Literal b)
type L3 a b c = M.And (M.Literal a) (M.And (M.Literal b) (M.Literal c))
type L4 a b c d = M.And (M.Literal a) (M.And (M.Literal b) (M.And (M.Literal c) (M.Literal d)))
type L5 a b c d e = M.And (M.Literal a) (M.And (M.Literal b) (M.And (M.Literal c) (M.And (M.Literal d) (M.Literal e))))
type L6 a b c d e f = M.And (M.Literal a) (M.And (M.Literal b) (M.And (M.Literal c) (M.And (M.Literal d) (M.And (M.Literal e) (M.Literal f)))))
type L7 a b c d e f g = M.And (M.Literal a) (M.And (M.Literal b) (M.And (M.Literal c) (M.And (M.Literal d) (M.And (M.Literal e) (M.And (M.Literal f) (M.Literal g))))))
type L8 a b c d e f g h = M.And (M.Literal a) (M.And (M.Literal b) (M.And (M.Literal c) (M.And (M.Literal d) (M.And (M.Literal e) (M.And (M.Literal f) (M.And (M.Literal g) (M.Literal h)))))))
type L9 a b c d e f g h i = M.And (M.Literal a) (M.And (M.Literal b) (M.And (M.Literal c) (M.And (M.Literal d) (M.And (M.Literal e) (M.And (M.Literal f) (M.And (M.Literal g) (M.And (M.Literal h) (M.Literal i))))))))
type L10 a b c d e f g h i j = M.And (M.Literal a) (M.And (M.Literal b) (M.And (M.Literal c) (M.And (M.Literal d) (M.And (M.Literal e) (M.And (M.Literal f) (M.And (M.Literal g) (M.And (M.Literal h) (M.And (M.Literal i) (M.Literal j)))))))))

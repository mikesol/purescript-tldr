module TLDR.Matchers.MatchWhitespace where

import Prim.TypeError (Beside, Text)
import TLDR.Result (Failure, SingleFailure, Success)

class MatchWhitespace :: Symbol -> Type -> Constraint
class MatchWhitespace i o | i -> o

instance MatchWhitespace " " (Success " " "")
else instance MatchWhitespace "\t" (Success "\t" "")
else instance MatchWhitespace "\n" (Success "\n" "")
else instance MatchWhitespace "\r" (Success "\r" "")
else instance MatchWhitespace fail (Failure (SingleFailure (Beside (Text "Could not match whitespace against ") (Text fail))))

module TLDR.Matchers.MatchUaz where

import Prim.TypeError (Beside, Text)
import TLDR.Result (Failure, SingleFailure, Success)

class MatchAZ :: Symbol -> Type -> Constraint
class MatchAZ i o | i -> o

instance MatchAZ "A" (Success "A" "")
else instance MatchAZ "B" (Success "B" "")
else instance MatchAZ "C" (Success "C" "")
else instance MatchAZ "D" (Success "D" "")
else instance MatchAZ "E" (Success "E" "")
else instance MatchAZ "F" (Success "F" "")
else instance MatchAZ "G" (Success "G" "")
else instance MatchAZ "H" (Success "H" "")
else instance MatchAZ "I" (Success "I" "")
else instance MatchAZ "J" (Success "J" "")
else instance MatchAZ "K" (Success "K" "")
else instance MatchAZ "L" (Success "L" "")
else instance MatchAZ "M" (Success "M" "")
else instance MatchAZ "N" (Success "N" "")
else instance MatchAZ "O" (Success "O" "")
else instance MatchAZ "P" (Success "P" "")
else instance MatchAZ "Q" (Success "Q" "")
else instance MatchAZ "R" (Success "R" "")
else instance MatchAZ "S" (Success "S" "")
else instance MatchAZ "T" (Success "T" "")
else instance MatchAZ "U" (Success "U" "")
else instance MatchAZ "V" (Success "V" "")
else instance MatchAZ "W" (Success "W" "")
else instance MatchAZ "X" (Success "X" "")
else instance MatchAZ "Y" (Success "Y" "")
else instance MatchAZ "Z" (Success "Z" "")
else instance MatchAZ fail (Failure (SingleFailure (Beside (Text "Could not match A-Z against ") (Text fail))))

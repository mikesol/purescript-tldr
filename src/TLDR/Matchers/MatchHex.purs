module TLDR.Matchers.MatchHex where

import Prim.TypeError (Beside, Text)
import TLDR.Result (Failure, SingleFailure, Success)

class MatchHex :: Symbol -> Type -> Constraint
class MatchHex i o | i -> o

instance MatchHex "0" (Success "0" "")
else instance MatchHex "1" (Success "1" "")
else instance MatchHex "2" (Success "2" "")
else instance MatchHex "3" (Success "3" "")
else instance MatchHex "4" (Success "4" "")
else instance MatchHex "5" (Success "5" "")
else instance MatchHex "6" (Success "6" "")
else instance MatchHex "7" (Success "7" "")
else instance MatchHex "8" (Success "8" "")
else instance MatchHex "9" (Success "9" "")
else instance MatchHex "A" (Success "A" "")
else instance MatchHex "B" (Success "B" "")
else instance MatchHex "C" (Success "C" "")
else instance MatchHex "D" (Success "D" "")
else instance MatchHex "E" (Success "E" "")
else instance MatchHex "F" (Success "F" "")
else instance MatchHex "a" (Success "a" "")
else instance MatchHex "b" (Success "b" "")
else instance MatchHex "c" (Success "c" "")
else instance MatchHex "d" (Success "d" "")
else instance MatchHex "e" (Success "e" "")
else instance MatchHex "f" (Success "f" "")
else instance MatchHex fail (Failure (SingleFailure (Beside (Text "Could not match a hexidecimal digit against ") (Text fail))))

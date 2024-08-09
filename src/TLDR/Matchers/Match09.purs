module TLDR.Matchers.Match09 where

import Prim.TypeError (Beside, Text)
import TLDR.Result (Failure, Success, SingleFailure)

class Match09 :: Symbol -> Type -> Constraint
class Match09 i o | i -> o

instance Match09 "0" (Success "0" "")
else instance Match09 "1" (Success "1" "")
else instance Match09 "2" (Success "2" "")
else instance Match09 "3" (Success "3" "")
else instance Match09 "4" (Success "4" "")
else instance Match09 "5" (Success "5" "")
else instance Match09 "6" (Success "6" "")
else instance Match09 "7" (Success "7" "")
else instance Match09 "8" (Success "8" "")
else instance Match09 "9" (Success "9" "")
else instance Match09 fail (Failure (SingleFailure (Beside (Text "Could not match 0-9 against ") (Text fail))))

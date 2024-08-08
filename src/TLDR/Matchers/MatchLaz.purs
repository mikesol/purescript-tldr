module TLDR.Matchers.MatchLaz where

import Prim.TypeError (Beside, Text)
import TLDR.Result (Failure, SingleFailure, Success)

class Matchaz :: Symbol -> Type -> Constraint
class Matchaz i o | i -> o

instance Matchaz "a" (Success "a" "")
else instance Matchaz "b" (Success "b" "")
else instance Matchaz "c" (Success "c" "")
else instance Matchaz "d" (Success "d" "")
else instance Matchaz "e" (Success "e" "")
else instance Matchaz "f" (Success "f" "")
else instance Matchaz "g" (Success "g" "")
else instance Matchaz "h" (Success "h" "")
else instance Matchaz "i" (Success "i" "")
else instance Matchaz "j" (Success "j" "")
else instance Matchaz "k" (Success "k" "")
else instance Matchaz "l" (Success "l" "")
else instance Matchaz "m" (Success "m" "")
else instance Matchaz "n" (Success "n" "")
else instance Matchaz "o" (Success "o" "")
else instance Matchaz "p" (Success "p" "")
else instance Matchaz "q" (Success "q" "")
else instance Matchaz "r" (Success "r" "")
else instance Matchaz "s" (Success "s" "")
else instance Matchaz "t" (Success "t" "")
else instance Matchaz "u" (Success "u" "")
else instance Matchaz "v" (Success "v" "")
else instance Matchaz "w" (Success "w" "")
else instance Matchaz "x" (Success "x" "")
else instance Matchaz "y" (Success "y" "")
else instance Matchaz "z" (Success "z" "")
else instance Matchaz fail (Failure (SingleFailure (Beside (Text "Could not match a-z against ") (Text fail)) ))
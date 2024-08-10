module TLDR.Matchers.Show where

import Prim.Symbol as S
import TLDR.Matchers

class ShowMatch :: Type -> Symbol -> Constraint
class ShowMatch m s | m -> s

instance showMatchAny :: ShowMatch Any "Any"
instance showMatchMatchAZ :: ShowMatch MatchAZ "A-Z"
instance showMatchMatch09 :: ShowMatch Match09 "0-9"
instance showMatchMatchaz :: ShowMatch Matchaz "a-z"
instance showMatchMatchAlpha :: ShowMatch MatchAlpha "A-Za-z"
instance showMatchMatchAlphanumeric :: ShowMatch MatchAlphanumeric "A-Za-z0-9"
instance showMatchMatchAZ09 :: ShowMatch MatchAZ09 "A-Z0-9"
instance showMatchMatchaz09 :: ShowMatch Matchaz09 "a-z0-9"
instance showMatchMatchHex :: ShowMatch MatchHex "0-9A-Fa-f"
instance showMatchMatchWhitespace :: ShowMatch MatchWhitespace "Whitespace"
instance showMatchTwo :: (ShowMatch m s, S.Append "Two " s o) => ShowMatch (Match2 m) o
instance showMatchThree :: (ShowMatch m s, S.Append "Three " s o) => ShowMatch (Match3 m) o
instance showMatchFour :: (ShowMatch m s, S.Append "Four " s o) => ShowMatch (Match4 m) o
instance showMatchFive :: (ShowMatch m s, S.Append "Five " s o) => ShowMatch (Match5 m) o
instance showMatchSix :: (ShowMatch m s, S.Append "Six " s o) => ShowMatch (Match6 m) o
instance showMatchSeven :: (ShowMatch m s, S.Append "Seven " s o) => ShowMatch (Match7 m) o
instance showMatchEight :: (ShowMatch m s, S.Append "Eight " s o) => ShowMatch (Match8 m) o
instance showMatchNine :: (ShowMatch m s, S.Append "Nine " s o) => ShowMatch (Match9 m) o
instance showMatchSome :: (ShowMatch m s, S.Append "Some " s o) => ShowMatch (Some m) o
instance showMatchMany :: (ShowMatch m s, S.Append "Many " s o) => ShowMatch (Many m) o
instance showMatchExcept :: (ShowMatch m s, ShowMatch n t, S.Append "Except " s o', S.Append o' t o) => ShowMatch (Except m n) o
instance showMatchOr :: (ShowMatch m s, ShowMatch n t, S.Append "Or " s o', S.Append o' t o) => ShowMatch (Or m n) o
instance showMatchAnd :: (ShowMatch m s, ShowMatch n t, S.Append "And " s o', S.Append o' t o) => ShowMatch (And m n) o
instance showMatchLiteral :: (S.Append "Literal " l o) => ShowMatch (Literal l) o
instance showMatchNoop :: ShowMatch Noop "Noop"
instance showMatchEOF :: ShowMatch EOF "EOF"

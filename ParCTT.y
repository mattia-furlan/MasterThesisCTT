{
module ParCTT ( happyError, myLexer, pProgram,pToplevel, pTerm )
where

import Prelude
import qualified Data.Map as Map

import Ident
import qualified CoreCTT
import Interval
import LexCTT
}

%name pProgram Program
%name pToplevel Toplevel
%name pTerm Term
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  '*' { PT _ (TS _ 3) }
  '+' { PT _ (TS _ 4) }
  ',' { PT _ (TS _ 5) }
  '->' { PT _ (TS _ 6) }
  '.1' { PT _ (TS _ 7) }
  '.2' { PT _ (TS _ 8) }
  '/\\' { PT _ (TS _ 9) }
  '0' { PT _ (TS _ 10) }
  '1' { PT _ (TS _ 11) }
  ':' { PT _ (TS _ 12) }
  ';' { PT _ (TS _ 13) }
  '<' { PT _ (TS _ 14) }
  '=' { PT _ (TS _ 15) }
  '>' { PT _ (TS _ 16) }
  'I' { PT _ (TS _ 17) }
  'N' { PT _ (TS _ 18) }
  'S' { PT _ (TS _ 19) }
  'U' { PT _ (TS _ 20) }
  'Z' { PT _ (TS _ 21) }
  '[' { PT _ (TS _ 22) }
  '\\/' { PT _ (TS _ 23) }
  ']' { PT _ (TS _ 24) }
  'comp' { PT _ (TS _ 25) }
  'ind' { PT _ (TS _ 26) }
  'inl' { PT _ (TS _ 27) }
  'inr' { PT _ (TS _ 28) }
  'split' { PT _ (TS _ 29) }
  '|' { PT _ (TS _ 30) }
  L_Ident  { PT _ (TV $$) }

%%

Ident :: { Ident }
Ident  : L_Ident { Ident $1 }

ListIdent :: { [Ident] }
ListIdent  : Ident { [$1] }
           | Ident ',' ListIdent { (:) $1 $3 }

Program :: { CoreCTT.Program }
Program : ListToplevel { CoreCTT.Program $1 }

Term :: { CoreCTT.Term }
Term : Term1 '->' Term { CoreCTT.Abst (Ident "") $1 $3 }
     | '['  Ident ':' Term '=' Term ']' Term { CoreCTT.TDef ($2,$4,$6) $8 }
     | '[' Ident ':' Term ']' Term { CoreCTT.Abst $2 $4 $6 }
     | '[' ListIdent ':' Term ']' Term
       { foldr (\i e -> CoreCTT.Abst i $4 e) $6 $2 }
     | Term1 { $1 }

Term1 :: { CoreCTT.Term }
Term1 : Term2 '+' Term1 { CoreCTT.Sum $1 $3 }
      | Term2 '*' Term1 { CoreCTT.Sigma (Ident "") $1 $3 }
      | '<' ListIdent ':' Term '>' Term
        { foldr (\i e -> CoreCTT.Sigma i $4 e) $6 $2 }
      | '[' DisjFormula ']' Term1 { CoreCTT.Partial $2 $4 }
      | System Term1 { CoreCTT.Restr $1 $2 }
      | Term2 ',' Term2 { CoreCTT.Pair $1 $3 }
      | Term2 { $1 }

Term2 :: { CoreCTT.Term }
Term2 : Term2 Term3 { CoreCTT.App $1 $2 }
      | 'ind' Term3 Term3 Term3 Term3 { CoreCTT.Ind $2 $3 $4 $5 }
      | 'comp' Term3 '(' DisjFormula ')' Term3 Term3 Term3 Term3
        { CoreCTT.Comp $2 $4 $6 $7 $8 $9 }
      | 'comp' Term3 '(' ')' Term3 Term3 Term3 Term3
        { CoreCTT.Comp $2 fFalse $5 $6 $7 $8 }
      | 'S' Term3 { CoreCTT.Succ $2 }
      | 'inl' Term3 { CoreCTT.InL $2 }
      | 'inr' Term3 { CoreCTT.InR $2 }
      | 'split' Term3 Term3 Term3 Term3 { CoreCTT.Split $2 $3 $4 $5 }
      | Term3 { $1 }

Term3 :: { CoreCTT.Term }
Term3 : Ident { CoreCTT.Var $1 }
      | 'U' { CoreCTT.Universe }
      | 'N' { CoreCTT.Nat }
      | 'Z' { CoreCTT.Zero }
      | 'I' { CoreCTT.I }
      | Term3 '.1' { CoreCTT.Fst $1 }
      | Term3 '.2' { CoreCTT.Snd $1 }
      | System { CoreCTT.Sys $1 }
      | '0' { CoreCTT.I0 }
      | '1' { CoreCTT.I1 }
      | '(' Term ')' { $2 }


Toplevel :: { CoreCTT.Toplevel }
Toplevel : Ident ':' Term '=' Term { CoreCTT.Definition $1 $3 $5 }
         | Ident ':' Term { CoreCTT.Declaration $1 $3 }
         | Term { CoreCTT.Example $1 }

ListToplevel :: { [CoreCTT.Toplevel] }
ListToplevel : {- empty -} { [] }
             | Toplevel ';' ListToplevel { (:) $1 $3 }


AtomicFormula :: { AtomicFormula }
AtomicFormula : Ident '=' '0' { Eq0 $1 }
              | Ident '=' '1' { Eq1 $1 }
              | Ident '=' Ident { Diag $1 $3 }

ConjFormula1 :: { [AtomicFormula] }
ConjFormula1 : AtomicFormula { [$1] }
             | AtomicFormula '/\\' ConjFormula1 { $1 : $3 }

ConjFormula :: { ConjFormula }
ConjFormula : ConjFormula1 { Conj $1 }
            | '(' ConjFormula ')' { $2 }

DisjFormula1 :: { [ConjFormula] }
DisjFormula1 : ConjFormula { [$1] }
             | ConjFormula '\\/' DisjFormula1 { $1 : $3 }

DisjFormula :: { DisjFormula }
DisjFormula : DisjFormula1 { Disj $1 }

System :: { CoreCTT.System }
System : '[' ListSysElem ']' { $2 }

SysElem :: { (ConjFormula,CoreCTT.Term) }
SysElem : ConjFormula '->' Term { ($1,$3) }

ListSysElem :: { [(ConjFormula,CoreCTT.Term)] }
ListSysElem : {- empty -} { [] }
            | SysElem { [$1] }
            | SysElem '|' ListSysElem { $1 : $3 }

{
type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
}
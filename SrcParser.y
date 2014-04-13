{
module SrcParser where

import Data.List
import qualified Data.Set as S

import SrcToken
import SrcLexer
import SyntaxSource

}

%name parser
%tokentype { SrcToken }
%error { parseError }
%monad { Maybe } { >>= } { return }

%token

var { SrcTokenVar $$ }
iface { SrcTokenIfaceName $$ }
int { SrcTokenInt $$ }
tint { SrcTokenTInt }
implicit { SrcTokenImplicit }
interface { SrcTokenInterface }
let { SrcTokenLet }
in { SrcTokenIn }
forall { SrcTokenForall }
'\\' { SrcTokenLambda }
'(' { SrcTokenOP }
')' { SrcTokenCP }
'{' { SrcTokenOB }
'}' { SrcTokenCB }
"=>" { SrcTokenBigArrow }
"->" { SrcTokenArrow }
'=' { SrcTokenEQ }
',' { SrcTokenComma }
'.' { SrcTokenDot }
':' { SrcTokenColon }
';' { SrcTokenSemiColon }
'?' { SrcTokenQuestion }

%left SrcTokenBigArrow
%left SrcTokenArrow

%%

Program 
: Interfaces Exp        {($1, $2)}

Exp
  : FExp                              { $1 }
  | '\\' var '.' Exp                  { SELam $2 Nothing $4 }
  | '\\' var ':' Tau '.' Exp          { SELam $2 (Just $4) $6 }
  | implicit '{' VarList '}' in Exp   { SEImplicit $3 $6 }
  | let var ':' Scheme '=' Exp in Exp { SELet $2 $4 $6 $8 }

AExp
  : int                    { SELit $1 }
  | var                    { SEVar $1 }
  | var ':' Tau            { SELVar (Just $3) $1 }
  | '?'                    { SEQuery Nothing }
  | '?' Tau                { SEQuery (Just $2) }
  | '(' Exp ')'            { $2 }
--  | '(' Exp ':' Scheme ')' { SEAnnot $2 $4 }
  | iface '{' DefList '}'  { SEImpl $1 $3 Nothing }
  | iface '{' DefList '}' ':' iface ATaus { SEImpl $1 $3 (Just (STIface $6 $7)) }

FExp
  : FExp AExp               { SEApp $1 $2 }
  | AExp                    { $1 }

VarList
  :                  { [] }
  | var  VarList     { $1:$2 }

Tau
  : ATau              { $1 }
  | ATau "->" Tau     { STFun $1 $3 }

ATaus
  : ATau              { [$1] }
  | ATau ATaus        { $1:$2 }

ATau
  : '(' Tau ')'      { $2 }
  | tint             { STInt }
  | var              { STVar $1 }
  | iface ATaus      { STIface $1 $2 }

Schemes
  :                      { [] }
  | AScheme              { [$1] }
  | AScheme ',' Schemes  { $1:$3 }

AScheme
  : Scheme               { $1 }

SchemeSet
  : '{' Schemes '}'      { $2 }

Scheme
  : forall VarList '.' SchemeSet "=>" Tau { Scheme $2 $4 $6 }
  | forall VarList '.' Tau                { Scheme $2 [] $4 }
  | SchemeSet "=>" Tau                    { Scheme [] $1 $3 }
  | Tau                                   { lift $1 }

Interfaces
  :                      { [] }
  | Interface Interfaces { $1 : $2 }

Interface
  : interface iface VarList '=' '{' DeclList '}' { Interface $2 $3 $6 }

DeclList
  : Decl                 { [$1] }
  | Decl ';' DeclList    { $1:$3 }

Decl
  : var ':' Tau          { ($1, $3) }

DefList
  : Def                { [$1] }
  | Def ',' DefList    { $1:$3 }

Def
  : var '=' Exp        { ($1, $3) }

{

parseError :: [SrcToken] -> Maybe a
parseError _ = error "Parse error"

srcparse inp = do
  (interfaces, pgm) <- parser (srclexer inp)
  let getDecls (Interface _ _ decls) = decls
  let fields = S.fromList $ map fst $ concatMap getDecls interfaces
  return $ (interfaces, transformLVarToField fields $ transformVarToLVar pgm)

}

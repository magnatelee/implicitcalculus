{
module SrcLexer (srclexer) where

import SrcToken

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+    ;
  \\         { \_ -> SrcTokenLambda }
  implicit   { \_ -> SrcTokenImplicit }
  interface  { \_ -> SrcTokenInterface }
  let        { \_ -> SrcTokenLet }
  in         { \_ -> SrcTokenIn }
  forall     { \_ -> SrcTokenForall }
  Int	       { \_ -> SrcTokenTInt }
  [A-Z] [$alpha $digit \_ \']* { \s -> SrcTokenIfaceName s }
  [a-z] [$alpha $digit \_ \']* { \s -> SrcTokenVar s }
  $digit+    { \s -> SrcTokenInt (read s) }
  \(         { \_ -> SrcTokenOP }
  \)         { \_ -> SrcTokenCP }
  \{         { \_ -> SrcTokenOB }
  \}         { \_ -> SrcTokenCB }
  \:         { \_ -> SrcTokenColon }
  \;         { \_ -> SrcTokenSemiColon }
  \-\>       { \_ -> SrcTokenArrow }
  \=\>       { \_ -> SrcTokenBigArrow }
  \.         { \_ -> SrcTokenDot }
  \,         { \_ -> SrcTokenComma }
  \=         { \_ -> SrcTokenEQ }
  \?         { \_ -> SrcTokenQuestion }
{
srclexer s = alexScanTokens s
}  



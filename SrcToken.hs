module SrcToken where

data SrcToken = SrcTokenLambda
              | SrcTokenImplicit
              | SrcTokenInterface
              | SrcTokenTInt
              | SrcTokenLet
              | SrcTokenIn
              | SrcTokenVar String
              | SrcTokenIfaceName String
              | SrcTokenInt Int
              | SrcTokenOP              -- (
              | SrcTokenCP              -- )
              | SrcTokenOB              -- {
              | SrcTokenCB              -- }
              | SrcTokenEQ
              | SrcTokenColon
              | SrcTokenSemiColon
              | SrcTokenArrow           -- ->
              | SrcTokenBigArrow        -- =>
              | SrcTokenComma
              | SrcTokenDot
              | SrcTokenQuestion
              | SrcTokenForall
              | SrcTokenWith
              deriving (Show)

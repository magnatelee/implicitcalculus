{-# OPTIONS -XFlexibleInstances #-}

module SyntaxSource where

import Control.Arrow (second)
import Data.List (intersperse, intercalate)
import Data.Set hiding (null, map)

import Types

data SrcType t =
  STVar t
  | STInt
  | STFun (SrcType t) (SrcType t)
  | STIface String [SrcType t]

data SrcScheme t = Scheme [t] [SrcScheme t] (SrcType t)
  
showSrcType :: SrcType String -> String
showSrcType (STVar var) = var
showSrcType STInt = "Int"
showSrcType (STFun a b) = "(" ++ showSrcType a ++ ") -> " ++
                          showSrcType b
showSrcType (STIface iface types) =
  iface ++ " " ++ (intercalate " " (map showSrcType types))

lift :: SrcType a -> SrcScheme a
lift srctype = Scheme [] [] srctype

showSrcScheme :: SrcScheme String -> String
showSrcScheme (Scheme tvars schemes srctype) =
  (if null tvars then ""
   else "forall " ++ intercalate "," tvars ++ ". ") ++
  (if null schemes then ""
   else "{" ++ intercalate "," (map showSrcScheme schemes) ++ "} => ") ++
  showSrcType srctype

instance Show (SrcType String) where
  show = showSrcType
  
instance Show (SrcScheme String) where
  show = showSrcScheme

data SrcInterface t e = Interface String [t] [(e, SrcType t)]

showSrcInterface :: SrcInterface String String -> String
showSrcInterface (Interface iface tvars decls) =
  "interface " ++ iface ++ " " ++ (intercalate " " tvars) ++ " = { " ++
  (intercalate "; " . map (\(u, t) -> u ++ ":" ++ showSrcType t)) decls ++
  " }"

showPgm (ifaces, exp) =
  "[Interfaces]\n" ++
    intercalate "\n" (map show ifaces) ++ "\n" ++
    "[Program]\n" ++
    show exp ++ "\n"

instance Show (SrcInterface String String) where
  show = showSrcInterface

data SrcExp t e =
  SEVar e
  | SELit Int
  | SELam e (Maybe (SrcType t)) (SrcExp t e)
  | SEApp (SrcExp t e) (SrcExp t e)
  | SELVar (Maybe (SrcType t)) e
  | SEField (Maybe (SrcType t)) e
  | SELet e (SrcScheme t) (SrcExp t e) (SrcExp t e)
  | SEImplicit [e] (SrcExp t e)
  | SEQuery (Maybe (SrcType t))
  -- | SEAnnot (SrcExp t e) (SrcScheme t)
  | SEImpl String [(e, SrcExp t e)] (Maybe (SrcType t))

showSrcExp :: SrcExp String String -> String
showSrcExp (SELit n) = show n
showSrcExp (SEVar var) = var
showSrcExp (SELam binder Nothing body) =
  "\\" ++ binder ++ ". " ++ showSrcExp body
showSrcExp (SELam binder (Just ty) body) =
  "\\" ++ binder ++ ":" ++ showSrcType ty ++
  ". " ++ showSrcExp body
showSrcExp (SEApp a b) = "(" ++ showSrcExp a ++ ") " ++
                         showSrcExp b
showSrcExp (SELVar (Just ty) lvar) = lvar ++ " : " ++ showSrcType ty
showSrcExp (SELVar Nothing lvar) = lvar
showSrcExp (SEField (Just ty) field) = "\"" ++ field ++ "\" : " ++ showSrcType ty
showSrcExp (SEField Nothing field) = "\"" ++ field ++ "\""
showSrcExp (SELet lvar scheme e1 e2) =
  "let " ++ lvar ++ ": " ++ showSrcScheme scheme ++ " = " ++
  showSrcExp e1 ++ " in " ++ showSrcExp e2
showSrcExp (SEImplicit lvars body) =
  "implicit {" ++ intercalate "," lvars ++ "} in " ++
  showSrcExp body
showSrcExp (SEQuery Nothing) = "?"
showSrcExp (SEQuery (Just ty)) = "?[" ++ showSrcType ty ++ "]"
-- showSrcExp (SEAnnot body scheme) =
--   "(" ++ showSrcExp body ++ " : " ++ showSrcScheme scheme ++ ")"
showSrcExp (SEImpl iface defs ty) =
  iface ++ " {" ++ 
  (intercalate ", " . map (\(u, e) -> u ++ " = " ++ showSrcExp e)) defs ++
  "}" ++ (case ty of Just ty -> " : " ++ showSrcType ty
                     Nothing -> "")

instance Show (SrcExp String String) where
  show = showSrcExp

transformVarToLVar :: Ord e => SrcExp t e -> SrcExp t e
transformVarToLVar = transform empty
  where
    transform :: Ord e => Set e -> SrcExp t e -> SrcExp t e
    transform lvars (SEVar v) | v `member` lvars = SELVar Nothing v
    transform lvars (SELam binder ty body) =
      SELam binder ty $ transform (binder `delete` lvars) body
    transform lvars (SEApp a b) =
      SEApp (transform lvars a) (transform lvars b)
    transform lvars (SELet lvar scheme e1 e2) =
      SELet lvar scheme (transform lvars e1) (transform (lvar `insert` lvars) e2)
    transform lvars (SEImplicit vars body) =
      SEImplicit vars (transform lvars body)
--    transform lvars (SEAnnot exp scheme) = SEAnnot (transform lvars exp) scheme
    transform lvars (SEImpl iface defs ty) =
      SEImpl iface (map (second $ transform lvars) defs) ty
    transform lvars e = e
  
transformLVarToField :: Ord e => Set e -> SrcExp t e -> SrcExp t e
transformLVarToField fields = transform fields
  where
    transform :: Ord e => Set e -> SrcExp t e -> SrcExp t e
    transform fields (SEVar v) | v `member` fields = SEField Nothing v
    transform fields (SELVar ty v) | v `member` fields = SEField ty v
    transform fields (SELam binder ty body) =
      SELam binder ty $ transform (binder `delete` fields) body
    transform fields (SEApp a b) =
      SEApp (transform fields a) (transform fields b)
    transform fields (SELet lvar scheme e1 e2) =
      SELet lvar scheme (transform fields e1) (transform (lvar `delete` fields) e2)
    transform fields (SEImplicit vars body) =
      SEImplicit vars (transform fields body)
--    transform fields (SEAnnot exp scheme) = SEAnnot (transform fields exp) scheme
    transform fields (SEImpl iface defs ty) =
      SEImpl iface (map (second $ transform fields) defs) ty
    transform fields e = e
  

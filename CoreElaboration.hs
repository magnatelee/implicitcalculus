{-# OPTIONS -XRankNTypes -XFlexibleInstances #-}

module CoreElaboration where

import Types hiding (matches, matchesFirst)
import Control.Arrow (second)
import Control.Monad (foldM)
import Control.Monad (msum)
import Data.Maybe
import Data.Char (chr)
import Data.List (foldl', intercalate)

data FType a =
  FTVar a
  | FTInt
  | FTFun (FType a) (FType a)
  | FTForall (a -> FType a)
  | FTRecord String [FType a]

instance Show (FType Int) where
  show t = showFType t 0

showFType :: FType Int -> Int -> String
showFType (FTVar a)     _ = showTVar a
showFType FTInt         _ = "Int"
showFType (FTFun p1 p2) n = "(" ++ showFType p1 n ++ ") -> " ++ showFType p2 n
showFType (FTForall f) n  =
  "(forall " ++ showVar n ++ ". " ++ showFType (f n) (n + 1) ++ ")"
showFType (FTRecord r tys) n  =
  r ++ " " ++ (intercalate " " $ map (flip showFType n) tys)

translatePContext :: PContext Int -> FType Int
translatePContext (Type t) = translatePType t
translatePContext (Forall f) =
  FTForall (\a -> translatePContext (f a))
translatePContext (Rule c1 c2) =
  FTFun (translatePContext c1) (translatePContext c2)

translatePType :: PType Int -> FType Int
translatePType (Var a) = FTVar a
translatePType TInt = FTInt
translatePType (Fun c1 c2) = 
  FTFun (translatePContext c1) (translatePContext c2)
translatePType (Record r tys) =
  FTRecord r $ map translatePType tys

ftype1 = translatePContext (fromJust test1)
ftype2 = translatePContext (fromJust test2)
ftype3 = translatePContext (fromJust test3)
ftype4 = translatePContext (fromJust test4)
ftype5 = translatePContext (fromJust test5)


data FExp t v =
  FEVar v
  | FELit Int
  | FELam (FType t) (v -> FExp t v)
  | FEApp (FExp t v) (FExp t v)
  | FETLam (t -> FExp t v)
  | FETApp (FExp t v) (FType t)
  | FERecord String [(String, FExp t v)]
  | FEField String

newtype SystemF = F {unF :: forall t v . FExp t v}

showFExp :: FExp Int Int -> Int -> String
showFExp (FEVar a) _ = showVar a
showFExp (FELit n) _ = show n
showFExp (FELam t f) n = "\\" ++ showVar n ++ ":" ++
                         showFType t n ++ "." ++
                         showFExp (f n) (n + 1)
showFExp (FEApp a b) n = "(" ++ showFExp a n ++ ") " ++
                         showFExp b n
showFExp (FETLam f) n = "/\\" ++ showTVar n ++ "." ++
                        showFExp (f n) (n + 1)
showFExp (FETApp a b) n = "(" ++ showFExp a n ++ ") " ++
                          showFType b n
showFExp (FERecord r fields) n =
  r ++ " {" ++
  (intercalate ", " $
   map (\(u, v) -> u ++ " = " ++  showFExp v n) fields) ++
  "}"
showFExp (FEField field) n = field

instance Show (FExp Int Int) where
  show e = showFExp e 0

translate :: PExp Int Int -> Maybe (FExp Int Int)
translate pexp = fmap fst $ translatePExp [] pexp 0

type TransEnv = [(PContext Int, Int)]
translatePExp :: TransEnv -> PExp Int Int -> Int -> Maybe (FExp Int Int, Int)
translatePExp env (EVar x)    n = return (FEVar x, n)
translatePExp env (ELit i)    n = return (FELit i, n)
translatePExp env (ELam p1 f) n = 
  do (e2, n') <- translatePExp env (f n) (n + 1)
     return (FELam (translatePContext p1)
             (\x -> renameVarInFExp e2 n x),
             n')
translatePExp env (EApp a b) n =
  do (e1, n')  <- translatePExp env a n
     (e2, n'') <- translatePExp env b n'
     return (FEApp e1 e2, n'')
translatePExp env (ETLam f) n = 
  do (e2, n') <- translatePExp env (f n) (n + 1)
     return (FETLam (\x -> renameTVarInFExp e2 n x), n')
translatePExp env (ETApp a b) n =
  do (e1, n') <- translatePExp env a n
     let t = translatePContext b
     return (FETApp e1 t, n')
translatePExp env (EILam p e) n = 
  do let t = translatePContext p
     (e2, n') <- translatePExp ((p, n):env) e (n + 1)
     return (FELam t (\x -> renameVarInFExp e2 n x), n')
translatePExp env (EIApp a b) n =
  do (e1, n')  <- translatePExp env a n
     (e2, n'') <- translatePExp env b n'
     return (FEApp e1 e2, n'')
translatePExp env (EQuery p) n = translateQuery env p n
translatePExp env (ERecord r fields) n = do
  (fields', n') <-
    foldM (\(fs, n) -> \(u, v) -> do
              (v', n') <- translatePExp env v n
              return ((u, v'):fs, n'))
    ([], n) fields
  return (FERecord r (reverse fields'), n')
translatePExp env (EField field) n = return (FEField field, n)

translateQuery :: TransEnv -> PContext Int -> Int -> Maybe (FExp Int Int, Int)
translateQuery env (Forall f) n    =
  do (e, n') <- translateQuery env (f n) (n + 1)
     return (FETLam (\x -> renameTVarInFExp e n x),
             n')
translateQuery env (Rule p1 p2) n  =
  do (e, n') <- translateQuery ((p1, n):env) p2 n 
     return (FELam (translatePContext p1)
             (\x -> renameVarInFExp e n x),
             n')
translateQuery env (Type t) n      =
  do (rs, ws, e, n') <- matchesFirst env t n
     (rs', n'') <- translateAll env rs n'
     return (substFExp (zip ws rs') e, n'')

translateAll :: TransEnv -> [PContext Int] -> Int -> Maybe ([FExp Int Int], Int)
translateAll env (r:rs) n =
  do (r', n')   <- translateQuery env r n
     (rs', n'') <- translateAll env rs n'
     return (r':rs', n'')
translateAll env [] n = return ([], n)

substFExp :: [(Int, FExp Int Int)] -> FExp Int Int -> FExp Int Int
substFExp ss exp = foldl' substOneFExp exp ss

substOneFExp :: FExp Int Int -> (Int, FExp Int Int) -> FExp Int Int
substOneFExp (FEVar a) (x, e) | a == x = e
substOneFExp (FELam t f) s  = FELam t (\x -> substOneFExp (f x) s)
substOneFExp (FEApp a b) s  = FEApp (substOneFExp a s) (substOneFExp b s)
substOneFExp (FETLam f) s   = FETLam (\x -> substOneFExp (f x) s)
substOneFExp (FETApp a b) s = FETApp (substOneFExp a s) b
substOneFExp (FERecord r fields) s =
  FERecord r $ map (second (flip substOneFExp s)) fields
substOneFExp a _ = a

substTyFExp :: [(Int, FType Int)] -> FExp Int Int -> FExp Int Int
substTyFExp ss exp = foldl' substTyOneFExp exp ss

substTyOneFExp :: FExp Int Int -> (Int, FType Int) -> FExp Int Int
substTyOneFExp (FELam t f) s  =
  FELam (substTyOneFType t s) (\x -> substTyOneFExp (f x) s)
substTyOneFExp (FEApp a b) s  =
  FEApp (substTyOneFExp a s) (substTyOneFExp b s)
substTyOneFExp (FETLam f) s   =
  FETLam (\x -> substTyOneFExp (f x) s)
substTyOneFExp (FETApp a b) s =
  FETApp (substTyOneFExp a s) (substTyOneFType b s)
substTyOneFExp (FERecord r fields) s =
  FERecord r $ map (second (flip substTyOneFExp s)) fields
substTyOneFExp a _ = a

substTyOneFType :: FType Int -> (Int, FType Int) -> FType Int
substTyOneFType (FTVar x) (y, t) | x == y = t
substTyOneFType (FTFun a b) s =
  FTFun (substTyOneFType a s) (substTyOneFType b s)
substTyOneFType (FTForall f) s =
  FTForall (\x -> substTyOneFType (f x) s)
substTyOneFType (FTRecord r tys) s =
  FTRecord r $ map (flip substTyOneFType s) tys
substTyOneFType a _ =  a

matchesFirst :: TransEnv -> PType Int -> Int ->
                  Maybe ([PContext Int], [Int], FExp Int Int, Int)
matchesFirst env t n = msum [matches r t x n | (r, x) <- env]

matches :: PContext Int -> PType Int -> Int -> Int ->
           Maybe ([PContext Int], [Int], FExp Int Int, Int)
matches r t x n = go r n [] [] [] (FEVar x)
  where
    go (Type t')     n recs vars evs exp  = 
      do subst <- unify t' t n vars
         let subst' = map (second translatePContext) subst
         return (map (apply subst) recs, evs, substTyFExp subst' exp, n)
    go (Forall f)    n recs vars evs exp  =
      go (f n) (n + 1) recs (n : vars) evs (FETApp exp (FTVar n))
    go (Rule ctxt r) n recs vars evs exp  =
      go r (n + 1) (ctxt : recs) vars (n : evs) (FEApp exp (FEVar n))

renameVarInFExp :: FExp Int Int -> Int -> Int -> FExp Int Int
renameVarInFExp (FEVar z) x y | z == x = FEVar y
renameVarInFExp (FELam a f) x y = FELam a (\n -> renameVarInFExp (f n) x y)
renameVarInFExp (FEApp a b) x y = FEApp (renameVarInFExp a x y) (renameVarInFExp b x y)
renameVarInFExp (FETLam f) x y = FETLam (\n -> renameVarInFExp (f n) x y)
renameVarInFExp (FETApp a b) x y =  FETApp (renameVarInFExp a x y) b
renameVarInFExp (FERecord r fields) x y =
  FERecord r $ map (second $ (\field -> renameVarInFExp field x y)) fields
renameVarInFExp a _ _ = a

renameTVarInFExp :: FExp Int Int -> Int -> Int -> FExp Int Int
renameTVarInFExp (FELam a f) x y =
  FELam (renameTVarInFType a x y) (\n -> renameTVarInFExp (f n) x y)
renameTVarInFExp (FEApp a b) x y =
  FEApp (renameTVarInFExp a x y) (renameTVarInFExp b x y)
renameTVarInFExp (FETLam f) x y =
  FETLam (\n -> renameTVarInFExp (f n) x y)
renameTVarInFExp (FETApp a b) x y =
  FETApp (renameTVarInFExp a x y) (renameTVarInFType b x y)
renameTVarInFExp (FERecord r fields) x y =
  FERecord r $ map (second $ (\field -> renameTVarInFExp field x y)) fields
renameTVarInFExp a _ _ = a

renameTVarInFType :: FType Int -> Int -> Int -> FType Int
renameTVarInFType (FTVar a) x y | a == x = FTVar y
renameTVarInFType (FTFun a b) x y =
  FTFun (renameTVarInFType a x y) (renameTVarInFType b x y)
renameTVarInFType (FTForall f) x y =
  FTForall $ \a -> renameTVarInFType (f a) x y
renameTVarInFType (FTRecord r tys) x y =
  FTRecord r $ map (\ty -> renameTVarInFType ty x y) tys
renameTVarInFType a _ _ = a

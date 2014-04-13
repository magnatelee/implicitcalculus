> {-# OPTIONS -XRankNTypes -XFlexibleInstances #-}

> module Types where

> import Control.Monad (msum, guard)
> import Control.Monad.State
> import Data.Char
> import Data.List (all, intercalate)
> -- import SystemF

> data PType a = 
>     Var a 
>   | TInt
>   | Fun (PContext a) (PContext a)
>   | Record String [PType a]
> 
> data PContext a = 
>     Type (PType a) 
>   | Forall (a -> PContext a) 
>   | Rule (PContext a)  (PContext a)

Type equality (syntactic)

> eqType :: PType Int -> PType Int -> Int -> Bool
> eqType (Var x)      (Var y)      _ = x == y
> eqType TInt         TInt         _ = True
> eqType (Fun p1 p2)  (Fun p3 p4)  n = eqContext p1 p3 n && eqContext p2 p4 n
> eqType (Record r1 tys1) (Record r2 tys2) n | r1 == r2 =
>  all (uncurry (==)) (zip tys1 tys2)
> eqType (Record _ _) (Record _ _) n = False
> eqType _ _                       _ = False
>
> eqContext :: PContext Int -> PContext Int -> Int -> Bool
> eqContext (Type t1)     (Type t2)     n = eqType t1 t2 n
> eqContext (Forall f)    (Forall g)    n = eqContext (f n) (g n) (n+1)
> eqContext (Rule p1 p3)  (Rule p2 p4)  n = eqContext p1 p3 n && eqContext p2 p4 n
> eqContext _             _             _ = False  

> instance Eq (PType Int) where
>   t1 == t2 = eqType t1 t2 0
>
> instance Eq (PContext Int) where
>   p1 == p2 = eqContext p1 p2 0

> newtype Type = T {unT :: forall a . PType a}
>
> newtype Context = C {unC :: forall a . PContext a}

> instance Eq Type where
>   t1 == t2 = eqType (unT t1) (unT t2) 0
>
> instance Eq Context where
>   p1 == p2 = eqContext (unC p1) (unC p2) 0

> instance Show (PContext Int) where
>   show t = showPContext t 0 

> showVar n  = [chr (n + 97)]
> showTVar n = [chr (n + 65)]

> showPType :: PType Int -> Int -> String
> showPType (Var x)      n = showTVar x 
> showPType TInt         n = "Int"
> showPType (Fun p1 p2)  n = "(" ++ showPContext p1 n ++ ") -> " ++ showPContext p2 n
> showPType (Record r tys) n = r ++ " " ++ (intercalate " " $ map (flip showPType n) tys)
>
> showPContext :: PContext Int -> Int -> String
> showPContext (Type t)      n = showPType t n
> showPContext (Forall f)    n = "(forall " ++ showTVar n ++ ". " ++ showPContext (f n) (n+1) ++ ")"
> showPContext (Rule p1 p2)  n = "(" ++ showPContext p1 n ++ ") => " ++ showPContext p2 n

> joinContext :: PContext (PContext a) -> PContext a
> joinContext (Type (Var x))      = x
> joinContext (Type TInt)         = Type TInt
> joinContext (Type (Fun p1 p2))  = Type (Fun (joinContext p1) (joinContext p2))
> joinContext (Rule p1 p2)        = Rule (joinContext p1) (joinContext p2)
> joinContext (Forall f)          = Forall (joinContext . f . Type . Var) 

System F (move to another file):

-- > data FExp v t = FVar v | FTVar t | Lam (v -> FExp v t) | Abs (t -> FExp v t) | App (

Type Checking 

> data PExp t e = 
>     EVar e
>   | ELit Int 
>   | ELam (PContext t) (e -> PExp t e)
>   | EApp (PExp t e) (PExp t e)
>   | ETLam (t -> PExp t e)
>   | ETApp (PExp t e) (PContext t)
>   | EQuery (PContext t)
>   | EILam (PContext t) (PExp t e)
>   | EIApp (PExp t e) (PExp t e)
>   | ERecord String [(String, PExp t e)]
>   | EField String
>
> newtype Exp = E {unE :: forall a t . PExp t a}  

> showPExp :: PExp Int Int -> Int -> String
> showPExp (EVar a) _ = showVar a
> showPExp (ELit n) _ = show n
> showPExp (ELam r f) n = "\\" ++ showVar n ++ ":" ++
>                         showPContext r n ++ "." ++
>                         showPExp (f n) (n + 1)
> showPExp (EApp a b) n = "(" ++ showPExp a n ++ ") " ++
>                         showPExp b n
> showPExp (ETLam f) n = "/\\" ++ showTVar n ++ "." ++
>                        showPExp (f n) (n + 1)
> showPExp (ETApp a b) n = "(" ++ showPExp a n ++ ") " ++
>                          showPContext b n
> showPExp (EQuery r) n = "?[" ++ showPContext r n ++ "]"
> showPExp (EILam r a) n = "\\?<" ++ showPContext r n ++ ">." ++
>                          showPExp a n
> showPExp (EIApp a b) n = showPExp a n ++ " with " ++ showPExp b n
> showPExp (ERecord r fields) n = r ++ " {" ++
>                                 (intercalate ", " $
>                                  map (\(u, v) -> u ++ " = " ++ 
>                                  showPExp v n) fields) ++
>                                 "}"
> showPExp (EField field) n = "\"" ++ field ++ "\""
  
> instance Show (PExp Int Int) where
>   show exp = showPExp exp 0

Tests

> idExp = E $ ETLam (\a -> ELam (Type (Var a)) (\x -> EVar x)) -- id : forall a . a -> a = /\a . \x : a . x

> appExp = E $ ETApp (unE idExp) (Type TInt) -- id Int
> appExp2 = E $ EApp (ETApp (unE idExp) (Type TInt)) (ELit 3) -- id Int 3

> iidExp = E $ ETLam (\a -> EILam (Type (Var a)) (EQuery (Type (Var a))))

> iappExp = E $ ETApp (unE iidExp) (Type TInt)
> iappExp2 = E $ EIApp (ETApp (unE iidExp) (Type TInt)) (ELit 3)

> inttype = Type TInt
> var x = Type (Var x)
> fun x y = Type (Fun x y)
> overlap1 = E $ EIApp
>                   (EILam (Type (Fun inttype inttype))
>                      (EIApp
>                         (EILam (Forall (\x -> fun (var x) (var x)))
>                            (EApp (EQuery (fun inttype inttype)) (ELit 1)))
>                         (ETLam (\x -> (ELam (var x) (\y -> EVar y))))))
>                   (ELam inttype (\x -> EVar x))
> overlap2 = E $ EIApp
>                   (EILam (Forall (\x -> fun (var x) (var x)))
>                      (EIApp
>                         (EILam (Type (Fun inttype inttype))
>                            (EApp (EQuery (fun inttype inttype)) (ELit 1)))
>                         (ELam inttype (\x -> EVar x))))
>                   (ETLam (\x -> (ELam (var x) (\y -> EVar y))))
> queryExp1 = E $ EIApp
>                   (EILam (Type (Fun inttype inttype))
>                      (EApp (EQuery (fun inttype inttype)) (ELit 1)))
>                   (ELam inttype (\x -> EVar x))
> queryExp2 = E $ EIApp
>                   (EILam (Forall (\x -> fun (var x) (var x)))
>                      (EApp (EQuery (fun inttype inttype)) (ELit 1)))
>                   (ETLam (\x -> (ELam (var x) (\y -> EVar y))))
> recordExp1 = E $ ELam (Type (Record "Succ" [TInt]))
>                   (\x -> EIApp (EQuery (Type (Record "Succ" [TInt]))) (EVar x))
>
>
> test1 = inferExp idExp
> test2 = inferExp appExp
> test3 = inferExp appExp2
> test4 = inferExp iidExp
> test5 = inferExp iappExp
> test6 = inferExp iappExp2
> test7 = inferExp overlap1
> test8 = inferExp overlap2
> test9 = inferExp queryExp1
> test10 = inferExp queryExp2

Inference

> inferExp :: Exp -> Maybe (PContext Int)
> inferExp (E e) = infer [] e 0

> infer :: [PContext Int] -> PExp Int (PContext Int) -> Int -> Maybe (PContext Int)
> infer env (EVar p1)    _ = return p1
> infer env (ELit x)     _ = return (Type TInt)
> infer env (ELam p1 f)  n = 
>   do p2 <- infer env (f p1) n
>      return (Type (Fun p1 p2))
> infer env (EApp e1 e2) n =
>   do (Type (Fun p1 p2)) <- infer env e1 n
>      p1' <- infer env e2 n
>      if (p1 ==  p1') then return p2 else fail ""
> infer env (ETLam f) n = 
>   do p <- infer env (f n) (n+1)
>      return (Forall (\a -> rename n a p))
> infer env (ETApp e1 p1) n = 
>   do (Forall f) <- infer env e1 n
>      return (subst n p1 (f n))
> infer env (EILam p1 e) n = 
>   do p2 <- infer (p1 : env) e n -- ambiguity check?
>      return (Rule p1 p2)
> infer env (EIApp e1 e2) n = 
>   do  (Rule p1 p2) <- infer env e1 n
>       p1' <- infer env e2 n
>       if (p1 == p1') then return p2 else fail ""
> infer env (EQuery p) n | tcResolve env p n = return p
> infer _ _ _ = Nothing

-- > fv :: Context Int -> Int -> [Int]
-- > fv (Type (Var z)) n      = [z]
-- > fv (Type TInt) n         = []
-- > fv (Type (Fun p1 p2)) n  = union (fv p1 n) (fv p2 n)
-- > fv (Forall f) n          = undefined

Substitution

> subst :: Int -> PContext Int -> PContext Int -> PContext Int
> subst x p (Type (Var z)) 
>   | x == z = p
>   | otherwise = Type (Var z)
> subst x p (Type TInt)         = Type TInt
> subst x p (Type (Fun p1 p2))  = Type (Fun (subst x p p1) (subst x p p2))
> subst x p (Rule p1 p2)        = Rule (subst x p p1) (subst x p p2)
> subst x p (Forall f)          = Forall (subst x p . f)

Renaming

> rename :: Int -> Int -> PContext Int -> PContext Int
> rename x y (Type t)      = Type (renameType x y t)
> rename x y (Forall f)    = Forall (rename x y . f)
> rename x y (Rule p1 p2)  = Rule (rename x y p1) (rename x y p2)
>
> renameType :: Int -> Int -> PType Int -> PType Int
> renameType x y (Var z)
>   | x == z     = Var y
>   | otherwise  = Var z
> renameType x y TInt = TInt
> renameType x y (Fun p1 p2) = Fun (rename x y p1) (rename x y p2)

Resolution type checking

> tcResolve :: [PContext Int] -> PContext Int -> Int -> Bool
> tcResolve env (Forall f) n    = tcResolve env (f n) (n+1)
> tcResolve env (Rule p1 p2) n  = tcResolve (p1:env) p2 n 
> tcResolve env (Type t) n      = 
>   maybe False
>         (\(rs,n') -> all (\t -> tcResolve env t n') rs) 
>         (matchesFirst env t n)

> matchesFirst :: [PContext Int] -> PType Int -> Int -> Maybe ([PContext Int], Int)
> matchesFirst env t n = msum [matches r t n | r <- env]

> matches :: PContext Int -> PType Int -> Int -> Maybe ([PContext Int], Int)
> matches r t n = go r n [] []
>  where 
>   go (Type t')     n vars recs  = 
>     do subst <- unify t' t n vars
>        return (map (apply subst) recs, n)
>   go (Forall f)    n vars recs  = go (f n) (n + 1) (n : vars) recs
>   go (Rule ctxt r) n vars recs  = go r n vars (ctxt:recs)

> apply :: [(Int,PContext Int)] -> PContext Int -> PContext Int
> apply subst r = gor r where
>   gor (Type t)      = got t
>   gor (Forall f)    = Forall (gor . f)
>   gor (Rule r1 r2)  = Rule (gor r1) (gor r2)
>
>   got (Var v)       = case lookup v subst of
>                         Just r   -> r
>                         Nothing  -> Type (Var v)
>   got TInt          = Type TInt
>   got (Fun r1 r2)   = Type (Fun (gor r1) (gor r2))
>   got (Record r tys)= Type (Record r $ map (unlift . got) tys)
>   unlift (Type ty) = ty

> unify :: PType Int -> PType Int -> Int -> [Int] -> Maybe [(Int,PContext Int)]
> unify t1 t2 n vars = execStateT (got t1 t2) (n,[]) >>= return . snd where
>
>   got :: PType Int -> PType Int -> StateT (Int, [(Int,PContext Int)]) Maybe ()
>   got (Var v1) t2
>     | elem v1 vars
>     = do (_,m) <- get
>          case lookup v1 m of
>            Just r1  -> gor r1 (Type t2)
>            Nothing  -> got_ (Var v1) t2
>   got t1 t2 = got_ t1 t2
>  
>   got_ :: PType Int -> PType Int -> StateT (Int, [(Int,PContext Int)]) Maybe ()
>   got_ (Var v1) (Var v2)
>     | v1 == v2
>     = return ()
>     | elem v1 vars
>     = do (n,m) <- get
>          put (n, (v1,Type (Var v2)) : m)
>     | otherwise
>     = fail ""
>   got_ TInt TInt = return () -- correct?
>   got_ (Fun r11 r12) (Fun r21 r22) = 
>     do gor r11 r21
>        gor r12 r22
>   got_ (Record r1 []) (Record r2 [])
>     | r1 == r2
>     = return ()
>   got_ (Record r1 (ty1:tys1)) (Record r2 (ty2:tys2))
>     | r1 == r2
>     = do got_ ty1 ty2
>          got_ (Record r1 tys1) (Record r2 tys2)
>   got_ (Var v) TInt  -- Bruno: added this clause to unify integers
>     | elem v vars 
>     = do (n,m) <- get
>          put (n, (v, Type TInt) : m)
>   got_ (Var v) (Record r tys)
>     | elem v vars
>     = do (n,m) <- get
>          put (n, (v, Type (Record r tys)) : m)
>   got_ (Var v) (Fun r1 r2)
>     | elem v vars
>     = do (n,m) <- get
>          put (n, (v, Type (Fun r1 r2)) : m)
>   got_ _ _
>     = fail ""
> 
>   gor :: PContext Int -> PContext Int -> StateT (Int, [(Int,PContext Int)]) Maybe ()
>   gor (Type (Var v1)) r2
>     | elem v1 vars
>     = do (_,m) <- get
>          case lookup v1 m of
>            Just r1  -> gor_ r1 r2
>            Nothing  -> gor_ (Type (Var v1)) r2
>   gor r1 r2
>     = gor_ r1 r2
>
>   gor_ :: PContext Int -> PContext Int -> StateT (Int, [(Int,PContext Int)]) Maybe ()
>   gor_ (Type t1) (Type t2)
>     = got t1 t2
>   gor_ (Forall f1) (Forall f2)
>     = do (n,m) <- get
>          put (n + 1,m)
>          gor (f1 n) (f2 n)
>   gor_ (Rule r11 r12) (Rule r21 r22)
>     = do gor r11 r21
>          gor r12 r22
>   gor_ r1 r2
>    = fail ""



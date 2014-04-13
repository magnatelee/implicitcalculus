{-# OPTIONS -XRankNTypes -XFlexibleInstances #-}

module SystemFSemantics where

import CoreElaboration

type EvalEnv = [(Int, FExp Int Int)]

isValue :: FExp Int Int -> Bool
isValue exp@(FELit _) = True
isValue exp@(FELam _ _) = True
isValue exp@(FETLam _) = True
isValue exp@(FERecord _ _) = True
isValue exp@(FEField _) = True
isValue _ = False

evalAll :: FExp Int Int -> Maybe (FExp Int Int)
evalAll fexp = do
  (fexp', _) <- eval [] fexp 0 
  return fexp'
  
eval :: EvalEnv -> FExp Int Int -> Int -> Maybe (FExp Int Int, Int)
eval env (FEVar v) n = do
  fexp <- lookup v env
  return (fexp, n)
eval env exp@(FELit _) n = return (exp, n)
eval env exp@(FELam _ _) n = return (exp, n)

eval env (FEApp a b) n | isValue a && isValue b = case a of
  FELam _ f -> eval ((n, b):env) (f n) (n + 1)
  FEField field -> case b of
    FERecord _ fields -> do
      value <- lookup field fields
      return (value, n)
    _ -> error "record lookup should be applied to records"
  _ -> error "application should be done with functions"

eval env (FEApp a b) n | isValue a = do
  (b', n') <- eval env b n
  eval env (FEApp a b') n'

eval env (FEApp a b) n | otherwise = do
  (a', n') <- eval env a n
  eval env (FEApp a' b) n'

eval env exp@(FETLam _) n = return (exp, n)
eval env (FETApp a b) n | isValue a = case a of
  (FETLam f) -> eval env (f n) (n + 1)
  _ -> error "type application should be done with type abstractions"
eval env (FETApp a b) n | otherwise = do
  (a', n') <- eval env a n
  eval env (FETApp a' b) n'
eval env exp@(FERecord _ _) n = return (exp, n)
eval env exp@(FEField _) n = return (exp, n)

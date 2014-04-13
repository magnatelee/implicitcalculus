module SourceElaboration where

import Types
import SyntaxSource

import Control.Arrow (first)
import qualified Data.Map as M
import Data.Maybe
import Data.List (foldl')
import qualified Data.List as L

translateSrcType :: SrcType String -> Int -> M.Map String Int -> PType Int
translateSrcType (STVar v) n map = Var v'
  where v' = M.findWithDefault n v map
translateSrcType STInt _ _ = TInt
translateSrcType (STFun a b) n map =
  Fun (Type $ translateSrcType a n map) (Type $ translateSrcType b n map)
translateSrcType (STIface iface tys) n map =
  Record iface $ L.map (\ty -> translateSrcType ty n map) tys

translateSrcScheme :: SrcScheme String -> PContext Int
translateSrcScheme srcscheme = translateSrcScheme' srcscheme 0 M.empty

translateSrcScheme' :: SrcScheme String -> Int -> M.Map String Int -> PContext Int
translateSrcScheme' (Scheme (tvar:tvars) schemes srctype) n map =
  Forall (\x -> translateSrcScheme' scheme' n (M.insert tvar x map))
  where scheme' = Scheme tvars schemes srctype
translateSrcScheme' (Scheme [] (scheme:schemes) srctype) n map =
  Rule (translateSrcScheme' scheme n map) (translateSrcScheme' scheme' n map)
  where scheme' = Scheme [] schemes srctype
translateSrcScheme' (Scheme [] [] srctype) n map =
  Type $ translateSrcType srctype n map

translateSrcPgm :: ([SrcInterface String String], SrcExp String String) -> PExp Int Int
translateSrcPgm (ifaces, exp) = translateSrcExp initEnv exp 0 M.empty where
  initEnv = L.concatMap getTypesOfFields ifaces
  getTypesOfFields (Interface iface tvars decls) = L.map getTypeOfField decls where
    getTypeOfField (u, ty) = (u, Scheme tvars [] $ STFun ifaceType ty)
    ifaceType = STIface iface $ L.map STVar tvars
  
translateSrcExp ::
  [(String, SrcScheme String)] -> SrcExp String String -> Int -> M.Map String Int -> PExp Int Int
translateSrcExp _ (SEVar v) n map = EVar v'
  where v' = M.findWithDefault n v map
translateSrcExp _ (SELit n) _ _ = ELit n
translateSrcExp env (SELam y (Just ty) body) n map =
  ELam
  (Type (translateSrcType ty n map))
  (\x -> translateSrcExp env body n (M.insert y x map))
translateSrcExp env (SEApp a b) n map =
  EApp (translateSrcExp env a n map) (translateSrcExp env b n map)
translateSrcExp env (SELVar (Just ty) u) n map = foldl' EIApp tapp qs 
  where (Scheme tvars schemes ty') = fromJust $ lookup u env
        theta = unifySrcType ty ty'
        tys = L.map (\tvar -> fromMaybe (STVar tvar) (lookup tvar theta)) tvars
        encodedTys = L.map (translateSrcScheme . lift) tys
        qs = L.map (EQuery . translateSrcScheme .
                    applySubstToScheme theta) schemes
        u' = M.findWithDefault n u map
        tapp = foldl' ETApp (EVar u') encodedTys
translateSrcExp env (SEField (Just ty) field) n map = foldl' EIApp tapp qs 
  where (Scheme tvars schemes ty') = fromJust $ lookup field env
        theta = unifySrcType ty ty'
        tys = L.map (\tvar -> fromMaybe (STVar tvar) (lookup tvar theta)) tvars
        encodedTys = L.map (translateSrcScheme . lift) tys
        qs = L.map (EQuery . translateSrcScheme .
                    applySubstToScheme theta) schemes
        tapp = foldl' ETApp (EField field) encodedTys
translateSrcExp env (SELet u scheme e1 e2) n map = EApp exp2 exp1
  where (Scheme tvars schemes ty) = scheme
        exp1 = mkAllTAbs tvars n map
        mkAllTAbs (tvar:tvars) n map =
          ETLam (\x -> mkAllTAbs tvars (n + 1) (M.insert tvar x map))
        mkAllTAbs [] n map =
          foldl' (flip EILam) (translateSrcExp env e1 n map)
          (L.map translateSrcScheme schemes)
        env' = (u, scheme) : env
        exp2 = ELam
               (translateSrcScheme scheme)
               (\x -> translateSrcExp env' e2 n (M.insert u x map))
translateSrcExp env (SEImplicit us e) n map =
  foldl' EIApp iabs us'
  where e' = (translateSrcExp env e n map)
        schemes = L.map (fromJust . flip lookup env) us
        iabs = foldl' (flip EILam) e' (L.map translateSrcScheme schemes)
        us' = L.map (\u -> EVar $ M.findWithDefault n u map) us
translateSrcExp _ (SEQuery (Just ty)) _ _ =
  EQuery (translateSrcScheme (lift ty))
translateSrcExp env (SEImpl iface defs ty) n map =
  ERecord iface $
  L.map (\(u, e) -> (u, translateSrcExp env e n map)) defs
--translateSrcExp env (SEAnnot exp scheme) n map =
--  translateSrcExp env exp n map
translateSrcExp _ _ _ _ = error "not implemented yet"                     

unifySrcType :: Eq a => SrcType a -> SrcType a -> [(a, SrcType a)]
unifySrcType (STFun ty1 ty2) (STFun ty1' ty2') =
  unifySrcType ty1 ty1' ++ unifySrcType ty2 ty2'
unifySrcType (STIface r1 tys1) (STIface r2 tys2) | r1 == r2 =
  concatMap (uncurry unifySrcType) $ zip tys1 tys2
unifySrcType (STVar v) ty' = unify' ty' where
  unify' (STVar v') | v == v' = []
  unify' _ =  [(v, ty')]
unifySrcType ty@STInt ty' = unify' ty' where
  unify' (STVar v') = [(v', ty)]
  unify' STInt = []
  unify' _ = error "failed to unify"
unifySrcType _ _ = error "unification error"

applySubstToScheme :: Eq a => [(a, SrcType a)] -> SrcScheme a -> SrcScheme a
applySubstToScheme subst (Scheme tvars schemes ty) =
  Scheme tvars (L.map (applySubstToScheme subst') schemes)
  (applySubstToType subst' ty)
  where subst' = L.filter (\(var, _) -> not (var `L.elem` tvars)) subst

applySubstToType subst ty@(STVar v) = case lookup v subst of
  Just ty' -> ty'
  Nothing -> ty
applySubstToType _ STInt = STInt
applySubstToType subst (STFun ty1 ty2) = STFun ty1' ty2' where
  ty1' = applySubstToType subst ty1
  ty2' = applySubstToType subst ty2
applySubstToType subst (STIface iface tys) = STIface iface tys' where
  tys' = map (applySubstToType subst) tys

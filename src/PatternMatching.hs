-- Pattern matching
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module PatternMatching where

import Control.Lens ((^.), over)
import Control.Monad.Extra (firstJustM)
import Control.Monad.Reader (local)
import Data.List (inits, intercalate)
import qualified Data.Map as Map

import Context
import ErrM
import Grammar
import Print

type MatchedLetPattern = (CanonicalName, Expr)

matchName ::
     BNFC'Position
  -> [UppercaseIdent]
  -> OIdent
  -> [LowercaseIdent]
  -> InterpreterT Expr
matchName pos modprefix x ridents = do
  scope <- getScope
  modpath <- getModuleName
  res <-
    case modprefix of
      [] ->
        case Map.lookup x (scope ^. visiblelocal) of
          Just ver ->
            return . Just
              $ CanonicalName (scope ^. hasTag) [identLocal] x ver ridents
          Nothing -> firstJustM checkPrefix (reverse $ inits modpath)
      _ -> firstJustM (\pref -> checkPrefix (pref ++ modprefix)) [modpath, []]
  case res of
    Nothing -> do
      pos' <- getPosition pos
      bad pos'
        $ "Cannot find "
            ++ show
                 (Variable npos
                    $ map (IUppercase npos) modprefix
                        ++ [x]
                        ++ map (ILowercase npos) ridents)
    Just name -> return . VCanonical npos $ name
  where
    checkPrefix path = do
      scope <- getScope
      (ver, _) <- getVersion $ map (IUppercase npos) path ++ [x]
      if ver /= (-1)
        then return . Just $ CanonicalName (scope ^. hasTag) path x ver ridents
        else return Nothing

match :: Expr -> Expr -> Err [MatchedLetPattern]
match pe ve =
  case (pe, ve) of
    (Blank _, _) -> Right []
    (EAppend _ ph ptl, List _ [vh]) -> match ph vh <+> match ptl (Nil2 npos)
    (EAppend _ ph ptl, List _ (vh:vtl)) ->
      match ph vh <+> match ptl (List npos vtl)
    (EConstant _ p, EConstant _ v)
      | p == v -> Right []
    (EConstructor _ (VCanonical _ pname) p, EConstructor _ (VCanonical _ vname) v)
      | pname == vname -> match p v
    (ETuple _ p ps, ETuple _ v vs)
      | length ps == length vs -> matchMany (p : ps) (v : vs)
    (ETyped _ p _, _) -> match p ve
    (List _ ps, List _ vs)
      | length ps == length vs -> matchMany ps vs
    (Nil2 _, Nil2 _) -> Right []
    (Record _ pfields, Record _ vfields) ->
      let extract =
            Map.fromList
              . map
                  (\case
                     EField _ name e -> (name, e)
                     _ -> undefined)
          ps = extract pfields
          vs = extract vfields
          common = Map.intersectionWith match ps vs
       in Map.foldr (<+>) (Right []) common
    (VCanonical _ pname, _) -> Right [(pname, ve)]
    _ ->
      Left
        $ intercalate
            "\n"
            ["Invalid pattern matching.", printTree pe, printTree ve]
  where
    matchMany :: [Expr] -> [Expr] -> Err [MatchedLetPattern]
    matchMany ps vs = foldl (<+>) (Right []) $ zipWith match ps vs

dummyMatch :: Expr -> [CanonicalName]
dummyMatch =
  \case
    EAppend _ h tl -> concatMap dummyMatch [h, tl]
    EApply _ v _ -> dummyMatch v
    EConstructor _ _ e -> dummyMatch e
    ETuple _ p ps -> concatMap dummyMatch (p : ps)
    ETyped _ v _ -> dummyMatch v
    List _ es -> concatMap dummyMatch es
    Record _ fields ->
      concatMap
        (\case
           EField _ _ e -> dummyMatch e
           _ -> undefined)
        fields
    VCanonical _ pname -> [pname]
    _ -> []

withLocal :: (CanonicalName, Memory) -> ScopeM -> ScopeM
withLocal (CanonicalName _ [i] name v [], ref) scope
  | i == identLocal = over localmemory (Map.insert (name, v) ref) <$> scope
withLocal (x, _) _ =
  Left $ "withLocal: Expected a local variable, got " ++ show x

prepare :: [MatchedLetPattern] -> InterpreterT Expr -> InterpreterT Expr
prepare matches stmt = do
  matches' <-
    mapM
      (\(name, e) -> do
         e' <- toR e
         return (name, e'))
      matches
  local (compose (map withLocal matches')) stmt

dummyPrepare :: [Expr] -> InterpreterT Expr -> InterpreterT Expr
dummyPrepare es = local (setVisibleLocals $ concatMap dummyMatch es)

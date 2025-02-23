{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module StaticBinding
  ( bind
  ) where

import Control.Lens ((^.), over)
import Control.Monad (forM)
import Control.Monad.Reader (lift, local)
import Control.Monad.State (modify)
import qualified Data.Set as Set

import Context
import Grammar
import PatternMatching
import Print
import Types

splitPath :: Path -> [[OIdent]] -> [[OIdent]]
splitPath [] res = map reverse $ reverse res
splitPath _ ([]:_) = undefined
splitPath (p:ps) [] = splitPath ps [[p]]
splitPath (p:ps) ((s:s'):ss) =
  splitPath ps
    $ if isUppercase p == isUppercase s
        then (p : s : s') : ss
        else [p] : (s : s') : ss

writeVersion :: Path -> (Integer, Bool) -> InterpreterT ()
writeVersion path ver = do
  context <- getContext
  _ <- write path ver (context ^. hversions)
  return ()

class StaticBinding a where
  bind :: a -> InterpreterT a
  bind = return

instance StaticBinding a => StaticBinding [a] where
  bind = mapM bind

bindOp2 :: BNFC'Position -> Infix -> Expr -> Expr -> InterpreterT Expr
bindOp2 pos op e1 e2 = do
  pos' <- getPosition pos
  op' <- bind $ Variable npos [IInfix npos op]
  e1' <- bind e1
  e2' <- bind e2
  return $ EApply pos' op' [e1', e2']

bindMultiplication :: BNFC'Position -> [Expr] -> InterpreterT Expr
bindMultiplication pos es = do
  op <- bind $ Variable pos [IInfix npos $ OpMulInt npos]
  es' <- bind es
  return $ foldl1 (\x y -> EApply npos op [x, y]) es'

instance StaticBinding Field where
  bind x = do
    scope <- getScope
    case (scope ^. hasNameContext, x) of
      (CtxUsage, EField _ (Variable _ [ILowercase _ i]) e) -> do
        let v' = VCanonical npos (CanonicalField i)
        e' <- bind e
        return $ EField npos v' e'
      (CtxGlobal, EFieldDecl _ (Variable _ [ILowercase _ i]) t) -> do
        let v' = VCanonical npos (CanonicalField i)
        t' <- local (setTag TagType) (bind t)
        return $ EFieldDecl npos v' t'
      (CtxGlobal, EFieldDeclMut _ (Variable _ [ILowercase _ i]) t) -> do
        let v' = VCanonical npos (CanonicalField i)
        t' <- local (setTag TagType) (bind t)
        lift $ modify $ over (htypes . mutablefields) (Set.insert i)
        return $ EFieldDeclMut npos v' t'
      _ -> bad Nothing $ "Invalid record usage..." ++ show x

bindType :: Expr -> InterpreterT Expr
bindType x =
  case x of
    EApply _ v [c] -> TPolymorphic npos <$> bind v <*> return c
    EApply _ v cs
      | length cs > 1 ->
        TPolymorphic npos
          <$> bind (EApply npos v (init cs))
          <*> return (last cs)
    ETuple _ e es -> TParams npos <$> bind (e : es)
    TFunction pos t1 t2 -> TFunction pos <$> bind t1 <*> bind t2
    TIdent {} -> return x
    TTuple _ e1 es -> TTuple npos <$> bind e1 <*> bind es
    Variable {} -> return x
    _ -> bad Nothing $ "Cannot bind invalid type " ++ show x

bindPattern :: Expr -> InterpreterT Expr
bindPattern x = do
  case x of
    Blank pos -> Blank <$> getPosition pos
    EAppend pos e1 e2 -> EAppend <$> getPosition pos <*> bind e1 <*> bind e2
  -- This is a n-parameter constructor.
    EApply pos v [e] -> do
      pos' <- getPosition pos
      v' <- local (setTag TagExpression . setNameContext CtxUsage) (bind v)
      EConstructor pos' v' <$> bind e
    EConstant {} -> return $ stripPosition x
    ETuple pos e1 es -> ETuple <$> getPosition pos <*> bind e1 <*> mapM bind es
    List pos es -> List <$> getPosition pos <*> bind es
    Nil1 pos -> Nil2 <$> getPosition pos
    Nil2 pos -> Nil2 <$> getPosition pos
    Record pos fields ->
      Record
        <$> getPosition pos
        <*> mapM
              (\case
                 EField _ (Variable _ [ILowercase _ i]) e -> do
                   let name' = VCanonical npos $ CanonicalField i
                   e' <- bind e
                   return $ EField npos name' e'
                 _ -> do
                   pos' <- getPosition (hasPosition x)
                   bad pos' $ "Invalid pattern (record), got " ++ show x)
              fields
  -- This is a 0-parameter constructor.
    Variable pos name
      | all isUppercase name -> do
        pos' <- getPosition pos
        VCanonical _ name' <-
          local (setTag TagExpression . setNameContext CtxUsage) (bind x)
        return . EConstant pos' $ CConstructor npos (to name' :: Path)
    Variable {} -> local (setTag TagExpression) (bind x)
    _ -> do
      pos' <- getPosition (hasPosition x)
      bad pos' $ "Invalid pattern, got " ++ show x

bindExpression :: Expr -> InterpreterT Expr
bindExpression =
  \case
    Blank pos -> Blank <$> getPosition pos
    EApply pos v es -> do
      scope <- getScope
      pos' <- getPosition pos
      case scope ^. hasNameContext of
        CtxDeclaration ->
          case v of
            Variable _ [_] -> do
              v' <- local (setNameContext CtxDeclaration) (bind v)
              params' <- local (setNameContext CtxLocal) (bind es)
              return $ EApply pos' v' params'
            _ -> bad pos' $ "Invalid function name, got " ++ printTree v
        CtxUsage -> EApply pos' <$> bind v <*> bind es
        _ ->
          EApply pos' <$> bind v <*> local (setNameContext CtxLocal) (bind es)
    EBegin pos e -> EBegin <$> getPosition pos <*> bind e
    x@EConstant {} -> return $ stripPosition x
    EFor pos (LetPattern _ v@(Variable _ [_]) e1) dir e2 e3 -> do
      pos' <- getPosition pos
      v'@(VCanonical _ name) <- local (setNameContext CtxLocal) (bind v)
      e1' <- bind e1
      [e2', e3'] <- local (setVisibleLocal name) (bind [e2, e3])
      return $ EFor pos' (LetPattern npos v' e1') dir e2' e3'
    EIf pos c e -> EIf <$> getPosition pos <*> bind c <*> bind e
    EIfElse pos c e1 e2 ->
      EIfElse <$> getPosition pos <*> bind c <*> bind e1 <*> bind e2
    ELambda pos f es e -> do
      pos' <- getPosition pos
      es' <- local (setNameContext CtxLocal) (bind es)
      e' <- dummyPrepare es' (bind e)
      return $ ELambda pos' f es' e'
    ELambdaTyped pos f es t e -> do
      pos' <- getPosition pos
      es' <- local (setNameContext CtxLocal) (bind es)
      t' <- local (setTag TagType) (bind t)
      e' <- dummyPrepare es' (bind e)
      return $ ELambdaTyped pos' f es' t' e'
    ELetIn pos rec patterns e -> do
      pos' <- getPosition pos
      patterns' <-
        mapM
          (\case
             LetPattern pos2 v expr -> do
               LetPattern
                 <$> getPosition pos2
                 <*> local (setNameContext CtxLocal) (bind v)
                 <*> return expr
             other -> do
               pos2' <- getPosition (hasPosition other)
               bad pos2' $ "Expected a let pattern, got... " ++ printTree other)
          patterns
      locals <-
        mapM
          (\case
             LetPattern _ v _ -> return v
             _ -> undefined)
          patterns'
      patterns'' <-
        mapM
          (\case
             LetPattern pos2 v expr ->
               case rec of
                 RecYes _ -> LetPattern pos2 v <$> dummyPrepare [v] (bind expr)
                 RecNo _ ->
                   case v of
                     EApply _ _ es ->
                       LetPattern pos2 v <$> dummyPrepare es (bind expr)
                     ETyped _ (VCanonical {}) _ ->
                       LetPattern pos v <$> bind expr
                     ETyped _ (EApply _ _ es) _ ->
                       LetPattern pos2 v <$> dummyPrepare es (bind expr)
                     _ -> LetPattern pos2 v <$> bind expr
             _ -> undefined)
          patterns'
      ELetIn pos' rec patterns'' <$> dummyPrepare locals (bind e)
    EMatch pos e p es -> do
      pos' <- getPosition pos
      e' <- bind e
      es' <-
        mapM
          (\case
             TFunction pos2 pat expr -> do
               pos2' <- getPosition pos2
               pat' <-
                 local (setTag TagPattern . setNameContext CtxLocal) (bind pat)
               expr' <- dummyPrepare [pat'] (bind expr)
               return $ ELambda pos2' (Fun1 npos) [pat'] expr'
             other -> do
               pos2' <- getPosition (hasPosition other)
               bad pos2' $ "Invalid expression in matching " ++ show other)
          es
      return $ EMatch pos' e' p es'
    ESeq pos e es -> ESeq <$> getPosition pos <*> bind e <*> bind es
    ETuple pos e es -> ETuple <$> getPosition pos <*> bind e <*> bind es
    ETyped pos v t -> do
      v' <- bind v
      t' <- local (setTag TagType) (bind t)
      return $ ETyped pos v' t'
    EWhile pos c e -> EWhile <$> getPosition pos <*> bind c <*> bind e
    List pos es -> List <$> getPosition pos <*> bind es
    Nil1 pos -> Nil2 <$> getPosition pos
    Nil2 pos -> Nil2 <$> getPosition pos
    Record pos fields -> Record <$> getPosition pos <*> bind fields
    Variable pos path -> do
      scope <- getScope
      pos' <- getPosition pos
      modpath <- getModuleName
      case (map stripPosition path, scope ^. hasNameContext) of
        ([i], CtxDeclaration) -> do
          let p = map (IUppercase npos) modpath ++ [i]
          (ver, _) <- getVersion p
          let ver' = ver + 1
          writeVersion p (ver', False)
          return
            $ VCanonical pos'
            $ CanonicalName (scope ^. hasTag) modpath i ver' []
        ([i], c)
          | c == CtxGlobal || c == CtxLocal -> do
            {- HLINT ignore "Use let" -}
            p <-
              return
                $ case c of
                    CtxGlobal -> map (IUppercase npos) modpath ++ [i]
                    CtxLocal -> [IUppercase npos identLocal, i]
                    _ -> undefined
            (ver, isBound) <- getVersion p
            let ver' =
                  if c == CtxGlobal && not isBound
                    then ver
                    else ver + 1
            writeVersion p (ver', True)
            return
              $ VCanonical pos'
              $ CanonicalName
                  (scope ^. hasTag)
                  (if c == CtxLocal
                     then [identLocal]
                     else modpath)
                  i
                  ver'
                  []
        (_, CtxUsage) -> do
          case splitPath (map stripPosition path) [] of
            -- Path without module
            [x:ridents]
              | not (isUppercase x) && all isLowercase ridents ->
                matchName pos' [] x
                  $ map
                      (\case
                         ILowercase _ i -> i
                         _ -> undefined)
                      ridents
            -- Constructor
            [x]
              | all isUppercase x ->
                matchName
                  pos'
                  (map (\(IUppercase _ i) -> i) $ init x)
                  (last x)
                  []
            -- Global variable inside a module...
            [modpath', x:ridents] ->
              matchName pos' (map (\(IUppercase _ i) -> i) modpath') x
                $ map (\(ILowercase _ i) -> i) ridents
            _ -> bad pos' $ "Invalid variable name " ++ show path
        _ -> bad pos' $ "Invalid name, got " ++ printTree path
  -- EOp1
    EPrefix pos op e1 -> do
      pos' <- getPosition pos
      op' <- bind $ Variable npos [IPrefix npos op]
      e1' <- bind e1
      return $ EApply pos' op' [e1']
    ENegateInt pos e1 -> do
      pos' <- getPosition pos
      op' <- bind $ Variable npos [IPrefix npos $ Prefix "~-"]
      e1' <- bind e1
      return $ EApply pos' op' [e1']
  -- EOp2
    EAdd pos e1 op e2 -> bindOp2 pos (OpAdd npos op) e1 e2
    EAt pos e1 op e2 -> bindOp2 pos (OpAt npos op) e1 e2
    EAnd pos e1 op e2 -> bindOp2 pos (OpAnd npos op) e1 e2
    EAppend pos e1 e2 -> EAppend <$> getPosition pos <*> bind e1 <*> bind e2
    EAssign pos e1 e2 -> EAssign <$> getPosition pos <*> bind e1 <*> bind e2
    EAssignRef pos e1 op e2 -> bindOp2 pos (OpAssignRef npos op) e1 e2
    ECompare pos e1 op e2 -> bindOp2 pos (OpCompare npos op) e1 e2
    EHash pos e1 op e2 -> bindOp2 pos (OpHash npos op) e1 e2
    EMultiply pos e1 op e2 -> bindOp2 pos (OpMul npos op) e1 e2
    EOr pos e1 op e2 -> bindOp2 pos (OpOr npos op) e1 e2
    EShift pos e1 op e2 -> bindOp2 pos (OpShift npos op) e1 e2
    ESubFloat pos e1 e2 -> bindOp2 pos (OpSubFloat npos) e1 e2
    ESubInt pos e1 e2 -> bindOp2 pos (OpSubInt npos) e1 e2
    LetPattern pos e1 e2 ->
      bindOp2 pos (OpCompare npos $ InfixCompare "=") e1 e2
    TTuple pos e1 es -> bindMultiplication pos (e1 : es)
    other -> do
      pos' <- getPosition (hasPosition other)
      bad pos' $ "Cannot bind invalid expression " ++ show other

instance StaticBinding Expr where
  bind x = do
    scope <- getScope
    case scope ^. hasTag of
      TagType -> bindType x
      TagPattern -> bindPattern x
      _ -> bindExpression x

instance StaticBinding Statement where
  bind x =
    case x of
      Definition pos rec@(RecYes _) defs -> do
        pos' <- getPosition pos
        defs' <-
          mapM
            (\case
               LetPattern _ p e -> do
                 p' <- local (setNameContext CtxGlobal) (bind p)
                 case p' of
                   ETyped _ (EApply _ _ params) _ -> do
                     e' <- dummyPrepare params (bind e)
                     return $ LetPattern npos p' e'
                   EApply _ _ params -> do
                     e' <- dummyPrepare params (bind e)
                     return $ LetPattern npos p' e'
                   _ -> LetPattern npos p' <$> bind e
               other -> do
                 pos2' <- getPosition (hasPosition other)
                 bad pos2' $ "Invalid definition, got... " ++ printTree other)
            defs
        return $ Definition pos' rec defs'
      Definition pos rec@(RecNo _) defs -> do
        pos' <- getPosition pos
        defs' <-
          mapM
            (\case
               LetPattern _ (ETyped _ (EApply _ v params) t) e -> do
                 params' <- local (setNameContext CtxLocal) (bind params)
                 e' <- dummyPrepare params' (bind e)
                 v' <- local (setNameContext CtxGlobal) (bind v)
                 t' <- local (setTag TagType) (bind t)
                 return
                   $ LetPattern
                       npos
                       (ETyped npos (EApply npos v' params') t')
                       e'
               LetPattern _ (EApply _ v params) e -> do
                 params' <- local (setNameContext CtxLocal) (bind params)
                 e' <- dummyPrepare params' (bind e)
                 v' <- local (setNameContext CtxGlobal) (bind v)
                 return $ LetPattern npos (EApply npos v' params') e'
               LetPattern _ v e -> do
                 e' <- bind e
                 v' <- local (setNameContext CtxGlobal) (bind v)
                 return $ LetPattern npos v' e'
               other -> do
                 pos2' <- getPosition (hasPosition other)
                 bad pos2' $ "Invalid definition, got... " ++ printTree other)
            defs
        return $ Definition pos' rec defs'
      Directive1 pos i v ->
        Directive2
          <$> getPosition pos
          <*> return (DirectiveIdent $ "#" ++ printTree i)
          <*> return v
      Directive2 pos i v ->
        Directive2 <$> getPosition pos <*> return i <*> return v
      EndStmt _ stmt -> do
        stmt' <- bind stmt
        return $ EndStmt (hasPosition stmt') stmt'
      Exception pos (ConstrOf _ v@(Variable _ [IUppercase {}]) t) -> do
        pos' <- getPosition pos
        v' <- local (setNameContext CtxGlobal) (bind v)
        t' <- local (setTag TagType) (bind t)
        return $ Exception pos' (ConstrOf npos v' t')
      Expression _ e -> Expression <$> getPosition (hasPosition e) <*> bind e
      External pos (LetPattern _ (ETyped _ v t) (EConstant _ (CString _ binding))) -> do
        pos' <- getPosition pos
        v' <- local (setNameContext CtxGlobal) (bind v)
        t' <- local (setTag TagType) (bind t)
        let count = countParameters t'
        params <-
          forM (toParameters count) $ \p ->
            local (setNameContext CtxLocal) (bind p)
        let expr = toLambda params $ EExternalLambda binding t' params
        return $ External pos' (LetPattern npos (ETyped npos v' t') expr)
      Module pos p -> Module <$> getPosition pos <*> return p
      ModuleType pos p -> ModuleType <$> getPosition pos <*> return p
      Signature pos (ETyped _ v@(Variable _ [ILowercase {}]) t) -> do
        pos' <- getPosition pos
        v' <- local (setNameContext CtxDeclaration) (bind v)
        t' <- local (setTag TagType) (bind t)
        return . Signature pos' $ ETyped npos v' t'
      SLetIn pos rec es e -> do
        ELetIn pos' rec' es' e' <- bind (ELetIn pos rec es e)
        return $ SLetIn pos' rec' es' e'
      Typedef pos t p es -> do
        pos' <- getPosition pos
        t' <- local (setTag TagType . setNameContext CtxGlobal) (bind t)
        es' <-
          mapM
            (\case
               v@Variable {} -> local (setNameContext CtxGlobal) (bind v)
               ConstrOf pos2 v@(Variable _ [IUppercase _ _]) type_ -> do
                 v' <- local (setNameContext CtxGlobal) (bind v)
                 type_' <- local (setTag TagType) (bind type_)
                 return $ ConstrOf pos2 v' type_'
               Record pos2 fields ->
                 local (setNameContext CtxGlobal) (Record pos2 <$> bind fields)
               other -> local (setTag TagType) (bind other))
            es
        return $ Typedef pos' t' p es'
      _ -> bad Nothing $ "Cannot bind statement " ++ show x

instance StaticBinding Toplevel where
  bind x =
    case x of
      Toplevel1 pos s -> Toplevel1 <$> getPosition pos <*> mapM bind s
      Toplevel2 pos e s ->
        Toplevel1 <$> getPosition pos <*> mapM bind (Expression pos e : s)

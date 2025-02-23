--
-- Type reconstruction algorithm.
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Prelude hiding (readFile)

import Control.Lens ((^.))
import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (liftIO, local)
import Control.Unification
  ( UTerm(..)
  , applyBindings
  , equiv
  , freshen
  , subsumes
  , unify
  )
import Control.Unification.IntVar (IntVar(..))
import Control.Unification.Types (BindingMonad(..))
import Data.Functor ((<&>))
import Data.IORef
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set

import Context
import Flags
import Grammar
import Operators ()
import PatternMatching
import Print
import Terms

assertSignature :: Expr -> Expr -> InterpreterT ()
assertSignature v@(VCanonical _ name) t = do
  t' <- toR t >>= freshen
  (_, fv) <- getTerm name
  -- Work on a copy.
  t1 <- freshen t' -- Signature
  t2 <- freshen (UVar fv) -- Evaluated type
  res1 <- t1 `equiv` t2
  res2 <- t2 `subsumes` t1
  unless (isJust res1 || res2) $ do
    t1' <- applyBindings t1 >>= fromR <&> simplify
    t2' <- applyBindings t2 >>= fromR <&> simplify
    bad Nothing
      $ "Mismatched signature of "
          ++ printTree v
          ++ ". Expected "
          ++ printTree t1'
          ++ " got "
          ++ printTree t2'
  tryUnify
    (UVar fv)
    t'
    (ERUnknown npos $ "(32) Mismatch with signature of " ++ printTree v)
assertSignature v _ =
  bad Nothing $ "Invalid signature. Expected a variable, got " ++ printTree v

assign :: Expr -> Expr -> InterpreterT ()
assign v@(VCanonical _ name) t = do
  (_, fv) <- getTerm name
  t' <- toR t >>= freshen
  tryUnify
    (UVar fv)
    t'
    (ERUnknown npos
       $ "(1) Assigning type " ++ printTree t ++ " to " ++ printTree v)
assign v _ =
  bad Nothing $ "Invalid assignment. Expected a variable, got.. " ++ printTree v

countParameters :: Expr -> Int
countParameters x =
  case x of
    TFunction _ _ x2 -> 1 + countParameters x2
    _ -> 0

getTerm :: CanonicalName -> InterpreterT (Memory, IntVar)
getTerm x =
  case x of
    CanonicalName _ _ _ _ [] -> follow (to x :: Path)
    CanonicalField {} -> follow (to x :: Path)
    _ -> bad Nothing $ "Cannot get term of " ++ show x
  where
    follow :: Path -> InterpreterT (Memory, IntVar)
    follow path = do
      context <- getContext
      Just ref <- go False path (context ^. hmemory)
      fv <- termOfMemory ref
      fv' <-
        if fv == IntVar 0
          then do
            res <- freeVar
            liftIO
              $ modifyIORef
                  ref
                  (\case
                     File v _ -> File v res
                     Directory m _ -> Directory m res)
            return res
          else return fv
      return (ref, fv')

save :: CanonicalName -> Expr -> InterpreterT ()
save name e = do
  (ref, _) <- getTerm name
  saveRef ref e

saveRef :: Memory -> Expr -> InterpreterT ()
saveRef ref e = do
  fv <- termOfMemory ref
  case e of
    Reference ref' -> liftIO $ readIORef ref' >>= writeIORef ref
    Record {} -> do
      Directory e' _ <- toR e >>= liftIO . readIORef
      liftIO $ writeIORef ref (Directory e' fv)
    _ -> liftIO $ writeIORef ref (File e fv)

simplify :: Expr -> Expr
simplify t = relabel t
  where
    idents :: Expr -> [OIdent]
    idents =
      \case
        TFunction _ t1 t2 -> idents t1 ++ idents t2
        TIdent _ t1 -> [t1]
        TParams _ ps -> concatMap idents ps
        TPolymorphic _ p _ -> idents p
        TTuple _ t1 ts -> concatMap idents (t1 : ts)
        Variable {} -> []
        _ -> undefined
    mapping :: Map.Map OIdent Expr
    mapping =
      let types' = nub $ idents t
       in Map.fromList $ zip types' [toTypeIdent i | i <- [0 .. length types']]
    relabel :: Expr -> Expr
    relabel =
      \case
        TFunction _ t1 t2 -> TFunction npos (relabel t1) (relabel t2)
        TIdent _ t1 -> mapping Map.! t1
        TParams _ ps -> TParams npos $ map relabel ps
        TPolymorphic _ p t1 -> TPolymorphic npos (relabel p) $ relabel t1
        TTuple _ t1 ts -> TTuple npos (relabel t1) $ map relabel ts
        Variable _ path -> Variable npos path
        _ -> undefined

tryUnify :: OpenTerm -> OpenTerm -> EError -> InterpreterT ()
tryUnify t1 t2 e' = do
  scope <- getScope
  when (Set.member FTraceTypeConstraints (scope ^. flags)) $ do
    case e' of
      ERUnknown _ msg -> do
        t1' <- applyBindings t1
        t2' <- applyBindings t2
        t1'' <- fromR t1' <&> simplify
        t2'' <- fromR t2' <&> simplify
        Context.log $ LogTypeConstraint msg (t1', t1'') (t2', t2'')
      _ -> return ()
  _ <- (t1 `unify` t2) `catchError` appendTrace e'
  return ()

termOfType :: Expr -> InterpreterT IntVar
termOfType x = do
  iv <- freeVar
  x' <- toR x >>= freshen
  tryUnify
    (UVar iv)
    x'
    (ERUnknown npos $ "(2) Assigning type to " ++ printTree x)
  return iv

class Typeable a where
  termOf :: a -> InterpreterT IntVar
  typeOf :: a -> InterpreterT Expr
  typeOf x = termOf x >>= applyBindings . UVar >>= fromR <&> simplify

instance Typeable Constant where
  termOf x = do
    context <- getContext
    case x of
      CBegin {} -> return $ context ^. htypes . tunit
      CChar {} -> return $ context ^. htypes . tchar
      CConstructor _ path -> do
        fv <- freeVar
        mfs <- go True path (context ^. hmemory)
        case mfs of
          Nothing ->
            bad Nothing $ "Cannot find the type of constructor " ++ show x
          Just fs -> do
            t' <- termOfMemory fs >>= freshen . UVar
            tryUnify
              (UVar fv)
              t'
              (ERUnknown npos
                 $ "(29) Resolving type of constructor " ++ printTree x)
        return fv
      CDouble {} -> return $ context ^. htypes . tfloat
      CFalse {} -> return $ context ^. htypes . tbool
      CFun {} -> bad Nothing $ "Expected a valid constant got " ++ show x
      CInteger {} -> return $ context ^. htypes . tinteger
      CString {} -> return $ context ^. htypes . tstring
      CTrue {} -> return $ context ^. htypes . tbool
      CUnit1 {} -> return $ context ^. htypes . tunit
      CUnit2 {} -> return $ context ^. htypes . tunit

instance Typeable Expr where
  termOf x =
    case x of
    -- Types...
      Variable {} -> termOfType x
      TIdent {} -> termOfType x
      TTuple {} -> termOfType x
      TPolymorphic {} -> termOfType x
      TFunction {} -> termOfType x
      TParams {} -> termOfType x
    -- Expressions ...
      Blank _ -> freeVar
      EAppend _ h tl -> do
        fv <- freeVar
        iv1 <- termOf h
        iv2 <- termOf tl
        tryUnify
          (UVar fv)
          (UTerm (termList (UVar iv1)))
          (ERUnknown npos $ "(11) Resolving type of list " ++ printTree x)
        tryUnify
          (UVar fv)
          (UVar iv2)
          (ERUnknown npos $ "(12) Resolving type of list " ++ printTree x)
        return fv
      EApply _ v es -> do
        fv <- freeVar
        iv1 <- termOf v
        iv2 <- mapM termOf es
        let f = termComposeFunction $ map UVar $ iv2 ++ [fv]
        tryUnify
          f
          (UVar iv1)
          (ERUnknown npos
             $ "(9) Resolving type of function application " ++ printTree x)
        return fv
      EAssign pos e1@(VCanonical _ (CanonicalName _ _ _ _ ridents)) e2
        | not (null ridents) -> do
          context <- getContext
          let i = last ridents
          unless (Set.member i (context ^. htypes . mutablefields))
            $ bad pos
            $ "Field is not mutable " ++ printTree i
          iv1 <- termOf e1
          iv2 <- termOf e2
          tryUnify
            (UVar iv1)
            (UVar iv2)
            (ERUnknown npos
               $ "(14) Resolving type of assignment " ++ printTree x)
          termOf unit
      EBegin _ e -> termOf e
      EConstant _ c -> termOf c
      EConstructor _ v e -> do
        fv <- freeVar
        iv1 <- termOf v
        iv2 <- termOf e
        let f = termComposeFunction [UVar iv2, UVar fv]
        tryUnify
          f
          (UVar iv1)
          (ERUnknown npos $ "(10) Resolving type of constructor " ++ printTree x)
        return fv
      EExternalLambda _ t es -> do
        fv <- freeVar
        iv <- mapM termOf es
        t' <- termOfType t
        let f = termComposeFunction . map UVar $ iv ++ [fv]
        tryUnify
          f
          (UVar t')
          (ERUnknown npos
             $ "(30) Resolving type of external function " ++ printTree x)
        return fv
      EFor _ start _ end loop -> do
        _ <- termOf start
        _ <- termOf end
        _ <- termOf loop
        termOf unit
      EIf _ c e -> do
        iv1 <- termOf c
        iv2 <- termOf e
        iv3 <- termOf (from True :: Constant)
        iv4 <- termOf unit
        tryUnify
          (UVar iv1)
          (UVar iv3)
          (ERUnknown npos
             $ "(15) Resolving type of if condition " ++ printTree c)
        tryUnify
          (UVar iv2)
          (UVar iv4)
          (ERUnknown npos $ "(16) Resolving type of if result " ++ printTree e)
        return iv4
      EIfElse _ c e1 e2 -> do
        iv1 <- termOf c
        iv2 <- termOf e1
        iv3 <- termOf e2
        iv4 <- termOf (from True :: Constant)
        tryUnify
          (UVar iv1)
          (UVar iv4)
          (ERUnknown npos
             $ "(17) Resolving type of if condition " ++ printTree x)
        tryUnify
          (UVar iv2)
          (UVar iv3)
          (ERUnknown npos
             $ "(18) Resolving type of if results "
                 ++ printTree e1
                 ++ " and "
                 ++ printTree e2)
        return iv3
      ELambda _ _ es e -> do
        fv <- freeVar
        iv1 <- mapM termOf es
        iv2 <- termOf e
        let f = termComposeFunction $ map UVar $ iv1 ++ [iv2]
        tryUnify
          (UVar fv)
          f
          (ERUnknown npos $ "(21) Resolving type of lambda " ++ printTree x)
        return fv
      ELetIn _ _ vs e -> do
        mapM_ termOf vs
        termOf e
      EMatch _ v _ patterns -> do
        fv <- freeVar
        iv1 <- termOf v
        let f = termComposeFunction $ map UVar [iv1, fv]
        forM_ patterns $ \p -> do
          iv2 <- termOf p
          tryUnify
            f
            (UVar iv2)
            (ERUnknown npos $ "(23) Resolving type of pattern " ++ printTree p)
        return fv
      EPartial patterns parts e -> do
        fv <- freeVar
        iv1 <- mapM termOf patterns
        iv2 <- mapM termOf (reverse parts)
        iv3 <- termOf e
        let f1 = termComposeFunction $ map UVar $ iv1 ++ [iv3]
        let f2 = termComposeFunction $ map UVar $ iv2 ++ [fv]
        tryUnify
          f1
          f2
          (ERUnknown npos
             $ "(19) Resolving type of partial function " ++ printTree x)
        return fv
      ESeq _ e es -> mapM termOf (e : es) <&> last
      ETuple _ e es -> do
        fv <- freeVar
        f <- UTerm . TermTuple . map UVar <$> mapM termOf (e : es)
        tryUnify
          (UVar fv)
          f
          (ERUnknown npos $ "(13) Resolving type of tuple " ++ printTree x)
        return fv
      ETyped _ e t -> do
        fv <- termOf e
        iv <- termOfType t
        tryUnify
          (UVar fv)
          (UVar iv)
          (ERUnknown npos
             $ "(31) Resolving signature of definition " ++ printTree x)
        return fv
      EWhile _ cond loop -> do
        iv1 <- termOf cond
        iv2 <- termOf (from True :: Constant)
        tryUnify
          (UVar iv1)
          (UVar iv2)
          (ERUnknown npos
             $ "(8) Checking if condition resolves to bool " ++ printTree cond)
        _ <- termOf loop
        termOf unit
      LetPattern _ e1 e2 -> do
        iv1 <- termOf e1
        iv2 <- termOf e2
        tryUnify
          (UVar iv1)
          (UVar iv2)
          (ERUnknown npos $ "(22) Resolving type of comparison " ++ printTree x)
        termOf (from True :: Constant)
      List _ exprs -> do
        fv <- freeVar
        fvl <- freeVar
        forM_
          exprs
          (\e -> do
             iv <- termOf e
             tryUnify
               (UVar fv)
               (UVar iv)
               (ERUnknown npos
                  $ "(5) Assigning type to element of list " ++ printTree e))
        tryUnify
          (UVar fvl)
          (UTerm (termList (UVar fv)))
          (ERUnknown npos $ "(6) Assigning type to list " ++ printTree x)
        return fvl
      Nil2 _ -> do
        fv <- freeVar
        fvl <- freeVar
        tryUnify
          (UVar fv)
          (UTerm (termList (UVar fvl)))
          (ERUnknown npos "(7) Assigning type to empty list.")
        return fv
      Record _ fields -> do
        fv <- freeVar
        forM_
          fields
          (\case
             EField _ v e -> do
               iv1 <- termOf e
               iv2 <- termOf v
               let f = termComposeFunction . map UVar $ [fv, iv1]
               tryUnify
                 (UVar iv2)
                 f
                 (ERUnknown npos
                    $ "(4) Assigning type to record field " ++ printTree x)
             _ -> undefined)
        return fv
      Reference ref -> termOfMemory ref
      VCanonical _ name@(CanonicalName _ _ _ _ ridents)
        | not (null ridents) -> do
          iv <- termOf $ VCanonical npos (name {recordidents = []})
          foldM
            (\acc ident -> do
               fv <- freeVar
               t <- termOf (VCanonical npos $ CanonicalField ident)
               let f = termComposeFunction . map UVar $ [acc, fv]
               tryUnify
                 f
                 (UVar t)
                 (ERUnknown npos
                    $ "(3) Resolving type of record ident " ++ printTree ident)
               return fv)
            iv
            ridents
      VCanonical _ name -> do
        scope <- getScope
        (_, fv) <- getTerm name
        if isLocalVariable name -- Issue with ad-hoc polymorphism. Ocaml does not support it.
             || isRecursive (to name :: Path) scope
          then return fv
          else do
            UVar fv' <- freshen $ UVar fv
            return fv'
      _ -> bad Nothing $ "Unknown term of " ++ show x

resolve :: Statement -> InterpreterT Statement
resolve x =
  case x of
    Definition pos rec defs -> do
      let visible name =
            setRecursive (RecYes npos) $ (to :: CanonicalName -> Path) name
      defs' <-
        mapM
          (\case
             LetPattern pos2 p e ->
               case p of
                 EApply _ v@(VCanonical _ name) params -> do
                   fv <- local (visible name) (termOf v)
                   let e' = ELambda npos (Fun1 npos) params e
                   iv <- local (setRecursive rec (to name :: Path)) (termOf e')
                   tryUnify
                     (UVar fv)
                     (UVar iv)
                     (ERUnknown npos
                        $ "(25) Resolving type of function definition "
                            ++ printTree x)
                   return $ LetPattern pos2 v e'
                 ETyped _ v@(VCanonical _ name) t -> do
                   fv <- local (visible name) (termOf v)
                   iv1 <- termOf t
                   tryUnify
                     (UVar fv)
                     (UVar iv1)
                     (ERUnknown npos
                        $ "(27) Resolving signature of definition  "
                            ++ printTree x)
                   iv2 <- termOf e
                   tryUnify
                     (UVar fv)
                     (UVar iv2)
                     (ERUnknown npos
                        $ "(28) Resolving type of definition " ++ printTree x)
                   return $ LetPattern pos2 v e
                 ETyped _ (EApply _ v@(VCanonical _ name) params) t -> do
                   let e' = ELambda npos (Fun1 npos) params (ETyped npos e t)
                   fv <- local (visible name) (termOf v)
                   iv <- termOf e'
                   tryUnify
                     (UVar fv)
                     (UVar iv)
                     (ERUnknown npos
                        $ "(26) Resolving type of typed function definition "
                            ++ printTree x)
                   return $ LetPattern pos2 v e'
                 _ -> do
                   let globals = dummyMatch p
                   fv <- local (compose . map visible $ globals) (termOf p)
                   iv <- termOf e
                   tryUnify
                     (UVar fv)
                     (UVar iv)
                     (ERUnknown npos
                        $ "(24) Resolving type of definition " ++ printTree x)
                   return $ LetPattern pos2 p e
             _ -> undefined)
          defs
      return $ Definition pos rec defs'
    Directive2 {} -> return x
    EndStmt pos stmt -> EndStmt pos <$> resolve stmt
    Exception _ (ConstrOf _ v t) -> do
      let t' = typeComposeFunction [t, Variable npos typeExn]
      Types.assign v t'
      return x
    Expression _ e -> do
      _ <- termOf e
      return x
    External pos1 (LetPattern pos2 (ETyped _ v t) e) -> do
      Types.assign v t
      return $ External pos1 (LetPattern pos2 v e)
    Module {} -> return x
    ModuleType {} -> return x
    Signature pos (ETyped _ v t) -> do
      Types.assign v t
      return $ Signature pos v
    Typedef pos t _ cases -> do
      context <- getContext
      path <-
        case t of
          Variable _ path -> return $ map stripPosition path
          TPolymorphic _ _ (Variable _ path) -> return $ map stripPosition path
          _ -> bad pos $ "Invalid type name " ++ show t
      stmt <- readFile path (context ^. htypes . types)
      case stmt of
        Just _ -> bad pos $ printTree path ++ " is already defined."
        Nothing -> do
        -- Save signature.
          write
            path
            (Typedef pos t (PatternPrefixNo npos) cases)
            (context ^. htypes . types)
          forM_ cases $ \case
            v@VCanonical {} -> Types.assign v t
            ConstrOf _ v type_ -> do
              let t' = typeComposeFunction [type_, t]
              Types.assign v t'
            Record _ fields ->
              forM_ fields $ \case
                EFieldDecl _ v type_ -> do
                  let t' = typeComposeFunction [t, type_]
                  Types.assign v t'
                EFieldDeclMut _ v type_ -> do
                  let t' = typeComposeFunction [t, type_]
                  Types.assign v t'
                other ->
                  bad Nothing
                    $ "Expected a record type declaration, but got..."
                        ++ show other
            other
              | length cases == 1 ->
                bad Nothing
                  $ "Aliases are not currently supported, got " ++ show other
            other -> bad Nothing $ "Invalid type declaration " ++ show other
      return x
    SLetIn _ _ defs e -> do
      mapM_ termOf defs
      _ <- termOf e
      return x
    _ -> bad Nothing $ "Cannot resolve type of statement " ++ show x

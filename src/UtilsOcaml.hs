{-# LANGUAGE RankNTypes #-}
module UtilsOcaml where

import Control.Lens (Lens', over, preview,(^.))
import Control.Monad.Catch (catchAll)
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import Data.List (find, length, nub)

import Grammar

-- Constants

identContents :: Ident
identContents = Ident "contents"

temporaryVariable :: SimpleVariable
temporaryVariable = VVariable [Ident "__temp__"]

-- Getters

fvPath :: CanonicalName -> NName
fvPath e = case e of
  CanonicalName (m, n) v r -> [m, n, Ident $ "v." ++ show v] ++ r
  CanonicalModule m -> [m]

-- Properties

-- [TODO] if b == EInt 1 ?
isTrue :: Expression -> Bool
isTrue x = (EConst . EBool $ ETrue) == x

isLocalVariable :: Variable -> Bool
isLocalVariable v = case v of
  VSimple (VCanonical (CanonicalName (m, _) _ _ )) -> m == Ident ".local"
  _ -> False

isModuleDocs :: Statement -> Bool
isModuleDocs x = case x of
  SExpression (EVar (VSimple (VCanonical CanonicalModule {}))) -> True
  _ -> False

-- Converters

toLambda :: [Expression] -> Expression -> Expression
toLambda mparams expression =
  if null mparams
    then expression
    else ELambda FunctionPrefix1 $ Matching1 mparams expression

toParameters :: Int -> [Expression]
toParameters n = 
  [EVar $ toVariableFromString ("." ++ show i) | i <- [1..n]]

toLambdaFromInfixOp :: InfixOp -> Expression
toLambdaFromInfixOp infixop =
  toLambda params expression where
    params@[v1, v2] = toParameters 2
    expression = EOp2 v1 infixop v2

toVariableFromIdents :: NName -> Variable
toVariableFromIdents = VSimple . VVariable

toVariableFromString :: String -> Variable
toVariableFromString s = toVariableFromIdents [Ident s]

-- Lenses

lensExpressionFromLabeledStatement :: Lens' LabeledStatement Expression
lensExpressionFromLabeledStatement f x = case x of
  SLabeled v e -> SLabeled v <$> f e
  SLabeledTyped v t e -> SLabeledTyped v t <$> f e
  SLabeledBound v i e -> SLabeledBound v i <$> f e

lensVariableFromLabeledStatement :: Lens' LabeledStatement Variable
lensVariableFromLabeledStatement f x = case x of
  SLabeled v e -> fmap (`SLabeled` e) (f v)
  SLabeledTyped v t e -> fmap (\v' -> SLabeledTyped v' t e) (f v)
  SLabeledBound v i e -> fmap (\v' -> SLabeledBound v' i e) (f v)

lensIdentsFromVariable :: Lens' Variable NName
lensIdentsFromVariable f v = case v of
  VSimple (VCanonical (CanonicalName index ver idents)) ->
    fmap (VSimple . VCanonical . CanonicalName index ver) (f idents)
  _ -> undefined

lensNameFromConstructor :: Lens' Constructor Ident
lensNameFromConstructor f v = case v of
  ConstructorEps name -> fmap ConstructorEps (f name)
  ConstructorType name t -> fmap (`ConstructorType` t) (f name)
  -- ConstructorAs name record -> fmap (`ConstructorAs` record) (f name)

lensNameFromModuleName :: Lens' ModuleName Ident
lensNameFromModuleName f v = case v of
  SModuleName0 name -> fmap SModuleName0 (f name)
  SModuleName1 name signature -> fmap (`SModuleName1` signature) (f name)

-- Miscellaneous

compose :: [a -> a] -> a -> a
compose = foldr (.) id

maybeDecrement :: Maybe Int -> Maybe Int
maybeDecrement v = case v of
  Just x -> if x == 1 then Nothing else Just $ x - 1
  Nothing -> Nothing

maybeIncrement :: Maybe Int -> Maybe Int
maybeIncrement x = Just $ case x of
  Nothing -> 0 -- This should be 1?
  Just i -> i + 1

maybePrepend :: a -> Maybe [a] -> Maybe [a]
maybePrepend v l = case l of
  Nothing -> Just [v]
  Just xs -> Just (v:xs)

maybeReplace :: a -> Maybe [a] -> Maybe [a]
maybeReplace v l = case l of
  Nothing -> Just [v]
  Just (x:xs) -> Just (v:xs)

replaceLabeledStatement :: LabeledStatement -> Expression -> LabeledStatement
replaceLabeledStatement x e = over lensExpressionFromLabeledStatement (const e) x

-- Types

typeConstant :: Constant -> Type
typeConstant x = T1Const $ case x of
  EBool _ -> TBool
  EInt _ -> TInt
  EFloat _ -> TFloat
  EChar _ -> TChar

typeNth :: Int -> Type
typeNth x = let character i = chr (ord 'a' + i) in
    typeIdent $ character (x `mod` 26) : (if x >= 26 then show (x `div` 26) else [])

-- Inverse function to typeNth
typeWhich :: Type -> Maybe Int
typeWhich x = case x of
  TIdent (Ident (c:n)) -> 
    Just $ (ord c - ord 'a') + (if null n then 0 else 26 * read n)
  _ -> Nothing 

typeIdent :: String -> Type
typeIdent = TIdent . Ident

-- Relabeling utils

typeSimplify :: Type -> Type
typeSimplify t = typeRelabel (typeRelabelingMap t) t

typeRelabelingMap :: Type -> Map.Map Type Type
typeRelabelingMap t = 
  let types = nub $ typeGetUsed t in
    Map.fromList $ zip types [typeNth i | i <- [0..length types]]

typeMerge :: ([a] -> a) -> [a] -> a
typeMerge f [] = undefined -- Void
typeMerge f [x] = x
typeMerge f xs = f xs

class IsType a where
  typeRelabel :: Map.Map Type Type -> a -> a
  typeGetUsed :: a -> [Type]
  -- typeFlatten fixes strange list based grammar... It should not exist...
  typeFlatten :: a -> a

instance IsType TypeParameters where
  typeRelabel m x = case x of
    TParameters2 t1 t2 -> TParameters2 (typeRelabel m t1) (typeRelabel m t2)
    TParametersN t1 t2 -> TParametersN (typeRelabel m t1) (typeRelabel m t2)
  typeGetUsed x = case x of
    TParameters2 t1 t2 -> typeGetUsed t1 ++ typeGetUsed t2
    TParametersN t1 t2 -> typeGetUsed t1 ++ typeGetUsed t2
  typeFlatten x = case x of
    TParameters2 t1 t2 -> TParameters2 (typeFlatten t1) (typeFlatten t2)
    TParametersN t1 t2 -> TParametersN (typeFlatten t1) (typeFlatten t2)

instance IsType Type where
  typeRelabel m x = case x of
    T1Const _ -> x
    TIdent t -> m Map.! x
    TAlias t -> x
    TParameterized1 t c -> TParameterized1 (typeRelabel m t) c
    TParameterizedN t c -> TParameterizedN (typeRelabel m t) c
    TTuple ts -> TTuple $ map (typeRelabel m) ts
    TFunction t1 t2 -> TFunction (typeRelabel m t1) (typeRelabel m t2)
  typeGetUsed x = case x of
    T1Const _ -> []
    TIdent _ -> [x]
    TAlias t -> [x]
    TParameterized1 t c -> typeGetUsed t
    TParameterizedN t c -> typeGetUsed t
    TTuple ts -> concatMap typeGetUsed ts
    TFunction t1 t2 -> typeGetUsed t1 ++ typeGetUsed t2 
  typeFlatten x = case x of
    T1Const _ -> x
    TIdent _ -> x
    TAlias t -> x
    TParameterized1 t c -> TParameterized1 (typeFlatten t) c
    TParameterizedN t c -> TParameterizedN (typeFlatten t) c
    TTuple ts -> case ts of
      [] -> undefined
      [t] -> typeFlatten t
      _ -> TTuple $ map typeFlatten ts
    TFunction t1 t2 ->  TFunction (typeFlatten t1) (typeFlatten t2)

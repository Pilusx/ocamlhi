--
-- Operator interpretation algorithm.
-- It describes what 'op1 x' or '(x op2 y)' really means.
-- Ocaml uses C bindings to simulate some of the functions.
-- To make it easier this file provides implementation of those functions written in Haskell. 
--

{-# LANGUAGE FlexibleInstances #-}
module Operators (
  applyBoolInfixOp,
  applyInfixOp,
  applyPrefixOp
  ) where

import Control.Monad.Catch (catchAll)
import Data.Bits (xor, (.&.), (.|.))
import Data.Fixed (mod')

import Context
import Grammar
import PrintOcaml

toBool :: Boolean -> Bool
toBool ETrue = True
toBool EFalse = False

fromBool :: Bool -> Boolean
fromBool True = ETrue
fromBool False = EFalse

class Parameter a where
  apply1 :: String -> a -> a
  apply1 = undefined 
  apply2 :: String -> a -> a -> a
  apply2 = undefined
  apply2b :: String -> a -> a -> Bool
  apply2b = undefined

instance Parameter Bool where
  apply1 x = case x of
    "not" -> not
    "-" -> undefined
  apply2b x = case x of
    "==" -> (==)
    "/=" -> (/=)
    "<>" -> (/=)
    "<" -> (<)
    "<=" -> (<=)
    ">" -> (>)
    ">=" -> (>=)
    "or" -> (.|.)
    "xor" -> xor
    "and" -> (.&.)

instance Parameter Integer where
  apply1 x v = case x of
    "not" -> undefined
    "-" -> -v

  apply2 op = case op of
    "+" -> (+)
    "-" -> (-)
    "*" -> (*)
    "mod" -> mod
    "/" -> div
    _ -> error "Unknown operator"
  
  apply2b op = case op of
    "==" -> (==)
    "/=" -> (/=)
    "<>" -> (/=)
    "<" -> (<)
    "<=" -> (<=)
    ">" -> (>)
    ">=" -> (>=)
    "or" -> \x y -> (.|.) x y /= 0
    "xor" -> \x y -> xor x y /= 0
    "and" -> \x y ->  (x /= 0) && (y /= 0)

instance Parameter String where
  apply2b op = case op of
    "==" -> (==)
    "/=" -> (/=)
    "<>" -> (/=)
    "<" -> (<)
    "<=" -> (<=)
    ">" -> (>)
    ">=" -> (>=)
    _ -> error "Unknown operator"

instance Parameter Double where
  apply2b op = case op of
    "==" -> (==)
    "/=" -> (/=)
    "<>" -> (/=)
    "<" -> (<)
    "<=" -> (<=)
    ">" -> (>)
    ">=" -> (>=)
    _ -> error "Unknown operator"

  apply1 x v = case x of
    "not" -> undefined
    "-" -> -v
  
  apply2 op = case op of
    "+" -> (+)
    "-" -> (-)
    "*" -> (*)
    "mod" -> mod'
    "/" -> (/)
    _ -> error "Unknown operator"

instance Parameter Boolean where
  apply1 f x = fromBool $ apply1 f (toBool x)
  apply2 f x y = fromBool $ apply2 f (toBool x) (toBool y)
  apply2b f x y = apply2b f (toBool x) (toBool y)

instance Parameter Constant where
  apply1 f (EBool x) = EBool $ apply1 f x
  apply1 f (EInt x) = EInt $ apply1 f x
  apply1 f (EFloat x) = EFloat $ apply1 f x

  apply2 f (EBool x) (EBool y) = EBool $ apply2 f x y
  apply2 f (EInt x) (EInt y) = EInt $ apply2 f x y
  apply2 f (EFloat x) (EFloat y) = EFloat $ apply2 f x y

  apply2b f (EBool x) (EBool y) = apply2b f x y
  apply2b f (EInt x) (EInt y) = apply2b f x y
  apply2b f (EFloat x) (EFloat y) = apply2b f x y

instance Parameter Expression where
  apply1 f (EConst x) = EConst $ apply1 f x
  apply2 f (EConst x) (EConst y) = EConst $ apply2 f x y

  apply2b f (EConst x) (EConst y) = apply2b f x y
  apply2b f (EString x) (EString y) = apply2b f x y
  
  -- Operator (==) for tuples
  apply2b "==" x y = case (x, y) of 
    (EVar (VConstructor [x]), EVar (VConstructor [y])) -> 
      x == y
    (ETuple xs, ETuple ys) -> 
      length xs == length ys && and (zipWith (apply2b "==") xs ys)
    (ENamedTuple xname xs, ENamedTuple yname ys) ->
      xname == yname && apply2b "==" (ETuple xs) (ETuple ys)

-- Exposed functions.

applyBoolInfixOp :: String -> Expression -> Expression -> AResult Expression
applyBoolInfixOp f x y = do
  r <- catchAll (return $ apply2b f x y) $
    appendSomeTrace (EROperator (printTree f) [x, y])
  return . EConst . EBool . fromBool $ r

applyInfixOp :: String -> Expression -> Expression -> AResult Expression
applyInfixOp f x y =
  catchAll (return $ apply2 f x y) $
    appendSomeTrace (EROperator (printTree f) [x, y])

applyPrefixOp :: String -> Expression -> AResult Expression
applyPrefixOp f x = 
  catchAll (return $ apply1 f x) $ 
    appendSomeTrace (EROperator (printTree f) [x])

-- This file is almost automatically generated (bnfc 2.8.2) with small manual patches.

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintOcaml where

import Data.Char

import Grammar
import UtilsOcaml

{-# ANN module "HLint: ignore" #-}

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . rend (i+1) ts
    "}"      :ts -> showChar '}' . rend (i-1) ts
    ";"      :ts -> showChar ';' . (if i >= 1 then new i else id) . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Ident where
  prt _ (Ident i) = doc (showString i)
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString "."), prt 0 xs]

instance Print InfixSymbol where
  prt _ (InfixSymbol i) = doc (showString i)

instance Print PrefixSymbol where
  prt _ (PrefixSymbol i) = doc (showString i)

instance Print InfixSymbolOther where
  prt _ (InfixSymbolOther i) = doc (showString i)

instance Print PrefixSymbolOther where
  prt _ (PrefixSymbolOther i) = doc (showString i)

instance Print Prog where
  prt i e = case e of
    Prog1 statements -> prPrec i 0 (concatD [prt 0 statements])

instance {-# OVERLAPS #-} Print [Statement] where
  prt = prtList

instance Print Statement where
  prt i e = case e of
    SDirective filename -> prPrec i 0 (concatD [doc (showString "#"), doc (showString "use"), prt 0 filename])
    SEnable str -> prPrec i 0 (concatD [doc (showString "#"), doc (showString "enable"), prt 0 str])
    SExpression expression -> prPrec i 0 (concatD [prt 0 expression])
    SExpressionWithType expression type_ -> prPrec i 0 (concatD [prt 0 expression, doc (showString "::"), prt 0 type_])
    SSignature signature -> prPrec i 0 (concatD [prt 0 signature])
    STypedef type_ typedefdefinition -> prPrec i 0 (concatD [doc (showString "type"), prt 0 type_, doc (showString "="), prt 0 typedefdefinition])
    SModuleContent modulecontent -> prPrec i 0 (concatD [prt 0 modulecontent])
    SModuleSignature id signatures -> prPrec i 0 (concatD [doc (showString "module"), doc (showString "type"), prt 0 id, doc (showString "="), doc (showString "sig"), prt 0 signatures, doc (showString "end")])
    SModuleDefinition modulename modulecontents -> prPrec i 0 (concatD [prt 0 modulename, doc (showString "="), doc (showString "struct"), prt 0 modulecontents, doc (showString "end")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";;"), prt 0 xs]

instance Print TypedefDefinition where
  prt i e = case e of
    TypedefConstructors constructors -> prPrec i 0 (concatD [prt 0 constructors])
    TypedefRecords recordfields -> prPrec i 0 (concatD [doc (showString "{"), prt 0 recordfields, doc (showString "}")])

instance Print Expression where
  prt i e = case e of
    EConst constant -> prPrec i 0 (concatD [prt 0 constant])
    EString str -> prPrec i 0 (concatD [prt 0 str])
    EVar variable -> prPrec i 0 (concatD [prt 0 variable])
    EReferenceMemory ptr -> prPrec i 0 (concatD [doc (showString "refmem"), doc (showString "("), doc (showString $ show ptr), doc (showString ")")])
    EOp1 prefixop expression -> prPrec i 0 (concatD [prt 0 prefixop, prt 0 expression])
    EOp2 expression1 infixop expression2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 expression1, prt 0 infixop, prt 0 expression2, doc (showString ")")])
    ERecord labeledstatements -> prPrec i 0 (concatD [doc (showString "{"), prt 0 labeledstatements, doc (showString "}")])
    ERecordMemory lstmts -> undefined
    EIfThenElse expression1 expression2 expression3 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expression1, doc (showString "then"), doc (showString "{"), prt 0 expression2, doc (showString "}"), doc (showString "else"), doc (showString "{"), prt 0 expression3, doc (showString "}")])
    EMatch simplevariables matchings -> prPrec i 0 (concatD [doc (showString "match"), prt 0 simplevariables, doc (showString "with"), doc (showString "("), prt 0 matchings, doc (showString ")")])
    ETypeOf expression -> prPrec i 0 (concatD [doc (showString "__typeof__"), prt 0 expression])
    ELocalDefinition valuedefinition expression -> prPrec i 0 (concatD [prt 0 valuedefinition, doc (showString "in"), prt 0 expression])
    EStack sequencingexpressions -> prPrec i 0 (concatD [doc (showString "begin"), prt 0 sequencingexpressions, doc (showString "end")])
    EFor variable expression1 foriterationtype expression2 sequencingexpressions -> prPrec i 0 (concatD [doc (showString "for"), prt 0 variable, doc (showString "="), prt 0 expression1, prt 0 foriterationtype, prt 0 expression2, doc (showString "do"), prt 0 sequencingexpressions, doc (showString "done")])
    EWhile expression sequencingexpressions -> prPrec i 0 (concatD [doc (showString "while"), prt 0 expression, doc (showString "do"), prt 0 sequencingexpressions, doc (showString "done")])
    EFunctionCall variable expressions -> prPrec i 0 (concatD [prt 0 variable, doc (showString "("), prt 0 expressions, doc (showString ")")])
    EExternalFunctionCall binding expressions -> prPrec i 0 (concatD [doc (showString "external"), doc (showString binding), doc (showString "("), prt 0 expressions, doc (showString ")")])
    ERecursiveFunctionCall variable expressions expression -> prPrec i 0 (concatD [prt 0 variable, doc (showString "rec"), doc (showString "("), prt 0 expressions, doc (showString ";"), prt 0 expression, doc (showString ")")])
    ELambda functionprefix matching -> prPrec i 0 (concatD [prt 0 functionprefix, prt 0 matching])
    ETuple expressions -> prt i (EFunctionCall (VOpTuple T) expressions)
    ENamedTuple {} -> case unfoldRecursivePair e e [] of
      ENamedTuple name expressions -> prPrec i 0 (concatD (prt 0 name : expr)) where expr = if null expressions then [] else [doc (showString "("), prt 0 expressions, doc (showString ")")]
      x -> prt i x
    ERaise exceptionname typeableexpression -> prPrec i 0 (concatD [doc (showString "raise"), doc (showString "("), prt 0 exceptionname, prt 0 typeableexpression, doc (showString ")")])
    ENull -> prPrec i 0 (concatD [doc (showString "*ENull*")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]


-- TODO refactor
-- List(a1, List(a2, List(a3, List(a4, Nil)))) -> List rec(a1, a2, a3, a4; Nil)
unfoldRecursivePair :: Expression -> Expression -> [Expression] -> Expression
unfoldRecursivePair d (ENamedTuple n1 [e1, e2@(ENamedTuple n2 t2es)]) es =
  case length t2es of
    0 -> ERecursiveFunctionCall (VSimple . VVariable $ n1) (reverse $ e1:es) e2
    2 -> if n1 == n2 then unfoldRecursivePair d e2 (e1:es) else d
    _ -> d
unfoldRecursivePair d _ _ = d

instance Print RecursivePrefix where
  prt i e = case e of
    RecNo -> prPrec i 0 (concatD [])
    RecYes -> prPrec i 0 (concatD [doc (showString "rec")])

instance Print Definition where
  prt i e = case e of
    SValue recursiveprefix variables expression -> prPrec i 0 (concatD [doc (showString "let"), prt 0 recursiveprefix, prt 0 variables, doc (showString "="), prt 0 expression])
    SExternal variable type_ str -> prPrec i 0 (concatD [doc (showString "external"), prt 0 variable, doc (showString ":"), prt 0 type_, doc (showString "="), prt 0 str])

instance {-# OVERLAPS #-} Print [SequencingExpression] where
  prt = prtList

instance Print SequencingExpression where
  prt i e = case e of
    ESeq expression -> prPrec i 0 (concatD [prt 0 expression])
    EAssign variable expression -> prPrec i 0 (concatD [prt 0 variable, doc (showString "<-"), prt 0 expression])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print ForIterationType where
  prt i e = case e of
    ForTo -> prPrec i 0 (concatD [doc (showString "to")])
    ForDownto -> prPrec i 0 (concatD [doc (showString "downto")])

instance {-# OVERLAPS #-} Print [Expression] where
  prt = prtList

instance {-# OVERLAPS #-} Print [LabeledStatement] where
  prt = prtList

instance Print LabeledStatement where
  prt i e = case e of
    SLabeled variable expression -> prPrec i 0 (concatD [prt 0 variable, doc (showString "="), prt 0 expression])
    SLabeledTyped variable type_ expression -> prPrec i 0 (concatD [prt 0 variable, doc (showString "of"), prt 0 type_, doc (showString "="), prt 0 expression])
    SLabeledBound variable iv expression -> prPrec i 0 (concatD [prt 0 variable, doc (showString "of"), doc (showString $ "IntVar " ++ show iv), doc (showString "="), prt 0 expression])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print FunctionPrefix where
  prt i e = case e of
    FunctionPrefix1 -> prPrec i 0 (concatD [doc (showString "function")])
    FunctionPrefix2 -> prPrec i 0 (concatD [doc (showString "fun")])

instance Print ExceptionName where
  prt i e = case e of
    NException id -> prPrec i 0 (concatD [prt 0 id])

instance Print ModuleContent where
  prt i e = case e of
    SExceptionDefinition exceptionname type_ -> prPrec i 0 (concatD [doc (showString "exception"), prt 0 exceptionname, doc (showString "of"), prt 0 type_])
    SDefinition definition -> prPrec i 0 (concatD [prt 0 definition])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";;"), prt 0 xs]

instance {-# OVERLAPS #-} Print [Constructor] where
  prt = prtList

instance Print Constructor where
  prt i e = case e of
    ConstructorEps id -> prPrec i 0 (concatD [prt 0 id])
    ConstructorType id type_ -> prPrec i 0 (concatD [prt 0 id, doc (showString "of"), prt 0 type_])
    -- ConstructorAs id record -> prPrec i 0 (concatD [prt 0 id, doc (showString "as"), prt 0 record])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString "|"), prt 0 xs]

instance Print TypeConst where
  prt i e = case e of
    TBool -> prPrec i 0 (concatD [doc (showString "bool")])
    TInt -> prPrec i 0 (concatD [doc (showString "int")])
    TChar -> prPrec i 0 (concatD [doc (showString "char")])
    TFloat -> prPrec i 0 (concatD [doc (showString "float")])
    TString -> prPrec i 0 (concatD [doc (showString "string")])
    TUnit -> prPrec i 0 (concatD [doc (showString "unit")])

instance Print TypeConstr where
  prt i e = case e of
    TConstr id -> prPrec i 0 (concatD [prt 0 id])

instance Print TypeParameters where
  prt i e = case e of
    TParameters2 type_1 type_2 -> prPrec i 0 (concatD [prt 0 type_1, doc (showString ","), prt 0 type_2])
    TParametersN type_ typeparameters -> prPrec i 0 (concatD [prt 0 type_, doc (showString ","), prt 0 typeparameters])

-- TODO These functions should not exist...
fixTypeComplexity :: Type -> Int
fixTypeComplexity x = case x of
  TFunction {} -> 1
  _ -> 0

fixTypeComplexity2 :: Type -> Int
fixTypeComplexity2 x = case x of
  TFunction {} -> 2
  TTuple {} -> 2
  _ -> 0

instance Print Type where
  prt i e = case e of
    T1Const typeconst -> prPrec i 2 (concatD [prt 0 typeconst])
    TIdent id -> prPrec i 0 (concatD [doc (showString "`" . showString (printTree id))])
    TAlias typeconstr -> prPrec i 2 (concatD [prt 0 typeconstr])
    TParameterized1 type_ typeconstr -> prPrec i 2 (concatD [prt (fixTypeComplexity2 type_) type_, prt 0 typeconstr])
    TParameterizedN typeparameters typeconstr -> prPrec i 2 (concatD [doc (showString "("), prt 0 typeparameters, doc (showString ")"), prt 0 typeconstr])
    TTuple [t] -> prt i t -- TODO This should not happen...
    TTuple types -> prPrec i 1 (concatD [prt 2 types])
    TFunction type_1 type_2 -> prPrec i 0 (concatD [prt (fixTypeComplexity type_1) type_1, doc (showString "->"), prt 0 type_2])
  prtList 2 [x] = concatD [prt (fixTypeComplexity2 x) x]
  prtList 2 (x:xs) = concatD [prt (fixTypeComplexity2 x) x, doc (showString "*"), prt 2 xs]

instance {-# OVERLAPS #-} Print [Signature] where
  prt = prtList

instance Print Signature where
  prt i e = case e of
    Signature1 variable type_ -> prPrec i 0 (concatD [doc (showString "val"), prt 0 variable, doc (showString "of"), prt 0 type_])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Mutable where
  prt i e = case e of
    MutableNo -> prPrec i 0 (concatD [])
    MutableYes -> prPrec i 0 (concatD [doc (showString "mutable")])

instance {-# OVERLAPS #-} Print [RecordField] where
  prt = prtList

instance Print RecordField where
  prt i e = case e of
    TRecordField mutable id type_ -> prPrec i 0 (concatD [prt 0 mutable, prt 0 id, doc (showString ":"), prt 0 type_])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print InfixOp where
  prt i e = case e of
    InfixOp1 infixsymbol -> prPrec i 0 (concatD [prt 0 infixsymbol])
    InfixOp2 infixsymbolother -> prPrec i 0 (concatD [prt 0 infixsymbolother])
    InfixOpSub -> prPrec i 0 (concatD [doc (showString "-")])
    InfixOpMul -> prPrec i 0 (concatD [doc (showString "*")])
    InfixOpAssign -> prPrec i 0 (concatD [doc (showString "<-")])

instance Print PrefixOp where
  prt i e = case e of
    PrefixOp1 prefixsymbol -> prPrec i 0 (concatD [prt 0 prefixsymbol])
    PrefixOp2 prefixsymbolother -> prPrec i 0 (concatD [prt 0 prefixsymbolother])
    PrefixOpMinus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print OperatorName where
  prt i e = case e of
    OpNamePrefix prefixsymbol -> prPrec i 0 (concatD [prt 0 prefixsymbol])
    OpNameInfix infixop -> prPrec i 0 (concatD [prt 0 infixop])

instance Print TupleConstr where
  prt i e = case e of
    T -> prPrec i 0 (concatD [doc (showString "T")])

instance Print Boolean where
  prt i e = case e of
    ETrue -> prPrec i 0 (concatD [doc (showString "true")])
    EFalse -> prPrec i 0 (concatD [doc (showString "false")])

instance Print Constant where
  prt i e = case e of
    EBool boolean -> prPrec i 0 (concatD [prt 0 boolean])
    EInt n -> prPrec i 0 (concatD [prt 0 n])
    EFloat d -> prPrec i 0 (concatD [prt 0 d])
    EChar c -> prPrec i 0 (concatD [prt 0 c])

instance {-# OVERLAPS #-} Print [Variable] where
  prt = prtList

instance Print Variable where
  prt i e = case e of
    VSimple simplevariable -> prPrec i 0 (concatD [prt 0 simplevariable])
    VOpTuple tupleconstr -> prPrec i 0 (concatD [prt 0 tupleconstr])
    VOp operatorname -> prPrec i 0 (concatD [doc (showString "("), prt 0 operatorname, doc (showString ")")])
    VConstructor name -> prPrec i 0 (concatD [prt 0 name])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance {-# OVERLAPS #-} Print [SimpleVariable] where
  prt = prtList

instance Print SimpleVariable where
  prt i e = case e of
    VVariable nname -> prPrec i 0 (concatD [prt 0 nname])
    VCanonical cname -> prPrec i 0 (concatD [prt 0 cname])
    VBlank -> prPrec i 0 (concatD [doc (showString "_")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance {-# OVERLAPS #-} Print [Ident] where
  prt = prtList

instance Print FileName where
  prt i e = case e of
    NFile str -> prPrec i 0 (concatD [prt 0 str])

instance Print ModuleName where
  prt i e = case e of
    SModuleName0 id -> prPrec i 0 (concatD [doc (showString "module"), prt 0 id])
    SModuleName1 id1 id2 -> prPrec i 0 (concatD [doc (showString "module"), prt 0 id1, doc (showString ":"), prt 0 id2])

instance {-# OVERLAPS #-} Print [ModuleContent] where
  prt = prtList

instance Print Matching where
  prt i e = case e of
    Matching1 expressions expression -> prPrec i 0 (concatD [prt 0 expressions, doc (showString "->"), prt 0 expression])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString "|"), prt 0 xs]

instance {-# OVERLAPS #-} Print [Matching] where
  prt = prtList

instance Print CanonicalName where
  prt i e = prPrec i 0 (concatD [prt 0 (fvPath e)])

instance Print ModuleDocsSignature where
  prt i (ModuleDocsSignature n t) = prPrec i 0 (concatD [prt 0 n, doc (showString " of type:"), prt 0 t])
  prtList i [x] = concatD [doc (showChar '\n'), prt i x]
  prtList i (x:xs) = concatD [prtList i [x], prtList i xs]

instance Print ModuleDocs where
  prt i e = case e of
    ModuleDocs name xs -> 
      prPrec i 0 (concatD [doc (showString "ModuleDocs of"), prt 0 name, prt 0 xs])

instance Print a => Print (DebugWrapper a) where
  prt i (Debug case_ msg x) = prPrec i 0 (concatD [
    doc $ showString $ "Debug " ++ show case_,
    doc $ showString $ "\n> context=" ++ msg,
    doc $ showString "\n> content=",
    prt 0 x])

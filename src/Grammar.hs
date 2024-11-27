
module Grammar where

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
newtype InfixSymbol = InfixSymbol String
  deriving (Eq, Ord, Show, Read)
newtype PrefixSymbol = PrefixSymbol String
  deriving (Eq, Ord, Show, Read)
newtype InfixSymbolOther = InfixSymbolOther String
  deriving (Eq, Ord, Show, Read)
newtype PrefixSymbolOther = PrefixSymbolOther String
  deriving (Eq, Ord, Show, Read)
data Prog = Prog1 [Statement]
  deriving (Eq, Ord, Show, Read)

type Ptr = Int

data Statement
    = SDirective FileName
    | SEnable String
    | SExpression Expression
    | SExpressionWithType Expression Type
    | SSignature Signature
    | STypedef Type TypedefDefinition
    | SModuleContent ModuleContent
    | SModuleSignature Ident [Signature]
    | SModuleDefinition ModuleName [ModuleContent]
  deriving (Eq, Ord, Show, Read)

data TypedefDefinition
    = TypedefConstructors [Constructor] | TypedefRecords [RecordField]
  deriving (Eq, Ord, Show, Read)

data Expression
    = EConst Constant
    | EString String
    | EVar Variable
    | EReferenceMemory Ptr
    | EOp1 PrefixOp Expression
    | EOp2 Expression InfixOp Expression
    | ERecord [LabeledStatement]
    | ERecordMemory [(Ident, Ptr)] -- TODO Use maps...
    | EIfThenElse Expression Expression Expression
    | EMatch [SimpleVariable] [Matching]
    | ETypeOf Expression
    | ELocalDefinition Definition Expression
    | EStack [SequencingExpression]
    | EFor Variable Expression ForIterationType Expression [SequencingExpression]
    | EWhile Expression [SequencingExpression]
    | EFunctionCall Variable [Expression]
    | EExternalFunctionCall String [Expression]
    | ETuple [Expression]
    | ENamedTuple NName [Expression]
    | ERecursiveFunctionCall Variable [Expression] Expression
    | ELambda FunctionPrefix Matching
    | ERaise ExceptionName Expression
    | ENull
  deriving (Eq, Ord, Show, Read)

data RecursivePrefix = RecNo | RecYes
  deriving (Eq, Ord, Show, Read)

data Definition
    = SValue RecursivePrefix [Variable] Expression
    | SExternal Variable Type String
  deriving (Eq, Ord, Show, Read)

data SequencingExpression
    = ESeq Expression | EAssign Variable Expression
  deriving (Eq, Ord, Show, Read)

data ForIterationType = ForTo | ForDownto
  deriving (Eq, Ord, Show, Read)

data LabeledStatement
    = SLabeled Variable Expression
    | SLabeledTyped Variable Type Expression
    | SLabeledBound Variable Int Expression
  deriving (Eq, Ord, Show, Read)

data FunctionPrefix = FunctionPrefix1 | FunctionPrefix2
  deriving (Eq, Ord, Show, Read)

data ExceptionName = NException Ident
  deriving (Eq, Ord, Show, Read)

data ModuleContent
    = SExceptionDefinition ExceptionName Type | SDefinition Definition
  deriving (Eq, Ord, Show, Read)

data Constructor
    = ConstructorEps Ident | ConstructorType Ident Type -- ConstructorEps?
  deriving (Eq, Ord, Show, Read)

data TypeConst = TBool | TInt | TChar | TFloat | TString | TUnit
  deriving (Eq, Ord, Show, Read)

data TypeConstr = TConstr Ident
  deriving (Eq, Ord, Show, Read)

data TypeParameters
    = TParameters2 Type Type | TParametersN Type TypeParameters
  deriving (Eq, Ord, Show, Read)

data Type
    = T1Const TypeConst
    | TIdent Ident
    | TAlias TypeConstr
    | TParameterized1 Type TypeConstr
    | TParameterizedN TypeParameters TypeConstr
    | TTuple [Type]
    | TFunction Type Type
  deriving (Eq, Ord, Show, Read)

data Signature = Signature1 Variable Type
  deriving (Eq, Ord, Show, Read)

data Mutable = MutableNo | MutableYes
  deriving (Eq, Ord, Show, Read)

data RecordField = TRecordField Mutable Ident Type
  deriving (Eq, Ord, Show, Read)

data InfixOp
    = InfixOp1 InfixSymbol
    | InfixOp2 InfixSymbolOther
    | InfixOpSub
    | InfixOpMul
    | InfixOpAssign
  deriving (Eq, Ord, Show, Read)

data PrefixOp
    = PrefixOp1 PrefixSymbol
    | PrefixOp2 PrefixSymbolOther
    | PrefixOpMinus
  deriving (Eq, Ord, Show, Read)

data OperatorName = OpNamePrefix PrefixSymbol | OpNameInfix InfixOp
  deriving (Eq, Ord, Show, Read)

data TupleConstr = T
  deriving (Eq, Ord, Show, Read)

data Boolean = ETrue | EFalse
  deriving (Eq, Ord, Show, Read)

data Constant
    = EBool Boolean | EInt Integer | EFloat Double | EChar Char
  deriving (Eq, Ord, Show, Read)

data Variable
    = VSimple SimpleVariable 
    | VOpTuple TupleConstr 
    | VOp OperatorName 
    | VConstructor NName 
  deriving (Eq, Ord, Show, Read)

data SimpleVariable = VVariable NName | VBlank | VCanonical CanonicalName
  deriving (Eq, Ord, Show, Read)

type NName = [Ident]

data FileName = NFile String
  deriving (Eq, Ord, Show, Read)

data ModuleName = SModuleName0 Ident | SModuleName1 Ident Ident
  deriving (Eq, Ord, Show, Read)

data Matching = Matching1 [Expression] Expression
  deriving (Eq, Ord, Show, Read)

-- Module.varname
type CanonicalIndex = (Ident, Ident)
data CanonicalName 
    = CanonicalName {
      cindex :: CanonicalIndex,
      version :: Int,
      recordidents :: NName
    } 
    | CanonicalModule Ident 
  deriving (Eq, Ord, Show, Read)

data ModuleDocsSignature = ModuleDocsSignature NName Type
  deriving Show

data ModuleDocs = ModuleDocs {
  name :: CanonicalName,
  elements :: [ModuleDocsSignature]
} deriving Show

-- Debugging
data DebugWrapper a = Debug {dbgcase :: Int, dbgmessage :: String, dbgdata :: a}

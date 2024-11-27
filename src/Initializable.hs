module Initializable where

import Data.Bimap (empty)
import qualified Data.Map as Map

import Context
import Dependencies
import Flags
import Grammar
import ParOcaml (myLexer, pProg)

-- Starting conditions
-- Types from Context.hs
class Initializable a where
  initial :: a

instance Initializable Memory where
  initial = Memory {
    _rootptr = 0,
    _nextptr = 1,
    _memory = Map.fromList [(0, ERecordMemory mempty)],
    _counters = mempty
  }

instance Initializable TypeHandler where
  initial = TypeHandler {
    _types = mempty,
    _typedconstructors = mempty,
    _typedfields = mempty,

    -- Unification
    _nextfresh = 0,
    _constraints = mempty,
    _variables = empty,
    _assignments = mempty
  }

instance Initializable ModuleHandler where
  initial = ModuleHandler {
    _signatures = mempty,
    _lastindex = mempty,
    _implements = mempty
  }

instance Initializable Context where
  initial = Context {
    _hglobals = initial :: Memory,
    _hversionmanager = mempty,
    _htypes = initial :: TypeHandler,
    _hmodules = initial :: ModuleHandler,
    _hflags = defaultFlags
  }

instance Initializable Scope where
  initial = Scope {
    _hlocals = mempty,
    _localversionmanager = mempty,
    _islocal = False,
    _isnext = False,
    _ismutable = False,
    _isprinting = True,
    _modulename = Nothing,
    _recursivefunctions = mempty
  }

-- Types from Dependencies.hs
instance Initializable DependencyGraph where
  initial = DependencyGraph {
    _gdependencies = mempty, 
    _gsource = mempty
  }

instance Initializable DependenciesState where
  initial = DependenciesState {
    _dsdependencygraph = initial :: DependencyGraph,
    _dsverbosity = 0,
    _dsparser = pProg,
    _dslexer = myLexer
  }

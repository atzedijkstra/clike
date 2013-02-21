-------------------------------------------------------------------------------------------
-- | State for running the preprocessor
-------------------------------------------------------------------------------------------

module Language.CLike.PreProcess.Types
  ( 
  -- * Preprocessing state
    PrePrState(..)
  , ppsEs, ppsErrs, ppsStack, ppsToks, ppsOpts
  , emptyPrePrState
  
  -- * Evaluation state
  , EvalState(..)
  , emptyEvalState
  , esEnv, esCache
  
  -- * Options
  , PrePrOpts(..)
  , ppoptsIncludePath
  , emptyPrePrOpts
  
  -- * Evaluation value
  , Val(..)

  -- * Evaluation cache (not yet used)
  , EvalCache
  
  -- * Internal only, continuation maintained in a stack
  , PrePrCont(..)
  , ppcontNext
  
  ) where

-------------------------------------------------------------------------------------------
import qualified Data.Map as Map
import           Data.Typeable
import           Data.Lens.Common
import           Data.Lens.Template
import qualified UHC.Util.FastSeq as Seq
-------------------------------------------------------------------------------------------
import           Language.CLike.AST
import qualified Language.CLike.PreProcess.Env as E
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
-- Options to the preprocessor
-------------------------------------------------------------------------------------------

data PrePrOpts = PrePrOpts
  { _ppoptsIncludePath		:: [FilePath] 	-- ^ search path for include, first of which is allways current dir
  }
  deriving (Typeable)

emptyPrePrOpts :: PrePrOpts
emptyPrePrOpts = PrePrOpts
  { _ppoptsIncludePath		= ["."]
  }

makeLens ''PrePrOpts

-------------------------------------------------------------------------------------------
-- State: evaluation
-------------------------------------------------------------------------------------------

-- | Evaluation value
data Val
  = Val_Int		Integer
  | Val_Str		LexString
  | Val_Undef				-- ^ could not compute

-- | Evaluation cache for names
type EvalCache = Map.Map E.EnvKey Val

emptyEvalCache :: EvalCache
emptyEvalCache = Map.empty

-- | Evaluation state
data EvalState = EvalState
  { _esEnv		:: E.Env
  , _esCache	:: EvalCache
  }
  deriving (Typeable)

emptyEvalState :: EvalState
emptyEvalState = EvalState E.empty emptyEvalCache

makeLens ''EvalState

-------------------------------------------------------------------------------------------
-- State: preprocessor
-------------------------------------------------------------------------------------------

-- | Continuation for if, include
data PrePrCont
  = PrePrCont_Next
      { _ppcontNext			:: [PrePr]		-- ^ next bunch to process
      }
  deriving (Typeable)

makeLens ''PrePrCont

-- | State threaded throughout preprocessing
data PrePrState = PrePrState
  { _ppsEs			:: EvalState		-- ^ eval state
  , _ppsErrs		:: Seq.Seq Err		-- ^ Accumulated errors
  , _ppsStack		:: [PrePrCont]		-- ^ stack of continuations
  , _ppsToks		:: Seq.Seq Token	-- ^ already gathered tokens
  , _ppsOpts		:: PrePrOpts		-- ^ options to preprocessor
  }
  deriving (Typeable)

-- | Empty/initial preprocessing state
emptyPrePrState :: PrePrState
emptyPrePrState = PrePrState
  { _ppsEs			= emptyEvalState
  , _ppsErrs		= Seq.empty
  , _ppsStack		= []
  , _ppsToks 		= Seq.empty
  , _ppsOpts		= emptyPrePrOpts
  }

makeLens ''PrePrState

-------------------------------------------------------------------------------------------
-- Preprocessor options parsing
-------------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------------
-- | #define'd Names environment for running the preprocessor
-------------------------------------------------------------------------------------------

module Language.CLike.PreProcess.Env
  ( NmInfo(..)
  , emptyNmInfo
  
  , EnvKey
  , Env
  
  , insert
  , lookup
  , empty
  ) where

-------------------------------------------------------------------------------------------
import           Prelude hiding(lookup)
-------------------------------------------------------------------------------------------
import           UHC.Util.ScopeMapGam
import           UHC.Util.VarMp			-- only for MetaLev etc, to be removed with uhc-util > 0.1.1
-------------------------------------------------------------------------------------------
import           Language.CLike.AST
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
-- Info about names (i.e. #define's)
-------------------------------------------------------------------------------------------

-- | Info to be remembered for a Name
data NmInfo = NmInfo
  { niToks			:: [Token]		-- ^ replacement tokens
  }

-- | Empty/initial NmInfo
emptyNmInfo :: NmInfo
emptyNmInfo = NmInfo
  { niToks			= []
  }

-------------------------------------------------------------------------------------------
-- Environment for names
-------------------------------------------------------------------------------------------

type EnvKey = LexString

-- | Environment
type Env = SGam EnvKey NmInfo

-- | Insert
insert n i g = sgamSingleton n i `sgamUnion` g

-- | Lookup
lookup n g = fmap head $ sgamLookupMetaLevDup metaLevVal n g

-- | Empty
empty = emptySGam

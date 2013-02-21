-------------------------------------------------------------------------------------------
-- | Preprocessor option parsing
-------------------------------------------------------------------------------------------

module Language.CLike.PreProcess.ParseOpts
  ( parsePrePrOpts
  
  -- * Re-exports
  -- , execParser
  ) where

-------------------------------------------------------------------------------------------
-- import qualified Data.Map as Map
-- import           Data.Typeable
import           Data.Lens.Common
-- import           Data.Lens.Template
-- import qualified UHC.Util.FastSeq as Seq
import           Options.Applicative
-------------------------------------------------------------------------------------------
import           Language.CLike.PreProcess.Types
-------------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------------
-- Preprocessor options parsing
-------------------------------------------------------------------------------------------

-- | Option parser, yielding an updater function for the options record
{-
parsePrePrOpts :: Parser (PrePrOpts -> PrePrOpts)
parsePrePrOpts
  =   (\o1 -> o1)
  <$> setBy ppoptsIncludePath (\d p -> (p++[d])) strOption
        (  short 'I'
        <> metavar "DIRECTORY"
        <> help "Directory for searching include files"
        )
  where setBy fld upd optkind opts = (\v -> fld ^%= upd v) <$> optkind opts
-}

parsePrePrOpts :: Parser (PrePrOpts -> PrePrOpts)
parsePrePrOpts
  =   (\o1 -> o1)
  <$> setBy ppoptsIncludePath (\md p -> maybe p (\d -> p++[d]) md) strOption
        (  short 'I'
        <> metavar "DIRECTORY"
        <> help "Directory for searching include files"
        )
  where setBy fld upd optkind opts = (\v -> fld ^%= upd v) <$> (optional $ optkind opts)

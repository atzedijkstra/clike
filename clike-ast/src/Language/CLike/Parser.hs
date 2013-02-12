-------------------------------------------------------------------------------------------
-- CLike parser
-------------------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.CLike.Parser
  ( pModule
  , run
  )
  where

import Control.Monad

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
-- import Text.ParserCombinators.UU.Demo.Examples as D

import Language.CLike.AST
import Language.CLike.Lexer

-------------------------------------------------------------------------------------------
-- Parser interface
-------------------------------------------------------------------------------------------

data Tokens = Tokens
  { toksInput		:: [Token]
  , toksErrs		:: [Error Pos]
  }

initTokens = Tokens [] []

instance Eof Tokens where
  eof = null . toksInput
  deleteAtEnd toks
    | eof toks  = Nothing
    | otherwise = Just (5, toks
                    { toksInput = tail $ toksInput toks
                    , toksErrs = toksErrs toks ++ [DeletedAtEnd (show (head $ toksInput toks))]
                    })

instance  StoresErrors Tokens (Error Pos) where
  getErrors toks = (toksErrs toks, toks {toksErrs = []})

instance  HasPosition Tokens Pos where
  getPos toks | eof toks  = noPos
              | otherwise = tokPos $ head $ toksInput toks

{-
instance  StoresErrors (Str a s loc) (Error loc) where
       getErrors   (Str  inp      msgs pos ok    )     = (msgs, Str inp [] pos ok)

instance  HasPosition (Str a s loc) loc where
       getPos   (Str  inp      msgs pos ok    )        = pos
-}

type CP a = P Tokens a

{-
type PlainParser tok gp = IsParser p tok => p gp
type P p = PlainParser Token p

parseToResMsgs, parseBIF :: (Symbol s,InputState inp s pos) => AnaParser inp Pair s pos a -> inp -> (a,[Message s pos])
parseToResMsgs p inp
  = toResMsgs (parse p inp)
  where toResMsgs steps
          = (r,getMsgs steps)
          where (Pair r _) = evalSteps steps

parseBIF = parseToResMsgs
-}

-- | Rudimentary running
run :: String -> IO ()
run inp = do
  case scanner ( -- scfgAddOpts [ScOpt_CommentAs1Token] $
                 scannerConfigCXX
               ) 0 inp of
    Left  m -> putStrLn m
    Right t -> do let r@(_, errors) = parse ((,) <$> pModule <*> pEnd) (initTokens {toksInput = t})
                  forM_ errors (putStrLn . show)

-------------------------------------------------------------------------------------------
-- Scanning
-------------------------------------------------------------------------------------------

{-
scanBIF :: FilePath -> IO [Token]
scanBIF fn
  = do txt <- readFile fn
       return $ scan keywordstxt keywordsops specchars opchars (initPos fn) txt
  where keywordstxt = [ "type", "discrete", "table", "hs", "var", "prob", "_", "__"
                      -- pattern names
                      , "noisy_or"
                      ]
        keywordsops = ["=", "|", "->", ":", "::", "."]
        specchars   = "{};()"
        opchars     = "=|->:."
-}
-------------------------------------------------------------------------------------------
-- Utility parsers
-------------------------------------------------------------------------------------------

-- | Make a semicolon terminated parser
{-
pMkSemi :: forall a. P a -> P a
pMkSemi p = p <* pSemi
-}
-------------------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------------------

pModule :: CP Module
pModule
  = Module_Mod <$> pure []
{-
  where 
    -- top level decls
    pDeclGroup :: P DeclGroup
    pDeclGroup = DeclGroup_Bn noPos <$> pList1 pBnDecl

    pBnDecl :: P BnDecl
    pBnDecl
      =   BnDecl_Var <$> pKeyPos "var" <*> pBnNm
          <*> pMkSemi
                (   pKey ":"
                      *> (   (:[]) <$> pBnVarAspect
                         <|> pCurly (pList (pMkSemi pBnVarAspect))
                         )
                <|> pKeyPos "::"
                      <**> ((\t p -> [BnVarAspect_Ty p t]) <$> pTyExpr)
                <|> pKeyPos "="
                      <**> ((\t p -> [BnVarAspect_CnProb p t]) <$> pBnCnPrExpr)
                )
      <|> BnDecl_Ty <$> pKeyPos "type" <*> pBnNm <* pKey "=" <*> pMkSemi pTyExpr
      <|> (\p n t arg res (a,r) -> BnDecl_Table p n t arg res a r)
          <$> pKeyPos "table"
          <*> pBnNm <* pKey ":"
          <*> pTyExpr <* pKey "="
          <*> pList pBnNm  <* pKey "->"
          <*> pBnNm <*> pCurly (pList1 pTblArgRow <+> (pKey "->" *> pList1 pTblResRow))
    
    -- var aspect
    pBnVarAspect :: P BnVarAspect
    pBnVarAspect
      =   BnVarAspect_Ty <$> pKeyPos "type" <*> pTyExpr
      <|> BnVarAspect_CnProb <$> pKeyPos "prob" <*> pBnCnPrExpr

    -- table
    pTblArgRow :: P TblArgRow
    pTblArgRow
      =   uncurry TblArgRow_Row <$> pBnNm' <* pKey "=" <*> pList1Sep (pKey "|") pTblArgEntry <* pSemi

    pTblArgEntry :: P TblArgEntry
    pTblArgEntry
      =   uncurry TblArgEntry_Nm <$> pBnNm'

    pTblResRow :: P TblResRow
    pTblResRow
      =   uncurry TblResRow_Row <$> pBnNm' <* pKey "=" <*> pList1Sep (pKey "|") pTblResEntry <* pSemi

    pTblResEntry :: P TblResEntry
    pTblResEntry
      =   TblResEntry_Prob noPos <$> pProbExpr

    -- conditional probability expr
    pBnCnPrExpr :: P BnCnPrExpr
    pBnCnPrExpr
      =   (\(p,n) ns e -> BnCnPrExpr_PrExpr p (n:ns) e) <$> pBnNm' <*> pList pBnNm <* pKey "->" <*> pBnPrExpr
      <|> (\e -> BnCnPrExpr_PrExpr noPos [] e) <$> pBnPrExpr

    pBnPrExpr :: P BnPrExpr
    pBnPrExpr
      =   BnPrExpr_Table <$> pKeyPos "table" <*> pBnPrTable
      <|> BnPrExpr_NoisyOr <$> pKeyPos "noisy_or" <*> pBnPrTable
    
    pBnPrTable :: P BnPrTable
    pBnPrTable = BnPrTable_Probs noPos <$> pList1Sep (pKey "|") pTblResEntry

    -- probability expr
    pProbExpr :: P ProbExpr
    pProbExpr
      =   (\(_,p) s2 -> ProbExpr_Prob p (Prob (read s2) (length s2))) <$> pInteger10Pos <* pKey "." <*> pInteger10
      <|> ProbExpr_Infer1    <$> pKeyPos "_"
      <|> ProbExpr_InferRest <$> pKeyPos "__"

    -- name
    pNm'' :: ((Pos,Nm) -> res) -> P (String,Pos) -> P res
    pNm'' mk pN = (\(s,p) -> mk (p,mkNm s)) <$> pN

    pHsNm' :: P (Pos,Nm)
    pHsNm' = pNm'' id pConidPos

    pBnNm' :: P (Pos,Nm)
    pBnNm' = pNm'' id (pVaridPos <|> pConidPos)

    pBnNm :: P Nm
    pBnNm = snd <$> pBnNm'

    -- type
    pTyExpr :: P TyExpr
    pTyExpr
      =   TyExpr_Bn noPos <$> pBnTyExpr
      <|> TyExpr_Hs <$> pKeyPos "hs" <*> pCurly pHsTyExprBase
    
    pHsTyExprBase :: P HsTyExpr
    pHsTyExprBase
      =   uncurry HsTyExpr_Con <$> pHsNm'

    pBnTyExprBase :: P BnTyExpr
    pBnTyExprBase
      =   uncurry BnTyExpr_Con <$> pBnNm'
      <|> BnTyExpr_Discrete <$> pKeyPos "discrete" <*> pCurly (pList pBnNm)
    
    pBnTyExpr :: P BnTyExpr
    pBnTyExpr
      = pBnTyExprBase
        <**> (   pSucceed id
             <|> (\as r a -> mkf (a:as) r) <$> pList pBnTyExprBase <* pKey "->" <*> pBnTyExprBase
             )
      where mkf a r   = BnTyExpr_Fun noPos a r
    
    -- tokens
-}

-------------------------------------------------------------------------------------------
-- CLike parser
-- See http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2012/n3485.pdf
--     http://stackoverflow.com/questions/14184203/changes-between-c-standard-working-drafts
-------------------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.CLike.Parser
  ( pModule
  , pPrePrL

  , parseModule

  , parseExpr
  
  , parsePrePr
  , parsePrePrFile
  , parsePrePrFileAndReport
  )
  where

-------------------------------------------------------------------------------------------
import           Control.Monad
import           System.IO
import           Data.Maybe
import qualified Data.Set as Set

import           Text.ParserCombinators.UU
import           Text.ParserCombinators.UU.BasicInstances
-- import           Text.ParserCombinators.UU.Utils
-- import           Text.ParserCombinators.UU.Demo.Examples as D
-------------------------------------------------------------------------------------------
import           Language.CLike.AST
import           Language.CLike.Lexer
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
-- Parser interface
-------------------------------------------------------------------------------------------

{-
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
-}

instance IsLocationUpdatedBy Pos Token where
  advance _ t = tokPos t

type Tokens = Str Token [Token] Pos

type CP a = P Tokens a

-- | Rudimentary running
parseModule :: String -> IO ()
parseModule inp = do
  case scanner ( -- scfgAddOpts [ScOpt_CommentAs1Token] $
                 scannerConfigCXX
               ) 0 inp of
    Left  m -> putStrLn m
    Right t -> do let r@(_, errors)
                        = parse ((,) <$> pModule <*> pEnd) (createStr noPos t)
                  forM_ errors (putStrLn . show)

-- Run preprocessor parser on string
parsePrePr :: String -> Either [String] [PrePr]
parsePrePr inp = do
  case scanner ( -- scfgAddOpts [ScOpt_CommentAs1Token] $
                 scannerConfigCXX
               ) 0 inp of
    Left  m -> Left [m]
    Right t -> case parse ((,) <$> pPrePrL <*> pEnd) (createStr noPos t) of
                 (m, []) -> Right m
                 (_, es) -> Left $ map show es

-- Run preprocessor parser on file
parsePrePrFile :: FilePath -> IO (Either [String] [PrePr])
parsePrePrFile f = do
  h <- openFile f ReadMode
  s <- hGetContents h
  return $ parsePrePr s

-- Run preprocessor parser on file
parsePrePrFileAndReport :: FilePath -> IO [PrePr]
parsePrePrFileAndReport f = do
  res <- parsePrePrFile f
  case res of
    Left msgs -> do forM_ msgs putStrLn
                    return []
    Right r   -> return r

-- | Run Expr parser through Tokens
parseExpr :: [Token] -> Either String Expr
parseExpr t | null errors = Right e
            | otherwise   = Left $ unlines $ map show errors
  where r@(e, errors) = parse ((,) <$> pExpr <*> pEnd) (createStr noPos t)


-------------------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------------------

-- | Token parser, the raw token itself (mainly intended to be reprocessed after preprocessing)
pTkRaw :: TokenKind -> CP Token
pTkRaw tk = pSym (mkToken tk)

-- | Token parser, extraction of relevant info for further processing
pTk :: TokenKind -> CP PosString
pTk tk = (\t -> (tokPos t, tokPayload t)) <$> pTkRaw tk

-- | Token parser, only position info
pTkP :: TokenKind -> CP Pos
pTkP tk = fst <$> pTk tk

-- | Token parser, Op specific
pTkO :: TokenKind -> CP (Pos,Op)
pTkO tk = (\t -> (tokPos t, tokKind t)) <$> pTkRaw tk

-- | Token parser, only string info
pTkS :: TokenKind -> CP LexString
pTkS tk = snd <$> pTk tk

-- | Set of tokens, returning the raw token
pSatToksRaw :: [TokenKind] -> CP Token
pSatToksRaw [t] = pTkRaw t
pSatToksRaw tl  = pSatisfy (\t -> Set.member (tokKind t) ts) ins
  where ts  = Set.fromList tl
        ins = Insertion "pSatToksRaw" (mkToken (head tl)) 5

-- | Set of tokens, Op specific
pSatToksO :: [TokenKind] -> CP (Pos,Op)
pSatToksO tl = (\t -> (tokPos t, tokKind t)) <$> pSatToksRaw tl

-- | Set of tokens, pos + string
pSatToks :: [TokenKind] -> CP PosString
pSatToks tl = (\t -> (tokPos t, tokPayload t)) <$> pSatToksRaw tl

-------------------------------------------------------------------------------------------
-- Utility parsers
-------------------------------------------------------------------------------------------

-- | Make a semicolon terminated parser
{-
pMkSemi :: forall a. P a -> P a
pMkSemi p = p <* pSemi
-}

-- | Variation of pPacked which allows use of left pack result
pPacked' :: IsParser p => p b1 -> p b2 -> p (b1->a) -> p a
pPacked' l r x = flip ($) <$> l <*>  x <* r

pParens  = pPacked  (pTk  C_oparen) (pTk C_cparen)
pParens' = pPacked' (pTkP C_oparen) (pTk C_cparen)

pAngles  = pPacked  (pTk  C_oangle) (pTk C_cangle)

pBracks  = pPacked  (pTk  C_obrack) (pTk C_cbrack)

pCurlys  = pPacked  (pTk  C_ocurly) (pTk C_ccurly)
pCurlys' = pPacked' (pTkP C_ocurly) (pTk C_ccurly)

pComma = pTk C_comma

-- | Convenience for tupling
p1 <+> p2 = (,) <$> p1 <*> p2

-- | Make infix op parser, also providing the chaining variant
mkInfP' :: (CP (Expr -> Expr -> Expr) -> CP Expr -> CP Expr) -> [TokenKind] -> CP Expr -> CP Expr
mkInfP' chn tks p = chn (uncurry Expr_OpInfix <$> pSatToksO tks) p

-- | Make infix op parser, defaulting to right associative
mkInfP :: [TokenKind] -> CP Expr -> CP Expr
mkInfP = mkInfP' pChainr

-- | Something optional, yielding only its presence
pOpt :: CP x -> CP Bool
pOpt p = isJust <$> pMaybe p

p3Dot :: CP Pos
p3Dot = pTkP C_3dot

pOpt3Dot :: CP Bool
pOpt3Dot = pOpt p3Dot

pOptComma3Dot :: CP Bool
pOptComma3Dot = pOpt (pComma *> p3Dot)

pOptComma :: CP Bool
pOptComma = pOpt pComma

-------------------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------------------

-- | Fill in a noPos where otherwise a Pos would be expected
unPos :: (Pos -> r) -> r
unPos f = f noPos

type PosString = (Pos,String)
noPosString = (noPos,"")

posStringConcat :: PosString -> PosString -> PosString
posStringConcat (p1,s1) (_,s2) = (p1,s1++s2)

-------------------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------------------

pModule :: CP Module
pPrePrL :: CP [PrePr]
pExpr :: CP Expr
(pModule,pPrePrL,pExpr)
  = ( Module_Mod <$> pure []
    , pPrePrL
    , pExpr
    )
  where 
    -- top level decls

    -- literal
    pLit :: CP Lit
    pLit
      =   uncurry Lit_Int    <$> pTk C_lit_int
      <|> uncurry Lit_Char   <$> pTk C_lit_char
      <|> uncurry Lit_Float  <$> pTk C_lit_float
      <|> uncurry Lit_String <$> pTk C_lit_string

    -- name
    pIdTok :: CP (Pos,LexString)
    pIdTok
      =   pTk C_name

    pIdBase :: CP Name
    pIdBase
      =   uncurry Name_Id <$> pIdTok

    pIdUnqualifiedBase :: CP Name
    pIdUnqualifiedBase
      =   pIdBase
      <|> pIdTemplate
      <|> pIdOperatorFunction

    pIdUnqualified :: CP Name
    pIdUnqualified
      =   pIdUnqualifiedBase
      <|> Name_Tilde <$> pTkP C_tilde <*> (pIdClass <|> pIdDeclType)

    pIdQualified :: CP Name
    pIdQualified
      =   Name_Qual <$> pTkP C_2colon <*> pIdUnqualifiedBase
      <|> unPos Name_QualNest <$> pIdNestedNameSpecifier <* pMaybe (pTk C_template) <*> pIdUnqualified

    pIdNestedNameSpecifier :: CP [Name]
    pIdNestedNameSpecifier
      = (\ns n -> ns ++ [n]) <$> pList (pIdNestedNameSpecifierPrefix <* pTk C_2colon) <*> (pIdNestedNameSpecifierLast <* pTk C_2colon)
      where pIdNestedNameSpecifierLast
              =   pIdBase
              <|> pMaybe (pTk C_template) *> pIdTemplateSimple
            pIdNestedNameSpecifierPrefix
              =   pMaybe (pTk C_2colon) *> pIdBase
              <|> pIdDeclType

    pIdOperatorFunction :: CP Name
    pIdOperatorFunction
      = pTk C_operator *> pIdOperator

    pIdDeclType :: CP Name
    pIdDeclType
      = Name_DeclType <$> pTkP C_decltype <*> pParens pExpr

    pIdOperator :: CP Name
    pIdOperator
      = uncurry Name_Op
        <$> (   pSatToks tokkindAllOp
            <|> pSatToks [C_star, C_tilde, C_oangle, C_cangle, C_arrow]
            <|> posStringConcat <$> pTk C_oparen <*> pTk C_cparen
            <|> pOCBrack
            <|> posStringConcat <$> pSatToks [C_new, C_delete] <*> (pOCBrack `opt` noPosString)
            )
      where pOCBrack = posStringConcat <$> pTk C_obrack <*> pTk C_cbrack

    pIdClass :: CP Name
    pIdClass
      =   pIdBase

    pIdTypeName :: CP Name
    pIdTypeName
      =   pIdBase				-- not further split up into class name, enum name etc
      <|> pIdTemplateSimple

    pIdTemplateSimple :: CP Name
    pIdTemplateSimple
      =   (\n (a,o) -> unPos Name_Templ n a o) <$> pIdBase <*> pAngles pTemplArgs
      where pTemplArg :: CP Name
            pTemplArg = pIdExpr

            pTemplArgs :: CP ([Name],Bool)
            pTemplArgs = pListSep pComma pTemplArg <+> pOpt3Dot

    pIdTemplate :: CP Name
    pIdTemplate
      =   pIdTemplateSimple

    pIdExpr :: CP Name
    pIdExpr
      =   pIdUnqualified
      <|> pIdQualified

    -- expr
    pExprPrimary :: CP Expr
    pExprPrimary
      =         Expr_This <$> pTkP C_this
      <|> unPos Expr_Lit  <$> pLit
      <|> unPos Expr_Name <$> pIdExpr
      <|> pParens pExpr

    pExprPostfix :: CP Expr
    pExprPostfix
      =   pExprPrimary
          <**> ((\ss e -> foldl (flip ($)) e ss) <$> pList pSuffix)
      -- <|> ...
      where pSuffix
              =   (\i e -> unPos Expr_Index e i) <$> pBracks pExpr
              <|> (\a e -> unPos Expr_Call e a) <$> pParens' pExprInitializerList

    pExprUnary :: CP Expr
    pExprUnary
      =   pExprPostfix
      -- <|> ...

    pExprCast :: CP Expr
    pExprCast
      =   pExprUnary
      -- <|> ...

    pExprAssign :: CP Expr
    pExprAssign
      =   foldr (uncurry mkInfP') pExprCast
             [ (pChainr, [C_op_log_or ])
             , (pChainr, [C_op_log_and])
             , (pChainl, [C_op_or     ])
             , (pChainl, [C_op_xor    ])
             , (pChainl, [C_op_and    ])
             , (pChainl, [C_op_eq, C_op_neq])
             , (pChainl, [C_op_ge, C_op_le] ++ [C_oangle, C_cangle])
             , (pChainl, [C_op_shl, C_op_shr])
             , (pChainl, [C_op_add, C_op_sub])
             , (pChainl, [C_op_div, C_op_mod] ++ [C_star])
             , (pChainl, [C_op_pm, C_op_ppm])
             ]
          <**> (   pure id
               <|> (\t e c -> unPos Expr_IfThenElse c t e) <$ pTk C_quest <*> pExpr <* pTk C_colon <*> pExprAssign
               <|> (\(p,o) r l -> Expr_OpInfix p o l r)
                   <$> pSatToksO [C_op_assign, C_op_assign_add, C_op_assign_sub, C_op_assign_mul, C_op_assign_div, C_op_assign_mod, C_op_assign_xor, C_op_assign_and, C_op_assign_or, C_op_assign_shl, C_op_assign_shr]
                   <*> pExprInitializerClause
               )

    pExprInitializerList :: CP (Pos -> Expr)
    pExprInitializerList
      =   (\l d p -> Expr_Init p l d) <$> pList1Sep pComma pExprInitializerClause <*> pOpt3Dot

    pExprInitializerClause :: CP Expr
    pExprInitializerClause
      =   pExprAssign
      <|> pCurlys'
            (   pExprInitializerList <* pOptComma
            <|> pure (\p -> Expr_Init p [] False)
            )

    pExpr :: CP Expr
    pExpr
      = mk <$> pList1Sep pComma pExprAssign
      where mk [e] = e
            mk es  = Expr_Exprs noPos es

    -- preprocessing
    pPrePrL :: CP PrePrL
    pPrePrL = pList_ng pPrePr
    
    pPrePr :: CP PrePr
    pPrePr
      =   unPos PrePr_Toks <$> pList1 pPrePrTok
      <|> pCtrlLine
            (   PrePr_Undef <$> pTkP C_hash_undef <*> pIdBase
            <|> (\p n (as,cm) ts -> PrePr_Define p n as cm ts)
                <$> pTkP C_hash_define <*> pIdBase
                <*> (maybe ([],False) id <$> (pMaybe (pParens (pListSep pComma pIdBase <+> pOptComma3Dot))))
                <*> pList pPrePrTok
            <|> uncurry PrePr_Include <$ pTkP C_hash_include <*> pTk C_lit_string
            )
      <|> (\((p,e),ths) elifs mbelse
            -> mkIf p e ths $
               foldr (\((p,e),ths) els -> [mkIf p e ths els])
                     (maybe [] snd mbelse)
                     elifs
          )
          <$> pIf <*> pList_ng pElif <*> pMaybe pElse <* pEndif
      where --
            pIf    = pCtrlLine
                       (   pTkP C_hash_if     <+> pPrePrExpr
                       <|> pTkP C_hash_ifdef  <+> (unPos PrePrExpr_IsDefined    <$> pIdBase)
                       <|> pTkP C_hash_ifndef <+> (unPos PrePrExpr_IsNotDefined <$> pIdBase)
                       )
                     <+> pPrePrL
            pElif  = pCtrlLine (pTkP C_hash_elif <+> pPrePrExpr) <+> pPrePrL
            pElse  = pCtrlLine (pTkP C_hash_else               ) <+> pPrePrL
            pEndif = pCtrlLine (pTkP C_hash_endif)
            --
            pPrePrTok = pSatToksRaw tokkindAllPrePr
            pCtrlLine = pPacked (pTk C_hash_bol) (pTk C_hash_eol)
            pPrePrExpr = unPos PrePrExpr_Expr <$> pExpr
            mkIf p c t e = PrePr_IfThenElse p c t e



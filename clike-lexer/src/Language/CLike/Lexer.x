{
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wwarn -w #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Language.CLike.Lexer
  ( scanner
  , Token(..)
  ) where
}

%wrapper "monadUserState"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
$white+ ;
"--".* ;

let { rt Let }
in { rt In }
$digit+ { rs (Int . read) }
[\=\+\-\*\/\(\)] { rs (Sym . head) }
$alpha [$alpha $digit \_ \']* { rs Var }

{
-- The token variations:
data TokenKind =
    Let
  | In
  | Sym Char
  | Var String
  | Int Integer
  | EOF
  deriving (Eq,Show)

-- The token type:
data Token = Token AlexPosn TokenKind (Maybe String)
  deriving (Eq,Show)

data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex Token
alexEOF = return (Token undefined EOF Nothing)

rt :: TokenKind -> AlexAction Token
rt t _ _ = return (Token undefined t Nothing)

rs :: (String -> TokenKind) -> AlexAction Token
rs t (p, _, _, input) len = return (Token p (t s) (Just s))
  where s = take len input

-- type signatures

-- Execution

scanner :: String -> Either String [Token]
scanner str = let loop = do -- (t, m) <- alexComplementError alexMonadScan
                            t <- alexMonadScan
                            -- when (isJust m) (lexerError (fromJust m))
                            let tok@(Token _ knd _) = t
                            if (knd == EOF)
                               then return []
                               {-
                               then do f1 <- getLexerStringState
                                       d2 <- getLexerCommentDepth
                                       if ((not f1) && (d2 == 0))
                                          then return [tok]
                                          else if (f1)
                                               then alexError "String not closed at end of file"
                                               else alexError "Comment not closed at end of file"
                               -}
                               else do toks <- loop
                                       return (tok : toks)
              in  runAlex str loop


}

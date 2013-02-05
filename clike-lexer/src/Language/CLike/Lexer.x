{
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wwarn -w #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

------------------------------------------------------------------------------------------------
module Language.CLike.Lexer
  ( scanner
  , Token(..)
  
  , ScannerOpt(..)
  
  , ScannerConfig(..), initScannerConfig
  , scannerConfigC, scannerConfigCXX
  , scfgAddOpts
  ) where
------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------
import Data.Bits
-- import Data.Word
-- import Control.Monad.Trans.State.Strict
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

}

%wrapper "monadUserState"

$alpha = [a-zA-Z] -- alphabetic characters

$whitechar   = [$white \xa0] -- \xa0 is Unicode no-break space
$white_no_nl = $whitechar # \n
$printable_no_nl = $printable # \n

$ascdigit  = 0-9
-- $unidigit  = \x01 -- Trick Alex into handling Unicode. See alexGetChar.
$digit     = [$ascdigit] -- $unidigit]
$octit	   = 0-7
$hexit     = [$digit A-F a-f]

-- $unilarge  = \x03 -- Trick Alex into handling Unicode. See alexGetChar.
$asclarge  = [A-Z \xc0-\xd6 \xd8-\xde]
$large     = [$asclarge] -- $unilarge]

-- $unismall  = \x04 -- Trick Alex into handling Unicode. See alexGetChar.
$ascsmall  = [a-z \xdf-\xf6 \xf8-\xff]
$small     = [$ascsmall \_] -- $unismall]

$namebegin = [$large $small \. \$ \@]
$namechar  = [$namebegin $digit]

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent

@escape      = \\ ([abfnrt\\\'\"\?] | x $hexit{1,2} | $octit{1,3})
@strchar     = ($printable # [\"\\]) | @escape

clike :-

-- comment
"//" [$printable_no_nl]* / {ifScannerOpt ScOpt_CommentAs1Token} {rs C_Comment}
"//" [$printable_no_nl]* ;

-- keywords
<0> {
	alignas 			/ {ifLangs langsCXX}		{rt C_alignas }
	alignof 			/ {ifLangs langsCXX}		{rt C_alignof }
	and					/ {ifLangs langsCXX}		{rt C_and}
	and_eq				/ {ifLangs langsCXX}		{rt C_and_eq}
	asm					/ {ifLangs langsCXX}		{rt C_asm}
	auto											{rt C_auto}
	bitand				/ {ifLangs langsCXX}		{rt C_bitand}
	bitor				/ {ifLangs langsCXX}		{rt C_bitor}
	bool				/ {ifLangs langsCXX}		{rt C_bool}
	break											{rt C_break}
	case											{rt C_case}
	catch				/ {ifLangs langsCXX}		{rt C_catch}
	char											{rt C_char}
	char16_t			/ {ifLangs langsCXX}		{rt C_char16_t}
	char32_t			/ {ifLangs langsCXX}		{rt C_char32_t}
	class				/ {ifLangs langsCXX}		{rt C_class}
	compl				/ {ifLangs langsCXX}		{rt C_compl}
	const											{rt C_const}
	const_cast			/ {ifLangs langsCXX}		{rt C_const_cast}
	constexpr			/ {ifLangs langsCXX}		{rt C_constexpr}
	continue										{rt C_continue}
	decltype			/ {ifLangs langsCXX}		{rt C_decltype}
	default											{rt C_default}
	delete				/ {ifLangs langsCXX}		{rt C_delete}
	do												{rt C_do}
	double											{rt C_double}
	dynamic_cast		/ {ifLangs langsCXX}		{rt C_dynamic_cast}
	else											{rt C_else}
	enum											{rt C_enum}
	explicit			/ {ifLangs langsCXX}		{rt C_explicit}
	export				/ {ifLangs langsCXX}		{rt C_export}
	extern											{rt C_extern}
	false				/ {ifLangs langsCXX}		{rt C_false}
	float											{rt C_float}
	for												{rt C_for}
	friend				/ {ifLangs langsCXX}		{rt C_friend}
	goto											{rt C_goto}
	if												{rt C_if}
	inline											{rt C_inline}
	int												{rt C_int}
	long											{rt C_long}
	mutable				/ {ifLangs langsCXX}		{rt C_mutable}
	namespace			/ {ifLangs langsCXX}		{rt C_namespace}
	new					/ {ifLangs langsCXX}		{rt C_new}
	noexcept			/ {ifLangs langsCXX}		{rt C_noexcept}
	not					/ {ifLangs langsCXX}		{rt C_not}
	not_eq				/ {ifLangs langsCXX}		{rt C_not_eq}
	nullptr 			/ {ifLangs langsCXX}		{rt C_nullptr }
	operator			/ {ifLangs langsCXX}		{rt C_operator}
	or					/ {ifLangs langsCXX}		{rt C_or}
	or_eq				/ {ifLangs langsCXX}		{rt C_or_eq}
	private				/ {ifLangs langsCXX}		{rt C_private}
	protected			/ {ifLangs langsCXX}		{rt C_protected}
	public				/ {ifLangs langsCXX}		{rt C_public}
	register										{rt C_register}
	reinterpret_cast	/ {ifLangs langsCXX}		{rt C_reinterpret_cast}
	restrict										{rt C_restrict}
	return											{rt C_return}
	short											{rt C_short}
	signed											{rt C_signed}
	sizeof											{rt C_sizeof}
	static											{rt C_static}
	static_assert		/ {ifLangs langsCXX}		{rt C_static_assert}
	static_cast			/ {ifLangs langsCXX}		{rt C_static_cast}
	struct											{rt C_struct}
	switch											{rt C_switch}
	template			/ {ifLangs langsCXX}		{rt C_template}
	this				/ {ifLangs langsCXX}		{rt C_this}
	thread_local		/ {ifLangs langsCXX}		{rt C_thread_local}
	throw				/ {ifLangs langsCXX}		{rt C_throw}
	true				/ {ifLangs langsCXX}		{rt C_true}
	try					/ {ifLangs langsCXX}		{rt C_try}
	typedef											{rt C_typedef}
	typeid				/ {ifLangs langsCXX}		{rt C_typeid}
	typename			/ {ifLangs langsCXX}		{rt C_typename}
	union											{rt C_union}
	unsigned										{rt C_unsigned}
	using				/ {ifLangs langsCXX}		{rt C_using}
	virtual				/ {ifLangs langsCXX}		{rt C_virtual}
	void											{rt C_void}
	volatile										{rt C_volatile}
	wchar_t				/ {ifLangs langsCXX}		{rt C_wchar_t}
	while											{rt C_while}
	xor					/ {ifLangs langsCXX}		{rt C_xor}
	xor_eq				/ {ifLangs langsCXX}		{rt C_xor_eq}
}

$whitechar+ ;

$digit+ { rs (Int . read) }
[\=\+\-\*\/\(\)] { rs (Sym . head) }
$alpha [$alpha $digit \_ \']* { rs Var }

-- fallback, unrecognizable
-- $printable_no_nl+ {rs C_Unknown}


{
------------------------------------------------------------------------------------------------
-- The token variations
------------------------------------------------------------------------------------------------

data TokenKind =
    Sym Char
  | Var String
  | Int Integer

  -- Preprocessor

  -- Reserved keywords shared by all clike variants: C, C++
  | C_auto
  | C_break
  | C_case
  | C_char
  | C_const
  | C_continue
  | C_default
  | C_do
  | C_double
  | C_else
  | C_enum
  | C_extern
  | C_float
  | C_for
  | C_goto
  | C_if
  | C_inline
  | C_int
  | C_long
  | C_register
  | C_restrict
  | C_return
  | C_short
  | C_signed
  | C_sizeof
  | C_static
  | C_struct
  | C_switch
  | C_typedef
  | C_union
  | C_unsigned
  | C_void
  | C_volatile
  | C_while

  -- Reserved keywords on top of shared, for: C++
  | C_alignas 
  | C_alignof 
  | C_and
  | C_and_eq
  | C_asm
  | C_bitand
  | C_bitor
  | C_bool
  | C_catch
  | C_char16_t
  | C_char32_t
  | C_class
  | C_compl
  | C_const_cast
  | C_constexpr
  | C_decltype
  | C_delete
  | C_dynamic_cast
  | C_explicit
  | C_export
  | C_false
  | C_friend
  | C_mutable
  | C_namespace
  | C_new
  | C_noexcept
  | C_not
  | C_not_eq
  | C_nullptr 
  | C_operator
  | C_or
  | C_or_eq
  | C_private
  | C_protected
  | C_public
  | C_reinterpret_cast
  | C_static_assert
  | C_static_cast
  | C_template
  | C_this
  | C_thread_local
  | C_throw
  | C_true
  | C_try
  | C_typeid
  | C_typename
  | C_using
  | C_virtual
  | C_wchar_t
  | C_xor
  | C_xor_eq

  -- Ident
  | C_Name	   	String
  
  -- Literal
  | C_String	String
  | C_Int	   	Integer
  | C_Float     Rational
  
  -- Comment
  | C_Comment	String		-- comment including delimiter(s)
  
  -- Meta
  | C_EOF
  | C_Unknown	String		-- unrecognizable
  deriving (Eq,Show)

------------------------------------------------------------------------------------------------
-- Token position
------------------------------------------------------------------------------------------------

alexNoPos :: AlexPosn
alexNoPos = AlexPn (-1) (-1) (-1)

------------------------------------------------------------------------------------------------
-- The token type:
------------------------------------------------------------------------------------------------

data Token = Token AlexPosn TokenKind (Maybe String)
  deriving (Eq,Show)

------------------------------------------------------------------------------------------------
-- Hooks used by machinery
------------------------------------------------------------------------------------------------

alexEOF :: Alex Token
alexEOF = return (Token alexNoPos C_EOF Nothing)

------------------------------------------------------------------------------------------------
-- Result combinators
------------------------------------------------------------------------------------------------

rt :: TokenKind -> AlexAction Token
rt t (p, _, _, _) _ = return (Token p t Nothing)

rs :: (String -> TokenKind) -> AlexAction Token
rs t (p, _, _, input) len = return (Token p (t s) (Just s))
  where s = take len input

------------------------------------------------------------------------------------------------
-- User level onfiguration
------------------------------------------------------------------------------------------------

data ScannerLanguage
  = ScLang_C				-- C
  | ScLang_CXX				-- C++
  | ScLang_CLike			-- Intersection of them all
  deriving (Eq,Enum,Bounded)

data ScannerOpt
  = ScOpt_Preprocessing		-- do CPP preprocessing
  | ScOpt_CommentAs1Token	-- pass comment through as single token
  deriving (Enum,Bounded)

data ScannerConfig = ScannerConfig
  { scfgLanguage		:: ScannerLanguage
  , scfgOpts			:: [ScannerOpt]
  }

initScannerConfig :: ScannerConfig
initScannerConfig = ScannerConfig
  {	scfgLanguage		= ScLang_CLike
  , scfgOpts			= []
  }

scannerConfigCXX, scannerConfigC :: ScannerConfig

scannerConfigCXX = initScannerConfig
  { scfgLanguage		= ScLang_CXX
  }

scannerConfigC = initScannerConfig
  { scfgLanguage		= ScLang_C
  }

scfgAddOpts :: [ScannerOpt] -> ScannerConfig -> ScannerConfig
scfgAddOpts o c = c {scfgOpts = o ++ scfgOpts c}

------------------------------------------------------------------------------------------------
-- User state
------------------------------------------------------------------------------------------------

type Bitmap = Int

data AlexUserState = AlexUserState
  { ausOptsBitmap		:: !Bitmap
  , ausLangBitmap		:: !Bitmap
  , ausConfig			:: ScannerConfig
  , ausStartCodeStack	:: [Int]
  , ausNonLexedStrings	:: [(AlexPosn,String)]		-- non recognized parts, all in reverse order
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { ausOptsBitmap		= 0
  , ausLangBitmap		= 0
  , ausConfig 			= initScannerConfig
  , ausStartCodeStack	= []
  , ausNonLexedStrings	= []
  }

-- User state access

-- This all should be state monad stuff instead of DIY...
ausModify' :: (AlexUserState -> (AlexUserState,x)) -> Alex x
ausModify' upd = Alex $ \s@AlexState{alex_ust=us} ->
                          let (us',x) = upd us
                          in  Right (s {alex_ust = us'}, x)

ausModify :: (AlexUserState -> AlexUserState) -> Alex ()
ausModify upd = ausModify' (\us -> (upd us,()))

ausGets :: (AlexUserState -> x) -> Alex x
ausGets get = Alex $ \s@AlexState{alex_ust=us} -> Right (s, get us)

ausGet = ausGets id
ausPut us = ausModify (const us)

------------------------------------------------------------------------------------------------
-- Alex & user state interaction
------------------------------------------------------------------------------------------------

alexPushStartCode :: Int -> Alex ()
alexPushStartCode newc = do
  c <- alexGetStartCode
  ausModify $ \us -> us {ausStartCodeStack = c : ausStartCodeStack us}
  alexSetStartCode newc

alexPopStartCode :: Alex ()
alexPopStartCode = do
  c <- ausModify' $ \us -> let (c:st) = ausStartCodeStack us
                           in  (us {ausStartCodeStack = st}, c)
  alexSetStartCode c

------------------------------------------------------------------------------------------------
-- Flags
------------------------------------------------------------------------------------------------

ifUserStatePred :: (AlexUserState -> Bool) -> AlexAccPred AlexUserState
ifUserStatePred pred s _ _ _ = pred s

ifScannerOpt :: ScannerOpt -> AlexAccPred AlexUserState
ifScannerOpt opt = ifUserStatePred (\s -> testBit (ausOptsBitmap s) (fromEnum opt))

ifLang :: ScannerLanguage -> AlexAccPred AlexUserState
ifLang lang = ifUserStatePred (\s -> scfgLanguage (ausConfig s) == lang)

ifLangs :: Bitmap -> AlexAccPred AlexUserState
ifLangs langs = ifUserStatePred (\s -> (ausLangBitmap s .&. langs) > 0)

langsAllCLike :: Bitmap
langsAllCLike = mkBitmap [minBound..(maxBound::ScannerLanguage)]

langsC :: Bitmap
langsC = mkBitmap [ScLang_C]

langsCXX :: Bitmap
langsCXX = mkBitmap [ScLang_CXX]

------------------------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------------------------

mkBitmap :: Enum x => [x] -> Bitmap
mkBitmap xs = foldr (.|.) 0 [bit $ fromEnum x | x <- xs]

------------------------------------------------------------------------------------------------
-- Execution
------------------------------------------------------------------------------------------------

scanner :: ScannerConfig -> String -> Either String [Token]
scanner cfg str
  = runAlex str (do
        ausModify $ \s ->
          s { ausOptsBitmap = mkBitmap (scfgOpts cfg)
            , ausLangBitmap = mkBitmap [scfgLanguage cfg]
            , ausConfig = cfg
            }
        loop
      )
  where loop = do -- (t, m) <- alexComplementError alexMonadScan
                  t <- alexMonadScanUser
                  -- when (isJust m) (lexerError (fromJust m))
                  let tok@(Token _ knd _) = t
                  if (knd == C_EOF)
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

-- Adapted alexMonadScan which propagates user state
alexMonadScanUser = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  us <- ausGet
  case alexScanUser us inp sc of
    AlexEOF -> alexEOF
    AlexError inp' -> alexError "lexical error"
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScanUser
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len


}

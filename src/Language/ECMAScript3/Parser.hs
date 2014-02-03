 -- | Parser for ECMAScript 3.
{-# LANGUAGE FlexibleContexts #-}
module Language.ECMAScript3.Parser
  (parse
  , parseScriptFromString
  , parseJavaScriptFromFile
  , parseJavaScriptFromFile'
  , parseScript
  , parseExpression
  , parseString
  , ParsedStatement
  , ParsedExpression
  , parseSimpleExpr'
  , parseBlockStmt
  , parseStatement
  , StatementParser
  , ExpressionParser
  , assignExpr
  -- debugging, remove the next 2 lines
  , mkDecimal
  , intLen
  , parseObjectLit  
  , Parser  
  , initialParserState
  ) where

import Language.ECMAScript3.Lexer hiding (identifier)
import qualified Language.ECMAScript3.Lexer as Lexer
-- import Language.ECMAScript3.Parser.State
import Language.ECMAScript3.Parser.Type
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Annotations
import Data.Default
import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Control.Monad(liftM,liftM2,liftM3)
import Control.Monad.Trans (MonadIO,liftIO)
import Control.Applicative ((<$>))
import Numeric(readDec,readOct,readHex)
import Data.Char
import Control.Monad.Identity
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Typeable
import Data.Generics hiding (Infix)
import qualified Data.HashMap.Strict as M
import Debug.Trace (trace, traceShow)

-- | Tag each entity with the span from the file from which it was parsed.


-- We parameterize the parse tree over source-locations.
type ParsedStatement  = Statement SourceSpan
type ParsedExpression = Expression SourceSpan
type ParsedClassElt   = ClassElt SourceSpan

-- These parsers can store some arbitrary state
type StatementParser s t  = Parser s t ParsedStatement
type ExpressionParser s t = Parser s t ParsedExpression
type ClassEltParser s t   = Parser s t ParsedClassElt




initialParserState :: Stream s Identity Char => (ParserState s t -> ExternP s t) -> ParserState s t
initialParserState p = PST p M.empty []


-- | checks if the label is not yet on the stack, if it is -- throws
-- an error; otherwise it pushes it onto the stack
pushLabel :: String -> Parser s t () 
pushLabel lab = do st <- getState
                   pos <- getPosition
                   if lab `elem` (labs st)
                     then fail $ "Duplicate label at " ++ show pos
                     else putState (st { labs = lab : labs st} )

popLabel :: Parser s t ()
popLabel = modifyState safeTail
  where safeTail (PST p m []) = PST p m [] 
        safeTail (PST p m (_:xs)) = PST p m xs

clearLabels :: ParserState s r -> ParserState s r 
clearLabels (PST p m _) = PST p m []

withFreshLabelStack :: Parser s t a -> Parser s t a
withFreshLabelStack p = do oldState <- getState
                           putState $ clearLabels oldState
                           a <- p
                           modifyState (\s -> s { labs = labs oldState })
                           return a

identifier :: Stream s Identity Char => Parser s t (Id SourceSpan)
identifier = withSpan Id Lexer.identifier 
  -- liftM2 Id getPosition Lexer.identifier

-- | Aliases for external parser types
type JScriptT a = JavaScript (SourceSpan, (Maybe (Id SourceSpan), a))



--{{{ Statements

-- Keep in mind that Token.reserved parsers (exported from the lexer) do not
-- consume any input on failure.  Note that all statements (expect for labelled
-- and expression statements) begin with a reserved-word.  If we fail to parse
-- this reserved-word, no input is consumed.  Hence, we can have the massive or
-- block that is parseExpression.  Note that if the reserved-word is parsed, it 
-- must be whatever statement the reserved-word indicates.  If we fail after the
-- reserved-word, we truly have a syntax error.  Since input has been consumed,
-- <|> will not try its alternate in parseExpression, and we will fail.

parseIfStmt:: Stream s Identity Char => StatementParser s t 
parseIfStmt = do
  pos <- getPosition
  reserved "if"
  test <- parseParenExpr <?> "parenthesized test-expression in if statement"
  consequent <- parseStatement <?> "true-branch of if statement"
  pos'       <- getPosition 
  optional semi -- TODO: in spec?
  ((do reserved "else"
       alternate <- parseStatement
       pos''     <- getPosition 
       return $ IfStmt (Span pos pos'') test consequent alternate)
   <|> return (IfSingleStmt (Span pos pos') test consequent))

parseSwitchStmt :: Stream s Identity Char => StatementParser s t
parseSwitchStmt =
  let parseDefault = do
        pos <- getPosition
        reserved "default"
        colon
        statements <- many parseStatement
        pos' <- getPosition
        return (CaseDefault (Span pos pos') statements)
      parseCase = do
         pos <- getPosition
         reserved "case"
         condition <- parseListExpr
         colon
         actions <- many parseStatement
         pos' <- getPosition 
         return (CaseClause (Span pos pos') condition actions)
      isCaseDefault (CaseDefault _ _) = True   
      isCaseDefault _                 = False
      checkClauses cs = case filter isCaseDefault cs of
        (_:c:_) -> fail $ "duplicate default clause in switch statement at " ++
                          show (getAnnotation c)
        _ -> return ()                  
    in do pos <- getPosition
          reserved "switch"
          test <- parseParenExpr
          clauses <- braces $ many $ parseDefault <|> parseCase
          checkClauses clauses
          pos' <- getPosition 
          return (SwitchStmt (Span pos pos') test clauses)

parseWhileStmt:: Stream s Identity Char => StatementParser s t
parseWhileStmt = do
  pos <- getPosition
  reserved "while"
  test <- parseParenExpr <?> "parenthesized test-expression in while loop"
  body <- parseStatement
  pos' <- getPosition
  return (WhileStmt (Span pos pos') test body)

parseDoWhileStmt:: Stream s Identity Char => StatementParser s t
parseDoWhileStmt = do
  pos <- getPosition
  reserved "do"
  body <- parseStatement
  reserved "while" <?> "while at the end of a do block"
  test <- parseParenExpr <?> "parenthesized test-expression in do loop"
  pos' <- getPosition
  optional semi
  return (DoWhileStmt (Span pos pos') body test)

parseContinueStmt:: Stream s Identity Char => StatementParser s t
parseContinueStmt = do
  pos <- getPosition
  reserved "continue"
  pos' <- getPosition
  -- Ensure that the identifier is on the same line as 'continue.'
  id <- if sourceLine pos == sourceLine pos'
        then liftM Just identifier <|> return Nothing
        else return Nothing
  optional semi
  pos'' <- getPosition 
  return $ ContinueStmt (Span pos pos') id

parseBreakStmt:: Stream s Identity Char => StatementParser s t
parseBreakStmt = do
  pos <- getPosition
  reserved "break"
  pos' <- getPosition
  -- Ensure that the identifier is on the same line as 'break.'
  id <- if sourceLine pos == sourceLine pos'
        then liftM Just identifier <|> return Nothing
        else return Nothing
  optional semi           
  pos'' <- getPosition
  return $ BreakStmt (Span pos pos') id

parseBlockStmt:: Stream s Identity Char => StatementParser s t
parseBlockStmt = do
  pos <- getPosition
  statements <- braces (many parseStatement)
  pos' <- getPosition
  return (BlockStmt (Span pos pos') statements)

parseEmptyStmt:: Stream s Identity Char => StatementParser s t
parseEmptyStmt = do
  pos <- getPosition
  semi
  pos' <- getPosition
  return (EmptyStmt (Span pos pos'))

parseLabelledStmt:: Stream s Identity Char => StatementParser s t
parseLabelledStmt = do
  pos <- getPosition
  -- Lookahead for the colon.  If we don't see it, we are parsing an identifier
  -- for an expression statement.
  label <- try (do label <- identifier
                   colon
                   return label)
  pushLabel $ unId label
  statement <- parseStatement
  popLabel
  pos' <- getPosition
  return (LabelledStmt (Span pos pos') label statement)

parseExpressionStmt:: Stream s Identity Char => StatementParser s t
parseExpressionStmt = do
  pos <- getPosition
  expr <- parseListExpr -- TODO: spec 12.4?
  optional semi
  pos' <- getPosition
  return $ ExprStmt (Span pos pos') expr


parseForInStmt:: Stream s Identity Char => StatementParser s t
parseForInStmt =
  let parseInit = (reserved "var" >> liftM ForInVar identifier)
               <|> liftM ForInLVal lvalue
  in do pos <- getPosition
        -- Lookahead, so that we don't clash with parseForStmt
        (init,expr) <- try $ do reserved "for"
                                parens $ do init <- parseInit
                                            reserved "in"
                                            expr <- parseExpression
                                            return (init,expr)
        body <- parseStatement
        pos' <- getPosition
        return $ ForInStmt (Span pos pos') init expr body

parseForStmt:: Stream s Identity Char => StatementParser s t
parseForStmt =
  let parseInit = (reserved "var" >> liftM VarInit (parseVarDecl `sepBy` comma))
               <|> liftM ExprInit parseListExpr
               <|> return NoInit
    in do pos <- getPosition
          reserved "for"
          reservedOp "("
          init <- parseInit
          semi
          test <- optionMaybe parseExpression
          semi
          iter <- optionMaybe parseListExpr
          reservedOp ")" <?> "closing paren"
          stmt <- parseStatement
          pos' <- getPosition
          return $ ForStmt (Span pos pos') init test iter stmt

parseTryStmt:: Stream s Identity Char => StatementParser s t
parseTryStmt =
  let parseCatchClause = do pos <- getPosition
                            reserved "catch"
                            id <- parens identifier
                            stmt <- parseStatement
                            pos' <- getPosition
                            return $ CatchClause (Span pos pos') id stmt
  in do reserved "try"
        pos <- getPosition
        guarded <- parseStatement
        mCatch <- optionMaybe parseCatchClause
        mFinally <- optionMaybe $ reserved "finally" >> parseStatement
        pos' <- getPosition
        -- the spec requires at least a catch or a finally block to
        -- be present
        if isJust mCatch || isJust mFinally 
          then return $ TryStmt (Span pos pos') guarded mCatch mFinally
          else fail $ "A try statement should have at least a catch\ 
                      \ or a finally block, at " ++ show pos

parseThrowStmt:: Stream s Identity Char => StatementParser s t
parseThrowStmt = do
  pos <- getPosition
  reserved "throw"
  expr <- parseExpression
  optional semi
  pos' <- getPosition
  return (ThrowStmt (Span pos pos') expr)

parseReturnStmt:: Stream s Identity Char => StatementParser s t
parseReturnStmt = do
  pos <- getPosition
  reserved "return"
  expr <- optionMaybe parseListExpr
  optional semi
  pos' <- getPosition
  return (ReturnStmt (Span pos pos') expr)

parseWithStmt:: Stream s Identity Char => StatementParser s t
parseWithStmt = do
  pos <- getPosition
  reserved "with"
  context <- parseParenExpr
  stmt <- parseStatement
  pos' <- getPosition
  return (WithStmt (Span pos pos') context stmt)

getExtP :: Stream s Identity Char => Parser s t (ExternP s t)
getExtP             = getState >>= \s@(PST e _ _) -> return (e s)

inAnnotP :: Stream s Identity Char => Parser s t a -> Parser s t a
inAnnotP p = do string "/*@"  >> whiteSpace
                t <- p
                whiteSpace    >> string "*/" >> whiteSpace
                return t


updSpan span (Just t) s = M.insert span [t] s
updSpan _ _ s = s

parseVarDecl :: Stream s Identity Char => Parser s t (VarDecl SourceSpan) 
parseVarDecl = do
    pos           <- getPosition
    id            <- identifier
    ot2           <- optionMaybe . inAnnotP . typeP =<< getExtP
    init          <- (reservedOp "=" >> liftM Just parseExpression) <|> return Nothing
    pos'          <- getPosition
    let span       = Span pos pos'

    modifyState    $ \s -> s { store = updSpan (getAnnotation id) ot2 (store s) }
    return         $ VarDecl span id init

parseVarDeclStmt:: Stream s Identity Char => StatementParser s t
parseVarDeclStmt = do 
    try (do ot          <- optionMaybe . inAnnotP . (`sepBy` comma) . bTypeP =<< getExtP
            pos         <- getPosition
            reserved "var"
            decls       <- parseVarDecl `sepBy` comma
            optional semi
            pos'        <- getPosition
            let span     = Span pos pos'
            modifyState  $ \s -> s { store = upds span ot (store s) }
            return       $ VarDeclStmt span decls)
  where
    upds span (Just t) s = M.insert span t s
    upds _ _ s           = s
    ss (Just tt) = show $ length tt
    ss _         = "0"


parseFunctionStmt:: Stream s Identity Char => StatementParser s t
parseFunctionStmt = do
  try (do s@(PST e _ _)     <- getState
          let (EP _ fP _ _)  = e s
          a                 <- inAnnotP fP
          pos               <- getPosition
          name              <- try (reserved "function" >> identifier)
          args              <- parens (identifier `sepBy` comma)
          -- label sets don't cross function boundaries
          BlockStmt _ body  <- withFreshLabelStack parseBlockStmt <?> 
                                "function body in { ... }"
          pos'              <- getPosition
          let span           = Span pos pos'
          PST e s l         <- getState
          putState           $ PST e (updSpan span (Just a) s) l
          return             $ FunctionStmt span name args body)

parseConstructor :: Stream s Identity Char => ClassEltParser s t
parseConstructor = do
  try (do s@(PST e _ _)     <- getState
          let (EP tP _ _ _)  = e s
          a                 <- inAnnotP tP
          pos               <- getPosition
          try                $ reserved "constructor"
          args              <- parens (identifier `sepBy` comma)
          BlockStmt _ body  <- withFreshLabelStack parseBlockStmt <?> 
                                "constructor body in { ... }"
          pos'              <- getPosition
          let span           = Span pos pos'
          PST e s l         <- getState
          putState           $ PST e (updSpan span (Just a) s) l
          return             $ Constructor span args body)

parseMemberFuncDecl :: Stream s Identity Char => ClassEltParser s t
parseMemberFuncDecl = do
  try (do s@(PST e _ _)     <- getState
          let (EP _ fP _ _)  = e s
          a                 <- inAnnotP fP
          pos               <- getPosition
          mod               <- try (reserved "public" >> return True) 
                           <|> try (option False (reserved "private" >> return False))
          static            <- option False (reserved "static" >> return True)
          name              <- identifier
          args              <- parens (identifier `sepBy` comma)
          BlockStmt _ body  <- withFreshLabelStack parseBlockStmt <?> 
                                "method body in { ... }"
          pos'              <- getPosition
          let span           = Span pos pos'
          PST e s l         <- getState
          putState           $ PST e (updSpan span (Just a) s) l
          return             $ MemberFuncDecl span mod static name args body)

parseMemberVarDecl :: Stream s Identity Char => ClassEltParser s t
parseMemberVarDecl = do
  try (do pos               <- getPosition
          mod               <- try (reserved "public" >> return True) 
                           <|> try (option False (reserved "private" >> return False))
          static            <- option False (reserved "static" >> return True)
          varDecl           <- parseVarDecl
          pos'              <- getPosition
          let span           = Span pos pos'
          semi
          return             $ MemberVarDecl span mod static varDecl)


parseClassStmt :: Stream s Identity Char => StatementParser s t
parseClassStmt = 
  try (do pos     <- getPosition
          name    <- try (reserved "class" >> identifier)
          extends <- optionMaybe (reserved "extends" >> identifier)
          impls   <- maybeToList <$> optionMaybe (reserved "implements" >> identifier `sepBy` comma)
          t       <- braces (parseClassElement `sepBy` whiteSpace)
          pos'    <- getPosition
          let span = Span pos pos'
          return   $ ClassStmt span name extends impls t)

maybeToList Nothing   = []
maybeToList (Just xs) = xs

parseClassElement :: Stream s Identity Char =>  ClassEltParser s t 
parseClassElement = parseConstructor <|> parseMemberFuncDecl <|> parseMemberVarDecl

            

parseTopLevel :: Stream s Identity Char => ParsecT s (ParserState s t) Identity (Maybe a)
parseTopLevel = do
    st@(PST e s l) <- getState
    pos     <- getPosition
    a       <- inAnnotP . topLevelP =<< getExtP
    pos'    <- getPosition
    let span = Span pos pos'
    putState $ PST e (M.insert span [a] s) l
    return   $ Nothing

parseStatement:: Stream s Identity Char => StatementParser s t
parseStatement = parseIfStmt <|> parseSwitchStmt <|> parseWhileStmt 
  <|> parseDoWhileStmt <|> parseContinueStmt <|> parseBreakStmt 
  <|> parseBlockStmt <|> parseEmptyStmt <|> parseForInStmt <|> parseForStmt
  <|> parseTryStmt <|> parseThrowStmt <|> parseReturnStmt <|> parseWithStmt 
  <|> parseVarDeclStmt  <|> parseFunctionStmt <|> parseClassStmt
  -- labelled, expression and the error message always go last, in this order
  <|> parseLabelledStmt <|> parseExpressionStmt <?> "statement"

--}}}

--{{{ Expressions

-- References used to construct this stuff:
-- + http://developer.mozilla.org/en/docs/
--     Core_JavaScript_1.5_Reference:Operators:Operator_Precedence
-- + http://www.mozilla.org/js/language/grammar14.html
--
-- Aren't expression tables nice?  Well, we can't quite use them, because of 
-- JavaScript's ternary (?:) operator.  We have to use two expression tables.
-- We use one expression table for the assignment operators that bind looser 
-- than ?: (assignTable).  The terms of assignTable are ternary expressions 
-- (parseTernaryExpr).  parseTernaryExpr left-factors the left-recursive
-- production for ?:, and is defined over the second expression table, 
-- exprTable, which consists of operators that bind tighter than ?:.  The terms
-- of exprTable are atomic expressions, parenthesized expressions, functions and
-- array references.

--{{{ Primary expressions

parseThisRef:: Stream s Identity Char => ExpressionParser s t
parseThisRef = do
  pos <- getPosition
  reserved "this"
  pos' <- getPosition
  return (ThisRef (Span pos pos'))

parseNullLit:: Stream s Identity Char => ExpressionParser s t
parseNullLit = do
  pos <- getPosition
  reserved "null"
  pos' <- getPosition
  return (NullLit (Span pos pos'))


parseBoolLit:: Stream s Identity Char => ExpressionParser s t
parseBoolLit = do
    pos <- getPosition
    let parseTrueLit  = reserved "true"  >> getPosition >>= \pos' -> return (BoolLit (Span pos pos') True)
        parseFalseLit = reserved "false" >> getPosition >>= \pos' -> return (BoolLit (Span pos pos') False)
    parseTrueLit <|> parseFalseLit

parseVarRef:: Stream s Identity Char => ExpressionParser s t
parseVarRef = withSpan VarRef identifier 

parseArrayLit:: Stream s Identity Char => ExpressionParser s t
parseArrayLit = do 
    {-p    <- typeP <$> extP <$> getState-}
    pos  <- getPosition
    {-a    <- do string "/*:"; whiteSpace; a <- p-}
    {-            whiteSpace; string "*/";-}
    {-            whiteSpace; return a-}
    e <- squares (parseExpression `sepEndBy` comma)
    pos' <- getPosition
    return $ ArrayLit (Span pos pos') e
  
--  parseArrayLit = ArrayLit (squares (parseExpression `sepEndBy` comma))

parseFuncExpr :: Stream s Identity Char => ExpressionParser s t
parseFuncExpr = do
  pos <- getPosition
  reserved "function"
  name <- optionMaybe identifier
  args <- parens (identifier `sepBy` comma)
  -- labels don't cross function boundaries
  BlockStmt _ body <- withFreshLabelStack parseBlockStmt
  pos' <- getPosition
  return $ FuncExpr (Span pos pos') name args body

--{{{ parsing strings

escapeChars =
 [('\'','\''),('\"','\"'),('\\','\\'),('b','\b'),('f','\f'),('n','\n'),
  ('r','\r'),('t','\t'),('v','\v'),('/','/'),(' ',' '),('0','\0')]

allEscapes:: String
allEscapes = map fst escapeChars

parseEscapeChar :: Stream s Identity Char => Parser s t Char
parseEscapeChar = do
  c <- oneOf allEscapes
  let (Just c') = lookup c escapeChars -- will succeed due to line above
  return c' 

parseAsciiHexChar :: Stream s Identity Char => Parser s t Char
parseAsciiHexChar = do
  char 'x'
  d1 <- hexDigit
  d2 <- hexDigit
  return ((chr.fst.head.readHex) (d1:d2:""))

parseUnicodeHexChar :: Stream s Identity Char => Parser s t Char
parseUnicodeHexChar = do
  char 'u'
  liftM (chr.fst.head.readHex) 
        (sequence [hexDigit,hexDigit,hexDigit,hexDigit])
        
isWhitespace ch = ch `elem` " \t"


-- The endWith argument is either single-quote or double-quote, depending on how
-- we opened the string.
parseStringLit' endWith =
  (char endWith >> return "") <|>
  (do try (string "\\'")
      cs <- parseStringLit' endWith
      return $ "'" ++ cs) <|>
  (do char '\\'
      c <- parseEscapeChar <|> parseAsciiHexChar <|> parseUnicodeHexChar <|> 
           char '\r' <|> char '\n'
      cs <- parseStringLit' endWith
      if c == '\r' || c == '\n' 
        then return (c:dropWhile isWhitespace cs) 
        else return (c:cs)) <|>
   liftM2 (:) anyChar (parseStringLit' endWith)

parseStringLit:: Stream s Identity Char => ExpressionParser s t
parseStringLit = do
  pos <- getPosition
  -- parseStringLit' takes as an argument the quote-character that opened the
  -- string.
  str <- lexeme $ (char '\'' >>= parseStringLit') <|> (char '\"' >>= parseStringLit')
  -- CRUCIAL: Parsec.Token parsers expect to find their token on the first
  -- character, and read whitespaces beyond their tokens.  Without 'lexeme'
  -- above, expressions like:
  --   var s = "string"   ;
  -- do not parse.
  pos' <- getPosition
  return $ StringLit (Span pos pos') str

--}}}

parseRegexpLit:: Stream s Identity Char => ExpressionParser s t
parseRegexpLit = do
  let parseFlags = do
        flags <- many (oneOf "mgi")
        return $ \f -> f ('g' `elem` flags) ('i' `elem` flags) 
  let parseEscape :: Stream s Identity Char => Parser s t Char
      parseEscape = char '\\' >> anyChar
  let parseChar :: Stream s Identity Char => Parser s t Char
      parseChar = noneOf "/"
  let parseRe = (char '/' >> return "") <|> 
                (do char '\\'
                    ch <- anyChar -- TODO: too lenient
                    rest <- parseRe
                    return ('\\':ch:rest)) <|> 
                liftM2 (:) anyChar parseRe
  pos <- getPosition
  char '/'
  notFollowedBy $ char '/' <|> char '*'
  pat <- parseRe --many1 parseChar
  flags <- parseFlags
  spaces -- crucial for Parsec.Token parsers
  pos' <- getPosition
  return $ flags (RegexpLit (Span pos pos') pat)
          
parseObjectLit:: Stream s Identity Char => ExpressionParser s t
parseObjectLit =
  let parseProp = do
        -- Parses a string, identifier or integer as the property name.  I
        -- apologize for the abstruse style, but it really does make the code
        -- much shorter.
        name <- liftM (\(StringLit p s) -> PropString p s) parseStringLit
            <|> withSpan PropId  identifier
            <|> withSpan PropNum decimal
        colon
        val <- assignExpr
        return (name,val)
    in do pos <- getPosition
          props <- braces (parseProp `sepEndBy` comma) <?> "object literal"
          pos' <- getPosition
          return $ ObjectLit (Span pos pos') props

--{{{ Parsing numbers.  From pg. 17-18 of ECMA-262.
hexLit :: Stream s Identity Char => Parser s t (Bool, Double)
hexLit = do
  try (string "0x")
  digits <- many1 (oneOf "0123456789abcdefABCDEF")
  [(hex,"")] <- return $ Numeric.readHex digits
  return (True, hex)

-- | Creates a decimal value from a whole, fractional and exponent part.
mkDecimal :: Integer -> Integer -> Integer -> Integer -> Double
mkDecimal whole frac fracLen exp = 
  ((fromInteger whole) + ((fromInteger frac) * (10 ^^ (-fracLen)))) * (10 ^^ exp)

exponentPart :: Stream s Identity Char => Parser s t Integer
exponentPart = do
  oneOf "eE"
  (char '+' >> decimal) <|> (char '-' >> negate `fmap` decimal) <|> decimal

--wrap a parser's result in a Just:
jparser :: Stream s Identity Char => Parser s t a -> Parser s t (Maybe a)
jparser = liftM Just

decLit :: Stream s Identity Char => Parser s t (Bool, Double)
decLit = 
  (do whole <- decimal
      mfrac <- option Nothing (jparser (char '.' >> decimal))
      mexp <-  option Nothing (jparser exponentPart)
      if isNothing mfrac && isNothing mexp
        then return (True, fromIntegral whole)
        else let frac = fromIntegral (fromMaybe 0 mfrac)
             in  return (False, mkDecimal (fromIntegral whole) frac 
                                          (intLen frac)
                                          (fromIntegral (fromMaybe 0 mexp))))
  <|>
  (do frac <- char '.' >> decimal
      exp <- option 0 exponentPart
      let ifrac = fromIntegral frac
      return (False, mkDecimal 0 ifrac (intLen frac) (fromIntegral exp)))

intLen i | i `div` 10 < 1 = 1
intLen i | otherwise = 1 + intLen (i `div` 10)

parseNumLit:: Stream s Identity Char => ExpressionParser s t
parseNumLit = do
    pos <- getPosition
    (isint, num) <- lexeme $ hexLit <|> decLit
    notFollowedBy identifierStart <?> "whitespace"
    pos' <- getPosition
    if isint
      then return $ IntLit (Span pos pos') (round num) 
      else return $ NumLit (Span pos pos') num


------------------------------------------------------------------------------
-- Position Helper
------------------------------------------------------------------------------

withPos cstr p = do { pos <- getPosition; e <- p; return $ cstr pos e }

withSpan cstr p = do pos   <- getPosition
                     x     <- p
                     pos'  <- getPosition
                     return $ cstr (Span pos pos') x

-------------------------------------------------------------------------------
-- Compound Expression Parsers
-------------------------------------------------------------------------------

dotRef e = (reservedOp "." >> withSpan cstr identifier) <?> "property.ref"
    where cstr pos = DotRef pos e

funcApp e = parens (withSpan cstr (parseExpression `sepBy` comma)) 
         <?>"(function application)"
    where cstr pos = CallExpr pos e

bracketRef e = brackets (withSpan cstr parseExpression) <?> "[property-ref]"
    where cstr pos = BracketRef pos e

-------------------------------------------------------------------------------
-- Expression Parsers
-------------------------------------------------------------------------------

parseParenExpr:: Stream s Identity Char => ExpressionParser s t
parseParenExpr = parens parseListExpr

-- everything above expect functions
parseExprForNew :: Stream s Identity Char => ExpressionParser s t
parseExprForNew = parseThisRef <|> parseNullLit <|> parseBoolLit <|> parseStringLit 
  <|> parseArrayLit <|> parseParenExpr <|> parseNewExpr <|> parseNumLit 
  <|> parseRegexpLit <|> parseObjectLit <|> parseVarRef

-- all the expression parsers defined above
parseSimpleExpr' :: Stream s Identity Char => ExpressionParser s t
parseSimpleExpr' = parseThisRef <|> parseNullLit <|> parseBoolLit 
  <|> parseStringLit <|> parseArrayLit <|> parseParenExpr
  <|> parseFuncExpr <|> parseNumLit <|> parseRegexpLit <|> parseObjectLit
  <|> parseVarRef

parseNewExpr :: Stream s Identity Char => ExpressionParser s t
parseNewExpr =
  (do pos <- getPosition
      reserved "new"
      constructor <- parseSimpleExprForNew Nothing -- right-associativity
      arguments <- try (parens (parseExpression `sepBy` comma)) <|> return []
      pos' <- getPosition
      return (NewExpr (Span pos pos') constructor arguments)) <|>
  parseSimpleExpr'


parseSimpleExpr :: Stream s Identity Char => Maybe (Expression SourceSpan) -> ExpressionParser s t
parseSimpleExpr (Just e) = ((dotRef e <|> funcApp e <|> bracketRef e) >>=
                            parseSimpleExpr . Just)  
                        <|> return e
parseSimpleExpr Nothing = do
  e <- parseNewExpr <?> "expression (3)"
  parseSimpleExpr (Just e)

parseSimpleExprForNew :: Stream s Identity Char
                      =>(Maybe ParsedExpression) -> ExpressionParser s t
parseSimpleExprForNew (Just e) = ((dotRef e <|> bracketRef e) >>=
                                  parseSimpleExprForNew . Just)
                              <|> return e
parseSimpleExprForNew Nothing = do
  e <- parseNewExpr <?> "expression (4)"
  parseSimpleExprForNew (Just e)
    
--}}}

makeInfixExpr str constr = Infix parser AssocLeft where
  parser = do
    pos <- getPosition
    reservedOp str
    pos' <- getPosition
    return (InfixExpr (Span pos pos') constr)  -- points-free, returns a function


-- apparently, expression tables can't handle immediately-nested prefixes
parsePrefixedExpr :: Stream s Identity Char => ExpressionParser s t
parsePrefixedExpr = do
  pos <- getPosition
  op <- optionMaybe $ (reservedOp "!" >> return PrefixLNot) <|> 
                      (reservedOp "~" >> return PrefixBNot) <|>
                      (try (lexeme $ char '-' >> notFollowedBy (char '-')) >>
                       return PrefixMinus) <|>
                      (try (lexeme $ char '+' >> notFollowedBy (char '+')) >>
                       return PrefixPlus) <|>
                      (reserved "typeof" >> return PrefixTypeof) <|>
                      (reserved "void" >> return PrefixVoid) <|>
                      (reserved "delete" >> return PrefixDelete)
  case op of
    Nothing -> unaryAssignExpr
    Just op -> do
      innerExpr <- parsePrefixedExpr
      pos'      <- getPosition
      return (PrefixExpr (Span pos pos') op innerExpr)

exprTable:: Stream s Identity Char => [[Operator s (ParserState s t) Identity ParsedExpression]]
exprTable = 
  [ [ makeInfixExpr "*" OpMul
    , makeInfixExpr "/" OpDiv
    , makeInfixExpr "%" OpMod
    ]
  , [ makeInfixExpr "+" OpAdd
    , makeInfixExpr "-" OpSub
    ]
  , [ makeInfixExpr "<<" OpLShift
    , makeInfixExpr ">>" OpSpRShift
    , makeInfixExpr ">>>" OpZfRShift
    ]
  , [ makeInfixExpr "<" OpLT
    , makeInfixExpr "<=" OpLEq
    , makeInfixExpr ">" OpGT
    , makeInfixExpr ">=" OpGEq
    , makeInfixExpr "instanceof" OpInstanceof
    , makeInfixExpr "in" OpIn
    ]
  , [ makeInfixExpr "==" OpEq
    , makeInfixExpr "!=" OpNEq
    , makeInfixExpr "===" OpStrictEq
    , makeInfixExpr "!==" OpStrictNEq
    ]
  , [ makeInfixExpr "&" OpBAnd ]
  , [ makeInfixExpr "^" OpBXor ]
  , [ makeInfixExpr "|" OpBOr ]
  , [ makeInfixExpr "&&" OpLAnd ]
  , [ makeInfixExpr "||" OpLOr ]
  ]

parseExpression' :: Stream s Identity Char => ExpressionParser s t
parseExpression' = buildExpressionParser exprTable parsePrefixedExpr <?> "simple expression"

asLValue :: Stream s Identity Char
         => SourcePos
         -> Expression SourceSpan
         -> Parser s t (LValue SourceSpan)
asLValue p' e = case e of
  VarRef p (Id _ x) -> return (LVar p x)
  DotRef p e (Id _ x) -> return (LDot p e x)
  BracketRef p e1 e2 -> return (LBracket p e1 e2)
  otherwise -> fail $ "expected a left-value at " ++ show p'

lvalue :: Stream s Identity Char => Parser s t (LValue SourceSpan)
lvalue = do
  p <- getPosition
  e <- parseSimpleExpr Nothing
  asLValue p e

unaryAssignExpr :: Stream s Identity Char => ExpressionParser s t
unaryAssignExpr = do
    p <- getPosition
    prefixInc p <|> prefixDec p <|> other p

-- prefixInc :: Stream s Identity Char => SourcePos -> ParsecT s (ParserState s a) Identity (ExprT a)
  where
    prefixInc p = do
        reservedOp "++"
        liftM2 (\x p' -> UnaryAssignExpr (Span p p') PrefixInc x) lvalue getPosition
    postfixInc e p = do
        reservedOp "++"
        liftM2 (\x p' -> UnaryAssignExpr (Span p p') PostfixInc x) (asLValue p e) getPosition
    postfixDec e p = do
        reservedOp "--"
        liftM2 (\x p' -> UnaryAssignExpr (Span p p') PostfixDec x) (asLValue p e) getPosition
    other p = do
        e <- parseSimpleExpr Nothing
        postfixInc e p <|> postfixDec e p <|> return e
    prefixDec p = do
        reservedOp "--"
        liftM2 (\x p' -> UnaryAssignExpr (Span p p') PrefixDec x) lvalue getPosition


parseTernaryExpr':: Stream s Identity Char => Parser s t (ParsedExpression, ParsedExpression)
parseTernaryExpr' = do
    reservedOp "?"
    l <- assignExpr
    colon
    r <- assignExpr
    return (l,r)

parseTernaryExpr:: Stream s Identity Char => ExpressionParser s t
parseTernaryExpr = do
  p <- getPosition
  e <- parseExpression'
  e' <- optionMaybe parseTernaryExpr'
  case e' of
    Nothing -> return e
    Just (l,r) -> do p' <- getPosition
                     return $ CondExpr (Span p p') e l r

assignOp :: Stream s Identity Char => Parser s t AssignOp
assignOp = (reservedOp "=" >> return OpAssign)
        <|>(reservedOp "+=" >> return OpAssignAdd)
        <|>(reservedOp "-=" >> return OpAssignSub)
        <|>(reservedOp "*=" >> return OpAssignMul)
        <|>(reservedOp "/=" >> return OpAssignDiv)
        <|>(reservedOp "%=" >> return OpAssignMod)
        <|>(reservedOp "<<=" >> return OpAssignLShift)
        <|>(reservedOp ">>=" >> return OpAssignSpRShift)
        <|>(reservedOp ">>>=" >> return OpAssignZfRShift)
        <|>(reservedOp "&=" >> return OpAssignBAnd)
        <|>(reservedOp "^=" >> return OpAssignBXor)
        <|>(reservedOp "|=" >> return OpAssignBOr)

assignExpr :: Stream s Identity Char => ExpressionParser s t
assignExpr = do
  p <- getPosition
  lhs <- parseTernaryExpr
  let assign = do
        op <- assignOp
        lhs <- asLValue p lhs
        rhs <- assignExpr
        p' <- getPosition
        return (AssignExpr (Span p p') op lhs rhs)
  assign <|> return lhs

parseExpression:: Stream s Identity Char => ExpressionParser s t
parseExpression = assignExpr

parseListExpr :: Stream s Identity Char => ExpressionParser s t
-- parseListExpr = assignExpr `sepBy1` comma >>= \exprs ->
--   case exprs of
--     [expr] -> return expr
--     es     -> liftM2 ListExpr getPosition (return es)

parseListExpr 
  = do pos   <- getPosition
       exprs <- assignExpr `sepBy1` comma
       pos'  <- getPosition 
       case exprs of
         [expr] -> return   expr
         es     -> return $ ListExpr (Span pos pos') es



parseScript:: Stream s Identity Char => Parser s t (JavaScript SourceSpan)
parseScript = do
  whiteSpace
  -- NOTE: Parse top-level annotations only at the script top-level, but yield
  -- no expression for them.
  withSpan Script $ catMaybes <$> (((try parseTopLevel) <|> (Just <$> parseStatement)) `sepBy` whiteSpace)
  
-- | Parse from a stream; same as 'Text.Parsec.parse'

-- NOTE: This only compiles if the external parser and the current parser are
-- working on separate streams.
parse externP p = runParser pp (initialParserState externP)
  where 
    pp = do { a <- p;
              st <- store <$> getState;
              return (a, st)
            }

-- | Read a JavaScript program from file and parse it into a list of statements. 

-- Params:
--  ∙ externP: a parser that will be used at places where annotations will need to
--    be parsed. At the moment this is just in `VarDeclStmt `.
--  ∙ fileName: The name of the file to be parsed.
parseJavaScriptFromFile' :: MonadIO m =>
  (ParserState String t -> ExternP String t) -> FilePath -> m ([Statement SourceSpan], M.HashMap SourceSpan [t])
parseJavaScriptFromFile' externP filename = do
  chars <- liftIO $ readFile filename
  case parse externP parseScript filename chars of
    Left err                   -> fail (show err)
    Right (Script _ stmts, st) -> return (stmts, st)

-- | Read a JavaScript program from file and parse it into a list of
-- statements. No external parsers will be used.

parseJavaScriptFromFile :: MonadIO m => FilePath -> m [Statement SourceSpan]
parseJavaScriptFromFile f =liftM fst $ parseJavaScriptFromFile' (\_ -> EP z z z z) f
  where    
    z = parserZero

-- | Parse a JavaScript program from a string

parseScriptFromString :: Stream s Identity Char =>
  (ParserState s t -> ExternP s t) -> SourceName -> s -> 
    Either ParseError (JavaScript SourceSpan, M.HashMap SourceSpan [t])
parseScriptFromString externP u s = parse externP parseScript u s

-- | Parse a JavaScript source string into a list of statements
parseString :: Stream s Identity Char => (ParserState s t -> ExternP s t) -> s -> [Statement SourceSpan]
parseString externP str = case parse externP parseScript "" str of
  Left err                    -> error (show err)
  Right (Script _ stmts, _ )  -> stmts

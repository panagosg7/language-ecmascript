-- | This isn't a lexer in the sense that it provides a JavaScript
-- token-stream. This module provides character-parsers for various
-- JavaScript tokens.

{-# LANGUAGE FlexibleContexts #-}

module Language.ECMAScript3.Lexer(lexeme,identifier,reserved,operator,reservedOp,charLiteral,
                        stringLiteral,natural,integer,float,naturalOrFloat,
                        decimal,hexadecimal,octal,symbol,whiteSpace,parens,
                        braces,brackets,squares,semi,comma,colon,dot,
                        identifierStart {-, noCommentEnd-} ) where

import Prelude hiding (lex)
import Text.Parsec
-- PV: using my version of Token ...
import qualified Language.ECMAScript3.Token as T
-- import qualified Text.Parsec.Token as T
-- import Language.ECMAScript3.Parser.State
import Language.ECMAScript3.Parser.Type
import Control.Monad.Identity

identifierStart :: Stream s Identity Char => Parser s t Char
identifierStart = letter <|> oneOf "$_"

commentStart = "/*"
commentEnd = "*/"

-- | Define the type of comment that should not be ignored by the parser.

-- Character that differentiates an annotation comment from a regular comment.
-- I.e. "/*@ ... */" from "/* ... */"
annotChar = '@'

javascriptDef :: Stream s Identity Char =>T.GenLanguageDef s (ParserState s t) Identity
javascriptDef =
  T.LanguageDef commentStart
                commentEnd
                annotChar
                "//"
                False -- no nested comments
                identifierStart
                (alphaNum <|> oneOf "$_") -- identifier rest
                (oneOf "{}<>()~.,?:|&^=!+-*/%!") -- operator start
                (oneOf "=<>|&+") -- operator rest
                ["break", "case", "catch", "const", "continue", "debugger", 
                 "default", "delete", "do", "else", "enum", "false", "finally",
                 "for", "function", "if", "instanceof", "in", "let", "new", 
                 "null", "return", "switch", "this", "throw", "true", "try", 
                 "typeof", "var", "void", "while", "with"]
                ["|=", "^=", "&=", "<<=", ">>=", ">>>=", "+=", "-=", "*=", "/=", 
                 "%=", "=", ";", ",", "?", ":", "||", "&&", "|", "^", "&", 
                 "===", "==", "=", "!==", "!=", "<<", "<=", "<", ">>>", ">>", 
                 ">=", ">", "++", "--", "+", "-", "*", "/", "%", "!", "~", ".", 
                 "[", "]", "{", "}", "(", ")","</","instanceof"]
                 True -- case-sensitive
            
lex :: Stream s Identity Char => T.GenTokenParser s (ParserState s t) Identity
lex = T.makeTokenParser javascriptDef

-- everything but commaSep and semiSep
identifier :: Stream s Identity Char => Parser s t String
identifier = T.identifier	 lex
reserved :: Stream s Identity Char => String -> Parser s t ()
reserved = T.reserved	 lex
operator :: Stream s Identity Char => Parser s t String
operator = T.operator	 lex 
reservedOp :: Stream s Identity Char => String -> Parser s t ()
reservedOp = T.reservedOp lex	
charLiteral :: Stream s Identity Char => Parser s t Char
charLiteral = T.charLiteral lex	
stringLiteral :: Stream s Identity Char => Parser s t String
stringLiteral = T.stringLiteral lex
natural :: Stream s Identity Char => Parser s t Integer
natural = T.natural lex	 
integer :: Stream s Identity Char => Parser s t Integer
integer = T.integer lex	
float :: Stream s Identity Char => Parser s t Double
float = T.float lex
naturalOrFloat :: Stream s Identity Char => Parser s t (Either Integer Double)
naturalOrFloat = T.naturalOrFloat lex
decimal :: Stream s Identity Char => Parser s t Integer
decimal = T.decimal lex	
hexadecimal :: Stream s Identity Char => Parser s t Integer
hexadecimal = T.hexadecimal lex	
octal :: Stream s Identity Char => Parser s t Integer
octal = T.octal lex
symbol :: Stream s Identity Char => String -> Parser s t String
symbol = T.symbol lex
whiteSpace :: Stream s Identity Char => Parser s t ()
whiteSpace = T.whiteSpace lex	
parens :: Stream s Identity Char =>  Parser s t a ->  Parser s t a
parens = T.parens	 lex
braces :: Stream s Identity Char =>  Parser s t a ->  Parser s t a
braces = T.braces	 lex
squares :: Stream s Identity Char =>  Parser s t a ->  Parser s t a
squares = T.squares lex	
semi :: Stream s Identity Char => Parser s t String
semi = T.semi	 lex
comma :: Stream s Identity Char => Parser s t String
comma = T.comma	 lex
colon :: Stream s Identity Char => Parser s t String
colon = T.colon lex
dot :: Stream s Identity Char => Parser s t String
dot = T.dot lex
brackets :: Stream s Identity Char =>  Parser s t a ->  Parser s t a
brackets = T.brackets lex
lexeme :: Stream s Identity Char =>  Parser s t a ->  Parser s t a
lexeme = T.lexeme lex

{-noCommentEnd :: Stream s Identity Char => ExternP s t-}
{-noCommentEnd =  EP p p-}
{-  where -}
{-    p = parsecMap (const Nothing) (skipMany $ noneOf commentEnd)-}

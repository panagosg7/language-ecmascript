-- | This isn't a lexer in the sense that it provides a JavaScript
-- token-stream. This module provides character-parsers for various
-- JavaScript tokens.

{-# LANGUAGE FlexibleContexts #-}

module Language.ECMAScript3.Lexer(lexeme,identifier,reserved,operator,reservedOp,charLiteral,
                        stringLiteral,natural,integer,float,naturalOrFloat,
                        decimal,hexadecimal,octal,symbol,whiteSpace,parens,
                        braces,brackets,squares,semi,comma,colon,dot,
                        identifierStart, noCommentEnd) where

import Prelude hiding (lex)
import Text.Parsec
-- PV: using my version of Token ...
import qualified Language.ECMAScript3.Token as T
-- import qualified Text.Parsec.Token as T
-- import Language.ECMAScript3.Parser.State
import Language.ECMAScript3.Parser.Type
import Control.Monad.Identity

identifierStart :: Stream s Identity Char => Parser s Char r
identifierStart = letter <|> oneOf "$_"

commentStart = "/*"
commentEnd = "*/"

-- Character that differentiates an annotation comment from a regular comment.
-- I.e. "/*: ... */" from "/* ... */"
annotChar = ':'

javascriptDef :: Stream s Identity Char =>T.GenLanguageDef s (ParserState s r) Identity
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
            
lex :: Stream s Identity Char => T.GenTokenParser s (ParserState s r) Identity
lex = T.makeTokenParser javascriptDef

-- everything but commaSep and semiSep
identifier :: Stream s Identity Char => Parser s String r
identifier = T.identifier	 lex
reserved :: Stream s Identity Char => String -> Parser s () r
reserved = T.reserved	 lex
operator :: Stream s Identity Char => Parser s String r
operator = T.operator	 lex 
reservedOp :: Stream s Identity Char => String -> Parser s () r
reservedOp = T.reservedOp lex	
charLiteral :: Stream s Identity Char => Parser s Char r 
charLiteral = T.charLiteral lex	
stringLiteral :: Stream s Identity Char => Parser s String r
stringLiteral = T.stringLiteral lex
natural :: Stream s Identity Char => Parser s Integer r
natural = T.natural lex	 
integer :: Stream s Identity Char => Parser s Integer r
integer = T.integer lex	
float :: Stream s Identity Char => Parser s Double r
float = T.float lex
naturalOrFloat :: Stream s Identity Char => Parser s (Either Integer Double) r
naturalOrFloat = T.naturalOrFloat lex
decimal :: Stream s Identity Char => Parser s Integer r
decimal = T.decimal lex	
hexadecimal :: Stream s Identity Char => Parser s Integer r
hexadecimal = T.hexadecimal lex	
octal :: Stream s Identity Char => Parser s Integer r
octal = T.octal lex
symbol :: Stream s Identity Char => String -> Parser s String r
symbol = T.symbol lex
whiteSpace :: Stream s Identity Char => Parser s () r
whiteSpace = T.whiteSpace lex	
parens :: Stream s Identity Char =>  Parser s a r ->  Parser s a r
parens = T.parens	 lex
braces :: Stream s Identity Char =>  Parser s a r ->  Parser s a r
braces = T.braces	 lex
squares :: Stream s Identity Char =>  Parser s a r ->  Parser s a r
squares = T.squares lex	
semi :: Stream s Identity Char => Parser s String r
semi = T.semi	 lex
comma :: Stream s Identity Char => Parser s String r
comma = T.comma	 lex
colon :: Stream s Identity Char => Parser s String r 
colon = T.colon lex
dot :: Stream s Identity Char => Parser s String r 
dot = T.dot lex
brackets :: Stream s Identity Char =>  Parser s a r ->  Parser s a r
brackets = T.brackets lex
lexeme :: Stream s Identity Char =>  Parser s a r ->  Parser s a r
lexeme = T.lexeme lex

noCommentEnd :: Stream s Identity Char => Parser s (Maybe a) r
noCommentEnd =  parsecMap (const Nothing) (skipMany $ noneOf commentEnd)

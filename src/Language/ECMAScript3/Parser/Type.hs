module Language.ECMAScript3.Parser.Type(
    Parser
  , ParserState (..)
  ) where

import Text.Parsec
import Control.Monad.Identity

type Parser s a r = ParsecT s (ParserState s r) Identity a

data ParserState s r = PST { 
    -- Variable declaration annotations parser
    externP :: Parser s (Maybe r) r,
    labs :: [String] 
  } 

module Language.ECMAScript3.Parser.Type(
    Parser
  , ParserState (..)
  ) where

-- import Language.ECMAScript3.Parser.State
import Text.Parsec
import Control.Monad.Identity

type Parser s a r = ParsecT s (ParserState r) Identity a

data ParserState r = PST { 
    -- Variable declaration annotations parser
    externP :: Parser String (Maybe r) r,
    labs :: [String] 
  } 

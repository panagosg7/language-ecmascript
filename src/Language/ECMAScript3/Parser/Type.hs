{-# LANGUAGE DeriveDataTypeable #-}
module Language.ECMAScript3.Parser.Type(
    Parser
  , ParserState (..)
  , ExternP (..)
  , SourceSpan (..)
  ) where

import Text.Parsec
import Control.Monad.Identity
import Language.ECMAScript3.Syntax
import Data.Typeable
import Data.Data
import Data.Hashable
import qualified Data.HashMap.Strict as M

data SourceSpan    = Span { sp_begin :: !SourcePos
                          , sp_end   :: !SourcePos 
                          }
                       deriving (Eq, Ord, Show, Data, Typeable)

instance Hashable SourceSpan where 
  hashWithSalt i z = hashWithSalt i (sp_begin z, sp_end z)

sourcePosElts s = (src, line, col)
  where 
    src         = sourceName   s 
    line        = sourceLine   s
    col         = sourceColumn s 

instance Hashable SourcePos where 
  hashWithSalt i   = hashWithSalt i . sourcePosElts


type Parser s t a = ParsecT s (ParserState s t) Identity a

-- | External parser

data ExternP s t = EP {
    typeP   :: Parser s t t,    -- A parser for type annotation
    funSigP :: Parser s t t     -- A parser for function signatures
  }

data ParserState s t = PST { 
    extP  :: ExternP s t,
    store :: M.HashMap SourceSpan t,
    labs  :: [String] 
  } 

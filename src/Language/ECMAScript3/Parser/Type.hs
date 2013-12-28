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
    typeP     :: Parser s t t,    -- ^ parser for type annotations
    funSigP   :: Parser s t t,    -- ^ parser for function signatures
    topLevelP :: Parser s t t     -- ^ parser for top-level annotations
  }

data ParserState s t = PST { 
    extP  :: ParserState s t -> ExternP s t,  -- ^ external parsers
    store :: M.HashMap SourceSpan t,          -- ^ store for annotations
    labs  :: [String]                         -- ^ labels
  } 

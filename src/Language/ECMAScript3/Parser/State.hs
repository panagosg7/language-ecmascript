module Language.ECMAScript3.Parser.State where

-- the statement label stack
type ParserState1 r = (r, [String])

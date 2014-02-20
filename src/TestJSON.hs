
-- |ECMAScript 3 syntax. /Spec/ refers to the ECMA-262 specification,
-- 3rd edition.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}

import Text.Parsec.Pos(initialPos,SourcePos) -- used by data JavaScript
import Data.Generics(Data,Typeable)
import Data.Aeson
import Data.Aeson.Types
import Data.Vector        ((!), fromList)
import Text.Parsec.Pos (newPos, sourceColumn, sourceLine, sourceName, SourcePos)
import Control.Monad.Trans (MonadIO,liftIO)
import Data.Text (pack)
import Prelude hiding (maybe)
import Text.PrettyPrint.HughesPJ
import qualified Data.ByteString.Lazy as B
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Default
import GHC.Generics

data SourceSpan    = Span { sp_begin :: !SourcePos
                          , sp_end   :: !SourcePos 
                          }
                       deriving (Eq, Ord, Show, Data, Typeable,Generic)


data JavaScript a
  -- |A script in \<script\> ... \</script\> tags.
  = Script a [Statement a] 
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)

instance Default a => Default (JavaScript a) where
  def = Script def []

-- | extracts statements from a JavaScript type
unJavaScript :: JavaScript a -> [Statement a]
unJavaScript (Script _ stmts) = stmts

instance Default SourcePos where
  def = initialPos ""

data Id a = Id a String 
          deriving (Show,Eq,Ord,Data,Typeable,Functor,Foldable,Traversable,Generic)

unId :: Id a -> String
unId (Id _ s) = s

-- | Infix operators: see spec 11.5-11.11
data InfixOp = OpLT -- ^ @<@
             | OpLEq -- ^ @<=@
             | OpGT -- ^ @>@
             | OpGEq -- ^ @>=@
             | OpIn -- ^ @in@
             | OpInstanceof -- ^ @instanceof@
             | OpEq -- ^ @==@
             | OpNEq -- ^ @!=@
             | OpStrictEq -- ^ @===@
             | OpStrictNEq -- ^ @!===@
             | OpLAnd -- ^ @&&@
             | OpLOr -- ^ @||@
             | OpMul -- ^ @*@
             | OpDiv -- ^ @/@
             | OpMod -- ^ @%@
             | OpSub -- ^ @-@
             | OpLShift -- ^ @<<@
             | OpSpRShift -- ^ @>>@
             | OpZfRShift -- ^ @>>>@
             | OpBAnd -- ^ @&@
             | OpBXor -- ^ @^@
             | OpBOr -- ^ @|@
             | OpAdd -- ^ @+@
    deriving (Show,Data,Typeable,Eq,Ord,Enum,Generic)

-- | Assignment operators: see spec 11.13
data AssignOp = OpAssign -- ^ simple assignment, @=@
              | OpAssignAdd -- ^ @+=@
              | OpAssignSub -- ^ @-=@
              | OpAssignMul -- ^ @*=@
              | OpAssignDiv -- ^ @/=@
              | OpAssignMod -- ^ @%=@
              | OpAssignLShift -- ^ @<<=@
              | OpAssignSpRShift -- ^ @>>=@
              | OpAssignZfRShift -- ^ @>>>=@
              | OpAssignBAnd -- ^ @&=@
              | OpAssignBXor -- ^ @^=@
              | OpAssignBOr -- ^ @|=@
  deriving (Show,Data,Typeable,Eq,Ord,Generic)

-- | Unary assignment operators: see spec 11.3, 11.4.4, 11.4.5
data UnaryAssignOp = PrefixInc -- ^ @++x@
                   | PrefixDec -- ^ @--x@
                   | PostfixInc -- ^ @x++@
                   | PostfixDec -- ^ @x--@
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

-- | Prefix operators: see spec 11.4 (excluding 11.4.4, 11.4.5)
data PrefixOp = PrefixLNot -- ^ @!@
              | PrefixBNot -- ^ @~@
              | PrefixPlus -- ^ @+@
              | PrefixMinus -- ^ @-@
              | PrefixTypeof -- ^ @typeof@
              | PrefixVoid -- ^ @void@
              | PrefixDelete -- ^ @delete@
  deriving (Show,Data,Typeable,Eq,Ord,Generic)

-- | Property names in an object initializer: see spec 11.1.5
data Prop a = PropId a (Id a) -- ^ property name is an identifier, @foo@
            | PropString a String -- ^ property name is a string, @\"foo\"@
            | PropNum a Integer -- ^ property name is an integer, @42@
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)
 
-- | Left-hand side expressions: see spec 11.2
data LValue a
  = LVar a String -- ^ variable reference, @foo@
  | LDot a (Expression a) String -- ^ @foo.bar@
  | LBracket a (Expression a) (Expression a) -- ^ @foo[bar]@
  deriving (Show, Eq, Ord, Data, Typeable, Functor,Foldable,Traversable,Generic) 

-- | Expressions, see spec 11
data Expression a
  = StringLit a String -- ^ @\"foo\"@, spec 11.1.3, 7.8
  | RegexpLit a String Bool Bool 
    -- ^ @RegexpLit a regexp global?  case_insensitive?@ -- regular
    -- expression, see spec 11.1.3, 7.8
  | NumLit a Double -- ^ @41.99999@, spec 11.1.3, 7.8
  | IntLit a Int -- ^ @42@, spec 11.1.3, 7.8
  | BoolLit a Bool -- ^ @true@, spec 11.1.3, 7.8
  | NullLit a -- ^ @null@, spec 11.1.3, 7.8
  | ArrayLit a [Expression a] -- ^ @[1,2,3]@, spec 11.1.4
  | ObjectLit a [(Prop a, Expression a)] -- ^ @{foo:\"bar\", baz: 42}@, spec 11.1.5
  | ThisRef a -- ^ @this@, spec 11.1.1
  | VarRef a (Id a) -- ^ @foo@, spec 11.1.2
  | DotRef a (Expression a) (Id a) -- ^ @foo.bar@, spec 11.2.1
  | BracketRef a (Expression a) {- container -} (Expression a) {- key -} 
    -- ^ @foo[bar@, spec 11.2.1
  | NewExpr a (Expression a) {- constructor -} [Expression a] 
    -- ^ @new foo(bar)@, spec 11.2.2
  | PrefixExpr a PrefixOp (Expression a) 
    -- ^ @\@e@, spec 11.4 (excluding 11.4.4, 111.4.5)
  | UnaryAssignExpr a UnaryAssignOp (LValue a) 
    -- ^ @++x@, @x--@ etc., spec 11.3, 11.4.4, 11.4.5
  | InfixExpr a InfixOp (Expression a) (Expression a) 
    -- ^ @e1\@e2@, spec 11.5, 11.6, 11.7, 11.8, 11.9, 11.10, 11.11
  | CondExpr a (Expression a) (Expression a) (Expression a)
    -- ^ @e1 ? e2 : e3@, spec 11.12
  | AssignExpr a AssignOp (LValue a) (Expression a)
    -- ^ @e1 \@=e2@, spec 11.13
  | ListExpr a [Expression a] -- ^ @e1, e2@, spec 11.14
  | CallExpr a (Expression a) [Expression a] -- ^ @f(x,y,z)@, spec 11.2.3
  | SuperExpr a [Expression a] -- ^ @super(x,y,z)@
  --funcexprs are optionally named
  | FuncExpr a (Maybe (Id a)) [Id a] [Statement a]
    -- ^ @function f (x,y,z) {...}@, spec 11.2.5, 13
  -- PV adding cast expression, the casted type will be in the annotation a
  | Cast a (Expression a)
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable,Generic)

-- | Case clauses, spec 12.11
data CaseClause a = CaseClause a (Expression a) [Statement a]
                    -- ^ @case e: stmts;@
                  | CaseDefault a [Statement a]
                    -- ^ @default: stmts;@
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)

-- | Catch clause, spec 12.14
data CatchClause a = CatchClause a (Id a) (Statement a) 
                     -- ^ @catch (x) {...}@
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)

-- | A variable declaration, spec 12.2
data VarDecl a = VarDecl a (Id a) (Maybe (Expression a)) 
                 -- ^ @var x = e;@
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)
  
-- | for initializer, spec 12.6
data ForInit a = NoInit -- ^ empty
               | VarInit [VarDecl a] -- ^ @var x, y=42@
               | ExprInit (Expression a) -- ^ @expr@
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)

-- | for..in initializer, spec 12.6
data ForInInit a = ForInVar (Id a) -- ^ @var x@
                 | ForInLVal (LValue a) -- ^ @foo.baz@, @foo[bar]@, @z@
 deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)
  
-- | Statements, spec 12.
data Statement a 
  = BlockStmt a [Statement a] -- ^ @{stmts}@, spec 12.1
  | EmptyStmt a -- ^ @;@, spec 12.3
  | ExprStmt a (Expression a) -- ^ @expr;@, spec 12.4
  | IfStmt a (Expression a) (Statement a) (Statement a) 
    -- ^ @if (e) stmt@, spec 12.5
  | IfSingleStmt a (Expression a) (Statement a)
    -- ^ @if (e) stmt1 else stmt2@, spec 12.5
  | SwitchStmt a (Expression a) [CaseClause a]
    -- ^ @switch (e) clauses@, spec 12.11
  | WhileStmt a (Expression a) (Statement a)
    -- ^ @while (e) do stmt@, spec 12.6
  | DoWhileStmt a (Statement a) (Expression a)
    -- ^ @do stmt while (e);@, spec 12.6
  | BreakStmt a (Maybe (Id a)) -- ^ @break lab;@, spec 12.8
  | ContinueStmt a (Maybe (Id a)) -- ^ @continue lab;@, spec 12.7
  | LabelledStmt a (Id a) (Statement a) -- ^ @lab: stmt@, spec 12.12
  | ForInStmt a (ForInInit a) (Expression a) (Statement a) 
    -- ^ @for (x in o) stmt@, spec 12.6
  | ForStmt a (ForInit a)        
              (Maybe (Expression a)) -- test
              (Maybe (Expression a)) -- increment
              (Statement a)          -- body 
    -- ^ @ForStmt a init test increment body@, @for (init; test,
    -- increment) body@, spec 12.6
  | TryStmt a (Statement a) {-body-} (Maybe (CatchClause a))
      (Maybe (Statement a)) {-finally-}
    -- ^ @try stmt catch(x) stmt finally stmt@, spec 12.14
  | ThrowStmt a (Expression a)
    -- ^ @throw expr;@, spec 12.13
  | ReturnStmt a (Maybe (Expression a))
    -- ^ @return expr;@, spec 12.9
  | WithStmt a (Expression a) (Statement a)
    -- ^ @with (o) stmt@, spec 12.10
  | VarDeclStmt a [VarDecl a]
    -- ^ @var x, y=42;@, spec 12.2
  | FunctionStmt a (Id a) {-name-} [Id a] {-args-} [Statement a] {-body-}
    -- ^ @function f(x, y, z) {...}@, spec 13
  | ClassStmt a (Id a) (Maybe (Id a)) {-extends-} [Id a] {-implem-} [ClassElt a]
    -- ^ @class C /*@ <t1, ...> {...}@
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable,Generic)

-- | Class element
-- http://www.typescriptlang.org/Content/TypeScript%20Language%20Specification.pdf
-- spec 8.1.2
data ClassElt a
  = Constructor a [Id a] {-args-} [Statement a] {-body-}
  | MemberVarDecl a Bool {-mod:pub/pri-} Bool {-static-} (VarDecl a)
  | MemberMethDecl a Bool {-mod:pub/pri-} Bool {-static-} (Id a) [Id a] [Statement a] 
--  | IndexSignature
  deriving (Show,Data,Typeable,Eq,Ord,Functor,Foldable,Traversable, Generic)  


-- | Returns 'True' if the statement is an /IterationStatement/
-- according to spec 12.6.
isIterationStmt :: Statement a -> Bool
isIterationStmt s = case s of
  WhileStmt {}   -> True
  DoWhileStmt {} -> True
  ForStmt {}     -> True
  ForInStmt {}   -> True
  _              -> False
 
------------------------------------------------------------------------------

class PP a where 
  pp :: a -> Doc

instance PP a =>  PP [Statement a] where 
  pp = stmtList 

instance PP a =>  PP (Expression a) where 
  pp = ppExpression True

instance PP a =>  PP (Statement a) where 
  pp = ppStatement

instance PP a =>  PP (CaseClause a) where
  pp = caseClause

instance PP a =>  PP [(CaseClause a)] where
  pp = caseClauseList

instance PP a =>  PP (ForInit a) where 
  pp = forInit 

instance PP a =>  PP (LValue a) where 
  pp = ppLValue 

instance PP a =>  PP InfixOp where 
  pp = infixOp 

instance PP a =>  PP AssignOp where 
  pp = assignOp

instance PP a =>  PP PrefixOp where 
  pp = prefixOp

instance PP a =>  PP (Prop a) where
  pp = prop


instance (PP a, PP b, PP c) => PP (a,b,c) where
  pp (x, y, z) = (pp x) <+> (text ":") <+> (pp y) <+> (text ":") <+> (pp z)

class HasAnnotation a where
  -- | Returns the annotation of the root of the tree
  getAnnotation :: a b -> b

instance HasAnnotation Expression where
  getAnnotation e = case e of
   StringLit a s              -> a
   RegexpLit a s g ci         -> a
   NumLit a d                 -> a
   IntLit a i                 -> a
   BoolLit a b                -> a
   NullLit a                  -> a
   ArrayLit a exps            -> a
   ObjectLit a props          -> a
   ThisRef a                  -> a
   VarRef a id                -> a
   DotRef a exp id            -> a
   BracketRef a container key -> a
   NewExpr a ctor params      -> a
   PrefixExpr a op e          -> a
   UnaryAssignExpr a op lv    -> a
   InfixExpr a op e1 e2       -> a
   CondExpr a g et ef         -> a
   AssignExpr a op lv e       -> a
   ListExpr a es              -> a
   CallExpr a fn params       -> a
   FuncExpr a mid args s      -> a
   Cast a e                   -> a
   SuperExpr a _ 	      -> a

instance HasAnnotation Statement where
  getAnnotation s = case s of
    BlockStmt a _        -> a
    EmptyStmt a          -> a
    ExprStmt a _         -> a
    IfStmt a _ _ _       -> a
    IfSingleStmt a _ _   -> a
    SwitchStmt a _ _     -> a
    WhileStmt a _ _      -> a
    DoWhileStmt a _ _    -> a
    BreakStmt a _        -> a
    ContinueStmt a _     -> a
    LabelledStmt a _ _   -> a
    ForInStmt a _ _ _    -> a
    ForStmt a _ _ _ _    -> a
    TryStmt a _ _ _      -> a
    ThrowStmt a _        -> a
    ReturnStmt a _       -> a
    WithStmt a _ _       -> a
    VarDeclStmt a _      -> a
    FunctionStmt a _ _ _ -> a
    ClassStmt a _ _ _ _  -> a
    
instance HasAnnotation LValue where
  getAnnotation lv = case lv of
    LVar a _ -> a
    LDot a _ _ -> a
    LBracket a _ _ -> a
  
instance HasAnnotation VarDecl where
  getAnnotation (VarDecl a _ _) = a

instance HasAnnotation Prop  where
  getAnnotation p = case p of
    PropId a _ -> a
    PropString a _ -> a
    PropNum a _ -> a
  
instance HasAnnotation CaseClause where
  getAnnotation c = case c of
    CaseClause a _ _ -> a
    CaseDefault a _ -> a
    
instance HasAnnotation CatchClause where
  getAnnotation (CatchClause a _ _) = a

instance HasAnnotation Id where
  getAnnotation (Id a _) = a 

instance HasAnnotation ClassElt where
  getAnnotation e = case e of
    Constructor a _ _          -> a
    MemberVarDecl a _ _ _      -> a
    MemberMethDecl a _ _ _ _ _ -> a


----------------------------------------------------------------------------



-- | Renders a list of statements as a 'String'
renderStatements :: PP a => [Statement a] -> String
renderStatements = render . stmtList

-- | Renders a list of statements as a 'String'
renderExpression :: PP a => Expression a -> String
renderExpression = render . (ppExpression True)

-- Displays the statement in { ... }, unless it is a block itself.
inBlock:: PP a => Statement a -> Doc
inBlock s@(BlockStmt _ _) = ppStatement s
inBlock s                 = ssAsBlock [s]

ssAsBlock :: PP a => [Statement a] -> Doc
asBlock f ss = lbrace $+$ nest 2 (f ss) $$ rbrace

ssAsBlock = asBlock stmtList

classEltAsBlock :: PP a => [ClassElt a] => Doc
classEltAsBlock = asBlock classEltList

ppId (Id _ str) = text str

forInit :: PP a =>  ForInit a -> Doc
forInit t = case t of
  NoInit     -> empty
  VarInit vs -> text "var" <+> cat (punctuate comma $ map (ppVarDecl False) vs)
  ExprInit e -> ppExpression False e

forInInit :: PP a =>  ForInInit a -> Doc  
forInInit t = case t of
  ForInVar id   -> text "var" <+> ppId id
  ForInLVal lv -> ppLValue lv

caseClause :: PP a =>  CaseClause a -> Doc
caseClause (CaseClause _ e ss) =
  text "case" $+$ ppExpression True e <+> colon $$ nest 2 (stmtList ss)
caseClause (CaseDefault _ ss) =
  text "default:" $$ nest 2 (stmtList ss)

ppVarDecl :: PP a =>  Bool -> VarDecl a -> Doc
ppVarDecl hasIn vd = case vd of
  VarDecl _ id Nothing  -> ppId id
  VarDecl _ id (Just e) -> ppId id <+> equals <+> ppAssignmentExpression hasIn e

ppStatement :: PP a =>  Statement a -> Doc
ppStatement s = case s of
  BlockStmt _ ss -> ssAsBlock ss
  EmptyStmt _ -> semi
  ExprStmt _ e@(CallExpr _ (FuncExpr {}) _ ) -> 
    parens (ppExpression True e) <> semi
  ExprStmt _ e -> ppExpression True e <> semi
  IfSingleStmt _ test cons -> text "if" <+> 
                              parens (ppExpression True test) $$ 
                              ppStatement cons
  IfStmt _ test cons alt -> text "if" <+> parens (ppExpression True test) $$ 
                            ppStatement cons $$ text "else" <+> ppStatement alt
  SwitchStmt _ e cases ->
    text "switch" <+> parens (ppExpression True e) $$ 
    braces (nest 2 (vcat (map caseClause cases)))
  WhileStmt _ test body -> text "while" <+> parens (ppExpression True test) $$
                           ppStatement body
  ReturnStmt _ Nothing -> text "return"
  ReturnStmt _ (Just e) -> text "return" <+> ppExpression True e
  DoWhileStmt _ s e -> 
    text "do" $$ 
    (ppStatement s <+> text "while" <+> parens (ppExpression True e) <> semi)
  BreakStmt _ Nothing ->  text "break" <> semi
  BreakStmt _ (Just label) -> text "break" <+> ppId label <> semi
  ContinueStmt _ Nothing -> text "continue" <> semi
  ContinueStmt _ (Just label) -> text"continue" <+> ppId label <> semi
  LabelledStmt _ label s -> ppId label <> colon $$ ppStatement s
  ForInStmt p init e body -> 
    text "for" <+> 
    parens (forInInit init <+> text "in" <+> ppExpression True e) $+$ 
    ppStatement body
  ForStmt _ init incr test body ->
    text "for" <+> 
    parens (forInit init <> semi <+> maybe incr (ppExpression True) <> 
            semi <+> maybe test (ppExpression True)) $$ 
    ppStatement body
  TryStmt _ stmt mcatch mfinally ->
    text "try" $$ inBlock stmt $$ ppCatch $$ ppFinally 
    where ppFinally = case mfinally of
            Nothing -> empty
            Just stmt -> text "finally" <> inBlock stmt
          ppCatch = case mcatch of
            Nothing -> empty
            Just (CatchClause _ id s) -> 
              text "catch" <+> (parens.ppId) id <+> inBlock s
  ThrowStmt _ e -> text "throw" <+> ppExpression True e <> semi
  WithStmt _ e s -> text "with" <+> parens (ppExpression True e) $$ ppStatement s
  VarDeclStmt a decls ->
    pp a $$
    text "var" <+> cat (punctuate comma (map (ppVarDecl True) decls)) <> semi
  FunctionStmt a name args body ->
    pp a $$
    text "function" <+> ppId name <> 
    parens (cat $ punctuate comma (map ppId args)) $$ 
    ssAsBlock body
  ClassStmt _ name ext imp body -> 
    text "class" <+> ppId name  <+> 
    ( case ext of 
        Just e  -> text "extends" <+> ppId e
        Nothing -> text "") <+>
    ( case imp of 
        [] -> text ""
        is -> text "implements" <+> cat (punctuate comma (map ppId is))) $$
    classEltAsBlock body

ppClassElt :: PP a =>  ClassElt a -> Doc
ppClassElt (Constructor _ args body) = 
  text "constructor" <>
  parens (cat $ punctuate comma (map ppId args)) $$ 
  ssAsBlock body
ppClassElt (MemberVarDecl _ m s vd) = 
  text (ite m "public" "private" ++ ite s " static" "") <+> 
  ppVarDecl False vd <+>
  text ";"
ppClassElt (MemberMethDecl _ m s name args body) = 
  text (ite m "public" "private" ++ ite s " static" "") <+> 
  ppId name <> 
  parens (cat $ punctuate comma (map ppId args)) $$ 
  ssAsBlock body

ite True a _  = a 
ite False _ a = a 

stmtList :: PP a =>  [Statement a] -> Doc
stmtList = vcat . map ppStatement

classEltList :: PP a =>  [ClassElt a] -> Doc
classEltList = vcat . map ppClassElt

caseClauseList :: PP a =>  [CaseClause a] -> Doc
caseClauseList = vcat . map caseClause

prop :: PP a =>  Prop a -> Doc
prop p = case p of
  PropId _ id -> ppId id
  PropString _ str -> doubleQuotes (text (jsEscape str))
  PropNum _ n -> text (show n)

infixOp op = text $ case op of
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%" 
  OpAdd -> "+" 
  OpSub -> "-"
  OpLShift -> "<<"
  OpSpRShift -> ">>"
  OpZfRShift -> ">>>"
  OpLT -> "<"
  OpLEq -> "<="
  OpGT -> ">"
  OpGEq -> ">="
  OpIn -> "in"
  OpInstanceof -> "instanceof"
  OpEq -> "=="
  OpNEq -> "!="
  OpStrictEq -> "==="
  OpStrictNEq -> "!=="
  OpBAnd -> "&"
  OpBXor -> "^"
  OpBOr -> "|"
  OpLAnd -> "&&"
  OpLOr -> "||"


prefixOp op = text $ case op of
  PrefixLNot -> "!"
  PrefixBNot -> "~"
  PrefixPlus -> "+"
  PrefixMinus -> "-"
  PrefixTypeof -> "typeof"
  PrefixVoid -> "void"
  PrefixDelete -> "delete"


assignOp op = text $ case op of
  OpAssign -> "="
  OpAssignAdd -> "+="
  OpAssignSub -> "-="
  OpAssignMul -> "*="
  OpAssignDiv -> "/="
  OpAssignMod -> "%="
  OpAssignLShift -> "<<="
  OpAssignSpRShift -> ">>="
  OpAssignZfRShift -> ">>>="
  OpAssignBAnd -> "&="
  OpAssignBXor -> "^="
  OpAssignBOr -> "|="

-- Based on:
--   http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Guide:Literals
jsEscape:: String -> String
jsEscape "" = ""
jsEscape (ch:chs) = sel ch ++ jsEscape chs where
    sel '\b' = "\\b"
    sel '\f' = "\\f"
    sel '\n' = "\\n"
    sel '\r' = "\\r"
    sel '\t' = "\\t"
    sel '\v' = "\\v"
    sel '\'' = "\\'"
    sel '\"' = "\\\""
    sel '\\' = "\\\\"
    sel x    = [x]
    -- We don't have to do anything about \X, \x and \u escape sequences.
    
regexpEscape :: String -> String
regexpEscape "" = ""
regexpEscape "\\" = "\\\\"
regexpEscape ('\\':c:rest) = '\\':c:(regexpEscape rest)
regexpEscape ('/':rest) = '\\':'/':regexpEscape rest
regexpEscape (c:rest)   = c:regexpEscape rest

ppLValue :: PP a =>  LValue a -> Doc
ppLValue (LVar _ x) = text x
ppLValue (LDot _ e x) = ppMemberExpression e <> text "." <> text x
ppLValue (LBracket _ e1 e2) = ppMemberExpression e1 <> 
                              brackets (ppExpression True e2)

-- 11.1
ppPrimaryExpression :: PP a =>  Expression a -> Doc
ppPrimaryExpression e = case e of
  ThisRef _ -> text "this"
  VarRef _ id -> ppId id
  NullLit _ -> text "null"
  BoolLit _ True -> text "true"
  BoolLit _ False -> text "false"
  NumLit  _ n -> text (show n)
  IntLit _ n ->  text (show n)
  StringLit _ str -> doubleQuotes (text (jsEscape str))
  RegexpLit _ reg g ci -> text "/" <> (text (regexpEscape reg)) <> text "/" <>
                          (if g then text "g" else empty) <> 
                          (if ci then text "i" else empty)
  ArrayLit _ es -> 
    brackets $ cat $ punctuate comma (map (ppAssignmentExpression True) es)
  ObjectLit _ xs ->  
    braces (hsep (punctuate comma (map pp' xs))) where
      pp' (n,v) =  prop n <> colon <+> ppAssignmentExpression True v
  _ -> parens $ ppExpression True e

-- 11.2
ppMemberExpression :: PP a =>  Expression a -> Doc
ppMemberExpression e = case e of
  FuncExpr _ name params body -> 
    text "function" <+> maybe name ppId <+>
    parens (cat $ punctuate comma (map ppId params)) $$ 
    ssAsBlock body
  DotRef _ obj id -> ppMemberExpression obj <> text "." <> ppId id
  BracketRef _ obj key -> 
    ppMemberExpression obj <> brackets (ppExpression True key)  
  NewExpr _ ctor args -> 
    text "new" <+> ppMemberExpression ctor <> ppArguments args
  _ -> ppPrimaryExpression e

ppCallExpression :: PP a =>  Expression a -> Doc
ppCallExpression e = case e of
  CallExpr _ f args -> ppCallExpression f <> ppArguments args
  DotRef _ obj id -> ppCallExpression obj <> text "." <> ppId id
  BracketRef _ obj key ->ppCallExpression obj <> brackets (ppExpression True key)
  _ -> ppMemberExpression e  
    
ppArguments :: PP a =>  [Expression a] -> Doc
ppArguments es = 
  parens $ cat $ punctuate comma (map (ppAssignmentExpression True) es)

ppLHSExpression :: PP a =>  Expression a -> Doc
ppLHSExpression = ppCallExpression

-- 11.3
ppPostfixExpression :: PP a =>  Expression a -> Doc
ppPostfixExpression e = case e of
  UnaryAssignExpr _ PostfixInc e' -> ppLValue e' <> text "++"
  UnaryAssignExpr _ PostfixDec e' -> ppLValue e' <> text "--"
  _ -> ppLHSExpression e
  
-- 11.4
ppUnaryExpression :: PP a =>  Expression a -> Doc
ppUnaryExpression e = case e of
  PrefixExpr _ op e' -> prefixOp op <+> ppUnaryExpression e'
  UnaryAssignExpr _ PrefixInc e' -> text "++" <> ppLValue e'
  UnaryAssignExpr _ PrefixDec e' -> text "--" <> ppLValue e'
  _ -> ppPostfixExpression e

-- 11.5
ppMultiplicativeExpression :: PP a =>  Expression a -> Doc
ppMultiplicativeExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpMul, OpDiv, OpMod] -> 
    ppMultiplicativeExpression e1 <+> infixOp op <+> ppUnaryExpression e2
  _ -> ppUnaryExpression e
  
-- 11.6
ppAdditiveExpression :: PP a =>  Expression a -> Doc
ppAdditiveExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpAdd, OpSub] -> 
    ppAdditiveExpression e1 <+> infixOp op <+> ppMultiplicativeExpression e2
  _ -> ppMultiplicativeExpression e

-- 11.7
ppShiftExpression :: PP a =>  Expression a -> Doc
ppShiftExpression e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpLShift, OpSpRShift, OpZfRShift] -> 
    ppShiftExpression e1 <+> infixOp op <+> ppAdditiveExpression e2  
  _ -> ppAdditiveExpression e

-- 11.8.  
-- | @ppRelationalExpression True@ is RelationalExpression,
-- @ppRelationalExpression False@ is RelationalExpressionNoIn
ppRelationalExpression :: PP a =>  Bool -> Expression a -> Doc
ppRelationalExpression hasIn e = 
  let opsNoIn = [OpLT, OpGT, OpLEq, OpGEq, OpInstanceof]
      ops     = if hasIn then OpIn:opsNoIn else opsNoIn
  in case e of    
    InfixExpr _ op e1 e2 | op `elem` ops -> 
      ppRelationalExpression hasIn e1 <+> infixOp op <+> ppShiftExpression e2
    _ -> ppShiftExpression e
    
-- 11.9
ppEqualityExpression :: PP a =>  Bool -> Expression a -> Doc
ppEqualityExpression hasIn e = case e of
  InfixExpr _ op e1 e2 | op `elem` [OpEq, OpNEq, OpStrictEq, OpStrictNEq] ->
    ppEqualityExpression hasIn e1 <+> infixOp op <+> 
    ppRelationalExpression hasIn e2
  _ -> ppRelationalExpression hasIn e
  
-- 11.10
ppBitwiseANDExpression :: PP a =>  Bool -> Expression a -> Doc
ppBitwiseANDExpression hasIn e = case e of
  InfixExpr _ op@OpBAnd e1 e2 -> ppBitwiseANDExpression hasIn e1 <+> 
                                 infixOp op <+>
                                 ppEqualityExpression hasIn e2
  _ -> ppEqualityExpression hasIn e
  
ppBitwiseXORExpression :: PP a =>  Bool -> Expression a -> Doc
ppBitwiseXORExpression hasIn e = case e of
  InfixExpr _ op@OpBXor e1 e2 -> ppBitwiseXORExpression hasIn e1 <+>
                                 infixOp op <+>
                                 ppBitwiseANDExpression hasIn e2
  _ -> ppBitwiseANDExpression hasIn e
  
ppBitwiseORExpression :: PP a =>  Bool -> Expression a -> Doc
ppBitwiseORExpression hasIn e = case e of
  InfixExpr _ op@OpBOr e1 e2 -> ppBitwiseORExpression hasIn e1 <+>
                                infixOp op <+>
                                ppBitwiseXORExpression hasIn e2
  _ -> ppBitwiseXORExpression hasIn e

-- 11.11
ppLogicalANDExpression :: PP a =>  Bool -> Expression a -> Doc
ppLogicalANDExpression hasIn e = case e of
  InfixExpr _ op@OpLAnd e1 e2 -> ppLogicalANDExpression hasIn e1 <+>
                                 infixOp op <+>
                                 ppBitwiseORExpression hasIn e2
  _ -> ppBitwiseORExpression hasIn e                                 
                                 
ppLogicalORExpression :: PP a =>  Bool -> Expression a -> Doc
ppLogicalORExpression hasIn e = case e of
  InfixExpr _ op@OpLOr e1 e2 -> ppLogicalORExpression hasIn e1 <+>
                                infixOp op <+>
                                ppLogicalANDExpression hasIn e2
  _ -> ppLogicalANDExpression hasIn e
  
-- 11.12
ppConditionalExpression :: PP a =>  Bool -> Expression a -> Doc
ppConditionalExpression hasIn e = case e of
  CondExpr _ c et ee -> ppLogicalORExpression hasIn c <+> text "?" <+> 
                        ppAssignmentExpression hasIn et <+> colon <+>
                        ppAssignmentExpression hasIn ee
  _ -> ppLogicalORExpression hasIn e

-- 11.13
ppAssignmentExpression :: PP a =>  Bool -> Expression a -> Doc
ppAssignmentExpression hasIn e = case e of
  AssignExpr _ op l r -> ppLValue l <+> assignOp op <+> 
                         ppAssignmentExpression hasIn r
  _ -> ppConditionalExpression hasIn e
  
-- 11.14
ppListExpression :: PP a =>  Bool -> Expression a -> Doc
ppListExpression hasIn e = case e of
  ListExpr _ es -> cat $ punctuate comma (map (ppExpression hasIn) es)
  _ -> ppAssignmentExpression hasIn e

-- PV Adding new levels for Casts
ppCastExpression :: PP a =>  Bool -> Expression a -> Doc
ppCastExpression hasIn e = case e of
  Cast _ e  ->  text "Cast" <> (parens $ ppExpression False e)
  _         -> ppListExpression hasIn e

-- PV Adding new levels for Super
ppExpression :: PP a =>  Bool -> Expression a -> Doc
ppExpression hasIn e = case e of
  SuperExpr _ es  ->  text "super" <> (ppArguments es)
  _         -> ppCastExpression hasIn e


maybe :: Maybe a -> (a -> Doc) -> Doc
maybe Nothing  _ = empty
maybe (Just a) f = f a

instance PP (SourceSpan, Maybe String) where
  pp (_, Just s)  = text "/*@" <+> text s <+> text "*/"
  pp (_, Nothing) = text ""

instance PP (RawSpec) where
  pp (RawMeas s) = text "/*@ meas" <+> text s <+> text "*/"
  pp (RawBind s) = text "/*@ bind" <+> text s <+> text "*/"
  pp (RawExtern s) = text "/*@ extern" <+> text s <+> text "*/"
  pp (RawType s) = text "/*@type" <+> text s <+> text "*/"
  pp (RawTAlias s) = text "/*@talias" <+> text s <+> text "*/"
  pp (RawPAlias s) = text "/*@palias" <+> text s <+> text "*/"
  pp (RawQual s) = text "/*@qual" <+> text s <+> text "*/"
  pp (RawInvt s) = text "/*@invt" <+> text s <+> text "*/"

instance PP [RawSpec] where 
  pp ss = cat (map pp ss)

instance PP (SourceSpan, [RawSpec]) where
  pp (_, ms) = pp ms

instance PP (SourceSpan) where
  pp (Span a b) = pp a <+> pp b

instance PP SourcePos where 
  pp = ppSourcePos 

ppSourcePos src = parens 
                $ int l <> comma <> int c
  where 
    (f,l,c)     = sourcePosElts src 

sourcePosElts s = (src, line, col)
  where 
    src         = sourceName   s 
    line        = sourceLine   s
    col         = sourceColumn s 


-- | Renders a JavaScript program as a document, the show instance of
-- 'Doc' will pretty-print it automatically
javaScript :: PP a => JavaScript a -> Doc
javaScript (Script _ ss) = stmtList ss

instance FromJSON (Expression SourceSpan)
instance FromJSON (Statement SourceSpan)
instance FromJSON (LValue SourceSpan)
instance FromJSON (JavaScript SourceSpan)
instance FromJSON (ClassElt SourceSpan)
instance FromJSON (CaseClause SourceSpan)
instance FromJSON (CatchClause SourceSpan)
instance FromJSON (ForInit SourceSpan)
instance FromJSON (ForInInit SourceSpan)
instance FromJSON (VarDecl SourceSpan)
instance FromJSON (Id SourceSpan)
instance FromJSON (Prop SourceSpan)

instance FromJSON (Expression (SourceSpan, [RawSpec]))
instance FromJSON (Statement (SourceSpan, [RawSpec]))
instance FromJSON (LValue (SourceSpan, [RawSpec]))
instance FromJSON (JavaScript (SourceSpan, [RawSpec]))
instance FromJSON (ClassElt (SourceSpan, [RawSpec]))
instance FromJSON (CaseClause (SourceSpan, [RawSpec]))
instance FromJSON (CatchClause (SourceSpan, [RawSpec]))
instance FromJSON (ForInit (SourceSpan, [RawSpec]))
instance FromJSON (ForInInit (SourceSpan, [RawSpec]))
instance FromJSON (VarDecl (SourceSpan, [RawSpec]))
instance FromJSON (Id (SourceSpan, [RawSpec]))
instance FromJSON (Prop (SourceSpan, [RawSpec]))
instance FromJSON (SourceSpan, [RawSpec])

instance FromJSON UnaryAssignOp
instance FromJSON InfixOp
instance FromJSON RawSpec
instance FromJSON PrefixOp
instance FromJSON AssignOp

instance ToJSON (Expression SourceSpan)
instance ToJSON (Statement SourceSpan)
instance ToJSON (LValue SourceSpan)
instance ToJSON (JavaScript SourceSpan)
instance ToJSON (ClassElt SourceSpan)
instance ToJSON (CaseClause SourceSpan)
instance ToJSON (CatchClause SourceSpan)
instance ToJSON (ForInit SourceSpan)
instance ToJSON (ForInInit SourceSpan)
instance ToJSON (VarDecl SourceSpan)
instance ToJSON InfixOp
instance ToJSON AssignOp
instance ToJSON (Id SourceSpan)
instance ToJSON PrefixOp
instance ToJSON (Prop SourceSpan)
instance ToJSON UnaryAssignOp
instance ToJSON SourceSpan

instance ToJSON SourcePos where
  toJSON sp = Array $ fromList [s, l, c]
    where s = String $ pack $ sourceName sp
          l = Number $ fromIntegral $ sourceLine sp
          c = Number $ fromIntegral $ sourceColumn sp

instance FromJSON SourcePos where
  parseJSON (Array v) = do
    v0 <- parseJSON (v!0) :: Parser String 
    v1 <- parseJSON (v!1) :: Parser Int
    v2 <- parseJSON (v!2) :: Parser Int
    return $ newPos v0 v1 v2
  parseJSON _ = error "SourcePos should only be an A.Array" 

instance FromJSON SourceSpan  

getJSON :: MonadIO m => FilePath -> m B.ByteString
getJSON = liftIO . B.readFile

instance (PP a, PP b) => PP (Either a b) where
  pp (Left l) = pp l
  pp (Right r) = pp r

instance PP [Char] where
  pp = ptext

data RawSpec
  = RawMeas   String
  | RawBind   String
  | RawExtern String
  | RawType   String
  | RawTAlias String
  | RawPAlias String
  | RawQual   String 
  | RawInvt   String
  deriving (Show,Eq,Ord,Data,Typeable,Generic)



decodeOrDie s = 
  case eitherDecode s :: Either String [Statement (SourceSpan, [RawSpec])] of
    Left msg -> error msg
    Right p  -> p

parseScriptFromJSON' filename = do
  chars <- getJSON filename
  let src = decodeOrDie chars 
  return $ pp src

parseScriptFromJSON :: MonadIO m => FilePath -> m Doc
parseScriptFromJSON = liftIO . parseScriptFromJSON'

main :: IO ()
main = return ()

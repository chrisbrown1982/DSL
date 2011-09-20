{-# LANGUAGE ViewPatterns #-}
module Parse where

import Data.Char
import System.IO
import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Debug.Trace

type Prog = [ Defn ]


data Expr = Ident String SourcePos SourcePos
          | Con String SourcePos SourcePos [ Expr ]
          | App Expr Expr
          | Lit Literal
    deriving Show

data Literal = Str String 
             | In Integer
             | Ch Char
    deriving Show


data Defn = Fun Name [ Pat ] Expr
          | Dat TyName [ Constr ]
          | TypeSig Name Typ
    deriving Show

data Typ = TyApp Typ Typ
         | TyArrow [Typ]
         | TyName TyName
    deriving Show

data Pat = PatIdent String SourcePos SourcePos
         | PatConstr String SourcePos SourcePos [ Pat ]
    deriving Show

data Name = DefIdent String SourcePos SourcePos
    deriving Show

data TyName = TyIdent String SourcePos SourcePos
            | TyConstr String SourcePos SourcePos
    deriving Show

data Constr = ConName String SourcePos SourcePos [ String ]
    deriving Show


-----------------------------------------------------------------------------------------------------

applyCon :: Expr -> Expr
applyCon (App (applyCon -> Con s s1 s2 as) e) = Con s s1 s2 (as ++ [applyCon e])
applyCon (App (applyCon -> l) e) = App l (applyCon e)
applyCon e = e

languageDef = 
    emptyDef {  Token.commentStart      = "{-"
             ,  Token.commentEnd        = "-}"
             ,  Token.commentLine       = "--"
             ,  Token.identStart        = letter
             ,  Token.identLetter       = alphaNum
             ,  Token.reservedNames     = [ "if"
                                          , "then"
                                          , "else"
                                          , "data"
                                          ]
             ,  Token.reservedOpNames   = [ "=" 
                                          , "("
                                          , ")"
                                          , "Ty"
                                          , "->"
                                          ]
            }


lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved lexer
parens     = Token.parens lexer
whiteSpace = Token.whiteSpace lexer
integer    = Token.integer lexer
semi       = Token.semi lexer
reservedOp = Token.reservedOp lexer
stringLiteral = Token.stringLiteral lexer
charLiteral = Token.charLiteral lexer

-----------------------------------------------------------------------------------------------------

whileParser :: Parser Prog
whileParser = do 
    i <- many datDefinition 
    l <- many definition
    return (i++l)


definition :: Parser Defn
definition = typDefinition <|> funDefinition


typDefinition :: Parser Defn
typDefinition = do
                    pos <- getPosition;
                    reservedOp "Ty";
                    ident <- identifier;
                    typ <- types
                    reservedOp ";"
                    return (TypeSig (DefIdent ident pos pos) typ)


{- data Typ = TyApp Typ Typ
         | TyArrow Typ Typ
         | TyName TyName
         | TyCon TyName
    deriving Show

data TyName = TyIdent String SourcePos SourcePos
            | TyConstr String SourcePos SourcePos
    deriving Show
-}

types :: Parser Typ
types = tyarrow
    <|> tyapp
    <|> tycon
    <|> tyname


tyapp :: Parser Typ
tyapp = TyApp <$> (reservedOp "(" >> (tycon <|> tyname)) <*> ((tycon <|> tyname) =>> reservedOp ")")

tyarrow :: Parser Typ
tyarrow = TyArrow <$> sepBy (tycon <|> tyname <|> tyapp) (reservedOp "->") 

tyname :: Parser Typ
tyname = do pos <- getPosition
            ident <- identifier
            return (TyName (TyIdent ident pos pos))

tycon :: Parser Typ
tycon = try (getPosition >>= \s -> identifier >>= \i -> when (isLower $ head i) (fail "Constructor") >> return (TyName (TyConstr i s s)))


funDefinition :: Parser Defn
funDefinition = Fun <$> funName <*> many pat <*> (reservedOp "=" >> applyCon <$> expression =>> semi)

datDefinition :: Parser Defn
datDefinition = Dat <$> (reserved "data" >> typ ) <*> (reservedOp "=" >> sepBy1 constr (char '|' ) =>> semi)

constr :: Parser Constr
constr = ConName <$> (whiteSpace >> up) <*> getPosition <*> getPosition <*> (many identifier) 

literal :: Parser Literal
literal =  (Str <$> stringLiteral)
       <|> (In <$> integer)
       <|> (Ch <$> charLiteral)

up :: Parser String
up = identifier >>= \i -> when (isLower $ head i) (fail "Constructor") >> return i


(=>>) :: Monad m => m a -> m b -> m a
m =>> m' = m >>= \v -> m' >> return v

pat :: Parser Pat
pat = patIdent
  <|> patConstr

patIdent :: Parser Pat
patIdent = do pos <- getPosition
              ident <- identifier
              return (PatIdent ident pos pos)


patConstr :: Parser Pat
patConstr = do
                p1 <- optionMaybe (reservedOp "(")
                pos <- getPosition
                i  <- up
                
                pats <- many pat
                case pats of 
                    [] -> do
                            case p1 of
                                Nothing -> return (PatConstr i pos pos [])
                                Just x  -> do
                                             reservedOp ")"
                                             return (PatConstr i pos pos [])
                    (x:xs) -> do
                                case p1 of
                                    Nothing -> fail "Missing ( on pattern"
                                    Just g  -> do 
                                                    reservedOp ")"
                                                    return (PatConstr i pos pos (x:xs))


typ :: Parser TyName
typ = do
        pos <- getPosition
        u <- up
        return (TyIdent u pos pos)


funName :: Parser Name
funName = do
            pos <- getPosition
            ident <- identifier
            return (DefIdent ident pos pos)

expression :: Parser Expr
expression = applify1 <$> many1 (parens expression <|> constructor <|> variable <|> (Lit <$>  literal))
                    

applify1 (v:vs) = applify v vs 

applify a [] = a
applify a (v:vs) = applify (App a v) vs

constructor :: Parser Expr
constructor = try (getPosition >>= \s -> identifier >>= \i -> when (isLower $ head i) (fail "Constructor") >> return (Con i s s []))

variable :: Parser Expr
variable = do
              pos <- getPosition
              ident <- identifier
              return (Ident ident pos pos)
         
parseString :: String -> Prog
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r
 
parseFile :: String -> IO Prog
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> do
                     -- print r 
                     let (_, _, t) = traverse [] [] r 
                     return t


-------------------------------------------------------------------------------------------------
--
-- build a stack tracer that resolves bindings in the tree
--
-------------------------------------------------------------------------------------------------
{-
data Expr = Ident String SourcePos SourcePos
          | Con String SourcePos SourcePos [ Expr ]
          | App Expr Expr
          | Lit Literal
    deriving Show

data Literal = Str String 
             | In Integer
             | Ch Char
    deriving Show


data Defn = Fun Name [ Pat ] Expr
          | Dat TyName [ Constr ]
          | TypeSig Name Typ
    deriving Show

data Pat = PatIdent String SourcePos SourcePos
         | PatConstr String SourcePos SourcePos [ Pat ]
    deriving Show

data Name = DefIdent String SourcePos SourcePos
    deriving Show

data TyName = TyIdent String SourcePos SourcePos
    deriving Show

data Constr = ConName String SourcePos SourcePos [ String ]
    deriving Show

data Typ = TyApp Typ Typ
         | TyArrow Typ Typ
         | TyName TyName
         | TyCon TyName
    deriving Show

data TyName = TyIdent String SourcePos SourcePos
            | TyConstr String SourcePos SourcePos
    deriving Show


-}

type LocalEnv = [ (String, SourcePos) ]

type GlobalEnv = [ (String, SourcePos) ]


traverse :: GlobalEnv -> LocalEnv -> Prog -> (GlobalEnv, LocalEnv, Prog)
traverse genv lenv [] = (genv, lenv, [])
traverse genv lenv (d:ds) = (genv2, lenv2, def:defs)
        where  (genv1, lenv1, def) = traverseDefn genv [] d 
               (genv2, lenv2, defs) = traverse genv1 lenv1 ds 

traverseDefn :: GlobalEnv -> LocalEnv -> Defn -> (GlobalEnv, LocalEnv, Defn)
traverseDefn genv lenv a@(TypeSig n t) 
  = (genv, lenv, a)



traverseDefn genv lenv (Dat n cons) = dat
    where
        dat = (genv3, [], Dat n cons)
        (genv3, [], newCons) = traverseCons genv [] cons

traverseDefn genv lenv (Fun n pats e) = defn
    where
        genv1 = pushNameStack n genv
        (genv2, lenv2, pats2) = traversePats genv1 lenv pats
        defn = (genv2, lenv2, Fun n pats2 newE)
        newE = fixBindings e lenv2 genv2


traverseCons :: GlobalEnv -> LocalEnv -> [ Constr ] -> (GlobalEnv, LocalEnv, [ Constr ])
traverseCons genv lenv [] = (genv, lenv, [])
traverseCons genv lenv (c:cs)
    = traverseCons genv1 [] cs
    where
        genv1 = pushConStack c genv

    
traversePats :: GlobalEnv -> LocalEnv -> [ Pat ] -> (GlobalEnv, LocalEnv, [ Pat ])
traversePats genv lenv [] = (genv, lenv, [])
traversePats genv lenv (p:ps) = (genv2, lenv2, p:pats)
        where
            lenv1 = case p of
                      (PatIdent n s1 s2) ->  pushPatStack p lenv
                      (PatConstr n s1 s2 pats') -> pushPatsStack pats' lenv
            (genv2, lenv2, pats) = traversePats genv lenv1 ps

pushPatStack :: Pat -> LocalEnv -> LocalEnv
pushPatStack (PatIdent n s1 s2) env = (n, s1) : env

pushPatsStack :: [ Pat ] -> LocalEnv -> LocalEnv
pushPatsStack [] env = env
pushPatsStack ((PatConstr n s1 s2 pats):ps) env = pushPatsStack ps (pushPatsStack pats env)
pushPatsStack ((PatIdent n s1 s2):ps) env = pushPatsStack ps ((n, s1) : env)

pushNameStack :: Name -> GlobalEnv -> GlobalEnv
pushNameStack (DefIdent n s1 s2) env = (n, s1) : env

pushDatNameStack :: TyName -> GlobalEnv -> GlobalEnv
pushDatNameStack (TyIdent n s1 s2) env = (n, s1) : env

pushConStack :: Constr -> GlobalEnv -> GlobalEnv
pushConStack (ConName n s1 s2 ts) env = (n, s1) : env

fixBindings :: Expr -> LocalEnv -> GlobalEnv -> Expr
fixBindings l@(Lit _) _ _ = l
fixBindings (App e1 e2) l g = App (fixBindings e1 g l) (fixBindings e2 g l)
fixBindings (Ident n s1 s2) l g = Ident n s1' s2
    where
        s1' = case checkEnv n l g of
                Nothing -> error ("Identifier: " ++ n ++ " is not bound!")
                Just x  -> x
fixBindings (Con n s1 s2 es) l g = Con n s1' s2 es'
    where
        s1' = case checkEnv n [] g of
                Nothing -> error ("Constructor: " ++ n ++ " is not bound!" ++ (show l))
                Just x  -> x
        es' = mapBinds es l g
        mapBinds [] l g = []
        mapBinds (e:es) l g = fixBindings e l g : mapBinds es l g
    

checkEnv :: String -> LocalEnv -> GlobalEnv -> Maybe SourcePos
checkEnv n locals globals 
  = case n `inEnv` locals of
        Nothing -> n `inEnv` globals
        Just x  -> Just x

inEnv :: String -> [ (String, SourcePos) ] -> Maybe SourcePos
inEnv _ [] = Nothing
inEnv n ( (n2, s):xs) 
 | n == n2 = Just s 
 | otherwise = inEnv n xs

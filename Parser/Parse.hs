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


data Expr = Ident String
          | Con String [ Expr ]
          | App Expr Expr
          | Lit Literal
    deriving Show

data Literal = Str String 
             | In Integer
             | Ch Char
    deriving Show


data Defn = Fun Name [ Pat ] Expr
          | Dat TyName [ Constr ]
    deriving Show

data Pat = PatIdent String
    deriving Show

data Name = DefIdent String
    deriving Show

data TyName = TyIdent String
    deriving Show

data Constr = ConName String [ String ]
    deriving Show



applyCon :: Expr -> Expr
applyCon (App (applyCon -> Con s as) e) = Con s (as ++ [applyCon e])
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

whileParser :: Parser Prog
whileParser = do 
    i <- many datDefinition 
    l <- many funDefinition
    return (i++l)

funDefinition :: Parser Defn
funDefinition = Fun <$> (DefIdent <$> identifier) <*> many pat <*> (reservedOp "=" >> applyCon <$> expression =>> semi)

datDefinition :: Parser Defn
datDefinition = Dat <$> (reserved "data" >> TyIdent <$> up) <*> (reservedOp "=" >> sepBy1 constr (char '|' ) =>> semi)

constr :: Parser Constr
constr = ConName <$> (whiteSpace >> up) <*> (many identifier )

literal :: Parser Literal
literal =  (Str <$> stringLiteral)
       <|> (In <$> integer)
       <|> (Ch <$> charLiteral)

up :: Parser String
up = identifier >>= \i -> when (isLower $ head i) (fail "Constructor") >> return i


(=>>) :: Monad m => m a -> m b -> m a
m =>> m' = m >>= \v -> m' >> return v

pat :: Parser Pat
pat = do pat <- identifier
         return (PatIdent pat)

funName :: Parser Name
funName = do var <- identifier
             return (DefIdent var)

expression :: Parser Expr
expression = applify1 <$> many1 (parens expression <|> constructor <|> variable <|> ( Lit <$>  literal))
                    

applify1 (v:vs) = applify v vs 

applify a [] = a
applify a (v:vs) = applify (App a v) vs

constructor :: Parser Expr
constructor = try (identifier >>= \i -> when (isLower $ head i) (fail "Constructor") >> return (Con i []))

variable :: Parser Expr
variable = do
                var <- identifier
                return (Ident var)
         
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
       Right r -> return r

module Transformation where

-- import Data.Generics.Uniplate.Data
import Data.Char
import System.IO
import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Parse
import Text.Parsec.Pos

-- rewrite this to work over Core...

transformDecls :: (Defn -> Defn) -> Prog -> Prog
transformDecls f (d:ds) = map f (d:ds)


transformExp :: (Expr -> Expr) -> Prog -> Prog
transformExp f [] = []
transformExp f (Fun n p e:ds) = Fun n p (f e) : transformExp f ds
transformExp f (d:ds) = d : transformExp f ds

------------------------------------------
--
-- conditions
--
------------------------------------------

-- a query takes a query function, returning
-- the type of its node (a)
-- and collects these together, forming [a]
-- 

queryDecls :: (Defn -> a) -> Prog -> [a]
queryDecls f (d:ds) = map f (d:ds)


queryExps :: (Expr -> a) -> Prog -> [a]
queryExps f [] = []
queryExps f (Fun n p e:ds) = f e : transformExp f ds
queryExps f (d:ds) = transformExp f ds


------------------------------------------




------------------------------------------

renameTyp :: String -> String -> Defn -> Defn
renameTyp oldName newName (TypeSig n t)
  | fromName n == oldName = TypeSig (toName newName) t
renameTyp _ _ d = d


addArgTyp :: String -> String -> Defn -> Defn 
addArgTyp oldName newArg (TypeSig n t)
  | fromName n == oldName = TypeSig n (TyApp (TyName (toTyName newArg)) t)
addArgTyp _ _ t = t


renameDef :: String -> String -> Defn -> Defn
renameDef oldName newName (Fun n ps e)
    | fromName n == oldName = Fun (toName newName) ps e
renameDef _ _ d = d

addArgDef :: String -> String -> Defn -> Defn
addArgDef newArg name (Fun n ps e)
   | fromName n == name = Fun n ((toPat newArg) : ps) e
addArgDef _ _ d = d

renameCall :: String -> String -> Expr -> Expr
renameCall oldName newName (Ident n s1 s2) 
 | n == oldName = Ident newName s1 s2
renameCall _ _ x = x

addArgExp :: String -> String -> Expr -> Expr
addArgExp oldName arg (Ident n s1 s2)
 | n == oldName = App (Ident n s1 s2) (Ident arg loc0 loc0)
addArgExp o a (App e1 e2) = App (addArgExp o a e1) (addArgExp o a e2)
addArgExp o a (Con x y z es) = Con x y z (map (addArgExp o a) es)
addArgExp _ _ x = x

-----------------------------------------

fromName :: Name -> String
fromName (DefIdent n _ _) = n

toName :: String -> Name
toName s = DefIdent s loc0 loc0

toTyName :: String -> TyName
toTyName s = TyIdent s loc0 loc0

toPat :: String -> Pat
toPat s = PatIdent s loc0 loc0

loc0 = newPos "s" 0 0 

transformFile tran file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> do
                     -- print r 
                     let (_, _, t) = traverse [] [] r 
                     return (tran t)


reDef = transformFile typ "../Examples/renaming.txt"
        where
            typ x = transformDecls (renameTyp "f" "bob") (exp x) 
            exp x = transformExp (renameCall "f" "bob") (def x)
            def x = transformDecls (renameDef "f" "bob") x 

addArg = transformFile typ "../Examples/renaming.txt"
        where
            def x = transformDecls (addArgDef "n" "f") x
            exp x = transformExp (addArgExp "f" "undefined") (def x)
            typ x = transformDecls (addArgTyp "f" "t") (exp x)            


genDef = transformFile typ "../Examples/renaming.txt"
        where
            def x = transformDecls (addArgDef "n" "f") x
            exp x = transformExp (renameCall "f" "1") (def x)
            typ x = transformDecls (addArgtyp "f" "t") (exp x)





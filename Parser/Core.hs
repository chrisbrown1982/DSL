module Core where

data Expr = data Expr = Ident String SourcePos SourcePos
          | Con String SourcePos SourcePos [ Expr ]
          | App Expr Expr
          | Lit Literal
          | Case Pat [ Alt ]

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
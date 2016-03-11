-- | An abstract syntax tree for core Typed Lua
-- | Based on figures 4.3 and 4.1 in
-- | Typed Lua: An Optional Type System for Lua by André Murbach Maidl
-- | https://github.com/andremm/typedlua/blob/master/doc/thesis/thesis_andre_certified.pdf
module AST where

data Statement = SkipStmt
               | SeqStmt Statement Statement
               | MultAssStmt [LeftHandValue] ExprList
               | CtrlFlowStmt
               | VarDeclStmt
               | RecDeclStmt VarExpr FType Expression Statement
               | ReturnStmt
               | ApplStmt Application
               | MethodDeclStmt
data CtrlFlowStmt = WhileStmt Expression Statement
                  | IfStmt Expression Statement Statement

data VarDeclStmt = TypVarDecl [(VarExpr,FType)] ExprList Statement
                 | NoTypVarDecl [VarExpr] ExprList Statement

data MethodDeclStmt = MethDecl VarExpr VarExpr ParList SType Statement ReturnStmt

newtype ReturnStmt = Return ExprList

data Expression = NilExpr
                | Literal
                | VarExpr
                | TablExr Expression Expression
                | TypCoercExpr FType VarExpr
                | FunDeclExpr ParList SType Statement ReturnStmt
                | TablConsExpr
                | BinOpExpr
                | UnOpExpr
                | OneResExpr MultRes

newtype VarExpr = Var Name

type Name = String

data TablConsExpr = TablCons [(Expression,Expression)]
                  | TableConsMult [(Expression,Expression)] MultRes

data BinOpExpr = Plus Expression Expression
               | Concat Expression Expression
               | Equal Expression Expression
               | LessThan Expression Expression
               | BitAnd Expression Expression
               | And Expression Expression
               | Or Expression Expression

data UnOpExpr = Not Expression | Length Expression

data LeftHandValue = LeftVar VarExpr
                   | LeftTable Expression Expression
                   | LeftTypeCoerc VarExpr Expression ValueType

data LitConst = FalseLC | TrueLC | IntLC | FloatLC | StringLC

data ExprList = ExList [Expression] | ExListMult [Expression] MultRes

data MultRes = MRApp Application
             | VarArg

data Application = FuncApp Expression ExprList
                 | MethApp Expression

data ParList = Params [(VarExpr,FType)] | ParamsLists [[(VarExpr,FType)]]

-- Types
data FType = LiteralType
           | BaseType
           | NilType
           | TopType
           | DynamicType
           | SelfType
           | UnionType FType FType
           | FunType SType SType
           | TableType Tag [(FType,ValueType)]
           | TypeVar
           | RecType FType

data Tag = Unique | Open | Fixed | Closed

data LiteralType = FalseLiteral
                 | TrueLiteral
                 | IntLiteral
                 | FloatLiteral
                 | StringLiteral

data BaseType = BoolType
              | IntType
              | NumType
              | StringType

data ValueType = ValType FType | ConstValType FType

data SType = TupleType | TupleUnion SType SType

data TupleType = VariadicType FType | PairType FType TupleType

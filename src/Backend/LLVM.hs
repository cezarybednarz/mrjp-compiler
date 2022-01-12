module Backend.LLVM where
import Latte.Abs (MulOp)

data Val = VConst Integer
         | VReg Reg
         | VGetElementPtr Int String
         | VTrue
         | VFalse
         | VUndef
         | VNull 
  deriving (Eq, Ord, Show)

data Reg = Reg Integer 
         | RArg String 
  deriving (Eq, Ord, Show)

newtype Label = Label Int 
  deriving (Eq, Ord, Show)

data Type = Ti64
          | Ti32
          | Tvoid
          | Ti1
          | Ti8
          | Ptr Type 
  deriving (Eq, Ord, Show)

data LLVMStmt = Call Type String [(Type, Val)] (Maybe Reg)
              | RetVoid
              | Ret Type Val
              | Arithm Type Val Val ArithmOp Reg
              | Br Label
              | BrCond Type Val Label Label
              | BlockLabel Label
              | Load
              | Store Type Val Type Reg
              | Alloca Type Reg
              | Cmp Cond Type Val Val Reg
              -- todo phi 
              | Unreachable
              | GetElementPtr Type Type Val Type Val Reg
              | Bitcast Type Val Type Reg
              | Sext Type Val Type Reg
  deriving (Eq, Ord, Show)

data Block = Block { bLabel :: Label,
                     bStmts :: [LLVMStmt],
                     bExit :: LLVMStmt }
  deriving (Eq, Ord, Show)

data Cond = RelEQ 
          | RelNE
          | RelSGT
          | RelSGE
          | RelSLT
          | RelSLE
  deriving (Eq, Ord, Show)

data ArithmOp = Add
              | Sub
              | Mul
              | Div
              | Rem
  deriving (Eq, Ord, Show)
  

data FnType = FnType Type [Type]
  deriving (Eq, Ord, Show)
              
data Fn = Fn { fType :: Type,
               fName :: String,
               fArgs :: [(Type, String)],
               fBlocks :: [Block] }
  deriving (Eq, Ord, Show)

data Constant = Constant Int String String
  deriving (Eq, Ord, Show)

data Declare = Declare String FnType
  deriving (Eq, Ord, Show)

data Module = Module { globals :: [Constant],
                       declares :: [Declare],
                       functions :: [Fn]}
  deriving (Eq, Ord, Show)

-- todo print llvm code of all data types


module Backend.LLVM where

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

newtype Label = Label Integer 
  deriving (Eq, Ord, Show)

data Type = Ti64
          | Ti32
          | Tvoid
          | Ti1
          | Ti8
          | Ptr Type 
  deriving (Eq, Ord, Show)

data LLVMStmt = Call (Maybe Reg) Type String [(Type, Val)] 
              | RetVoid
              | Ret Type Val
              | Arithm Reg Type Val Val ArithmOp
              | Br Label
              | BrCond Type Val Label Label
              | BlockLabel Label
              | Load Reg Type Type Reg 
              | Store Type Val Type Reg
              | Alloca Reg Type 
              | Cmp Reg Cond Type Val Val 
              -- todo phi 
              | Unreachable
              | GetElementPtr Reg Type Type Val Type Val 
              | Bitcast Reg Type Val Type 
              | Sext Reg Type Val Type 
  deriving (Eq, Ord, Show)

data LLBlock = LLBlock { bLabel :: Label,
                     bStmts :: [LLVMStmt]}
                     -- todo bExit LLVMStmt ??
  deriving (Eq, Ord, Show)

data FnType = FnType Type [Type]
  deriving (Eq, Ord, Show)
              
data Fn = Fn { fType :: Type,
               fName :: String,
               fArgs :: [(Type, String)],
               fBlocks :: [LLBlock] }
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

data Constant = Constant Int String String
  deriving (Eq, Ord, Show)

data Declare = Declare String FnType
  deriving (Eq, Ord, Show)

data Module = Module { globals :: [Constant],
                       declares :: [Declare],
                       functions :: [Fn]}
  deriving (Eq, Ord, Show)

-- converters between llvm and latte types --
-- todo

-- print llvm code from in-memory structures -- 
-- todo print llvm code of all data types


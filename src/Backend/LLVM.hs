module Backend.LLVM where

data Val = VConst Integer
         | VReg Reg
         | VGetElementPtr Int String
         | VTrue
         | VFalse
         | VUndef
         | VNull
  deriving (Eq, Ord)

data Reg = Reg Integer
         | RArg String
  deriving (Eq, Ord)

newtype Label = Label Integer
  deriving (Eq, Ord)

data Type = Ti64
          | Ti32
          | Tvoid
          | Ti1
          | Ti8
          | Ptr Type
  deriving (Eq, Ord)


data LLVMStmt = Call (Maybe Reg) Type String [(Type, Val)]
              | RetVoid
              | Ret Type Val
              | Arithm Reg Type Val Val ArithmOp
              | Br Label
              | BrCond Type Val Label Label
              | Load Reg Type Type Reg
              | Store Type Val Type Reg
              | Alloca Reg Type
              | Cmp Reg Cond Type Val Val
              -- todo phi 
              | Unreachable
              | GetElementPtr Reg Type Type Val Type Val
              | Bitcast Reg Type Val Type
              | Sext Reg Type Val Type
  deriving (Eq, Ord)

data LLBlock = LLBlock { bLabel :: Label,
                     bStmts :: [LLVMStmt]}
                     -- todo bExit LLVMStmt ??
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

data StrConstant = StrConstant Int String String
  deriving (Eq, Ord, Show)

-- show for llvm types --
instance Show Reg where
  show (Reg i) = "%" ++ show i 
  -- show (RArg String) = 

instance Show Val where 
  show (VConst i) = show i
  show (VReg reg) = show reg
  -- show (VGetElementPtr Int String) =
  -- show VTrue = 
  -- show VFalse = 
  -- show VUndef = 
  -- show VNull = 

instance Show Label where
  show (Label l) = "L" ++ show l ++ ":"

instance Show Type where
  show Ti64 = "i64"
  show Ti32 = "i32"
  show Tvoid = "void"
  show Ti1 = "i1"
  show Ti8 = "i8"
  show (Ptr t) = "*" ++ show t

instance Show LLVMStmt where
  --show (Call (Maybe Reg) Type String [(Type, Val)]) =
  show RetVoid = "ret void"
  show (Ret t val) = "ret " ++ show t ++ " " ++ show val
  -- show (Arithm Reg Type Val Val ArithmOp) = 
  -- show (Br Label) = 
  -- show (BrCond Type Val Label Label) = 
  -- show (Load Reg Type Type Reg) = 
  show (Store t1 v1 t2 r) = "store " ++ show t1 ++ " " ++ show v1 ++ " " ++ show t2 ++ " " ++ show r
  show (Alloca reg t) = show reg ++ " = alloca " ++ show t
  -- show (Cmp Reg Cond Type Val Val) = 
  -- show (Unreachable) = 
  -- show (GetElementPtr Reg Type Type Val Type Val) = 
  -- show (Bitcast Reg Type Val Type) = 
  -- show (Sext Reg Type Val Type) = 
   
-- print llvm code from in-memory structures -- 

printStmt :: LLVMStmt -> String
printStmt stmt = "  " ++ show stmt

printStrConstants :: [StrConstant] -> [String]
printStrConstants strContants = []
  -- todo

printLLBlocks :: Bool -> [LLBlock] -> [String]
printLLBlocks _ [] = []
printLLBlocks first (block:blocks) = do
  let stmts = bStmts block
  let blockStmts = map printStmt (reverse stmts)
  let nextBlockStmts = printLLBlocks False blocks
  if not first then
    show (bLabel block) : blockStmts ++ nextBlockStmts
  else
    blockStmts ++ nextBlockStmts

printArgs :: Bool -> Integer -> [(Type, String)] -> String
printArgs _ _ [] = []
printArgs first register ((t, _):args) =
  (if first then "" else ", ")
  ++
  show t ++ " " 
  ++
  show (Reg register)
  ++
  printArgs False (register + 1) args
 

printFunctions :: [Fn] -> [String]
printFunctions [] = []
printFunctions (fn:fns) = 
  [
    "\ndefine " ++ show (fType fn) ++ " @" ++ fName fn 
    ++ "(" ++ printArgs True 0 (fArgs fn) ++ ") {"
  ]
  ++
  printLLBlocks True (reverse $ fBlocks fn)
  ++
  ["}"]
  ++
  printFunctions fns

printLLVMProgram :: [StrConstant] -> [Fn] -> [String]
printLLVMProgram strConstants fns =
  [
    "declare void @printInt(i32)",
    "declare void @printString(i8*)",
    "declare i32 @readInt()",
    "declare i8* @readString()",
    "declare void @error()",
    "declare i32 @equStrings(i8*, i8*)",
    "declare i32 @neStrings(i8*, i8*)",
    "declare i8* @concatStrings(i8*, i8*)",
    "declare i32 @compareStrings(i8*, i8*)"
  ]
  ++
  printStrConstants strConstants
  ++
  printFunctions (reverse fns)


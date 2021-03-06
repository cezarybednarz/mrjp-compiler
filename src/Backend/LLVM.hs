module Backend.LLVM where
import           Data.Map as Map

data Val = VConst Integer
         | VReg Reg
         | VGetElementPtr Int Int String -- id length value
         | VTrue
         | VFalse
         | VNone
         -- array
         | VArr Reg Type Val -- array register, type, length
  deriving (Eq, Ord)

newtype Reg = Reg Integer
  deriving (Eq, Ord)

newtype Label = Label Integer
  deriving (Eq, Ord)

data Type = Ti64
          | Ti32
          | Tvoid
          | Ti1
          | Ti8
          | Ptr Type
          | TArr
  deriving (Eq, Ord)


data LLVMStmt = Call Reg Type String [(Type, Val)]
              | CallVoid Type String [(Type, Val)]
              | Ret Type Val
              | RetVoid
              | Arithm Reg Type Val Val ArithmOp
              | Br Label
              | BrCond Type Val Label Label
              | Load Reg Type Type Reg
              | Store Type Val Type Reg
              | Alloca Reg Type
              | Cmp Reg Cond Type Val Val
              | Xor Reg Type Val Val
              | Phi Reg Type [(Val, Label)]
              -- arrays --
              | CallArr Reg Type String [(Type, Val)]
              | RetArr Type Val
              | AllocaArr Reg Type 
              | StoreArr Type Val Type Reg
              | LoadArr Reg Type Type Reg
              | GetElementPtrArr Reg Type Type Reg Type Val
              | GetElementPtrRetArr Reg Type Type Reg Type Val Type Val
              | Sext Reg Type Val Type
              | Bitcast Reg Type Val Type
  deriving (Eq, Ord)

data LLBlock = LLBlock { bLabel    :: Label,
                         bStmts    :: [LLVMStmt],
                         bPhis     :: Map.Map Reg (Type, [(Val, Label)]),
                         bInBlocks :: [Label]
                  }
  deriving (Eq, Ord, Show)

data Fn = Fn { fType        :: Type,
               fName        :: String,
               fArgs        :: [(Type, String)],
               fBlocks      :: Map.Map Label LLBlock,
               fMaxRegister :: Reg}
  deriving (Eq, Ord, Show)

data Cond = RelEQ
          | RelNE
          | RelSGT
          | RelSGE
          | RelSLT
          | RelSLE
  deriving (Eq, Ord)

data ArithmOp = Add
              | Sub
              | Mul
              | Div
              | Rem
  deriving (Eq, Ord)

data StrConstant = StrConstant Int Int String -- id length value
  deriving (Eq, Ord, Show)

data LLVMProgram = LLVMProgram {
  pStrConstants :: [StrConstant],
  pCurrReg      :: Reg,
  pFunctions    :: [Fn]
}
  deriving (Show, Eq, Ord)

-- helper for arrays -- 

getArrayStructId :: Type -> Integer
getArrayStructId t = 
  case t of 
    (Ptr Ti32) -> 1
    (Ptr Ti1) -> 2
    (Ptr (Ptr Ti8)) -> 3

isArrayType :: Type -> Bool
isArrayType t = 
  case t of 
    (Ptr Ti32) -> True
    (Ptr Ti1) -> True
    (Ptr (Ptr Ti8)) -> True
    _ -> False

-- show for llvm types --

instance Show Cond where
  show RelEQ  = "eq"
  show RelNE  = "ne"
  show RelSGT = "sgt"
  show RelSGE = "sge"
  show RelSLT = "slt"
  show RelSLE = "sle"

instance Show ArithmOp where
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show Div = "sdiv"
  show Rem = "srem"

instance Show Reg where
  show (Reg i) = "%r" ++ show i

instance Show Val where
  show (VConst i) = show i
  show (VReg reg) = show reg
  show (VGetElementPtr id len str) =
    "getelementptr inbounds ([" ++ show len ++ " x i8], ["
    ++ show len ++ " x i8]* @.str." ++ show id ++ ", i32 0, i32 0)"
  show VTrue = "true"
  show VFalse = "false"
  show VArr {} = "backend error: show for VNewArr"
  show VNone = "backend error: show VNone"

instance Show Label where
  show (Label l) = "L" ++ show l

showLabel :: Label -> String
showLabel label =
  show label ++ ":"

showLabelReg :: Label -> String
showLabelReg label =
  "%" ++ show label

instance Show Type where
  show Ti64    = "i64"
  show Ti32    = "i32"
  show Tvoid   = "void"
  show Ti1     = "i1"
  show Ti8     = "i8"
  show (Ptr t) = show t ++ "*"
  show TArr    = "%ArrRetVal"

instance Show LLVMStmt where
  show (Call reg t name args) = show reg ++ " = call " ++ show t ++ " @"
    ++ name ++ "(" ++ printArgsWithVals True args ++ ")"
  show (CallVoid t name args) = "call " ++ show t ++ " @"
    ++ name ++ "(" ++ printArgsWithVals True args ++ ")"
  show RetVoid = "ret void"
  show (Ret t val) = "ret " ++ show t ++ " " ++ show val
  show (Arithm reg t v1 v2 op) = show reg ++ " = " ++ show op ++ " "
    ++ show t ++ " " ++ show v1 ++ ", " ++ show v2
  show (Br label) = "br label " ++ showLabelReg label
  show (BrCond t val l1 l2) = "br " ++ show t ++ " " ++ show val
    ++ ", label " ++ showLabelReg l1 ++ ", label " ++ showLabelReg l2
  show (Load r1 t1 t2 r2) = show r1 ++ " = load " ++ show t1 ++ ", "
    ++ show t2 ++ " " ++ show r2
  show (Store t1 v1 t2 r) = "store " ++ show t1 ++ " " ++ show v1 ++ ", "
    ++ show t2 ++ " " ++ show r
  show (Alloca reg t) = show reg ++ " = alloca " ++ show t
  show (Xor reg t v1 v2) = show reg ++ " = xor " ++ show t ++ " "
    ++ show v1 ++ ", " ++ show v2
  show (Cmp reg cond t v1 v2) = show reg ++ " = icmp " ++ show cond
    ++ " " ++ show t ++ " " ++ show v1 ++ ", " ++ show v2
  show (Phi reg t phiArgs) = show reg ++ " = phi " ++ show t
    ++ " " ++ printPhiArgs True phiArgs
  -- arrays --
  show (CallArr reg t name args) = " " ++ show (Call reg t name args)
  show (RetArr t val) = " " ++ show (Ret t val)
  show (AllocaArr r1 t1) = " " ++ show (Alloca r1 t1)
  show (StoreArr t1 v1 t2 r1) = " " ++ show (Store t1 v1 t2 r1)
  show (LoadArr r1 t1 t2 r2) = " " ++  show (Load r1 t1 t2 r2)
  show (GetElementPtrArr r1 t1 t2 r2 t3 v1) = " " ++ show r1 
    ++ " = getelementptr inbounds " ++ show t1 ++ ", " ++ show t2 
    ++ " " ++ show r2 ++ ", " ++ show t3 ++ " " ++ show v1 
  show (GetElementPtrRetArr r1 t1 t2 r2 t3 v1 t4 v2) = " " ++ show r1 
    ++ " = getelementptr inbounds " ++ show t1 ++ ", " ++ show t2 
    ++ " " ++ show r2 ++ ", " ++ show t3 ++ " " ++ show v1 
    ++ ", " ++ show t4 ++ " " ++ show v2
  show (Sext r1 t1 v1 t2) = " " ++ show r1 ++ " = sext " ++ show t1 ++ " " 
    ++ show v1 ++ " to " ++ show t2
  show (Bitcast r1 t1 v1 t2) = " " ++ show r1 ++ " = bitcast " ++ show t1 ++ " " ++ show v1 
    ++ " to " ++ show t2

-- print llvm code from in-memory structures --

printPhiArgs :: Bool -> [(Val, Label)] -> String
printPhiArgs _ [] = []
printPhiArgs first ((v, l):args) =
  (if first then "" else ", ")
  ++
  "[ " ++ show v ++ ", " ++ showLabelReg l ++ " ]"
  ++
  printPhiArgs False args

printArgsWithVals :: Bool -> [(Type, Val)] -> String
printArgsWithVals _ [] = []
printArgsWithVals first ((t, val):args) =
  (if first then "" else ", ")
  ++
  show t ++ " "
  ++
  show val
  ++
  printArgsWithVals False args

printStmt :: LLVMStmt -> String
printStmt stmt = "  " ++ show stmt

-- conversion to printable string --
convertStr :: String -> String
convertStr "" = ""
convertStr (c:str) =
  case c of
    '\n' -> "\\" ++ "0A"
    '\\' -> "\\" ++ "%C"
    '\t' -> "\\" ++ "09"
    '"'  -> "\\" ++ "22"
    cc   -> [cc]
  ++
  convertStr str

printStrConstants :: [StrConstant] -> [String]
printStrConstants [] = []
printStrConstants ((StrConstant id len str):strConstants) =
  ("@.str." ++ show id ++ " = private unnamed_addr constant ["
  ++ show len ++ " x i8] c\"" ++ convertStr str ++ "\\00\", align 1")
  : printStrConstants strConstants

convPhisToLLVMStmts :: [(Reg, (Type, [(Val, Label)]))] -> [LLVMStmt]
convPhisToLLVMStmts [] = []
convPhisToLLVMStmts ((reg, (t, phiVals)):phis) =
  Phi reg t phiVals
  :
  convPhisToLLVMStmts phis

printLLBlocks :: Bool -> [LLBlock] -> [String]
printLLBlocks _ [] = []
printLLBlocks first (block:blocks) = do
  let phis = bPhis block
  let stmts = bStmts block ++ convPhisToLLVMStmts (Map.toList phis)
  let blockStmts = Prelude.map printStmt (reverse stmts)
  let nextBlockStmts = printLLBlocks False blocks
  if not first then
    (showLabel (bLabel block)
    ++
    "                              ; preds = "
    ++
    show (bInBlocks block))
    :
    blockStmts ++ nextBlockStmts
  else
    blockStmts ++ nextBlockStmts

printArgs :: Bool -> Integer -> [(Type, String)] -> String
printArgs _ _ [] = []
printArgs first register ((t, _):args) =
  case t of 
    (Ptr Ti32) -> 
      printArrArgs
    (Ptr Ti1) -> 
      printArrArgs
    (Ptr (Ptr Ti8)) -> 
      printArrArgs
    _ -> 
      (if first then "" else ", ")
      ++
      show t ++ " "
      ++
      show (Reg register)
      ++
      printArgs False (register + 1) args
  where 
    printArrArgs = 
      (if first then "" else ", ")
      ++
      show t ++ " "
      ++
      show (Reg register)
      ++
      ", " ++ show Ti32 ++ " "
      ++ 
      show (Reg (register + 1))
      ++
      printArgs False (register + 2) args

printFType :: Type -> String
printFType t = 
  if isArrayType t then
    "%ArrRetVal*"
  else show t

printFunctions :: [Fn] -> [String]
printFunctions [] = []
printFunctions (fn:fns) =
  [
    "\ndefine " ++ printFType (fType fn) ++ " @" ++ fName fn
    ++ "(" ++ printArgs True 0 (fArgs fn) ++ ") {"
  ]
  ++
  printLLBlocks True (Prelude.map snd (toList (fBlocks fn)))
  ++
  ["}"]
  ++
  printFunctions fns

printLLVMProgram :: [StrConstant] -> [Fn] -> [String]
printLLVMProgram strConstants fns =
  [
    "%ArrRetVal = type {",
    "  i32,           ; length of array",
    "  i32*,",
    "  i1*,",
    "  i8**",
    "}",
    "declare void @printInt(i32)",
    "declare void @printString(i8*)",
    "declare i32 @readInt()",
    "declare i8* @readString()",
    "declare void @error()",
    "declare i32 @__equStrings__(i8*, i8*)",
    "declare i8* @__concatStrings__(i8*, i8*)",
    "declare i8* @malloc(i32) nounwind"
  ]
  ++
  printStrConstants strConstants
  ++
  printFunctions (reverse fns)


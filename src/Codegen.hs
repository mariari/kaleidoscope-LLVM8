module Codegen where

import Data.ByteString.Short

import Protolude hiding (Type, moduleName, local)
import Data.String
import Prelude (String, error)

import LLVM.AST
import LLVM.AST.Typed (typeOf)
import LLVM.AST.AddrSpace
import LLVM.AST.Type hiding (double)
import LLVM.AST.Global as Global
import qualified LLVM.AST as AST
import qualified Data.Map.Strict as Map

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP


-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------


---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- 64-bit floating point value
double :: Type
double = FloatingPointType DoubleFP


-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = Map.Map ShortByteString Operand


data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm <> show ix, Map.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )


emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks = fmap makeBlock . sortBlocks . Map.toList . blocks

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing  = error $ "Block has no terminator: " <> show l

entryBlockName :: String
entryBlockName = "entry"

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (mkName entryBlockName) Map.empty Map.empty 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = succ i }
  return $ succ i
-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: ShortByteString -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs <> [d] }


define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body =
  addDefn
  $ GlobalDefinition
  $ functionDefaults
    {
      parameters               = ((\(ty,nm) → Parameter ty nm []) <$> argtys, False)
    , Global.callingConvention = CC.Fast -- for fun!
    , returnType               = retty
    , basicBlocks              = body
    , name                     = mkName label
    }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys =
  addDefn
  $ GlobalDefinition
  $ functionDefaults
    { parameters               = ((\(ty,nm) → Parameter ty nm []) <$> argtys, False)
    , Global.callingConvention = CC.Fast -- for fun!
    , returnType               = retty
    , basicBlocks              = []
    , name                     = mkName label
    , linkage                  = L.External
    }

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------
entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names
  let new             = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks     = Map.insert (mkName qname) new bls
                   , blockCount = succ ix
                   , names      = supply
                   }
  return (mkName qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname


getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c    <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x  -> return x
    Nothing -> error $ "No such block: " <> show c


local :: Name -> Operand
local = LocalReference double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i } )
  return $ local ref

unnminstr :: Instruction -> Codegen ()
unnminstr ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (Do ins) : i } )


terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv noFastMathFlags a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = fmap (\x -> (x, []))

-------------------------------------------------------------------------------
-- Control Flow
-------------------------------------------------------------------------------

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

-------------------------------------------------------------------------------
-- Effects
-------------------------------------------------------------------------------
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.Fast [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = unnminstr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = Map.insert (fromString var) x lcls  }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case Map.lookup (fromString var) syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " <> show var

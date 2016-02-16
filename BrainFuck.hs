{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, RecordWildCards #-}
import Data.Array
import Data.Char
import Data.Foldable (toList)
import Data.Int (Int8)

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.Trans

import Data.Sequence (Seq)

import qualified Data.Sequence as Seq

import System.Environment (getArgs)

main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  mapM_ (putStrLn . interpret) (lines input)

data Memory i = Memory { previousCells :: [i], currentCell :: i, nextCells :: [i], counter :: Int }
  deriving (Show)

data Program = Program { instructions :: Array Int Char, jumps :: Array Int Int }
  deriving (Show)


type Machine i = MachineT i Identity

runMachine :: Machine i a -> Program -> Memory i -> (a, Memory i, Seq Char)
runMachine mch prg mem = runIdentity (runMachineT mch prg mem)


newtype MachineT i m a = MachineT { _runMachineT :: RWST Program (Seq Char) (Memory i) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Program
    , MonadWriter (Seq Char)
    , MonadState (Memory i)
    , MonadRWS Program (Seq Char) (Memory i)
    , MonadTrans
    )

runMachineT :: MachineT i m a -> Program -> Memory i -> m (a, Memory i, Seq Char)
runMachineT mch = runRWST (_runMachineT mch)

interpret :: String -> String
interpret prog =
  let (_,_,out) = runMachine (execute :: Machine Int8 ()) (initProgram prog) initMemory
  in  toList out

initMemory :: Integral i => Memory i
initMemory = Memory { previousCells = [], currentCell = 0, nextCells = [], counter = 0 }

initProgram :: String -> Program
initProgram prog =
  let bnds         = (0, length prog-1)
      instructions = listArray bnds prog
      combine :: (Int,Char) -> ([(Int,Int)],[Int]) -> ([(Int,Int)],[Int])
      combine (i,c) (out,stk) = case c of
        ']' -> (out,i:stk)
        '[' -> let j:stk' = stk
               in  ((i,j):(j,i):out,stk')
        _   -> (out,stk)
      (pairs,_) = foldr combine ([],[]) (zip [0..] prog)
      jumps    = accumArray (flip const) 0 bnds pairs
  in  Program { .. }
  
moveNext :: (Integral i, Monad m) => MachineT i m ()
moveNext = modify $ \(Memory {..}) -> case nextCells of
  []          -> Memory { previousCells = currentCell:previousCells, currentCell = 0    , nextCells = []   , .. }
  curr':next' -> Memory { previousCells = currentCell:previousCells, currentCell = curr', nextCells = next', .. }

movePrev :: (Integral i, Monad m) => MachineT i m ()
movePrev = modify $ \(Memory {..}) -> case previousCells of
  []          -> Memory { previousCells = []   , currentCell = 0    , nextCells = currentCell:nextCells, .. }
  curr':prev' -> Memory { previousCells = prev', currentCell = curr', nextCells = currentCell:nextCells, .. }

updateCurrentCell :: Monad m => (i -> i) -> MachineT i m ()
updateCurrentCell upd = modify $ \(Memory { .. }) -> Memory { currentCell = upd currentCell, .. }

updateCounter :: Monad m => (Int -> Int) -> MachineT i m ()
updateCounter upd = modify $ \(Memory { .. }) -> Memory { counter = upd counter, .. }

output :: (Integral i, Monad m) => MachineT i m ()
output = gets currentCell >>= tell . Seq.singleton . chr . fromIntegral

jump :: Monad m => MachineT i m ()
jump = do
  jmps <- asks jumps
  updateCounter (jmps !)


execute1 :: (Integral i, Monad m) => MachineT i m ()
execute1 = do
  ctr <- gets counter
  instruction <- asks ((! ctr) . instructions)
  case instruction of
    '>' -> moveNext
    '<' -> movePrev
    '+' -> updateCurrentCell (+1)
    '-' -> updateCurrentCell (subtract 1)
    '.' -> output
    '[' -> whenM (liftM (0 ==) (gets currentCell)) jump
    ']' -> whenM (liftM (0 /=) (gets currentCell)) jump
    _   -> return ()
  updateCounter succ

execute :: (Integral i, Monad m) => MachineT i m ()
execute = whileM (liftM2 (<=) (gets counter) (asks (snd . bounds . instructions))) execute1

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond act = cond >>= flip when act

whileM :: Monad m => m Bool -> m () -> m ()
whileM cond act = whenM cond (act >> whileM cond act)

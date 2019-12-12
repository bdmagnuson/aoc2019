{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Debug.Trace
import Control.Monad
import Control.Monad.State
import Control.Lens
import Debug.Trace
import qualified Data.IntMap.Strict as M

data St = St {
    _pc   :: Int,
    _prog :: M.IntMap Int,
    _output :: [Int],
    _inval :: Int
} deriving (Show)

makeLenses ''St


data Opcode =
  Add    |
  Mult   |
  Input  |
  Output |
  JIfT   |
  JIfF   |
  Lt     |
  Equal     |
  Halt deriving (Show)

opSize Add    = 4
opSize Mult   = 4
opSize Halt   = 1
opSize Input  = 2
opSize Output = 2
opSize JIfT   = 3
opSize JIfF   = 3
opSize Lt     = 4
opSize Equal  = 4

type Addr = Int
data Mode
  = Pos
  | Imm deriving  (Show)

data Inst = Inst {
    op :: Opcode,
    opa :: Int,
    opb :: Int,
    opd :: Int,
    modea :: Mode,
    modeb :: Mode
} deriving (Show)

update :: Addr -> Int -> State St ()
update k v = prog %= M.insert k v

fetch :: Addr -> State St Int
fetch a = do
    p <- use prog
    case p ^? ix a of
        Nothing -> error "bad addr"
        Just a -> return a

fromInt 0 = Pos
fromInt 1 = Imm

decode :: State St Inst
decode = do
    pc' <- use pc
    inst <- use pc >>= fetch
    let (rest, op)  = inst `divMod` 100
    let (rest', m1) = rest `divMod` 10
    let (_, m2)     = rest' `divMod` 10
    case op of
        1 -> do
               op1 <- fetch (pc' + 1)
               op2 <- fetch (pc' + 2)
               opd <- fetch (pc' + 3)
               return $ Inst Add op1 op2 opd (fromInt m1) (fromInt m2)
        2 -> do
               op1 <- fetch (pc' + 1)
               op2 <- fetch (pc' + 2)
               opd <- fetch (pc' + 3)
               return $ Inst Mult op1 op2 opd (fromInt m1) (fromInt m2)
        3 -> do
               op1 <- fetch (pc' + 1)
               return $ Inst Input op1 0 0 Imm Imm
        4 -> do
               op1 <- fetch (pc' + 1)
               return $ Inst Output op1 0 0 Imm Imm
        5 -> do
               op1 <- fetch (pc' + 1)
               op2 <- fetch (pc' + 2)
               return $ Inst JIfT op1 op2 0 (fromInt m1) (fromInt m2)
        6 -> do
               op1 <- fetch (pc' + 1)
               op2 <- fetch (pc' + 2)
               return $ Inst JIfF op1 op2 0 (fromInt m1) (fromInt m2)
        7 -> do
               op1 <- fetch (pc' + 1)
               op2 <- fetch (pc' + 2)
               opd <- fetch (pc' + 3)
               return $ Inst Lt op1 op2 opd (fromInt m1) (fromInt m2)
        8 -> do
               op1 <- fetch (pc' + 1)
               op2 <- fetch (pc' + 2)
               opd <- fetch (pc' + 3)
               return $ Inst Equal op1 op2 opd (fromInt m1) (fromInt m2)
        _ -> return $ Inst Halt 0 0 0 Imm Imm

execute :: Inst -> State St Bool
execute w@(Inst op a b d am bm) = do
    case op of
        Add -> do
           pc += opSize op
           op1 <- case am of
                     Imm -> return a
                     Pos -> fetch a
           op2 <- case bm of
                     Imm -> return b
                     Pos -> fetch b
           update d (op1 + op2)
           return True
        Mult -> do
           pc += opSize op
           op1 <- case am of
                     Imm -> return a
                     Pos -> fetch a
           op2 <- case bm of
                     Imm -> return b
                     Pos -> fetch b
           update d (op1 * op2)
           return True
        Input -> do
           pc += opSize op
           inval' <- use inval
           update a inval'
           return True
        Output -> do
           pc += opSize op
           op1 <- fetch a
           output %= (++ [op1])
           return True
        JIfT -> do
           op1 <- case am of
                     Imm -> return a
                     Pos -> fetch a
           op2 <- case bm of
                     Imm -> return b
                     Pos -> fetch b
           (if op1 /= 0 then pc .= op2
                        else pc += (opSize op)) >> return True
        JIfF -> do
           op1 <- case am of
                     Imm -> return a
                     Pos -> fetch a
           op2 <- case bm of
                     Imm -> return b
                     Pos -> fetch b
           (if op1 == 0 then pc .= op2
                        else pc += (opSize op)) >> return True
        Lt -> do
           pc += opSize op
           op1 <- case am of
                     Imm -> return a
                     Pos -> fetch a
           op2 <- case bm of
                     Imm -> return b
                     Pos -> fetch b
           update d (if op1 < op2 then 1 else 0)
           return True
        Equal -> do
           pc += opSize op
           op1 <- case am of
                     Imm -> return a
                     Pos -> fetch a
           op2 <- case bm of
                     Imm -> return b
                     Pos -> fetch b
           update d (if op1 == op2 then 1 else 0)
           return True
        Halt -> return False

run prog input = doit init
    where doit st = case runState (decode >>= execute) st of
                          (False, st') -> st'
                          (True, st')  -> doit st'
          init = St 0 prog [] input

getoutput x = (run input x) ^. output

input :: M.IntMap Int
input = M.fromList $ zip ([0..] :: [Int])
  [3, 225, 1, 225, 6, 6, 1100, 1, 238, 225, 104, 0, 1102, 46, 47, 225, 2, 122,
  130, 224, 101, -1998, 224, 224, 4, 224, 1002, 223, 8, 223, 1001, 224, 6, 224,
  1, 224, 223, 223, 1102, 61, 51, 225, 102, 32, 92, 224, 101, -800, 224, 224, 4,
  224, 1002, 223, 8, 223, 1001, 224, 1, 224, 1, 223, 224, 223, 1101, 61, 64,
  225, 1001, 118, 25, 224, 101, -106, 224, 224, 4, 224, 1002, 223, 8, 223, 101,
  1, 224, 224, 1, 224, 223, 223, 1102, 33, 25, 225, 1102, 73, 67, 224, 101,
  -4891, 224, 224, 4, 224, 1002, 223, 8, 223, 1001, 224, 4, 224, 1, 224, 223,
  223, 1101, 14, 81, 225, 1102, 17, 74, 225, 1102, 52, 67, 225, 1101, 94, 27,
  225, 101, 71, 39, 224, 101, -132, 224, 224, 4, 224, 1002, 223, 8, 223, 101, 5,
  224, 224, 1, 224, 223, 223, 1002, 14, 38, 224, 101, -1786, 224, 224, 4, 224,
  102, 8, 223, 223, 1001, 224, 2, 224, 1, 223, 224, 223, 1, 65, 126, 224, 1001,
  224, -128, 224, 4, 224, 1002, 223, 8, 223, 101, 6, 224, 224, 1, 224, 223, 223,
  1101, 81, 40, 224, 1001, 224, -121, 224, 4, 224, 102, 8, 223, 223, 101, 4,
  224, 224, 1, 223, 224, 223, 4, 223, 99, 0, 0, 0, 677, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1105, 0, 99999, 1105, 227, 247, 1105, 1, 99999, 1005, 227, 99999,
  1005, 0, 256, 1105, 1, 99999, 1106, 227, 99999, 1106, 0, 265, 1105, 1, 99999,
  1006, 0, 99999, 1006, 227, 274, 1105, 1, 99999, 1105, 1, 280, 1105, 1, 99999,
  1, 225, 225, 225, 1101, 294, 0, 0, 105, 1, 0, 1105, 1, 99999, 1106, 0, 300,
  1105, 1, 99999, 1, 225, 225, 225, 1101, 314, 0, 0, 106, 0, 0, 1105, 1, 99999,
  1008, 677, 226, 224, 1002, 223, 2, 223, 1005, 224, 329, 1001, 223, 1, 223,
  107, 677, 677, 224, 102, 2, 223, 223, 1005, 224, 344, 101, 1, 223, 223, 1107,
  677, 677, 224, 102, 2, 223, 223, 1005, 224, 359, 1001, 223, 1, 223, 1108, 226,
  226, 224, 1002, 223, 2, 223, 1006, 224, 374, 101, 1, 223, 223, 107, 226, 226,
  224, 1002, 223, 2, 223, 1005, 224, 389, 1001, 223, 1, 223, 108, 226, 226, 224,
  1002, 223, 2, 223, 1005, 224, 404, 1001, 223, 1, 223, 1008, 677, 677, 224,
  1002, 223, 2, 223, 1006, 224, 419, 1001, 223, 1, 223, 1107, 677, 226, 224,
  102, 2, 223, 223, 1005, 224, 434, 1001, 223, 1, 223, 108, 226, 677, 224, 102,
  2, 223, 223, 1006, 224, 449, 1001, 223, 1, 223, 8, 677, 226, 224, 102, 2, 223,
  223, 1006, 224, 464, 1001, 223, 1, 223, 1007, 677, 226, 224, 1002, 223, 2,
  223, 1006, 224, 479, 1001, 223, 1, 223, 1007, 677, 677, 224, 1002, 223, 2,
  223, 1005, 224, 494, 1001, 223, 1, 223, 1107, 226, 677, 224, 1002, 223, 2,
  223, 1006, 224, 509, 101, 1, 223, 223, 1108, 226, 677, 224, 102, 2, 223, 223,
  1005, 224, 524, 1001, 223, 1, 223, 7, 226, 226, 224, 102, 2, 223, 223, 1005,
  224, 539, 1001, 223, 1, 223, 8, 677, 677, 224, 1002, 223, 2, 223, 1005, 224,
  554, 101, 1, 223, 223, 107, 677, 226, 224, 102, 2, 223, 223, 1006, 224, 569,
  1001, 223, 1, 223, 7, 226, 677, 224, 1002, 223, 2, 223, 1005, 224, 584, 1001,
  223, 1, 223, 1008, 226, 226, 224, 1002, 223, 2, 223, 1006, 224, 599, 101, 1,
  223, 223, 1108, 677, 226, 224, 102, 2, 223, 223, 1006, 224, 614, 101, 1, 223,
  223, 7, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 629, 1001, 223, 1, 223, 8,
  226, 677, 224, 1002, 223, 2, 223, 1006, 224, 644, 101, 1, 223, 223, 1007, 226,
  226, 224, 102, 2, 223, 223, 1005, 224, 659, 101, 1, 223, 223, 108, 677, 677,
  224, 1002, 223, 2, 223, 1006, 224, 674, 1001, 223, 1, 223, 4, 223, 99, 226]

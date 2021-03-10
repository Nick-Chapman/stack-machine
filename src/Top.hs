
module Top (main) where

import Control.Monad (ap,liftM)
import Prelude hiding (seq)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "*stack-machine*"
  let x = Name "x"
  --let exp = Add (Num 1) (Add (Var x) (Num 2))
  let env = emptyEnv
  --let v = eval env exp

  --print exp
  --print v

  let s = seq [ Assign x (Num 13),
                Print (Var x),
                Assign x (Add (Var x) (Var x)),
                Print (Var x)
              ]

  let _s = fact 5
  print s
  print (exec env s)

  let code = runEmit (compileS s)
  --let m = load code
  let v2 = runExecution (execute code)

  print v2

  pure ()


fact :: Int -> Stat
fact x = do
  seq [
    Assign acc (Num 1),
    Assign n (Num x),
    Repeat (seq
      [
        Assign acc (Mul (Var acc) (Var n)),
        Assign n (Sub (Var n) (Num 1)),
        Print (Var (acc))
      ]) (PredEq (Var n) (Num 1)),
      Print (Var (acc))
    ]
    where
      acc = Name "acc"
      n = Name "n"


seq :: [Stat] -> Stat
seq = foldl Seq Null


data Stat where
  Null :: Stat
  Seq :: Stat -> Stat -> Stat
  Assign :: Name -> Exp -> Stat
  Print :: Exp -> Stat
  Repeat :: Stat -> Pred -> Stat
  deriving Show

data Pred = PredEq Exp Exp
  deriving Show

data Exp where
  Var :: Name -> Exp
  Num :: Int -> Exp
  Add :: Exp -> Exp -> Exp
  Sub :: Exp -> Exp -> Exp
  Mul :: Exp -> Exp -> Exp
  deriving Show

newtype Name = Name String deriving (Eq,Ord)
instance Show Name where show (Name x) = x


type Value = Int


exec :: Env -> Stat -> [Value]
exec env0 s = snd (loop env0 s) where

  loop :: Env -> Stat -> (Env,[Value])
  loop q = \case
    Null{} -> (q,[])

    Seq s1 s2 -> do
      let (q1,xs1) = loop q s1
      let (q2,xs2) = loop q1 s2
      (q2,xs1++xs2)

    Assign x exp -> do
      let v = eval q exp
      let q' = updateEnv q x v
      (q',[])

    Print exp -> do
      let v = eval q exp
      (q,[v])

    stat0@(Repeat stat pred) -> do
      let (q',xs) = loop q stat
      let b = evalPred q' pred
      if b then (q',xs) else do
        let (q'',xs2) = loop q' stat0
        (q'',xs ++ xs2)


evalPred :: Env -> Pred -> Bool
evalPred q = \case
  PredEq e1 e2 -> do
    let v1 = eval q e1
    let v2 = eval q e2
    v1 == v2


eval :: Env -> Exp -> Value
eval env = \case
  Num n -> n
  Mul e1 e2 -> eval env e1 * eval env e2
  Add e1 e2 -> eval env e1 + eval env e2
  Sub e1 e2 -> eval env e1 - eval env e2
  Var x -> lookEnv env x


newtype Env = Env (Map Name Value)

emptyEnv :: Env
emptyEnv = Env Map.empty

lookEnv :: Env -> Name -> Value
lookEnv (Env m) x = maybe 0 id (Map.lookup x m)

updateEnv :: Env -> Name -> Value -> Env
updateEnv (Env m) x v = Env (Map.insert x v m)


{-compile :: Exp -> Code
compile = \case
  Num n -> [NUM n]
  Add e1 e2 -> compile e1 ++ compile e2 ++ [ADD]
  Mul e1 e2 -> compile e1 ++ compile e2 ++ [MUL]
  Var x -> [LOAD x]
-}


compileS :: Stat -> Emit ()
compileS = \case
    Null -> pure ()

    Seq s1 s2 -> do
      undefined s1 s2
    Assign x exp -> do
      undefined x exp
    Print exp -> do
      compileE exp
      Emit PRINT

    Repeat{} -> do
      undefined

compileE :: Exp -> Emit ()
compileE = \case
  Num n -> Emit (NUM n)
  Var x -> Emit (LOAD x)

  Add e1 e2 -> do
    compileE e1
    compileE e2
    Emit ADD

  Sub e1 e2 -> do
    compileE e1
    compileE e2
    Emit SUB

  Mul e1 e2 -> do
    compileE e1
    compileE e2
    Emit MUL



instance Functor Emit where fmap = liftM
instance Applicative Emit where pure = return; (<*>) = ap
instance Monad Emit where return = Ret; (>>=) = Bind

data Emit a where
  Ret :: a -> Emit a
  Bind :: Emit a -> (a -> Emit b) -> Emit b
  Emit :: Op -> Emit ()

runEmit :: Emit () -> Code
runEmit = undefined


type Code = [Op]

data Op = MUL | ADD | SUB | NUM Int | LOAD Name | PRINT | JMP RelOffset | JMPIF0 RelOffset
  deriving Show

newtype RelOffset = RelOffset Int deriving Show


--data Machine = Machine { stack :: [Value], code :: [Op] }

--load :: Code -> Machine
--load code = Machine { stack = [], code }

{-

execute :: Machine -> Printed
execute Machine{stack=stack0,code=code0} = head (loop printed0 stack0 code0) where

  env = emptyEnv

  loop :: Printed -> [Value] -> [Op] -> ([Value],Printed)
  loop printed stack = \case
    op1:ops -> do
      let (stack',printed') = step op1 stack
      loop printed' stack' ops
    [] -> stack

  step :: Printed -> Op -> [Value] -> ([Value],Printed)
  step = \case
    NUM n -> \stack -> (n:stack, p0)
    LOAD x -> \stack -> lookEnv env x : stack
    ADD -> \case
      v1:v2:stack -> ((v2+v1):stack, p0)
      _ -> error "step:ADD"
    SUB -> \case
      v1:v2:stack -> ((v2-v1):stack, p0)
      _ -> error "step:SUB"
    MUL -> \case
      v1:v2:stack -> ((v2*v1):stack, p0)
      _ -> error "step:MUL"
    JMP{} -> undefined
    JMPIF0{} -> undefined

newtype Printed = Printed [Value]

prin0 :: Printed
p0 = Printed []
-}


execute :: Code -> Execution ()
execute ops = sequence_ (map execute0 ops)

execute0 :: Op -> Execution ()
execute0 = \case
  NUM{} -> undefined
  LOAD{} -> undefined Set Get
  PRINT{} -> undefined XPrint
  ADD -> do
    v1 <- Pop
    v2 <- Pop
    Push (v1+v2)
  SUB -> undefined
  MUL -> undefined
  JMP{} -> undefined
  JMPIF0{} -> undefined



instance Functor Execution where fmap = liftM
instance Applicative Execution where pure = return; (<*>) = ap
instance Monad Execution where return = XRet; (>>=) = XBind

data Execution a where
  XRet :: a -> Execution a
  XBind :: Execution a -> (a -> Execution b) -> Execution b
  XPrint :: Value -> Execution ()
  Push :: Value -> Execution ()
  Pop :: Execution Value
  Set :: Name -> Value -> Execution ()
  Get :: Name -> Execution Value

runExecution :: Execution () -> [Value]
runExecution = undefined Machine stack ip


data Machine = Machine { stack :: [Value], ip :: IP }

data IP

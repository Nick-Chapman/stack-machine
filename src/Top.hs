
module Top (main) where

import Prelude hiding (seq)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "*stack-machine*"
  let x = Name "x"
  let exp = Add (Num 1) (Add (Var x) (Num 2))
  let env = emptyEnv
  let v = eval env exp

  print exp
  print v

  let s = seq [ Assign x (Num 13),
                Print (Var x),
                Assign x (Add (Var x) (Var x)),
                Print (Var x)
              ]

  print (exec env s)

  let code = compile exp
  let m = load code
  let v2 = execute m

  print v2


seq :: [Stat] -> Stat
seq = foldl Seq Null


data Stat where
  Null :: Stat
  Seq :: Stat -> Stat -> Stat
  Assign :: Name -> Exp -> Stat
  Print :: Exp -> Stat

data Exp where
  Var :: Name -> Exp
  Num :: Int -> Exp
  Add :: Exp -> Exp -> Exp
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

eval :: Env -> Exp -> Value
eval env = \case
  Num n -> n
  Add e1 e2 -> eval env e1 + eval env e2
  Var x -> lookEnv env x


newtype Env = Env (Map Name Value)

emptyEnv :: Env
emptyEnv = Env Map.empty

lookEnv :: Env -> Name -> Value
lookEnv (Env m) x = maybe 0 id (Map.lookup x m)

updateEnv :: Env -> Name -> Value -> Env
updateEnv (Env m) x v = Env (Map.insert x v m)


compile :: Exp -> Code
compile = \case
  Num n -> [NUM n]
  Add e1 e2 -> compile e1 ++ compile e2 ++ [ADD]
  Var x -> [LOAD x]


type Code = [Op]

data Op = ADD | NUM Int | LOAD Name
  deriving Show


data Machine = Machine { stack :: [Value], code :: [Op] }

load :: Code -> Machine
load code = Machine { stack = [], code }

execute :: Machine -> Value
execute Machine{stack=stack0,code=code0} = head (loop stack0 code0) where

  env = emptyEnv

  loop :: [Value] -> [Op] -> [Value]
  loop stack = \case
    op1:ops -> loop (step op1 stack) ops
    [] -> stack

  step :: Op -> [Value] -> [Value]
  step = \case
    NUM n -> \stack -> n:stack
    LOAD x -> \stack -> lookEnv env x : stack
    ADD -> \case
      v1:v2:stack -> (v1+v2):stack
      _ -> error "step:ADD"


module Top (main) where

import Control.Monad (ap,liftM)
import Prelude hiding (seq)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "*stack-machine*"
  let x = Name "x"
  let env = emptyEnv

  let _s = seq [ Assign x (Num 13),
                Print (Var x),
                Assign x (Add (Var x) (Var x)),
                Print (Var x)
              ]

  let s = fact 5
  print s
  print (evalS env s)

  let code = runEmit (compileS s)
  print code
  v2 <- runExecution code
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


evalS :: Env -> Stat -> [Value]
evalS env0 s = snd (loop env0 s) where

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


newtype Env = Env (Map Name Value) deriving Show

emptyEnv :: Env
emptyEnv = Env Map.empty

lookEnv :: Env -> Name -> Value
lookEnv (Env m) x = maybe 0 id (Map.lookup x m)

updateEnv :: Env -> Name -> Value -> Env
updateEnv (Env m) x v = Env (Map.insert x v m)


compileS :: Stat -> Emit ()
compileS = \case
    Null -> pure ()
    Seq s1 s2 -> do
      compileS s1
      compileS s2
    Assign x exp -> do
      compileE exp
      Emit (STORE x)
    Print exp -> do
      compileE exp
      Emit PRINT
    Repeat stat pred -> do
      AbsOffset a <- Here
      compileS stat
      compileP pred
      AbsOffset b <- Here
      Emit (JMPIF0 (RelOffset (a - b)))


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

compileP :: Pred -> Emit ()
compileP = \case
  PredEq e1 e2 -> do
    compileE e1
    compileE e2
    Emit EQUAL



instance Functor Emit where fmap = liftM
instance Applicative Emit where pure = return; (<*>) = ap
instance Monad Emit where return = Ret; (>>=) = Bind

data Emit a where
  Ret :: a -> Emit a
  Bind :: Emit a -> (a -> Emit b) -> Emit b
  Emit :: Op -> Emit ()
  Here :: Emit AbsOffset

newtype AbsOffset = AbsOffset Int deriving (Num,Show)

runEmit :: Emit () -> [Op]
runEmit e0 = loop (AbsOffset 0) e0 (\_ () -> []) where
  loop :: AbsOffset -> Emit a -> (AbsOffset -> a -> [Op]) -> [Op]
  loop off e k = case e of
    Ret x -> k off x
    Bind e f -> loop off e $ \off a -> loop off (f a) k
    Emit op -> op : k (off + 1) ()
    Here -> k off off


type Code = [Op]

data Op = EQUAL | MUL | ADD | SUB | NUM Int | LOAD Name | STORE Name | PRINT | JMP RelOffset | JMPIF0 RelOffset
  deriving Show

newtype RelOffset = RelOffset Int deriving (Show,Num)


executeOp :: Op -> Execution ()
executeOp = \case
  NUM n -> Push n
  LOAD x -> Get x >>= Push
  STORE x  -> Pop >>= Set x
  PRINT -> do v <- Pop; XPrint v
  EQUAL -> binary (\v1 v2 -> if v1 == v2 then 1 else 0)
  ADD -> binary (+)
  SUB -> binary (-)
  MUL -> binary (*)
  JMP{} -> undefined
  JMPIF0 rel -> do
    v <- Pop
    if v == 0 then JumpRelative rel else pure ()

binary :: (Value -> Value -> Value) -> Execution ()
binary f = do
  v2 <- Pop
  v1 <- Pop
  Push (f v1 v2)

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
  FetchOp :: Execution (Maybe Op)
  JumpRelative :: RelOffset -> Execution ()

fetchExecLoop :: Execution ()
fetchExecLoop = do
  FetchOp >>= \case
    Nothing -> pure () -- machine halts
    Just op -> do
      executeOp op
      fetchExecLoop

runExecution :: Code -> IO [Value]
runExecution code = loop machine0 fetchExecLoop $ (\_ _ -> pure [])
  where
    loop :: Machine -> Execution a -> (Machine -> a -> IO [Value]) -> IO [Value]
    loop m x k = do
      --print m
      loop' m x k

    loop' :: Machine -> Execution a -> (Machine -> a -> IO [Value]) -> IO [Value]
    loop' m@Machine{env,stack,ip=IP ip} exe k = case exe of
      XRet x -> k m x
      XBind e f -> loop m e $ \m a -> loop m (f a) k
      XPrint v -> do
        vs <- k m ()
        pure (v:vs)
      Push v -> k m { stack = v : stack } ()
      Pop -> case stack of [] -> error "Pop"; v:stack -> k m {stack} v
      Set x v -> k m { env = updateEnv env x v } ()
      Get x -> k m (lookEnv env x)
      FetchOp -> do
        let opm = (if ip >= length code || ip < 0 then Nothing else Just (code !! ip))
        --print opm
        k m { ip = IP (ip + 1) } opm
      JumpRelative (RelOffset n) -> k m { ip = IP (ip + n - 1) } ()


data Machine = Machine { env :: Env, stack :: [Value], ip :: IP } deriving Show
machine0 :: Machine
machine0 = Machine { env = emptyEnv, stack = [], ip = IP 0 }

newtype IP = IP Int deriving Show -- instruction pointer

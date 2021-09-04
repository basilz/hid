module EvalRPN where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Applicative
import qualified Text.Read as T


type Stack = [Integer]
type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push x = modify (x :)

pop :: EvalM Integer
pop = do
    (x:xs) <- get
    put xs
    return x

pop' :: EvalM Integer
pop' = state $ \xs -> (head xs, tail xs)

oneElementOnStack :: EvalM ()
oneElementOnStack = do
    l <- length <$> get
    guard (l == 1)

readSafe :: (Read a, Alternative m) => String -> m a
readSafe str =
  case T.readMaybe str of
    Nothing -> empty
    Just n -> pure n

evalRPN :: String -> Maybe Integer
evalRPN expr = evalStateT evalRPN' []
  where
    evalRPN' = traverse step (words expr) >> oneElementOnStack >> pop
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t = readSafe t >>= push
    processTops op = flip op <$> pop <*> pop >>= push

processTops' :: (Integer -> Integer -> Integer) -> EvalM ()
processTops' op = do
    r <- pop
    l <- pop
    let x = l `op` r
    push x
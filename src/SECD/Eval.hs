{-# LANGUAGE FlexibleContexts #-}

module SECD.Eval
    ( eval
    ) where

import Control.Monad.Except
import SECD.Types

data EvalError = EvalError deriving Show

step :: (MonadError EvalError m) => SECD -> m SECD
step (IInt n : c, e, s) = return (c, e, MInt n : s)
step (IClosure c' : c, e, s) = return (c, e, MClosure c' e : s)
step (IApply:c, e, v : MClosure c' e' : s) = return (c', v:e', MClosure c e : s)
step (IAdd:c, e, MInt x : MInt y : s) = return (c, e, MInt (x + y) : s)
step (IReturn:c, e, v : MClosure c' e' :s) = return (c', e', v : s)
step (IAccess n : c, e, s) = return (c, e, (e !! n) : s)
step _ = throwError EvalError

-- | Evaluate the SECD commands into a value
eval :: [Command] -> MValue
eval cmds = either (const MUndefined) id $ eval' (cmds, [], [])
  where
    eval' ([], _, top : s) = return top
    eval' state = do state' <- step state
                     eval' state'

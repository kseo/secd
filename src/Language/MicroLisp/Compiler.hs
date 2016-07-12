{-# LANGUAGE FlexibleContexts #-}

module Language.MicroLisp.Compiler
    ( compile
    , CompileError(..)
    ) where

import Control.Monad.Except
import Data.Bifunctor
import Language.MicroLisp.SExprParser
import SECD.Types

data SF a = FF | SS a
  deriving (Show, Eq)

data CompileError = CompileError
                  | ParseError
                    deriving Show

data LTerm = LInt Int
           | LVar String
           | LAdd LTerm LTerm
           | LSub LTerm LTerm
           | LApp LTerm LTerm
           | LAbs String LTerm
             deriving (Show, Eq)

data DBTerm = DBInt Int
            | DBAdd DBTerm DBTerm
            | DBSub DBTerm DBTerm
            | DBApp DBTerm DBTerm
            | DBAbs DBTerm
            | DBIndex Int
              deriving (Show, Eq)

parseTerm :: (MonadError CompileError m) => SExpr -> m LTerm
parseTerm (SAtom v) = return $ LVar v
parseTerm (SInt n) = return $ LInt n
parseTerm (SList [SAtom "+", t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  return $ LAdd t1' t2'
parseTerm (SList [SAtom "-", t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  return $ LSub t1' t2'
parseTerm (SList [SAtom "lambda", SList [SAtom var], t]) = do
  t' <- parseTerm t
  return $ LAbs var t'
parseTerm (SList [t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  return $ LApp t1' t2'
parseTerm _ = throwError CompileError

toDeBrujin :: (MonadError CompileError m) => LTerm -> m DBTerm
toDeBrujin = toDeBrujin' [] where
  toDeBrujin' env (LInt n) = return $ DBInt n
  toDeBrujin' env (LAdd t1 t2) = do
    t1' <- toDeBrujin' env t1
    t2' <- toDeBrujin' env t2
    return $ DBAdd t1' t2'
  toDeBrujin' env (LSub t1 t2) = do
    t1' <- toDeBrujin' env t1
    t2' <- toDeBrujin' env t2
    return $ DBSub t1' t2'
  toDeBrujin' env (LAbs v t) = do
    t' <- toDeBrujin' (v:env) t
    return $ DBAbs t'
  toDeBrujin' env (LApp t1 t2) = do
    t1' <- toDeBrujin' env t1
    t2' <- toDeBrujin' env t2
    return $ DBApp t1' t2'
  toDeBrujin' env (LVar v) =
    case find v env of
        SS n -> return $ DBIndex n
        FF -> throwError CompileError
      where
        find v [] = FF
        find v (v':rest) | v == v' = SS 0
                         | otherwise = case find v rest of
                                          FF -> FF
                                          SS n -> SS (n + 1)

compileToSECD :: (MonadError CompileError m) => DBTerm -> m [Command]
compileToSECD (DBInt n) = return [IInt n]
compileToSECD (DBAbs t) = do
  c <- compileToSECD t
  return [IClosure (c ++ [IReturn])]
compileToSECD (DBApp t1 t2) = do
  c1 <- compileToSECD t1
  c2 <- compileToSECD t2
  return $ c1 ++ c2 ++ [IApply]
compileToSECD (DBAdd t1 t2) = do
  c1 <- compileToSECD t1
  c2 <- compileToSECD t2
  return $ c2 ++ c1 ++ [IAdd]
compileToSECD (DBSub t1 t2) = do
  c1 <- compileToSECD t1
  c2 <- compileToSECD t2
  return $ c2 ++ c1 ++ [ISub]
compileToSECD (DBIndex i) = return [IAccess i]

-- | Compile the given lisp code to SECD commands
compile :: String -> Either CompileError [Command]
compile = readSExpr' >=> parseTerm >=> toDeBrujin >=> compileToSECD
  where
    readSExpr' :: String -> Either CompileError SExpr
    readSExpr' = first (const ParseError) . readSExpr


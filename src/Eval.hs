module Eval
  ( evalR
  , defineOp
  )
where

import           Common
import           Monad
import           Operations
import           Data.Maybe
import qualified Data.List                     as L

-- Evaluador de relaciones
evalR :: Env -> Relation -> Either String Table
evalR e r = case runStateError (evalR' r) e of
              Left s       -> Left s
              Right (_, t) -> Right t

evalR' :: MonadStateError m => Relation -> m (Name, Table)
evalR' (Table n)     = do v <- lookfor n
                          case v of
                            Left t -> return (n, t)
                            _      -> throw (n ++ " es un operador, no una tabla.")
evalR' (Op n args)   = do v <- lookfor n
                          case v of
                            Right op -> do r' <- evalOp op args
                                           evalR' r'
                            _        -> throw (n ++ " es una tabla, no un operador.")
evalR' (Sel c r)     = do (n, t) <- evalR' r
                          t' <- sel n t c
                          return (n, t')
evalR' (Proj as r)   = do (n, t) <- evalR' r
                          t' <- proj n t as
                          return (n, t')
evalR' (Rename n' r) = do (_, t) <- evalR' r
                          t' <- rename n' t
                          return (n', t')
evalR' (Union r1 r2) = do (n1, t1) <- evalR' r1
                          (_, t2) <- evalR' r2
                          t' <- union t1 t2
                          return (n1, t')
evalR' (Diff r1 r2)  = do (n1, t1) <- evalR' r1
                          (_, t2) <- evalR' r2
                          t' <- diff t1 t2
                          return (n1, t')
evalR' (Prod r1 r2)  = do (n1, t1) <- evalR' r1
                          (n2, t2) <- evalR' r2
                          t' <- prod n1 n2 t1 t2
                          return (n1, t')

-- Evaluador de operaciones
evalOp :: MonadStateError m => Operator -> Args -> m Relation
evalOp (op, vars) args = let l1 = length vars
                             l2 = length args
                         in if l1 /= l2
                            then throw ("Se esperaban "
                                       ++ show l1
                                       ++ " argumentos, pero se obtuvieron"
                                       ++ show l2
                                       )
                            else return (replaceVars op vars args)


defineOp :: Env -> Operator -> Either String Relation
defineOp e op = case runStateError (defineOp' op) e of
                       Left s  -> Left s
                       Right r -> Right r

-- Reemplaza las ocurrencias de otros operadores por su definición, así como sus parámetros
defineOp' :: MonadStateError m => Operator -> m Relation
defineOp' (Table n, args)     = if elem n args
                                then return (Table n)
                                else throw (n ++ " no es un parámetro del operador")
defineOp' (Op n as, _)        = do r <- lookfor n
                                   case r of
                                     Right (op, vars) -> return (replaceVars op vars as)
                                     _                -> throw (n ++ " es una tabla, no un operador")
defineOp' (Sel c r, args)     = do op <- defineOp' (r, args)
                                   return (Sel c op)
defineOp' (Proj as r, args)   = do op <- defineOp' (r, args)
                                   return (Proj as op)
defineOp' (Rename n r, args)  = do op <- defineOp' (r, args)
                                   return (Rename n op)
defineOp' (Union r1 r2, args) = do op1 <- defineOp' (r1, args)
                                   op2 <- defineOp' (r2, args)
                                   return (Union op1 op2)
defineOp' (Diff r1 r2, args)  = do op1 <- defineOp' (r1, args)
                                   op2 <- defineOp' (r2, args)
                                   return (Diff op1 op2)
defineOp' (Prod r1 r2, args)  = do op1 <- defineOp' (r1, args)
                                   op2 <- defineOp' (r2, args)
                                   return (Prod op1 op2)

-- Reemplaza las variables por los argumentos dados
replaceVars :: Relation -> Args -> Args -> Relation
replaceVars (Table n)     vars args = let i = fromJust (L.elemIndex n vars)
                                      in (Table (args !! i))
replaceVars (Op n as)     vars args = let i = map (\a -> fromJust $ L.elemIndex a vars) args
                                          args' = map (as !!) i
                                      in (Op n args')
replaceVars (Sel c r)     vars args = let r' = replaceVars r vars args
                                      in (Sel c r')
replaceVars (Proj as r)   vars args = let r' = replaceVars r vars args
                                      in (Proj as r')
replaceVars (Rename n r)  vars args = let r' = replaceVars r vars args
                                      in (Rename n r')
replaceVars (Union r1 r2) vars args = let r1' = replaceVars r1 vars args
                                          r2' = replaceVars r2 vars args
                                      in (Union r1' r2')
replaceVars (Diff r1 r2)  vars args = let r1' = replaceVars r1 vars args
                                          r2' = replaceVars r2 vars args
                                      in (Diff r1' r2')
replaceVars (Prod r1 r2)  vars args = let r1' = replaceVars r1 vars args
                                          r2' = replaceVars r2 vars args
                                      in (Prod r1' r2')

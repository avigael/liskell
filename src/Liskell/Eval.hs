module Liskell.Eval where

import Liskell.Syntax

eval :: RExpr -> Either String RExpr
eval e = case e of
           NumC e -> Right (NumC e)
           BoolC e -> Right (BoolC e)
           StrC e -> Right (StrC e)
           Lam e -> Right (Lam e)
           Plus e1 e2 -> evalPlus e1 e2
           Subt e1 e2 -> evalSubt e1 e2
           Mult e1 e2 -> evalMult e1 e2
           Ifte e e1 e2 -> evalIfte e e1 e2
           And e1 e2 -> evalAnd e1 e2
           Or e1 e2 -> evalOr e1 e2
           Not e -> evalNot e
           IsEq e1 e2 -> evalIsEq e1 e2
           IsLt e1 e2 -> evalIsLt e1 e2
           IsGt e1 e2 -> evalIsGt e1 e2
           IsNil e -> evalIsNil e
           List es -> evalList es
           Cons e es -> evalCons e es
           Car e -> evalCar e
           Cdr e -> evalCdr e
           Var e -> Right (Var e)
           App e e' -> evalApp e e'
           Rec e -> evalRec e
           _ -> Left "Non-RExpr"

evalPlus :: RExpr -> RExpr -> Either String RExpr
evalPlus e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ NumC $ i1 + i2
                      _ -> Left "Add on non-numeric"

evalSubt :: RExpr -> RExpr -> Either String RExpr
evalSubt e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ NumC $ i1 - i2
                      _ -> Left "Sub on non-numeric"

evalMult :: RExpr -> RExpr -> Either String RExpr
evalMult e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ NumC $ i1 * i2
                      _ -> Left "Mul on non-numeric"

evalIfte :: RExpr -> RExpr -> RExpr -> Either String RExpr
evalIfte e e1 e2 = do e' <- eval e
                      e1' <- eval e1
                      e2' <- eval e2
                      case (e', e1', e2') of
                        (BoolC True, i1, i2) -> return i1
                        (BoolC False, il, i2) -> return i2
                        _ -> Left "If-then-else guard not Boolean"

evalAnd :: RExpr -> RExpr -> Either String RExpr
evalAnd e1 e2 = do e1' <- eval e1
                   e2' <- eval e2
                   case (e1', e2') of
                      (BoolC True, BoolC b1) -> return $ BoolC b1
                      (BoolC False, _) -> return $ BoolC False
                      _ -> Left "And on non-Boolean"

evalOr :: RExpr -> RExpr -> Either String RExpr
evalOr e1 e2 = do e1' <- eval e1
                  e2' <- eval e2
                  case (e1', e2') of
                    (BoolC True, _) -> return $ BoolC True
                    (BoolC False, BoolC b1) -> return $ BoolC b1
                    _ -> Left "Or on non-Boolean"

evalNot :: RExpr -> Either String RExpr
evalNot e = do e' <- eval e
               case e' of
                 (BoolC True) -> return (BoolC False)
                 (BoolC False) -> return (BoolC True)
                 _ -> Left "Not on non-Boolean"

evalIsEq :: RExpr -> RExpr -> Either String RExpr
evalIsEq e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ BoolC $ i1 == i2
                      _ -> Left "Equality on non-numeric"

evalIsLt :: RExpr -> RExpr -> Either String RExpr
evalIsLt e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ BoolC $ i1 < i2
                      _ -> Left "Lt on non-numeric"

evalIsGt :: RExpr -> RExpr -> Either String RExpr
evalIsGt e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ BoolC $ i1 > i2
                      _ -> Left "Gt on non-numeric"

evalIsNil :: RExpr -> Either String RExpr
evalIsNil e = do e' <- eval e
                 case e' of
                   (List []) -> return (BoolC True)
                   (List xs) -> return (BoolC False)
                   _ -> Left "IsNil on non-list"

evalList :: [RExpr] -> Either String RExpr
evalList es = case es of
                [] -> return $ List []
                (e:es) -> do e' <- eval e
                             es' <- evalList es
                             case (e',es') of
                               (e, List es) -> return $ List (e:es)

evalCons :: RExpr -> RExpr -> Either String RExpr
evalCons e e' = do e1 <- eval e
                   e2 <- eval e'
                   case (e1, e2) of
                     (e1, List e2) -> return $ List $ e1 : e2
                     _ -> Left "Cons on non-list"

evalCar :: RExpr -> Either String RExpr
evalCar e = do e1 <- eval e
               case e1 of
                 List [] -> Left "Car of empty list"
                 List (e:_) -> return e
                 _ -> Left "Car of non-list"

evalCdr :: RExpr -> Either String RExpr
evalCdr e = do e1 <- eval e
               case e1 of
                 List [] -> Left "Cdr of empty list"
                 List (_:es) -> return (List es)
                 _ -> Left "Cdr of non-list"

evalApp :: RExpr -> RExpr -> Either String RExpr
evalApp e e' = do e1 <- eval e
                  e2 <- eval e'
                  case (e1, e2) of
                    (Lam e1, e2) -> do e3 <- eval (subst e2 e1)
                                       return $ e3
                    _ -> Left "Apply non-function"

evalRec :: RExpr -> Either String RExpr
evalRec e = do e1 <- eval e
               case e1 of
                 (Rec e1) -> eval (subst e e1)

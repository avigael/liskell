module Liskell.Tests where

import Liskell.Eval
import Liskell.Parser
import Liskell.Syntax

import Control.Applicative (Alternative, liftA2)
import qualified Data.Map.Strict as Map
import Test.QuickCheck

--
--
-- Tests
--
-- This file is entirely optional, but it may be useful for testing. We give
-- some basic tests for evaluating and parsing here, you can run these tests in
-- `cabal v2-repl` by first loading the module with `:l Liskell.Tests`.
--
--


checkEq :: (Eq a, Show a) => a -> a -> String
checkEq got expected
    | expected == got = "PASS"
    | otherwise       = "FAIL Expected: " ++ show expected ++ " Got: " ++ show got

testEval :: IO ()
testEval = do
  putStr "Plus: "
  putStrLn $ checkEq (eval (Plus (NumC 1) (NumC 1))) (Right $ NumC 2)

  putStr "Ifte: "
  putStrLn $ checkEq (eval (Ifte (BoolC True) (NumC 1) (NumC 2))) (Right $ NumC 1)

  putStr "And: "
  putStrLn $ checkEq (eval (And (BoolC True) (BoolC False))) (Right $ BoolC False)

  putStr "IsEq: "
  putStrLn $ checkEq (eval (IsEq (Plus (NumC 2) (NumC 2)) (NumC 5))) (Right $ BoolC False)

  putStr "List: "
  putStrLn $ checkEq (eval (List [(Plus (NumC 1) (NumC 1)), (Plus (NumC 1) (NumC 1))])) (Right $ List [NumC 2, NumC 2])

  putStr "Car: "
  putStrLn $ checkEq (eval (Car $ List [NumC 2, NumC 2])) (Right $ NumC 2)

  putStr "Cdr: "
  putStrLn $ checkEq (eval (Cdr $ List [NumC 2, NumC 2])) (Right $ List [NumC 2])

  putStr "App: "
  putStrLn $ checkEq (eval (App (Lam (Plus (Var 1) (NumC 1))) (NumC 1))) (Right $ NumC 2)

testParse :: IO ()
testParse = do
  putStr "Num: "
  putStrLn $ checkEq (parseTest (parseRExpr Map.empty) "42") (Just $ NumC 42)

  putStr "Bool: "
  putStrLn $ checkEq (parseTest (parseRExpr Map.empty) "#t") (Just $ BoolC True)

  putStr "Plus: "
  putStrLn $ checkEq (parseTest (parseRExpr Map.empty) "(+ 1 1)") (Just $ Plus (NumC 1) (NumC 1))

  putStr "Ifte: "
  putStrLn $ checkEq (parseTest (parseRExpr Map.empty) "(if #t 1 2)") (Just $ Ifte (BoolC True) (NumC 1) (NumC 2))

  putStr "And: "
  putStrLn $ checkEq (parseTest (parseRExpr Map.empty) "(and #t #f)") (Just $ And (BoolC True) (BoolC False))

  putStr "List: "
  putStrLn $ checkEq (parseTest (parseRExpr Map.empty) "(list 1 2)") (Just $ List [NumC 1, NumC 2])

  putStr "Lam1: "
  putStrLn $ checkEq (parseTest (parseRExpr Map.empty) "(lambda x x)") (Just $ Lam (Var 1))

  putStr "Lam2: "
  putStrLn $ checkEq (parseTest (parseRExpr Map.empty) "(lambda x (lambda x x))") (Just $ Lam $ Lam (Var 1))

  putStr "Lam3: "
  putStrLn $ checkEq (parseTest (parseRExpr Map.empty) "(lambda x (lambda y x))") (Just $ Lam $ Lam (Var 2))

  putStr "App: "
  putStrLn $ checkEq (parseTest (parseRExpr Map.empty) "((lambda x x) 1)") (Just $ App (Lam (Var 1)) (NumC 1))

  putStr "Def: "
  putStrLn $ checkEq (parseTest (parseRExpr Map.empty) "(define x (lambda y x))") (Just $ Rec $ Lam (Var 2))

--
--
-- Expression generator
--
-- For more advanced tests, we can randomly generate expressions. Here, we set
-- up a QuickCheck generator for RExpr. You can use this generator with
-- QuickCheck properties to do property-based testing of your parser (see the
-- properties below). The basic generator we have given here is quite
-- limited----it doesn't generate very many kinds of RExpr. But you can extend
-- the generator to cover more expressions.
--
-- To run the QuickCheck tests, use `cabal v2-repl` and then type `:l
-- Liskell.Tests` to load the module. Then, you can do
--
-- `quickCheck genClosed`
-- `quickCheck printParse`
--
-- to run each test 100 times.
--
--
genSize :: Int
genSize = 5

genRExpr :: Gen RExpr
genRExpr = resize genSize $ sized $ \d -> genRExpr' d 0

--
--
-- An expression generator
--
-- We've given you a basic expression generator. To improve your tests, you can
-- extend this generator to cover more of the AST.
--
-- The first parameter d controls the size. Note that we cut down the size when
-- generating lists, so that the size of generated terms doesn't explode. The
-- second parameter n measures how local variables there are in the context.
-- Note that a Var node is only generated when n is at least 1, and n should be
-- incremented when generating the body of a lambda or a recursive definition.
--
--
genRExpr' :: Int -> Int -> Gen RExpr
genRExpr' d n
  | d <= 0 = oneof [ NumC <$> choose(1,9)
                   , BoolC <$> arbitrary
                   , StrC <$> pure "str"
                   ]
  | n == 0 = oneof astOpts
  | n > 0 = oneof $ (Var <$> choose (1, n)) : astOpts
  where astOpts = [ NumC <$> arbitrary
                  , Plus <$> genRExpr' (d - 1) n <*> genRExpr' (d - 1) n
                  , List <$> resize (d `div` 2) (listOf (genRExpr' (d - 1) n))
                  ]

--
--
-- Check if an expression is closed (has no free variables).
--
--
isClosed :: RExpr -> Bool
isClosed = isClosed' 0
  where both f (x, y) = (f x, f y)
        isClosed' n e = case e of
                       Plus e1 e2 -> isClosed' n e1 && isClosed' n e2
                       Subt e1 e2 -> isClosed' n e1 && isClosed' n e2
                       Mult e1 e2 -> isClosed' n e1 && isClosed' n e2
                       Ifte e e1 e2 -> isClosed' n e && isClosed' n e1 && isClosed' n e2
                       Cond cs ow -> all (uncurry (&&)) (map (both (isClosed' n)) cs) && isClosed' n ow
                       And e1 e2 -> isClosed' n e1 && isClosed' n e2
                       Or e1 e2 -> isClosed' n e1 && isClosed' n e2
                       Not e -> isClosed' n e
                       IsEq e1 e2 -> isClosed' n e1 && isClosed' n e2
                       IsLt e1 e2 -> isClosed' n e1 && isClosed' n e2
                       IsGt e1 e2 -> isClosed' n e1 && isClosed' n e2
                       IsNil e -> isClosed' n e
                       List es -> all (isClosed' n) es
                       Cons e1 e2 -> isClosed' n e1 && isClosed' n e2
                       Car e -> isClosed' n e
                       Cdr e -> isClosed' n e
                       Var i -> i <= n
                       Lam e -> isClosed' (n + 1) e
                       App e1 e2 -> isClosed' n e1 && isClosed' n e2
                       Rec e -> isClosed' (n + 1) e
                       _ -> True

--
--
-- Quickcheck property: generated terms are closed expressions
--
-- You can use this property to check if your generator is producing closed
-- expressions (this is particularly important if you are generating lambdas or
-- recursive definitions).
--
--
genClosed :: Property
genClosed = forAll genRExpr isClosed

--
--
-- Quickcheck property: pretty-print/parse
--
--
printParse :: Property
printParse = forAll genRExpr (\e -> Just e == parseTest (parseRExpr Map.empty) (ppRExpr e))

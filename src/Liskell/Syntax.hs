module Liskell.Syntax where

--
--
-- Abstract Syntax Tree for Liskell
--
--
data RExpr = NumC Int
           | BoolC Bool
           | StrC String
           | Plus RExpr RExpr
           | Subt RExpr RExpr
           | Mult RExpr RExpr
           | Ifte RExpr RExpr RExpr
           | Cond [(RExpr, RExpr)] RExpr
           | And RExpr RExpr
           | Or RExpr RExpr
           | Not RExpr
           | IsEq RExpr RExpr
           | IsLt RExpr RExpr
           | IsGt RExpr RExpr
           | IsNil RExpr
           | List [RExpr]
           | Cons RExpr RExpr
           | Car RExpr
           | Cdr RExpr
           | Var Int
           | Lam RExpr
           | App RExpr RExpr
           | Rec RExpr deriving (Eq, Show)

--
--
-- Substitute an expression into another expression
--
-- `subst from into` gives the result of substituting `from` for the first free
-- variable in `into`. For instance, the result of calling a function `Lam e` on
-- argument `e'` is `subst e e'`.
--
--
subst :: RExpr -> RExpr -> RExpr
subst from = s' 1
  where s' n into = case into of
                      Plus e1 e2 -> Plus (s' n e1) (s' n e2)
                      Subt e1 e2 -> Subt (s' n e1) (s' n e2)
                      Mult e1 e2 -> Mult (s' n e1) (s' n e2)
                      Ifte e e1 e2 -> Ifte (s' n e) (s' n e1) (s' n e2)
                      Cond cs ow -> Cond (map (fmap $ s' n) cs) (s' n ow)
                      And e1 e2 -> And (s' n e1) (s' n e2)
                      Or e1 e2 -> Or (s' n e1) (s' n e2)
                      Not e -> Not (s' n e)
                      IsEq e1 e2 -> IsEq (s' n e1) (s' n e2)
                      IsLt e1 e2 -> IsLt (s' n e1) (s' n e2)
                      IsGt e1 e2 -> IsGt (s' n e1) (s' n e2)
                      IsNil e -> IsNil (s' n e)
                      List es -> List $ map (s' n) es
                      Cons e es -> Cons (s' n e) (s' n es)
                      Car e -> Car (s' n e)
                      Cdr e -> Cdr (s' n e)
                      Var i -> if i == n then from else into
                      Lam e -> Lam $ s' (n + 1) e
                      App e e' -> App (s' n e) (s' n e')
                      Rec e -> Rec $ s' (n + 1) e
                      _ -> into

--
--
-- Pretty Printer
--
--
ppRExpr :: RExpr -> String
ppRExpr = pp' 1
  where pp' n e = case e of
                    NumC i -> show i
                    BoolC True -> "#t"
                    BoolC False -> "#f"
                    StrC s -> "\"" ++ s ++ "\""
                    Plus e1 e2 -> "(+ " ++ pp' n e1 ++ " " ++ pp' n e2 ++ ")"
                    Subt e1 e2 -> "(- " ++ pp' n e1 ++ " " ++ pp' n e2 ++ ")"
                    Mult e1 e2 -> "(* " ++ pp' n e1 ++ " " ++ pp' n e2 ++ ")"
                    Ifte e e1 e2 -> "(if " ++ pp' n e ++ " " ++ pp' n e1 ++ " " ++ pp' n e2 ++ ")"
                    Cond cs ow -> "(cond (" ++ unwords (map (ppCase n) cs) ++ ") (else " ++ pp' n ow ++ "))"
                    And e1 e2 -> "(and " ++ pp' n e1 ++ " " ++ pp' n e2 ++ ")"
                    Or e1 e2 -> "(or " ++ pp' n e1 ++ " " ++ pp' n e2 ++ ")"
                    Not e -> "(not " ++ pp' n e ++ ")"
                    IsEq e1 e2 -> "(eq? " ++ pp' n e1 ++ " " ++ pp' n e2 ++ ")"
                    IsLt e1 e2 -> "(< " ++ pp' n e1 ++ " " ++ pp' n e2 ++ ")"
                    IsGt e1 e2 -> "(> " ++ pp' n e1 ++ " " ++ pp' n e2 ++ ")"
                    IsNil e -> "(nil? " ++ pp' n e ++ ")"
                    List [] -> "()"
                    List es -> "(list " ++ unwords (map (pp' n) es) ++ ")"
                    Cons e e' -> "(cons " ++ pp' n e ++ " " ++ pp' n e' ++ ")"
                    Car e -> "(car " ++ pp' n e ++ ")"
                    Cdr e -> "(cdr " ++ pp' n e ++ ")"
                    Var i -> "x" ++ show (n - i)
                    Lam e -> "(lambda x" ++ show n ++ " " ++ pp' (n + 1) e ++ ")"
                    App e e' -> "(" ++ pp' n e ++ " " ++ pp' n e' ++ ")"
                    Rec e -> "(define x" ++ show n ++ " " ++ pp' (n + 1) e ++ ")"
        ppCase n (g, b) = "(" ++ pp' n g ++ " " ++ pp' n b ++ ")"

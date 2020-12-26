# Liskell Language

Liskell is a simple example of the Lisp family of programming languages. Liskell is an untyped lambda calculus with useful extensions and different syntax.

## Testing

To run these tests, you can use the Haskell repl (`cabal v2-repl`), load the Tests module (`:l Liskell.Tests`), and then run your test (`testEval`/`testParse`).

## Conditional expressions

Liskell has conditional expressions, much like other Lisp languages. Conditional expressions have the following syntax:

```
(cond ((e1 e1') (e2 e2')... (en en')) (else e))
```

where all `e` are expressions.

That is, there is a list of pairs `(e e')` where the components of each pair are separated by one-or-more spaces, and each pair is separated by one-or-more spaces, and there is a final pair of the form `(else e)`. There may be zero pairs, in which case the expression looks like this:

```
(cond () (else e))
```

To evaluate conditional expressions, the idea is that in each pair, the first element should be a boolean-valued guard expression. Evaluation should evaluate each pair---in order---until a guard expression evaluates to `#t`. The conditional expression should then step to the second component of that pair. If no guard evaluates to true, the expression steps to the body of the `else` branch. For instance, the following expression steps to 5:

```
(cond (
  ((eq? 1 2) 4)
  ((eq? 1 1) 5))
  (else 6))
```
Linebreaks have been added for readability, but they are not needed.

Extend the evaluator and parser to handle conditional expressions. You should use the `Cond` AST node we have given you in `RExpr`.


## Variable-length operations

Many of the arithmetic and boolean operations in Liskell can be extended to handle multiple arguments, like in Scheme:

```
(+ 1 2 3)
(and #t #t #f #t)
```

```
(+ 1 2 3 4)
```

should parse to

```
Plus (NumC 1) (Plus (NumC 2) (Plus (NumC 3) (NumC 4)))
```

## Let-bindings

Scheme has let-bindings to declare new local variables. These have the following syntax:

```
(let ((var1 e1) (var2 e2) ... (varn en)) e)
```

The idea is the pairs bind the variables `var1`, ..., `varn` to `e1`, ..., `en`, and the body expression `e` can mention `var1`, ..., `varn`. Also, later bindings can mention variables from earlier bindings.

```
(let ((x e1) (y e2)) e)
```

is *syntactic sugar* for nested functions and applications:

```
((lambda x ((lambda y e) e2)) e1)
```

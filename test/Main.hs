{-# LANGUAGE NoMonomorphismRestriction #-}

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import LambdaCalculusDSL

ex1 = add (lit 1) (lit 2)
ex2 = sub (lit 1) (lit 2)
ex3 = sub (mul (lit 1) (lit 2)) (lit 3)
ex4 = add (lit 1) (sym "x")
ex5 = add (lit 1) (mul (sym "x") (sym "y"))
ex6 = neg (sub (mul (lit 1) (lit 2)) (sym "x"))
ex7 = neg (sub (mul (lit 1) (lit 2)) (neg (sym "x")))
ex8 = app (lam (\x -> add (lit 1) x)) (lit 2)
ex9 = app (lam (\x -> add ex7 x)) (sym "z")

symMap :: String -> Int
symMap "x" = 42
symMap "y" = -42
symMap "z" = 0

tests = [
        testGroup "eval" [
            testCase "eval ex1" (eval ex1 @?= 3),
            testCase "eval ex2" (eval ex2 @?= -1),
            testCase "eval ex3" (eval ex3 @?= -1),
            testCase "eval ex8" (eval ex8 @?= 3)
        ],
        testGroup "evalSym" [
            testCase "evalSym sym 'x'" (evalSym (sym "x") symMap @?= 42),
            testCase "evalSym sym 'y'" (evalSym (sym "y") symMap @?= -42),
            testCase "evalSym sym 'z'" (evalSym (sym "z") symMap @?= 0),
            testCase "evalSym ex1" (evalSym ex1 symMap @?= 3),
            testCase "evalSym ex2" (evalSym ex2 symMap @?= -1),
            testCase "evalSym ex3" (evalSym ex3 symMap @?= -1),
            testCase "evalSym ex4" (evalSym ex4 symMap @?= 43),
            testCase "evalSym ex5" (evalSym ex5 symMap @?= -1763),
            testCase "evalSym ex6" (evalSym ex6 symMap @?= 40),
            testCase "evalSym ex7" (evalSym ex7 symMap @?= -44),
            testCase "evalSym ex8" (evalSym ex8 symMap @?= 3),
            testCase "evalSym ex9" (evalSym ex9 symMap @?= -44)
        ],
        testGroup "view" [
            testCase "view ex1" (view ex1 @?= "1 + 2"),
            testCase "view ex2" (view ex2 @?= "1 + -2"),
            testCase "view ex3" (view ex3 @?= "1 * 2 + -3"),
            testCase "view ex4" (view ex4 @?= "1 + x"),
            testCase "view ex5" (view ex5 @?= "1 + x * y"),
            testCase "view ex6" (view ex6 @?= "-(1 * 2 + -x)"),
            testCase "view ex7" (view ex7 @?= "-(1 * 2 + --x)"),
            testCase "view ex8" (view ex8 @?= "(\\x0 -> 1 + x0) 2"),
            testCase "view ex9" (view ex9 @?= "(\\x0 -> -(1 * 2 + --x) + x0) z")
        ],
        testGroup "doubleNegElimination" [
            testCase "doubleNegElimination ex7" (view (doubleNegElimination ex7) @?= "-(1 * 2 + x)"),
            testCase "doubleNegElimination ex9" (view (doubleNegElimination ex9) @?= "(\\x0 -> -(1 * 2 + x) + x0) z")
        ],
        testGroup "pushNeg" [
            testCase "pushNeg ex6" (view (pushNeg ex6) @?= "-1 * 2 + x"),
            testCase "pushNeg ex7" (view (pushNeg ex7) @?= "-1 * 2 + -x")
        ],
        testGroup "constantProp" [
            testCase "constantProp ex1" (view (constantProp ex1) @?= "3"),
            testCase "constantProp ex2" (view (constantProp ex2) @?= "-1"),
            testCase "constantProp ex3" (view (constantProp ex3) @?= "-1"),
            testCase "constantProp ex4" (view (constantProp ex4) @?= "1 + x"),
            testCase "constantProp ex5" (view (constantProp ex5) @?= "1 + x * y"),
            testCase "constantProp ex6" (view (constantProp ex6) @?= "-(2 + -x)"),
            testCase "constantProp ex7" (view (constantProp ex7) @?= "-(2 + --x)"),
            testCase "constantProp ex8" (view (constantProp ex8) @?= "3"),
            testCase "constantProp ex9" (view (constantProp ex9) @?= "-(2 + --x) + z")
        ]
    ]

main :: IO ()
main = defaultMain tests

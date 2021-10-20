module LambdaCalculusDSL.Base.R where

import LambdaCalculusDSL.Base.Base

data R a = R { eval :: a}

instance Base R where
    lit = R
    neg x = R $ -(eval x)
    add x y = R $ eval x + eval y
    mul x y = R $ eval x * eval y

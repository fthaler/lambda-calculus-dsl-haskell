module LambdaCalculusDSL.HigherOrder.R where

import LambdaCalculusDSL.Base.R (R(..))
import LambdaCalculusDSL.HigherOrder.HigherOrder

instance HigherOrder R where
    lam f = R $ eval . f . R
    app f x = R $ (eval f) (eval x)

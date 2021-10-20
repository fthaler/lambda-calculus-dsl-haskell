module LambdaCalculusDSL.HigherOrder.RV where

import LambdaCalculusDSL.Symbolic.RV
import LambdaCalculusDSL.HigherOrder.HigherOrder

instance HigherOrder RV where
    lam f = RV $ \symMap -> (\x -> evalSym (f (RV . const $ x)) symMap)
    app f x = RV $ \symMap -> (evalSym f symMap) (evalSym x symMap)

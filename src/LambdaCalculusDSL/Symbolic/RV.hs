module LambdaCalculusDSL.Symbolic.RV where

import LambdaCalculusDSL.Base.Base
import LambdaCalculusDSL.Symbolic.Symbolic

type SymbolMap = String -> Int
data RV a = RV { evalSym :: (SymbolMap -> a) }

instance Base RV where
    lit x = RV $ const x
    neg x = RV $ \symMap -> -(evalSym x symMap)
    add x y = RV $ \symMap -> evalSym x symMap + evalSym y symMap
    mul x y = RV $ \symMap -> evalSym x symMap * evalSym y symMap

instance Symbolic RV where
    sym x = RV $ \symMap -> symMap x

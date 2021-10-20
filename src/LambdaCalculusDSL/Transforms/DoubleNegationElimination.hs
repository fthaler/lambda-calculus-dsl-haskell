{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module LambdaCalculusDSL.Transforms.DoubleNegationElimination (doubleNegElimination) where

import LambdaCalculusDSL.Base.Base
import LambdaCalculusDSL.Symbolic.Symbolic
import LambdaCalculusDSL.HigherOrder.HigherOrder
import LambdaCalculusDSL.Transforms.RR

data Annotation repr a where
    Keep :: repr a -> Annotation repr a
    Negate :: repr Int -> Annotation repr Int

instance Base repr => RR Annotation repr where
    bwd (Keep x) = x
    bwd (Negate x) = neg x
    fwd = Keep

instance Base repr => Base (Annotation repr) where
    lit = fwd . lit
    neg (Keep x) = Negate x
    neg (Negate x) = Keep x
    add = map2 add
    mul = map2 mul

instance Symbolic repr => Symbolic (Annotation repr) where
    sym = fwd . sym

instance HigherOrder repr => HigherOrder (Annotation repr) where
    lam f = fwd $ lam $ bwd . f . fwd
    app = map2 app

doubleNegElimination :: Base repr => Annotation repr a -> repr a
doubleNegElimination = bwd

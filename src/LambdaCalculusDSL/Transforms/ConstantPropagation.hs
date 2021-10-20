{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module LambdaCalculusDSL.Transforms.ConstantPropagation (constantProp) where

import LambdaCalculusDSL.Base.Base
import LambdaCalculusDSL.Symbolic.Symbolic
import LambdaCalculusDSL.HigherOrder.HigherOrder
import LambdaCalculusDSL.Transforms.RR

data Annotation repr a where
    Unknown :: repr a -> Annotation repr a
    Literal :: Int -> Annotation repr Int
    Function :: (Annotation repr a -> Annotation repr b) -> Annotation repr (a -> b)

instance (Base repr, HigherOrder repr) => RR Annotation repr where
    bwd (Unknown x) = x
    bwd (Literal x) = lit x
    bwd (Function f) = lam $ bwd . f . fwd
    fwd = Unknown

instance (Base repr, HigherOrder repr) => Base (Annotation repr) where
    lit x = Literal x
    neg (Literal x) = Literal $ -x
    neg x = map1 neg x
    add (Literal x) (Literal y) = Literal $ x + y
    add x y = map2 add x y
    mul (Literal x) (Literal y) = Literal $ x * y
    mul x y = map2 mul x y

instance HigherOrder repr => HigherOrder (Annotation repr) where
    lam f = Function f
    app (Function f) x = f x
    app f x = map2 app f x

instance (Symbolic repr, HigherOrder repr) => Symbolic (Annotation repr) where
    sym = fwd . sym

constantProp :: HigherOrder repr => Annotation repr a -> repr a
constantProp = bwd

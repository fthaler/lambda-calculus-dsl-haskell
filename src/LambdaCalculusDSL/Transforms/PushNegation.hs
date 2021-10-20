{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module LambdaCalculusDSL.Transforms.PushNegation (pushNeg) where

import LambdaCalculusDSL.Base.Base
import LambdaCalculusDSL.Symbolic.Symbolic
import LambdaCalculusDSL.HigherOrder.HigherOrder
import LambdaCalculusDSL.Transforms.RR

data Context = Positive | Negative

data Annotation repr a where
    Unknown :: (Context -> repr a) -> Annotation repr a
    Invertible :: (Context -> repr Int) -> Annotation repr Int

instance Base repr => RR Annotation repr where
    bwd (Unknown f) = f Positive
    bwd (Invertible f) = f Positive
    fwd x = Unknown $ \ctx -> case ctx of
        Positive -> x

instance Base repr => Base (Annotation repr) where
    lit x = Invertible $ \ctx -> case ctx of
                            Positive -> lit x
                            Negative -> lit (-x)
    neg (Invertible x) = Invertible $ \ctx -> case ctx of
                            Positive -> x Negative
                            Negative -> x Positive
    add (Invertible x) (Invertible y) = Invertible $ \ctx -> case ctx of
                            Positive -> add (x Positive) (y Positive)
                            Negative -> add (x Negative) (y Negative)
    mul (Invertible x) (Invertible y) = Invertible $ \ctx -> case ctx of
                            Positive -> mul (x Positive) (y Positive)
                            Negative -> mul (x Negative) (y Positive)

instance Symbolic repr => Symbolic (Annotation repr) where
    sym x = Invertible $ \ctx -> case ctx of
                            Positive -> sym x
                            Negative -> neg (sym x)

instance HigherOrder repr => HigherOrder (Annotation repr) where
    lam f = fwd $ lam $ bwd . f . fwd
    app = map2 app

pushNeg :: Base repr => Annotation repr a -> repr a
pushNeg = bwd

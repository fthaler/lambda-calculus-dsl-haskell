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

maybe_negate x = Invertible $ \ctx -> case ctx of
                            Positive -> x
                            Negative -> neg x

instance Base repr => Base (Annotation repr) where
    lit x = Invertible $ \ctx -> case ctx of
                            Positive -> lit x
                            Negative -> lit (-x)
    neg (Invertible x) = Invertible $ \ctx -> case ctx of
                            Positive -> x Negative
                            Negative -> x Positive
    neg x = Invertible $ \ctx -> case ctx of
                            Positive -> neg (bwd x)
                            Negative -> bwd x
    add (Invertible x) (Invertible y) = Invertible $ \ctx -> case ctx of
                            Positive -> add (x Positive) (y Positive)
                            Negative -> add (x Negative) (y Negative)
    add x y = maybe_negate $ add (bwd x) (bwd y)
    mul (Invertible x) y = Invertible $ \ctx -> case ctx of
                            Positive -> mul (x Positive) (bwd y)
                            Negative -> mul (x Negative) (bwd y)
    mul x (Invertible y) = Invertible $ \ctx -> case ctx of
                            Positive -> mul (bwd x) (y Positive)
                            Negative -> mul (bwd x) (y Negative)
    mul x y = maybe_negate $ mul (bwd x) (bwd y)

instance Symbolic repr => Symbolic (Annotation repr) where
    sym x = maybe_negate $ sym x

instance HigherOrder repr => HigherOrder (Annotation repr) where
    lam f = fwd $ lam $ bwd . f . fwd
    app = map2 app

pushNeg :: Base repr => Annotation repr a -> repr a
pushNeg = bwd

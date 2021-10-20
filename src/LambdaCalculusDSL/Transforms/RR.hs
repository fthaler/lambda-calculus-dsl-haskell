{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaCalculusDSL.Transforms.RR where

class RR t repr where
    fwd :: repr a -> t repr a
    bwd :: t repr a -> repr a
   
    map1 :: (repr a -> repr b) -> (t repr a -> t repr b)
    map1 f = fwd . f . bwd

    map2 :: (repr a -> repr b -> repr c) -> (t repr a -> t repr b -> t repr c)
    map2 f x y = fwd $ f (bwd x) (bwd y)

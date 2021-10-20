module LambdaCalculusDSL.Base.Base where

class Base repr where
    lit :: Int -> repr Int
    neg :: repr Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    mul :: repr Int -> repr Int -> repr Int

sub x y = add x (neg y)

module LambdaCalculusDSL.Viewer (view) where

import LambdaCalculusDSL.Base.Base
import LambdaCalculusDSL.Symbolic.Symbolic
import LambdaCalculusDSL.HigherOrder.HigherOrder

type Precedence = Int
data S a = S (Precedence -> String)

parenp :: Bool -> String -> String
parenp True x = "(" ++ x ++ ")"
parenp False x = x

instance Base S where
    lit x = S $ const $ show x
    neg (S x) = S $ \p -> parenp (p > 3) $ "-" ++ x 3
    add (S x) (S y) = S $ \p -> parenp (p > 1) $ x 1 ++ " + " ++ y 1
    mul (S x) (S y) = S $ \p -> parenp (p > 2) $ x 2 ++ " * " ++ y 2

instance Symbolic S where
    sym x = S $ const x

instance HigherOrder S where
    lam f = S $ \p -> parenp (p > 0) $ "\\" ++ var ++ " -> " ++ view (f (S . const $ var))
        where
            body0 = view (f (S . const $ "DUMMY"))
            var = "x" ++ show (count '\\' body0)
            count c = length . filter (==c)

    app (S f) (S x) = S $ \p -> parenp (p > 4) $ f 4 ++ " " ++ x 5

view :: S a -> String
view (S x) = x 0

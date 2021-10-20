module LambdaCalculusDSL.HigherOrder.HigherOrder where

import LambdaCalculusDSL.Base.Base

class Base repr => HigherOrder repr where
    lam :: (repr a -> repr b) -> repr (a -> b)
    app :: repr (a -> b) -> (repr a -> repr b)

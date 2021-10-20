module LambdaCalculusDSL.Symbolic.Symbolic where

import LambdaCalculusDSL.Base.Base

class Base repr => Symbolic repr where
    sym :: String -> repr Int

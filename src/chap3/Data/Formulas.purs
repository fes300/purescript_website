module Formulas where

import Prelude ((*))
import Data.Int as Int
import Data.Either (Either(..))
import Math as Math

circleArea :: Either Number Int -> Number
circleArea (Left x) = (Math.pow x  (Int.toNumber 2)) * Math.pi
circleArea (Right x) = (Int.toNumber (Int.pow x  2)) * Math.pi

module Measurement where

data Unit = Meter
  | Yard
  deriving (Ord, Eq)

instance Show Unit where
  show Meter = "m"
  show Yard = "y"

factor :: Unit -> Double
factor Meter = 1.2
factor Yard = 1 / (factor Meter)

inverse :: Unit -> Unit
inverse Meter = Yard
inverse Yard = Meter

convert :: (Double, Unit) -> (Double, Unit)
convert (n,u) = (n * (factor u), inverse u)

data Measurement = Measurement Double Unit deriving (Eq)

instance Show Measurement where
  show (Measurement n u) = (show n) ++ " " ++ (show u)





data MetricUnit = Meter | Liter deriving (Show, Eq)

data ImpU = Yard | Gallon deriving (Show, Eq)

symbol :: MetricUnit -> String
symbol Meter = "m"
symbol Liter = "L"


s2 x
  | x==Meter = "m"

data Measurement = MetricM Double MetricUnit | ImpM Double ImpU deriving (Show)



data Point = Point {xval::Double, yval::Double}

data ThreeT a b = ThreeT a b a deriving (Show)

module Main where

import Prelude
import Math
import Flare

data TUnit = Celsius | Kelvin | Fahrenheit

instance showTUnit :: Show TUnit where
  show Celsius = "°C"
  show Kelvin = "K"
  show Fahrenheit = "°F"

toKelvin :: TUnit -> Number -> Number
toKelvin Celsius    tc = tc + 273.15
toKelvin Kelvin     tk = tk
toKelvin Fahrenheit t = (t + 459.67) * 5.0 / 9.0

fromKelvin :: TUnit -> Number -> Number
fromKelvin Celsius    tc = tc - 273.15
fromKelvin Kelvin     tk = tk
fromKelvin Fahrenheit tf = (tf * 9.0 / 5.0) - 459.67

convert :: Number -> TUnit -> TUnit -> Number
convert t from to = (round <<< fromKelvin to <<< toKelvin from) t

flare = convert <$> number "Temperature" 100.0
                <*> unit "Unit"
                <*> unit "Convert to"

  where unit label = radioGroup label Celsius [Kelvin, Fahrenheit]

main = runFlare "controls" "output" flare

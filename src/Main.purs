module Main where

import Prelude
import Math
import Flare

data TUnit = Celsius | Kelvin | Fahrenheit

toString :: TUnit -> String
toString Celsius    = "°C"
toString Kelvin     = "K"
toString Fahrenheit = "°F"

toKelvin :: TUnit -> Number -> Number
toKelvin Celsius    tc = tc + 273.15
toKelvin Kelvin     tk = tk
toKelvin Fahrenheit t = (t + 459.67) * 5.0 / 9.0

fromKelvin :: TUnit -> Number -> Number
fromKelvin Celsius    tc = tc - 273.15
fromKelvin Kelvin     tk = tk
fromKelvin Fahrenheit tf = (tf * 9.0 / 5.0) - 459.67

convert :: TUnit -> TUnit -> Number -> Number
convert from to = round <<< fromKelvin to <<< toKelvin from

render :: Number -> TUnit -> TUnit -> String
render t from to = show t  ++ toString from ++ " corresponds to " ++
                   show t' ++ toString to
  where t' = convert from to t

flare = render <$> number "Temperature" 100.0
               <*> unit "Unit"
               <*> unit "Convert to"

  where unit label = radioGroup label Celsius [Kelvin, Fahrenheit] toString

main = runFlare "controls" "output" flare

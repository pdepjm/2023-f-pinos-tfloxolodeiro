{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat

pesoPino :: Number -> Number
pesoPino altura = pesoBase altura + pesoCopa altura

pesoBase :: Number -> Number
pesoBase altura = 3 * metrosACm (longitudDeLaBase altura)

pesoCopa :: Number -> Number
pesoCopa altura = 2 * metrosACm (longitudDeLaCopa altura)

longitudMaximaDeBase :: Number
longitudMaximaDeBase = 3

longitudDeLaBase :: Number -> Number
longitudDeLaBase altura = min longitudMaximaDeBase altura

longitudDeLaCopa :: Number -> Number
longitudDeLaCopa altura = max 0 (altura - longitudMaximaDeBase)

metrosACm :: Number -> Number
metrosACm metros = metros * 100

esPesoUtil :: Number -> Bool
esPesoUtil peso = 400 <= peso && peso <= 1000

sirvePino :: Number -> Bool
sirvePino altura = esPesoUtil (pesoPino altura)

costoTransporte :: Number -> Number
costoTransporte altura
    | esLigero altura = 5000
    | esMediano altura = costoTransporteMediano altura
    | esPesado altura = costoTransporteMediano altura + metrosACm altura --Aca se podria usar otherwise en vez de "esPesado altura"

esLigero :: Number -> Bool
esLigero altura = pesoPino altura < 500

esMediano :: Number -> Bool
esMediano altura = pesoPino altura >= 500 && pesoPino altura <= 800 --El enunciado tiene un error, deberia decir que uno mediano pesa entre 500 y 800.

esPesado :: Number -> Bool
esPesado altura = pesoPino altura > 800

costoTransporteMediano :: Number -> Number
costoTransporteMediano altura = 10 * pesoPino altura


---- Alternativa de pesoPino con guardas:
pesoPino2 :: Number -> Number
pesoPino2 altura 
    | altura <= longitudMaximaDeBase = pesoBase2 altura
    | otherwise = pesoBase2 longitudMaximaDeBase + pesoCopa2 (altura - longitudMaximaDeBase)

pesoBase2 :: Number -> Number
pesoBase2 alturaBase = 3 * metrosACm alturaBase

pesoCopa2 :: Number -> Number
pesoCopa2 alturaCopa = 2 * metrosACm alturaCopa


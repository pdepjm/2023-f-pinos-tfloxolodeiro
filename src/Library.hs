module Library where
import PdePreludat

pesoPino :: Number -> Number
pesoPino altura = pesoBase altura + pesoCopa altura

pesoBase :: Number -> Number
pesoBase altura = 3 * metrosACm (longitudDeLaBase altura)

pesoCopa :: Number -> Number
pesoCopa altura = 2 * metrosACm (longitudDeLaCopa altura)

longitudDeLaBase :: Number -> Number
longitudDeLaBase altura = min 3 altura

longitudDeLaCopa :: Number -> Number
longitudDeLaCopa altura = max 0 (altura - 3)

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
    | esPesado altura = costoTransporteMediano altura + metrosACm altura

esLigero :: Number -> Bool
esLigero altura = pesoPino altura < 500

esMediano :: Number -> Bool
esMediano altura = pesoPino altura >= 500 && pesoPino altura <= 800

esPesado :: Number -> Bool
esPesado altura = pesoPino altura > 800

costoTransporteMediano :: Number -> Number
costoTransporteMediano altura = 10 * pesoPino altura


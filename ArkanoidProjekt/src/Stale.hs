module Stale where
import Graphics.Gloss.Interface.Pure.Game

--ZMIANA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!------
--ogolne

trybWyswietlania :: Display
trybWyswietlania = InWindow "Arkanoid" (1024, 600) (0, 0)
--InWindow "ARKANOID" (1600, 900) (100, 100)

wysokoscObszaruGry :: Float
wysokoscObszaruGry = 600

szerokoscObszaruGry :: Float
szerokoscObszaruGry = 400

kolorTla :: Color
kolorTla = black

wspolczynnikPredkosci :: Float
wspolczynnikPredkosci = 65

zakresLosowosci :: (Float, Float)
zakresLosowosci = (-2 * wspolczynnikPredkosci, 2 * wspolczynnikPredkosci)

fps :: Int
fps = 120

sciezkaDoWynikow :: String
sciezkaDoWynikow = "ZapisWynikow/wyniki.txt"


--deska

wysokoscDeski :: Float
wysokoscDeski = 10

dlugoscDeski :: Float
dlugoscDeski = 100

wyjsciowaPozycjaDeski :: (Float, Float)
wyjsciowaPozycjaDeski = (0, -250)

predkoscDeski :: Float
predkoscDeski = 200

zakresKataUderzeniaDeski :: (Float, Float)
zakresKataUderzeniaDeski = (-4.3 * wspolczynnikPredkosci, 4.3 * wspolczynnikPredkosci)


--pilka

promienPilki :: Float
promienPilki = 5

wyjsciowaPozycjaPilki :: Point
wyjsciowaPozycjaPilki = (0,-100)

stalaPredkosciPilki :: Float
stalaPredkosciPilki = 4.5 * wspolczynnikPredkosci

wyjsciowaPredkoscPilki :: Vector
wyjsciowaPredkoscPilki = (0,-stalaPredkosciPilki/fromIntegral fps) --wektor predkosci pilki na starcie


--klocek

wysokoscKlocka :: Float
wysokoscKlocka = 15

szerokoscKlocka ::Float
szerokoscKlocka = 50

szerokoscSciany :: Float
szerokoscSciany = 5





{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module DzialanieGry where

import System.Directory ( createDirectoryIfMissing, doesFileExist )
import Data.Maybe (isNothing)
import Graphics.Gloss.Interface.Pure.Game

import Stale
import TypyDanych


ileZostaloKlockowNaPlanszy :: PlanszaKlockow -> Int
ileZostaloKlockowNaPlanszy (PlanszaKlockow [] jakiesUderzenie) = 0
ileZostaloKlockowNaPlanszy (PlanszaKlockow (rzad:xs) jakiesUderzenie) = ileZostaloKlockowWRzedzie rzad + ileZostaloKlockowNaPlanszy (PlanszaKlockow xs jakiesUderzenie)
    where
        ileZostaloKlockowWRzedzie :: RzadKlockow -> Int
        ileZostaloKlockowWRzedzie [] = 0
        ileZostaloKlockowWRzedzie (BrakKlocka : xs) = ileZostaloKlockowWRzedzie xs
        ileZostaloKlockowWRzedzie (klocek:xs) = 1 + ileZostaloKlockowWRzedzie xs

rzadKlockowPoUderzeniu :: Point -> RzadKlockow -> RzadKlockowPoUderzeniu
rzadKlockowPoUderzeniu _ [] = RzadKlockowPoUderzeniu [] BrakUderzenia
rzadKlockowPoUderzeniu (x,y) (k@Klocek{..}:xs)
    | aktualneUderzenie == BrakUderzenia = RzadKlockowPoUderzeniu (k:rzad) uderzenie
    | otherwise = RzadKlockowPoUderzeniu (nowyKlocek : xs) aktualneUderzenie
    where
        aktualneUderzenie = zidentyfikujRodzajUderzenia (x,y) k
        nowyKlocek = if liczbaPozostalychUderzen <= 1 then BrakKlocka else Klocek pozycja rozmiar (liczbaPozostalychUderzen - 1)
        RzadKlockowPoUderzeniu rzad uderzenie = rzadKlockowPoUderzeniu (x,y) xs
rzadKlockowPoUderzeniu (x,y) (BrakKlocka:xs) = RzadKlockowPoUderzeniu (BrakKlocka:rzad) uderzenie
    where
        RzadKlockowPoUderzeniu rzad uderzenie = rzadKlockowPoUderzeniu (x,y) xs

planszaKlockowPoUderzeniu :: Point -> [RzadKlockow] -> PlanszaKlockow
planszaKlockowPoUderzeniu _ [] = PlanszaKlockow [] BrakUderzenia
planszaKlockowPoUderzeniu (x,y) (rzad:xs)
    | aktualneUderzenieDlaAktualnegoRzedu == BrakUderzenia = PlanszaKlockow (rzad:pozostaleRzedyPoUderzeniach) ostatnieUderzenieDlaCalejPlanszy
    | otherwise = PlanszaKlockow (rzadPoUderzeniu:xs) aktualneUderzenieDlaAktualnegoRzedu
        where
            RzadKlockowPoUderzeniu rzadPoUderzeniu aktualneUderzenieDlaAktualnegoRzedu = rzadKlockowPoUderzeniu (x,y) rzad
            PlanszaKlockow pozostaleRzedyPoUderzeniach ostatnieUderzenieDlaCalejPlanszy = planszaKlockowPoUderzeniu (x,y) xs




-- funkcja zwracajaca nowa pozycje deski
ruchDeski :: StanGry -> Point
ruchDeski s = aktualnaPozycjaDeski (ruchDeskiWLewo (ruchDeskiWPrawo s))

ruchDeskiWLewo :: StanGry -> StanGry
ruchDeskiWLewo s@StanGry{..}
    | WcisnietyKlawiszLewy `elem` wcisnieteKlawisze 
      = s{aktualnaPozycjaDeski = (max ((-szerokoscObszaruGry + dlugoscDeski)/2) (fst aktualnaPozycjaDeski - (predkoscDeski/fromIntegral fps)), snd wyjsciowaPozycjaDeski)}
    | otherwise = s

ruchDeskiWPrawo :: StanGry -> StanGry
ruchDeskiWPrawo s@StanGry{..}
    | WcisnietyKlawiszPrawy `elem` wcisnieteKlawisze 
      = s{aktualnaPozycjaDeski = (min ((szerokoscObszaruGry - dlugoscDeski)/2) (fst aktualnaPozycjaDeski + (predkoscDeski/fromIntegral fps)), snd wyjsciowaPozycjaDeski)}
    | otherwise = s





--polozenie aktualne pilki to (x,y)
zidentyfikujRodzajUderzenia :: Point -> Klocek -> Uderzenie
zidentyfikujRodzajUderzenia (x,y) Klocek{..} 
    | y + (promienPilki/sqrt 2) >= dolnyBrzegKlocka && 
      y - (promienPilki/sqrt 2) < dolnyBrzegKlocka && 
      x - (promienPilki/sqrt 2) <= prawyBrzegKlocka && 
      x + (promienPilki/sqrt 2) > prawyBrzegKlocka 
       = UderzenieLewaGora
    | y + (promienPilki/sqrt 2) >= dolnyBrzegKlocka && 
      y - (promienPilki/sqrt 2) < dolnyBrzegKlocka && 
      x + (promienPilki/sqrt 2) >= lewyBrzegKlocka && 
      x - (promienPilki/sqrt 2) < lewyBrzegKlocka 
       = UderzeniePrawaGora
    | y - (promienPilki/sqrt 2) <= gornyBrzegKlocka && 
      y + (promienPilki/sqrt 2) > gornyBrzegKlocka && 
      x - (promienPilki/sqrt 2) <= prawyBrzegKlocka && 
      x + (promienPilki/sqrt 2) > prawyBrzegKlocka 
       = UderzenieLewyDol
    | y - (promienPilki/sqrt 2) <= gornyBrzegKlocka && 
      y + (promienPilki/sqrt 2) > gornyBrzegKlocka && 
      x + (promienPilki/sqrt 2) >= lewyBrzegKlocka && 
      x - (promienPilki/sqrt 2) < lewyBrzegKlocka 
       = UderzeniePrawyDol
    | lewyBrzegKlocka <= x && 
      x <= prawyBrzegKlocka && 
      y + promienPilki > gornyBrzegKlocka && 
      y - promienPilki < gornyBrzegKlocka 
       = UderzenieDol
    | lewyBrzegKlocka <= x && 
      x <= prawyBrzegKlocka && 
      y + promienPilki > dolnyBrzegKlocka && 
      y - promienPilki < dolnyBrzegKlocka 
       = UderzenieGora
    | dolnyBrzegKlocka <= y && 
      y <= gornyBrzegKlocka && 
      x - promienPilki < lewyBrzegKlocka && 
      x + promienPilki > lewyBrzegKlocka 
       = UderzeniePrawa
    | dolnyBrzegKlocka <= y && 
      y <= gornyBrzegKlocka && 
      x - promienPilki < prawyBrzegKlocka && 
      x + promienPilki > prawyBrzegKlocka 
       = UderzenieLewa
    | otherwise = BrakUderzenia
        where
            lewyBrzegKlocka = fst pozycja - (fst rozmiar/2) 
            prawyBrzegKlocka = fst pozycja + (fst rozmiar/2) 
            gornyBrzegKlocka = snd pozycja + (snd rozmiar/2) 
            dolnyBrzegKlocka = snd pozycja - (snd rozmiar/2) 


zmianaKierunkuPilki :: Point -> Vector -> Uderzenie -> Vector
zmianaKierunkuPilki (x,y) (xDelta, yDelta) aktualneUderzenie
    | aktualneUderzenie == UderzenieLewa || aktualneUderzenie == UderzeniePrawa || 
      x - promienPilki <= lewaKrawedzObszaruGry || x + promienPilki >= prawaKrawedzObszaruGry 
       = (-xDelta, yDelta)
    | aktualneUderzenie == UderzenieGora || aktualneUderzenie == UderzenieDol || 
      y + promienPilki >= gornaKrawedzObszaruGry || y - promienPilki  <= dolnaKrawedzObszaruGry || --to drugie chyba zbedne
      aktualneUderzenie == UderzenieLewaGora || aktualneUderzenie == UderzeniePrawaGora || 
      aktualneUderzenie == UderzenieLewyDol || aktualneUderzenie == UderzeniePrawyDol 
       = (xDelta, -yDelta)
    | otherwise = (xDelta, yDelta)
        where
            lewaKrawedzObszaruGry = -szerokoscObszaruGry  / 2
            prawaKrawedzObszaruGry = szerokoscObszaruGry / 2
            gornaKrawedzObszaruGry = wysokoscObszaruGry  / 2
            dolnaKrawedzObszaruGry = -wysokoscObszaruGry / 2

przemieszczeniePilki :: Point -> Vector -> Point
przemieszczeniePilki (x, y) (xDelta, yDelta) = (nowyX, nowyY)
  where
    lewaKrawedzObszaruGry = -szerokoscObszaruGry  / 2
    prawaKrawedzObszaruGry = szerokoscObszaruGry / 2
    gornaKrawedzObszaruGry = wysokoscObszaruGry  / 2
    dolnaKrawedzObszaruGry = -wysokoscObszaruGry / 2
    nowyX = obliczNowaWspolrzedna x xDelta lewaKrawedzObszaruGry prawaKrawedzObszaruGry promienPilki 
    nowyY = obliczNowaWspolrzedna y yDelta dolnaKrawedzObszaruGry gornaKrawedzObszaruGry promienPilki

obliczNowaWspolrzedna :: Float -> Float -> Float -> Float -> Float -> Float
obliczNowaWspolrzedna wspolrzedna wspolrzednaDelta minusKrawedzObszaruGry plusKrawedzObszaruGry promien
  | wspolrzednaDelta < 0 && max minusKrawedzObszaruGry (wspolrzedna + wspolrzednaDelta - promien) == minusKrawedzObszaruGry = minusKrawedzObszaruGry + promien
  | wspolrzednaDelta > 0 && min plusKrawedzObszaruGry (wspolrzedna + wspolrzednaDelta + promien) == plusKrawedzObszaruGry = plusKrawedzObszaruGry - promien
  | otherwise = wspolrzedna + wspolrzednaDelta

--(x, y) to przyszla pozycja pilki, ktora jesli jest niedozwolona, to bedzie korygowana przez zmiane kierunku w stanie gry
odbicieOdDeski :: Point -> StanGry -> DeskaUderzenie
odbicieOdDeski (x, y) stan@StanGry{..}
    | fst aktualnaPozycjaDeski - dlugoscDeski/2 <= x &&  x <= fst aktualnaPozycjaDeski + dlugoscDeski/2 &&
      snd aktualnaPozycjaDeski - wysokoscDeski/2 <= (y - promienPilki) &&  (y - promienPilki) <= snd aktualnaPozycjaDeski + wysokoscDeski/2
        = DeskaUderzenie True (nowaDeltaX,nowaDeltaY)
    | otherwise = DeskaUderzenie False (0,0) --zamiast (0,0) mogloby byc cokolwiek 
        where
            nowaDeltaX = ((x - fst aktualnaPozycjaDeski)/(dlugoscDeski/2) * snd zakresKataUderzeniaDeski) / fromIntegral fps
            nowaDeltaY = sqrt ((stalaPredkosciPilki * stalaPredkosciPilki) - (nowaDeltaX * fromIntegral fps * nowaDeltaX * fromIntegral fps)) / fromIntegral fps
            --   sqrt (nowaDeltaX^2 + nowaDeltaY^2)
            --   sqrt(nowaDeltaX^2  +  (stalaPredkosciPilki^2 - nowaDeltaX^2 * (fromIntegral fps)^2 )/ (fromIntegral fps)^2 ))
            -- = sqrt(nowaDeltaX^2  +  (stalaPredkosciPilki^2 - nowaDeltaX^2 * (fromIntegral fps)^2 )/ (fromIntegral fps)^2 ))
            -- = stalaPredkosciPilki/fromIntegral fps 
            -- to oznacza, ze pilka ma stala predkosc rowna stalaPredkosciPilki 
            -- (pilka stale pokonuje ten sam dystans w tym samym czasie, ale zmieniają sie wspolrzedne skladowe wektora predkosci)
            
 --(x, y) to przyszla pozycja pilki, ktora nie dojdzie do skutki dla wartosci True       
czyPilkaUpadnie :: Point -> StanGry -> Bool
czyPilkaUpadnie (x,y) stan@StanGry{..} 
    | y - promienPilki < snd aktualnaPozycjaDeski  - (promienPilki*8) - (wysokoscDeski/2) = True
    | otherwise = False


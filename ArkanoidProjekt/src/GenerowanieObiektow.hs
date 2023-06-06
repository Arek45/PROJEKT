{-# LANGUAGE RecordWildCards #-}
module GenerowanieObiektow where

import Data.Maybe (isNothing)
import Graphics.Gloss.Interface.Pure.Game

import Stale
import TypyDanych

kolorKlocka :: Klocek -> Color
kolorKlocka Klocek{..} = case liczbaPozostalychUderzen of
    3 -> dark red
    2 -> orange
    1 -> yellow
    _ -> white -- dodaj jakiś domyślny kolor dla innych przypadków
kolorKlocka BrakKlocka = white

stworzKlocek :: Klocek -> Picture
stworzKlocek k@Klocek{..} = color (kolorKlocka k) $ uncurry translate pozycja (uncurry rectangleSolid rozmiar)
stworzKlocek _ = Blank

stworzRzadKlockow :: RzadKlockow -> Picture
stworzRzadKlockow rzad = pictures $ map stworzKlocek rzad

stworzPlansze :: PlanszaKlockow -> Picture
stworzPlansze PlanszaKlockow{..} = pictures $ map stworzRzadKlockow klocki

--funkcja rysujaca plansze
generujRzadKlockow :: Int -> Float -> [Klocek]
generujRzadKlockow liczbaUderzen y =
--    [Klocek (x, y) (szerokoscKlocka, wysokoscKlocka) liczbaUderzen | x <- [150]]
    [Klocek (x, y) (szerokoscKlocka, wysokoscKlocka) liczbaUderzen | x <- [-150, -90 .. 150]]

generujPoziom :: Int -> PlanszaKlockow
generujPoziom 3 = PlanszaKlockow (map (generujRzadKlockow 1) [100.0]) BrakUderzenia
generujPoziom 1 = PlanszaKlockow (map (generujRzadKlockow 3) [100.0, 120.0, 150.0] ++ map (generujRzadKlockow 1) [170.0]) BrakUderzenia
generujPoziom 2 = PlanszaKlockow (map (generujRzadKlockow 1) [100.0, 120.0, 150.0, 170.0]) BrakUderzenia
generujPoziom _ = error "Nieznany poziom"
{-
generujPoziom :: Int -> PlanszaKlockow
generujPoziom 1 = PlanszaKlockow
                  [[Klocek (-150, 100) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-90, 100.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-30, 100.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (30, 100.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (90, 100.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (150, 100.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                  [Klocek (-150, 120) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-90, 120.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-30, 120.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (30, 120.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (90, 120.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (150, 120.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                  [Klocek (-150, 150) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-90, 150.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (-30, 150.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (30, 150.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (90, 150.0) (szerokoscKlocka, wysokoscKlocka) 3,
                   Klocek (150, 150.0) (szerokoscKlocka, wysokoscKlocka) 3
                  ],
                  [Klocek (-150, 170) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-90, 170.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 170.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 170.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (90, 170.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (150, 170.0) (szerokoscKlocka, wysokoscKlocka) 1
                  ]] BrakUderzenia
generujPoziom 2 = PlanszaKlockow
                  [[Klocek (-150, 100) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-90, 100.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 100.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 100.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (90, 100.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (150, 100.0) (szerokoscKlocka, wysokoscKlocka) 1
                  ],
                  [Klocek (-150, 120) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-90, 120.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 120.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 120.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (90, 120.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (150, 120.0) (szerokoscKlocka, wysokoscKlocka) 1
                  ],
                  [Klocek (-150, 150) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-90, 150.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 150.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 150.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (90, 150.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (150, 150.0) (szerokoscKlocka, wysokoscKlocka) 1
                  ],
                  [Klocek (-150, 170) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-90, 170.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (-30, 170.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (30, 170.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (90, 170.0) (szerokoscKlocka, wysokoscKlocka) 1,
                   Klocek (150, 170.0) (szerokoscKlocka, wysokoscKlocka) 1
                  ]] BrakUderzenia
-}
--funkcja generujaca plansze w zaleznosci od poziomu
{-
generujPlansze :: Int -> PlanszaKlockow
generujPlansze poziom = PlanszaKlockow
                  [[Klocek (-150, 100) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (-90, 100.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (-30, 100.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (30, 100.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (90, 100.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (150, 100.0) (szerokoscKlocka, wysokoscKlocka) poziom
                  ],
                  [Klocek (-150, 120) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (-90, 120.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (-30, 120.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (30, 120.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (90, 120.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (150, 120.0) (szerokoscKlocka, wysokoscKlocka) poziom
                  ],
                  [Klocek (-150, 150) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (-90, 150.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (-30, 150.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (30, 150.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (90, 150.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (150, 150.0) (szerokoscKlocka, wysokoscKlocka) poziom
                  ],
                  [Klocek (-150, 170) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (-90, 170.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (-30, 170.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (30, 170.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (90, 170.0) (szerokoscKlocka, wysokoscKlocka) poziom,
                   Klocek (150, 170.0) (szerokoscKlocka, wysokoscKlocka) poziom
                  ]] BrakUderzenia
-}
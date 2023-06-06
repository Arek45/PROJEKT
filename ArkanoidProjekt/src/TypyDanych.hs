module TypyDanych where

import Data.Fixed
import Data.Time.Clock
import Graphics.Gloss.Interface.Pure.Game

import Stale

---ZMIANA 2---
data StatusGry =
    Nieukonczona | Wygrana | Przegrana  deriving (Eq, Show)

data Widok =
    EkranStartowy | Menu | Poziom | Pauza | KomunikatOWygranej | KomunikatOPrzegranej | NajlepszeWyniki | Wyjscie deriving Eq

data WcisnietyKlawisz = WcisnietyKlawiszLewy | WcisnietyKlawiszPrawy | NieWcisnietyZadenKlawisz deriving Eq
type WcisnieteKlawisze = [WcisnietyKlawisz]

data StanGry = StanGry {
    nickGracza :: String, 
    osobistyRekordGracza  :: Pico,
    czasGry :: NominalDiffTime, 
    statusGry :: StatusGry, 
    czyGraJestZapisana :: Bool, 
    aktualnyWidok :: Widok, 
    aktualnaPozycjaPilki :: Point, 
    aktualnyWektorPredkosciPilki :: Vector, 
    aktualnaPozycjaDeski :: Point, 
    aktualnaPlanszaKlockow :: PlanszaKlockow, 
    wcisnieteKlawisze :: WcisnieteKlawisze,
    liczbaPozostalychKlockowNaPlanszy :: Int
}

data Klocek = Klocek {
    pozycja :: Point,
    rozmiar :: Point,
    liczbaPozostalychUderzen :: Int
} | BrakKlocka deriving (Eq, Show)

type RzadKlockow = [Klocek]

data PlanszaKlockow = PlanszaKlockow {
    klocki :: [RzadKlockow],
    ostatnieUderzenie :: Uderzenie
}
--nazwy wiaza sie z kierunkiem w ktorym zmierzala pilka przed uderzenie
data Uderzenie = 
    UderzenieGora | UderzenieDol | UderzenieLewa | UderzeniePrawa | UderzenieLewaGora | UderzeniePrawaGora | UderzenieLewyDol | UderzeniePrawyDol | UderzeniePlatformy | BrakUderzenia deriving (Eq,Show)


data RzadKlockowPoUderzeniu = RzadKlockowPoUderzeniu{
    rzad :: RzadKlockow,
    uderzenie :: Uderzenie
}

data DeskaUderzenie = DeskaUderzenie {
    czyPilkaUderzaWDeske :: Bool,
    kierunekPoOdbiciu :: Point  -- nowy kierunek pilki po odbiciu od platformy
}


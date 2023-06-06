{-# LANGUAGE RecordWildCards #-}
module Uruchomienie where

import System.Exit
import Data.Time.Clock
import Data.List
import Graphics.Gloss.Interface.IO.Game

import Stale
import TypyDanych
import DzialanieGry
import GenerowanieObiektow
import ObslugaRekordow



stanPoczatkowyGry :: String -> Widok -> IO StanGry
stanPoczatkowyGry nick widok = do
    pb <- najlepszyRekordGracza nick
    return $ StanGry nick pb (secondsToNominalDiffTime 0) Nieukonczona False widok wyjsciowaPozycjaPilki wyjsciowaPredkoscPilki wyjsciowaPozycjaDeski (generujPoziom 1) [NieWcisnietyZadenKlawisz] (ileZostaloKlockowNaPlanszy (generujPoziom 1))

--zmiana stanu gry po kliknieciu
iteracja :: Float -> StanGry -> IO StanGry
iteracja s stan@StanGry{..}
    | aktualnyWidok == Wyjscie = do --wyjscie z gry
        exitSuccess
        return stan
    | aktualnyWidok /= Poziom = return stan --jesli nie gramy, to nic nie zmienia sie na ekranie (tryb Poziom jest w momencie grania, Wygranej lub Przegranej)
    | statusGry == Wygrana = do
        zapiszWynik stan
        let wynNaj = if osobistyRekordGracza == 0 then nominalDiffTimeToSeconds czasGry else min osobistyRekordGracza (nominalDiffTimeToSeconds czasGry)
        return $ StanGry nickGracza wynNaj czasGry Wygrana True KomunikatOWygranej aktualnaPozycjaPilki (0,0) aktualnaPozycjaDeski aktualnaPlanszaKlockow [NieWcisnietyZadenKlawisz] 0
    | statusGry == Przegrana = return $ StanGry nickGracza osobistyRekordGracza czasGry Przegrana czyGraJestZapisana KomunikatOPrzegranej aktualnaPozycjaPilki (0,0) aktualnaPozycjaDeski aktualnaPlanszaKlockow [NieWcisnietyZadenKlawisz] 0
    --przypadek otherwise to sytuacja w trakcie rozgrywki
    | otherwise = return $ StanGry nickGracza osobistyRekordGracza (czasGry + secondsToNominalDiffTime (realToFrac s)) nowyStatus czyGraJestZapisana aktualnyWidok nowaPozycjaPilki nowyWektorPredkosciPilki nowaPozycjaDeski nowaPlansza wcisnieteKlawisze nowaLiczbaPozostalychKlockow
        where
            nowaPozycjaPilki = przemieszczeniePilki aktualnaPozycjaPilki aktualnyWektorPredkosciPilki
            nowaPlansza = planszaKlockowPoUderzeniu nowaPozycjaPilki (klocki aktualnaPlanszaKlockow) --klocki aktualnaPlanszaKlockow to lista rzedow klockow z planszy
            nowaLiczbaPozostalychKlockow = ileZostaloKlockowNaPlanszy nowaPlansza
            nowaPozycjaDeski = ruchDeski stan
            ostatnieuderzenie = ostatnieUderzenie nowaPlansza
            DeskaUderzenie czyuderzylo kierunekpoodbiciu = odbicieOdDeski nowaPozycjaPilki stan
            rodzajUderzenia | czyuderzylo = UderzeniePlatformy
                            | otherwise = ostatnieuderzenie
            nowyWektorPredkosciPilki | czyuderzylo = kierunekpoodbiciu
                                     | otherwise = zmianaKierunkuPilki nowaPozycjaPilki aktualnyWektorPredkosciPilki rodzajUderzenia
            nowyStatus | czyPilkaUpadnie nowaPozycjaPilki stan = Przegrana
                       | nowaLiczbaPozostalychKlockow == 0 = Wygrana
                       | otherwise = Nieukonczona

{-
textWithStyle :: Picture
textWithStyle = scale 1.5 1.5 $ textInFont "Arial" "Hello, World!"

textInFont :: String -> String -> Picture
textInFont fontName content = text content
    where
        style = TextStyle (makeFont fontName) solidLine (Just solidLine)
        makeFont name = Font name FontWeightNormal FontSlantNormal
        solidLine = solid 1
-}

narysujGre :: StanGry -> IO Picture
narysujGre stan@StanGry{..} = do
    --do wyswietlenia czasu gry
    let czasDoWysietleniaNaEkranie = formatujCzasDoWyswietlenia (show (nominalDiffTimeToSeconds czasGry))
        czasObraz = scale 0.2 0.2 $ translate (szerokoscObszaruGry * 4) (-100) $ color yellow $ text czasDoWysietleniaNaEkranie
    --do wyswietlenia rekordu globalnego w menu gry
    rekord <- najlepszyRekord
    let rekordDoWyswietleniaNaEkranie = if rekord == 0 then "Brak rekordu" else "Rekord: " ++ formatujCzasDoWyswietlenia (show rekord) 
        rekordObraz = scale  0.2 0.2 $ translate (szerokoscObszaruGry * 4) (-500) $ color yellow $ text rekordDoWyswietleniaNaEkranie
    --do wyswietlenia nowego PB gracza w menu wygranej
        aktualnyCzasGry = nominalDiffTimeToSeconds czasGry
        nowyRekordDoWyswietleniaNaEkranie = if aktualnyCzasGry <= osobistyRekordGracza then if aktualnyCzasGry <= rekord then "Nowy rekord!" else "Nowy osobisty rekord!" else ""
        nowyRekordObraz = scale 0.2 0.2 $ translate (- szerokoscObszaruGry / 4 - 10) (-60) $ color yellow $ text nowyRekordDoWyswietleniaNaEkranie
    --do wyswietlenia wynikow w widoku Wyniki
    wynikText <- wczytajWynikiZPliku sciezkaDoWynikow
    let ostatnie30Wynikow = drop 1 $ reverse $ drop (length (podzielWynikiZPliku wynikText) - 30) $ podzielWynikiZPliku wynikText
        ostatnie30WynikowObrazy = map (scale 0.5 0.5 . color white . text) ostatnie30Wynikow
        pierwsze10WynikowObraz = translate (-960 + 130) 0 $ pictures $ wyswietlWRzedach (540 - 200) 80 $ take 10 ostatnie30WynikowObrazy
        drugie10WynikowObraz = translate (-250) 0 $ pictures $ wyswietlWRzedach (540 - 200) 80 $ take 10 $ drop 10 ostatnie30WynikowObrazy
        trzecie10WynikowObraz = translate (960 - 500) 0 $ pictures $ wyswietlWRzedach (540 - 200) 80 $ take 10 $ drop 20 ostatnie30WynikowObrazy
        wynikTextPic = pictures [pierwsze10WynikowObraz, drugie10WynikowObraz, trzecie10WynikowObraz]

    case aktualnyWidok of
        EkranStartowy -> return (pictures [startInfoGraObraz, startInfoRozpocznijObraz])
        Menu -> return (pictures [menuObraz, menuInfoSterowanieObraz, menuInfoPauzaObraz, menuInfoRestartObraz, menuInfoWyjscieObraz, menuInfoRozpocznijObraz])
        Poziom -> return (pictures [rekordObraz, pbObraz, czasObraz, nickGraczaObraz, pilkaObraz, infoKontynuujObraz, planszaObraz, deskaObraz, scianyKoloroweObraz, infoMenuObraz, infoPauzaObraz, infoWyjscieObraz,  infoRestartObraz, infoWynikiObraz])
        Pauza -> return (pictures [rekordObraz, pbObraz, czasObraz, nickGraczaObraz, pauzaObraz, infoKontynuujObraz, pilkaObraz, planszaObraz, deskaObraz, scianyKoloroweObraz, infoMenuObraz, infoPauzaObraz, infoWyjscieObraz, infoRestartObraz, infoWynikiObraz])
        KomunikatOWygranej -> return (pictures [nowyRekordObraz, rekordObraz, pbObraz, czasObraz, nickGraczaObraz, wygranaObraz, infoKontynuujObraz, deskaObraz, scianyKoloroweObraz, infoMenuObraz, infoPauzaObraz, infoWyjscieObraz, infoRestartObraz, infoWynikiObraz])
        KomunikatOPrzegranej -> return (pictures [rekordObraz, pbObraz, czasObraz, nickGraczaObraz, przegranaObraz, infoKontynuujObraz, pilkaObraz, deskaObraz, scianyKoloroweObraz,  infoMenuObraz, infoPauzaObraz, infoWyjscieObraz, infoRestartObraz, infoWynikiObraz])
        NajlepszeWyniki -> return (pictures[wynikiTytul, wynikiRamka, wynikTextPic, wynikiPomoc])
        _ -> return (pictures [pilkaObraz, infoKontynuujObraz, planszaObraz, deskaObraz, scianyKoloroweObraz, infoMenuObraz, infoPauzaObraz, infoWyjscieObraz, infoRestartObraz, infoWynikiObraz])
    where
        --teksty pomocy w ekranie startowym
        startInfoGraObraz = scale 0.33 0.35 $ translate (-szerokoscObszaruGry * 2.5) 110 $ color white $ text "Gra Arkanoid"
        startInfoRozpocznijObraz = scale 0.33 0.35 $ translate (-szerokoscObszaruGry * 1.5) (-100) $ color white $ text "Nacisnij 'Enter' by zaczac"
        --teksty pomocy w menu glownym
        menuObraz = scale 0.45 0.45 $ translate (-szerokoscObszaruGry * 1.7) 370 $ color yellow $ text "Menu"
        menuInfoSterowanieObraz = scale 0.45 0.45 $ scale 0.70 0.70 $ translate (-szerokoscObszaruGry * 1.6) 300 $ color white $ text "Uzywaj <- oraz ->, by grac"
        menuInfoPauzaObraz = scale 0.45 0.45 $ scale 0.70 0.70 $ translate (-szerokoscObszaruGry * 1.6) 150 $ color white $ text "Pauza - P"
        menuInfoRestartObraz = scale 0.45 0.45 $ scale 0.70 0.70 $ translate (-szerokoscObszaruGry * 1.6) 0 $ color white $ text "Restart - R"
        menuInfoWyjscieObraz = scale 0.45 0.45 $ scale 0.70 0.70 $ translate (-szerokoscObszaruGry * 1.6) (-150) $ color white $ text "Wyjscie Esc"
        menuInfoRozpocznijObraz = scale 0.45 0.45 $ scale 0.70 0.70 $ translate (-szerokoscObszaruGry * 1.6) (-300) $ color white $ text "Rozpocznij  - Spacja"
        --wyswietlanie obiektow w menu gry, przegranej i wygranej
        pilkaObraz = uncurry translate aktualnaPozycjaPilki $ color white (circleSolid promienPilki)
        planszaObraz = stworzPlansze aktualnaPlanszaKlockow
        scianyKoloroweObraz = color green scianyObrazy
        scianyObrazy= pictures [
            translate 0 (wysokoscObszaruGry / 2.0) (rectangleSolid szerokoscObszaruGry szerokoscSciany),
            translate 0 (- wysokoscObszaruGry / 2.0) (rectangleSolid szerokoscObszaruGry szerokoscSciany),
            translate ((- szerokoscObszaruGry) / 2.0) 0 (rectangleSolid szerokoscSciany wysokoscObszaruGry),
            translate (szerokoscObszaruGry / 2.0) 0 (rectangleSolid szerokoscSciany wysokoscObszaruGry)
            ]
        deskaObraz = pictures [
            uncurry translate aktualnaPozycjaDeski $ color violet (rectangleSolid dlugoscDeski wysokoscDeski)]
        --do wyswietlenia nicku gracza w menu gry
        nickGraczaObraz = scale 0.55 0.55 $ translate (szerokoscObszaruGry * 2.5) 100 $ color white $ text nickGracza
        --do wyswietlenia nowego PB gracza w menu gry
        pbDowyswietleniaNaEkranie = if osobistyRekordGracza == 0 then "Brak osobistego rekordu" else "Osobisty rekord: " ++ formatujCzasDoWyswietlenia (show osobistyRekordGracza) 
        pbObraz = scale  0.2 0.2 $ translate (szerokoscObszaruGry * 4) (-300) $ color yellow $ text pbDowyswietleniaNaEkranie
        --teksty pomocy w menu gry
        infoMenuObraz = scale 0.2 0.2 $ translate (-szerokoscObszaruGry * 8.8) 150 $ color yellow $ text "Menu - M"
        infoPauzaObraz = scale  0.2 0.2 $ translate (-szerokoscObszaruGry * 8.8) 0 $ color yellow $ text "Pauza - P"
        infoRestartObraz = scale  0.2 0.2 $ translate (-szerokoscObszaruGry * 8.8) (-150) $ color yellow $ text "Restart - R"
        infoWyjscieObraz = scale  0.2 0.2 $ translate (-szerokoscObszaruGry * 8.8) (-300) $ color yellow $ text "Wyjscie Esc"
        infoKontynuujObraz = scale 0.2 0.2 $ translate (-szerokoscObszaruGry * 8.8) (-450) $ color yellow $ text "Kontynuuj - Spacja"
        infoWynikiObraz = scale  0.2 0.2 $ translate (-szerokoscObszaruGry * 8.8) (-600) $ color yellow $ text "Wyniki - V"
        --tekst PAUZA w menu pauzy
        pauzaObraz = scale 0.35 0.35 $ translate (-szerokoscObszaruGry * 0.67) 0 $ color yellow $ text "PAUZA"
        --komunikaty o wygranej i przegranej w menu wygranej i przegranej
        wygranaObraz = translate (- szerokoscObszaruGry / 4) 0 $ color yellow $ text "Wygrana!"
        przegranaObraz = translate (- szerokoscObszaruGry / 3) 0 $ color yellow $ text "Przegrana"
        --do wyswietlenia elementow widoku Wyniki
        wynikiTytul = translate (-150) (wysokoscObszaruGry - 150) $ scale 0.8 0.8 $ color white $ text "Wyniki"
        wynikiRamka = color yellow $ pictures [wynikiRamkaLewa, wynikiRamkaPrawa, wynikiRamkaGorna, wynikiRamkaDolna]
        wynikiRamkaLewa = translate (-960 + 100) 0 $ line [(0, 540 - 120), (0, -540 + 80)]
        wynikiRamkaPrawa = translate (960 - 100) 0 $ line [(0, 540 - 120), (0, -540 + 80)]
        wynikiRamkaGorna = translate 0 (540 - 120) $ line [(-960 + 100, 0), (960 - 100, 0)]
        wynikiRamkaDolna = translate 0 (-540 + 80) $ line [(-960 + 100, 0), (960 - 100, 0)]
        wynikiPomoc = scale 0.25 0.25 $ translate (-szerokoscObszaruGry * 8.8) (4 * (wysokoscObszaruGry - 150)) $ color yellow $ text " Nacisnij 'M' - by wlaczyc MENU "



--obsluga przyciskow
--data Event - Possible input events.
--EventKey Key KeyState Modifiers (Float, Float)
--Keystate moze byc Down albo Up
--Modifiers to rzeczy typu KeyState (shift, ctrl, alt)
--SpecialKey to specjalne klawisze (wszystkie poza literami)
--delete usuwa pierwsze wystapienie pierwszego argumentu z drugiego argumentu (listy)
obslugaWejscia :: Event -> StanGry -> IO StanGry
obslugaWejscia (EventKey (SpecialKey key) keyState _ _) stan@StanGry{..}
    | key == KeySpace && aktualnyWidok == Menu = return (stan {aktualnyWidok = Poziom})
    | key == KeySpace && aktualnyWidok == Pauza = return (stan {aktualnyWidok = Poziom})
    | key == KeyEnter && aktualnyWidok == EkranStartowy = return (stan {aktualnyWidok = Menu})
    | key == KeyLeft = return (stan{ wcisnieteKlawisze = if keyState == Down then WcisnietyKlawiszLewy : wcisnieteKlawisze else delete WcisnietyKlawiszLewy wcisnieteKlawisze})
    | key == KeyRight = return (stan{ wcisnieteKlawisze = if keyState == Down then WcisnietyKlawiszPrawy : wcisnieteKlawisze else delete WcisnietyKlawiszPrawy wcisnieteKlawisze})
    | key == KeyEsc = return (stan { aktualnyWidok = Wyjscie})
    | otherwise = return stan
obslugaWejscia (EventKey (Char znak) Down _ _ ) stan@StanGry{..}
    | znak == 'm' = return stan { aktualnyWidok = Menu}
    | znak == 'p' = return stan { aktualnyWidok = Pauza}
    | znak == 'r' = stanPoczatkowyGry nickGracza Poziom
    | znak == 'v' = return stan { aktualnyWidok = NajlepszeWyniki}
    | otherwise = return stan
obslugaWejscia _ stan = return stan



--podaj imie
podajImie :: IO String
podajImie = do
    putStrLn "Podaj nick: "
    nick <- getLine
    if ' ' `elem` nick
    then do
        putStrLn "Nick nie moze zawierac spacji."
        podajImie
    else return nick

-- uruchomienie gry
gra :: IO ()
gra = do
    utworzPlikDoZapisuWynikow sciezkaDoWynikow
    nicki <- wczytajNickiZPlikuZWynikami
    let nickiStr = getProfileStr 0 nicki
    putStrLn "Podaj liczbe:"
    putStrLn nickiStr
    numerNickuWybranegoPrzezGracza <- getLine
    let numerNickuInt = if (read numerNickuWybranegoPrzezGracza :: Int) <= length nicki then (read numerNickuWybranegoPrzezGracza :: Int) else 0
    if numerNickuInt == 0 then do
        nickgracza <- podajImie
        stPocz <- stanPoczatkowyGry nickgracza EkranStartowy
        playIO trybWyswietlania kolorTla fps stPocz narysujGre obslugaWejscia iteracja
    else do
        let nickgracza = nicki !! (numerNickuInt - 1)
        stPocz <- stanPoczatkowyGry nickgracza EkranStartowy
        playIO trybWyswietlania kolorTla fps stPocz narysujGre obslugaWejscia iteracja


data Zdanie = Z Char
              | N Zdanie
              | K Zdanie Zdanie
              | A Zdanie Zdanie
              | C Zdanie Zdanie
              
data BoolLukas = Falsz 
                | Prawda 
                | Nic
                deriving(Show)
              
konw_na_char :: Zdanie -> [Char]
konw_na_char z = case z of
                           Z x -> [x]
                           N x -> konw_na_char x
                           K x y -> konw_na_char x ++ konw_na_char y
                           A x y -> konw_na_char x ++ konw_na_char y
                           C x y -> konw_na_char x ++ konw_na_char y
              
zmienne_do_listy :: Zdanie -> [Char]
zmienne_do_listy z = let zmienne = [] in case z of Z x -> [x] ++ zmienne
                                                   N x -> konw_na_char x ++ zmienne
                                                   K x y -> konw_na_char x ++ konw_na_char y ++ zmienne
                                                   A x y -> konw_na_char x ++ konw_na_char y ++ zmienne
                                                   C x y -> konw_na_char x ++ konw_na_char y ++ zmienne
                                          
znajdz :: Char -> [Char] -> Bool
znajdz zmienna [] = False
znajdz zmienna (x:xs) = if x == zmienna then True
                        else znajdz zmienna xs
                        

usun_duplikaty :: [Char] -> [Char]
usun_duplikaty (x:[]) = x : ']' : []
usun_duplikaty (x:xs) = if znajdz x xs == True then usun_duplikaty xs else x : ", " ++ usun_duplikaty xs

wypisz_zmienne :: Zdanie -> [Char]
wypisz_zmienne zdanie = let lista = zmienne_do_listy zdanie in '[' : usun_duplikaty lista

drukuj::Zdanie -> String
drukuj (Z x) = [x]
drukuj (N x) = "~" ++ drukuj x
 
drukuj (K x y) = "(" ++ drukuj x ++ " & " ++ drukuj y ++ ")"
 
drukuj (C x y) = "(" ++ drukuj x ++ " => " ++ drukuj y ++ ")"
 
drukuj (A x y) = "(" ++ drukuj x ++ " | " ++ drukuj y ++ ")"


sprawdz :: Zdanie -> [(Char, Bool)] -> Bool
sprawdz zdanie mapa = case zdanie of
                                        Z x -> wartosciuj x mapa
                                        N x -> not (sprawdz x mapa)
                                        K x y -> sprawdz x mapa && sprawdz y mapa
                                        A x y -> sprawdz x mapa || sprawdz y mapa
                                        C x y -> sprawdz x mapa `implikacja` sprawdz y mapa

wartosciuj :: Char -> [(Char, Bool)] -> Bool
wartosciuj zmienna [] = error "Mapa nie zawiera zmiennej"
wartosciuj zmienna ((nazwa, wartosc):xs) = if nazwa == zmienna then wartosc else wartosciuj zmienna xs


implikacja :: Bool -> Bool -> Bool
implikacja True False = False
implikacja z1 z2 = True


sprawdz_Lukas :: Zdanie -> [(Char, BoolLukas)] -> BoolLukas
sprawdz_Lukas zdanie mapa = case zdanie of
											Z x -> wartosciuj_Lukas x mapa
                                            N x -> not_Lukas (sprawdz_Lukas x mapa)
                                            K x y -> sprawdz_Lukas x mapa `and_Lukas` sprawdz_Lukas y mapa
                                            A x y -> sprawdz_Lukas x mapa `or_Lukas` sprawdz_Lukas y mapa
                                            C x y -> sprawdz_Lukas x mapa `implikacja_Lukas` sprawdz_Lukas y mapa

wartosciuj_Lukas :: Char -> [(Char, BoolLukas)] -> BoolLukas
wartosciuj_Lukas zmienna [] = error "Mapa nie zawiera zmiennej"
wartosciuj_Lukas zmienna ((nazwa, wartosc):xs) = if nazwa == zmienna then wartosc else wartosciuj_Lukas zmienna xs

not_Lukas :: BoolLukas -> BoolLukas
not_Lukas z1 = float_na_Lukas ( 1 - lukas_na_Float z1)

and_Lukas :: BoolLukas -> BoolLukas -> BoolLukas
and_Lukas z1 z2 = let wartosc1 = lukas_na_Float z1; wartosc2 = lukas_na_Float z2 in if wartosc1 < wartosc2 then z1 else z2

or_Lukas :: BoolLukas -> BoolLukas -> BoolLukas
or_Lukas z1 z2 = let wartosc1 = lukas_na_Float z1; wartosc2 = lukas_na_Float z2 in if wartosc1 < wartosc2 then z2 else z1

implikacja_Lukas :: BoolLukas -> BoolLukas -> BoolLukas
implikacja_Lukas z1 z2 = let wartosc = 1 - lukas_na_Float z1 + lukas_na_Float z2 in if 1 < wartosc then Prawda else float_na_Lukas wartosc

lukas_na_Float :: BoolLukas -> Float
lukas_na_Float Prawda = 1.0
lukas_na_Float Falsz = 0.0
lukas_na_Float Nic = 0.5

float_na_Lukas :: Float -> BoolLukas
float_na_Lukas 1.0 = Prawda
float_na_Lukas 0.0 = Falsz
float_na_Lukas 0.5 = Nic
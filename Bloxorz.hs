{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Bloxorz where

import ProblemState

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East  deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = HardTile | SoftTile | Block | Switch | EmptySpace | WinningTile  deriving (Eq, Ord)

instance Show Cell where
   show c
		| (c == HardTile) = [hardTile]
		| (c == SoftTile) = [softTile]
		| (c == Block) = [block]
		| (c == Switch) = [switch]
		| (c == EmptySpace) = [emptySpace]
		| otherwise = [winningTile]
	
{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Level { 
				level_actual :: [[Char]],
				bloc :: [Position],
				dimensiune :: Position
			}
    deriving (Eq, Ord)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

test_on_Empty :: Level -> Bool
test_on_Empty lev = let (x1, y1) = ((bloc lev)!!0)
                        (x2, y2) = ((bloc lev)!!1)
					    in (((((level_actual lev)!!x1)!!y1) == emptySpace) || ((((level_actual lev)!!x2)!!y2) == emptySpace))

test_Vertical_on_Soft :: Level -> Bool
test_Vertical_on_Soft lev = (vertical (bloc lev)) && (let (x, y) = (head (bloc lev)) 
                                                      in ((((level_actual lev)!!x)!!y) == softTile))

test_Vertical_on_Winning :: Level -> Bool
test_Vertical_on_Winning lev = (vertical (bloc lev)) && (let (x, y) = (head (bloc lev)) 
                                                      in ((((level_actual lev)!!x)!!y) == winningTile))
													  
test_GameOver :: Level -> Bool
test_GameOver lev = (test_on_Empty lev) || (test_Vertical_on_Soft lev)   

show_Map :: Level -> [Char]
show_Map lev = foldl (\x y -> x ++ "\n" ++ y) ""  (
	                                                 modifyXY (snd ((bloc lev) !! 0)) (fst ((bloc lev) !! 0)) block 
													    (
														    modifyXY (snd ((bloc lev) !! 1)) (fst ((bloc lev) !! 1)) block 
															    (level_actual lev))) ++ "\n"

instance Show Level where
    show lev =
			if (test_GameOver lev) 
			  then (show_Map lev)  ++ "Game Over\n" 
			  else if (test_Vertical_on_Winning lev) then (show_Map lev)  ++ "Congrats! You won!\n"
			       else (show_Map lev)
	
{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

changeNth :: Int->a->[a]->[a]
changeNth _ _ [] = [] 
changeNth n change (x:xs)
     | n == 0 = (change:xs)
     | otherwise = x:changeNth (n-1) change xs
   
modifyXY :: Int -> Int -> a -> [[a]]->[[a]]
modifyXY x y f nList = changeNth y (changeNth x f (nList !! y)) nList

emptyLevel :: Position -> Position -> Level
emptyLevel pozitia_1 pozitia_2 = Level {
									level_actual = modifyXY (snd pozitia_2) (fst pozitia_2) hardTile
														(take (1 + (fst pozitia_1)) (repeat $ take (1 + (snd pozitia_1)) (repeat emptySpace))),
									bloc = [pozitia_2, pozitia_2],
									dimensiune = pozitia_1
								}

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile c pozitia_adaugata level_adaugat 
		| ((c == 'H') && (pozitia_adaugata /= (head (bloc level_adaugat))) && (pozitia_adaugata /= (head (tail (bloc level_adaugat) )))) =  Level {
																									level_actual = modifyXY  (snd pozitia_adaugata) (fst pozitia_adaugata) hardTile (level_actual level_adaugat),
																									bloc = (bloc level_adaugat),
																									dimensiune = (dimensiune level_adaugat)
																								}
		| ((c == 'S') && (pozitia_adaugata /= (head (bloc level_adaugat))) && (pozitia_adaugata /= (head (tail (bloc level_adaugat) )))) = Level {
																									level_actual = modifyXY (snd pozitia_adaugata) (fst pozitia_adaugata) softTile (level_actual level_adaugat),
																									bloc = (bloc level_adaugat),
																									dimensiune = (dimensiune level_adaugat)
																								}

		| ((c == 'W') && (pozitia_adaugata /= (head (bloc level_adaugat))) && (pozitia_adaugata /= (head (tail (bloc level_adaugat) )))) = Level {
																									level_actual = modifyXY (snd pozitia_adaugata) (fst pozitia_adaugata) winningTile (level_actual level_adaugat),
																									bloc = (bloc level_adaugat),
																									dimensiune = (dimensiune level_adaugat)
																								}
		| otherwise = level_adaugat
		
{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch pozitia_swich_cell pozitiile_pe_care_le_activez level_adaugat 
		| ((pozitia_swich_cell /= (head (bloc level_adaugat))) && (pozitia_swich_cell /= (head (tail (bloc level_adaugat))))) = Level {
																								level_actual = modifyXY (snd pozitia_swich_cell) (fst pozitia_swich_cell) switch (level_actual level_adaugat),
																								bloc = (bloc level_adaugat),
																								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
																								dimensiune = (dimensiune level_adaugat)
																								}
		| otherwise = level_adaugat

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
	Să verifice (si sa execute) modificari pe harta. 
	Esti pe un switch - activezi sau dezactivezi celule, esti pe win - ai castigat etc.
-}

activate :: Cell -> Level -> Level
activate = undefined

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
	
	blocul ocupa o poziție sau 2. Se poate muta in 4 directii. Ai cate 3 cazuri pentru fiecare directie:
	e in picioare si ajunge culcat, e culcat pe direcția E-V, e culcat pe direcția N-S.
	De exemplu, daca e culcat pe N-S si te muți in nord, se ridică in picioare, daca te muți in vest, ramane culcat.
	Deci ai 12 tipuri de mutari
-}

horizontal_NS :: [Position] -> Bool
horizontal_NS bloc = (not (vertical bloc)) && snd (bloc !! 0) /= snd (bloc !! 1)

horizontal_WE :: [Position] -> Bool
horizontal_WE bloc = (not (vertical bloc)) && fst (bloc !! 0) /= fst (bloc !! 1)

vertical :: [Position] -> Bool
vertical bloc = bloc !! 0 == bloc !! 1

north_vertical :: [Position] -> [Position]
north_vertical((x,y):rest) = [(x-2, y), (x-1, y)]

east_vertical :: [Position] -> [Position]
east_vertical((x,y):rest) = [(x, y+1), (x, y+2)]

west_vertical :: [Position] -> [Position]
west_vertical((x,y):rest) = [(x, y-2), (x, y-1)]

south_vertical :: [Position] -> [Position]
south_vertical((x,y):rest) = [(x+1, y), (x+2, y)]

west_horizontal_NS :: [Position] -> [Position]
west_horizontal_NS((x,y):rest) = [(x, y-1), (x, y-1)]

south_horizontal_NS :: [Position] -> [Position]
south_horizontal_NS((x,y):rest) = [(x+1, y), (x+1, y+1)]

north_horizontal_NS :: [Position] -> [Position]
north_horizontal_NS((x,y):rest) = [(x-1, y), (x-1, y+1)]

east_horizontal_NS :: [Position] -> [Position]
east_horizontal_NS((x,y):rest) = [(x, y+2), (x, y+2)]

west_horizontal_WE :: [Position] -> [Position]
west_horizontal_WE((x,y):rest) = [(x, y-1), (x+1, y-1)]

south_horizontal_WE :: [Position] -> [Position]
south_horizontal_WE((x,y):rest) = [(x+2, y), (x+2, y)]

north_horizontal_WE :: [Position] -> [Position]
north_horizontal_WE((x,y):rest) = [(x-1, y), (x-1, y)]

east_horizontal_WE :: [Position] -> [Position]
east_horizontal_WE((x,y):rest) = [(x, y+1), (x+1, y+1)]


move :: Directions -> Level -> Level
move directie level_adaugat 
	| (directie == North) && (vertical (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = north_vertical (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}
	| (directie == East) && (vertical (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = east_vertical (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}
	| (directie == West) && (vertical (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = west_vertical (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}
	| (directie == South) && (vertical (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = south_vertical (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}								
	| (directie == North) && (horizontal_NS (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = north_horizontal_NS (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}
	| (directie == East) && (horizontal_NS (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = east_horizontal_NS (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}
	| (directie == West) && (horizontal_NS (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = west_horizontal_NS (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}
	| (directie == South) && (horizontal_NS (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = south_horizontal_NS (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}
						
    | (directie == North) && (horizontal_WE (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = north_horizontal_WE (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}
	| (directie == East) && (horizontal_WE (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = east_horizontal_WE (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}
	| (directie == West) && (horizontal_WE (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = west_horizontal_WE (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}
	| (directie == South) && (horizontal_WE (bloc level_adaugat))= Level {
								level_actual = level_actual level_adaugat,
								bloc = south_horizontal_WE (bloc level_adaugat),
								--switch_cell = (switch_cell level_adaugat) ++ [(pozitia_swich_cell,pozitiile_pe_care_le_activez)],
								dimensiune = (dimensiune level_adaugat)
								}
    
{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame = undefined

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

{-nivel actual este castigat
nivel urmator poti pierde
tuplu de directe si nivelul urm 
-}

instance ProblemState Level Directions where
    successors = undefined

    isGoal = undefined

    -- Doar petru BONUS
    -- heuristic = undefined

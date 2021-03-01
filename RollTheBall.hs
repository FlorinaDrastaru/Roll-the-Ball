{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A


{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = NoDir | North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}

data Cell = Hor | Ver | TopL | BotL | BotR | TopR 
            | EmptyS | EmptyC | StartU | StartD | StartL 
            | StartR | WinU | WinD | WinL | WinR | Endl 
    deriving (Eq, Ord)


instance Show Cell where
    show cell = case cell of
        Hor -> [horPipe]
        Ver -> [verPipe]
        TopL -> [topLeft]
        BotL -> [botLeft]
        BotR -> [botRight]
        TopR -> [topRight]
        EmptyS -> [emptySpace]
        EmptyC -> [emptyCell]
        StartU -> [startUp]
        StartD -> [startDown]
        StartL -> [startLeft]
        StartR -> [startRight]
        WinU -> [winUp]
        WinD -> [winDown]
        WinL -> [winLeft]
        WinR -> [winRight]
        Endl -> [endl]
        
   



{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = EmptyLevel | Level {lvlM :: (A.Array Position Cell)}
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

showLvlHelper :: Level -> Int -> Int -> [String]
showLvlHelper EmptyLevel _ _ = []
showLvlHelper (Level level) maxX maxY = 
    let cellAt x y = level A.! (x, y) in
        [concat [show (cellAt x y) | y <- [0..maxY]] | x <- [0..maxX]]
    

instance Show Level
    where 
        show EmptyLevel = ""
        show (Level cells) = [endl] ++ unlines (showLvlHelper (Level cells) (fst (snd (A.bounds cells))) (snd (snd (A.bounds cells))))
                     
      
{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (x, y) = Level cellsMatr
    where 
        cellsMatr = A.array ((0, 0), (x, y)) [((i, j), EmptyS) | i <- [0..x], j <- [0..y]]

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (_, (_, _)) EmptyLevel = EmptyLevel
addCell (pipe, (x, y)) (Level cells)  
    | x < fst (fst (A.bounds cells)) 
        || x > fst (snd (A.bounds cells)) 
        || y < snd (fst (A.bounds cells)) 
        || y > snd (snd (A.bounds cells))
        || cells A.! (x, y) /= EmptyS = Level cells
    | x >= fst (fst (A.bounds cells)) 
        && y >= snd (fst (A.bounds cells)) 
        && x <= fst (snd (A.bounds cells)) 
        && y <= snd (snd (A.bounds cells)) 
        && cells A.! (x, y) == EmptyS = Level (cells A.// [((x, y), newCell)])
    | otherwise = Level cells
        where 
            newCell = case pipe of
                '═' -> Hor
                '║' -> Ver
                '╔' -> TopL
                '╚' -> BotL
                '╝' -> BotR
                '╗' -> TopR
                '░' -> EmptyS
                '▓' -> EmptyC
                '┴' -> StartU
                '┬' -> StartD
                '┤' -> StartL
                '├' -> StartR
                '╨' -> WinU
                '╥' -> WinD
                '╡' -> WinL
                '╞' -> WinR
                '\n' -> Endl

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos list  = foldr (\pair acc -> addCell (fst pair, snd pair) acc) (emptyLevel pos) list            

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

verifyStartEnd :: Cell -> Bool
verifyStartEnd cell 
    | cell /= StartU && cell /= StartD && cell /= StartL && cell /= StartR
      && cell /= WinU && cell /= WinD && cell /= WinL && cell /= WinR = True
    | otherwise = False 


moveCell :: Position -> Directions -> Level -> Level
moveCell (_, _) _ EmptyLevel = EmptyLevel
moveCell (x, y) dir (Level cells) 
    | dir == North = if cells A.! (x-1, y) == EmptyS && verifyStartEnd myCell
                        then Level (cells A.// ([((x-1, y), myCell)] ++ [((x, y), EmptyS)]))
                    else Level cells
    | dir == South = if cells A.! (x+1, y) == EmptyS && verifyStartEnd (cells A.! (x, y))
                        then Level (cells A.// ([((x+1, y), myCell)] ++ [((x, y), EmptyS)]))
                    else Level cells
    | dir == West = if cells A.! (x, y-1) == EmptyS && verifyStartEnd (cells A.! (x, y))
                        then Level (cells A.// ([((x, y-1), myCell)] ++ [((x, y), EmptyS)]))
                    else Level cells
    | dir == East = if y+1 < snd (snd (A.bounds cells))
                        then if cells A.! (x, y+1) == EmptyS && verifyStartEnd (cells A.! (x, y))
                            then Level (cells A.// ([((x, y+1), myCell)] ++ [((x, y), EmptyS)]))
                         else Level cells
                    else Level cells
    | otherwise = Level cells
    where myCell = cells A.! (x, y)
    

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection cell1 cell2 dir 
    | dir == East = if cell1 `elem` list1
                        then if cell2 `elem`list2
                            then True
                        else False
                    else False
                    
    | dir == West = if cell1 `elem` list3
                        then if cell2 `elem` list4
                            then True else False
                    else False

    | dir == South = if cell1 `elem` list5
                        then if cell2 `elem` list6
                            then True else False
                    else False

    | dir == North = if cell1 `elem` list7
                        then if cell2 `elem` list8
                            then True else False
                    else False
    | otherwise = False
    where list1 = [Hor, TopL, BotL, StartR]
          list2 = [Hor, BotR, TopR, WinL]
          list3 = [Hor, BotR, TopR, StartL]
          list4 = [Hor, TopL, BotL, WinR]
          list5 = [Ver, TopL, TopR, StartD]
          list6 = [Ver, BotL, BotR, WinU]
          list7 = [Ver, BotL, BotR, StartU]
          list8 = [Ver, TopL, TopR, WinD]


{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

verifyStart :: Cell -> Bool
verifyStart cell 
    | cell == StartU || cell == StartD || cell == StartL || cell == StartR = True
    | otherwise = False

verifyEnd :: Cell -> Bool
verifyEnd cell 
    |  cell == WinU || cell == WinD || cell == WinL || cell == WinR = True
    | otherwise = False

findStart :: [(Position, Cell)] -> (Position, Cell)
findStart list = 
    if length list == 0
        then ((-1, -1), EmptyS)
    else if verifyStart (snd (head list))
        then (head list)
    else 
        findStart (tail list)

findEnd :: [((Int, Int), Cell)] -> (Position, Cell)
findEnd list =
    if length list == 0
        then ((-1, -1), EmptyS)
    else if verifyEnd (snd (head list))
        then (head list)
    else 
        findEnd (tail list)

getCell :: [(Position, Cell)] -> Position -> Cell
getCell list pos = 
    if length list == 0
        then EmptyS
    else if fst (head list) == pos
        then snd (head list)
    else 
        getCell (tail list) pos


findConnection :: Level -> Position -> Cell -> (Position, Cell)
findConnection EmptyLevel _ _ = ((-1, -1), EmptyS)
findConnection (Level cells) (x, y) lastC  
    | connection myCell upCell North == True && x-1 >= fst (fst (A.bounds cells)) && upCell /= lastC = ((x-1, y), upCell)
    | connection myCell leftCell West == True && y-1 >= snd (fst (A.bounds cells)) && leftCell /= lastC = ((x, y-1), leftCell)
    | connection myCell downCell South == True && x+1 <= fst (snd (A.bounds cells)) && downCell /= lastC= ((x+1, y), downCell)
    | connection myCell rightCell East == True && y+1 <= snd (snd (A.bounds cells)) && rightCell /= lastC = ((x, y+1), rightCell)
    | otherwise = ((-1, -1), EmptyS)
    where myCell = cells A.! (x, y)
          leftCell = cells A.! (x, y-1)
          rightCell = cells A.! (x, y+1)
          upCell = cells A.! (x-1, y)
          downCell = cells A.! (x+1, y)
          

findPath :: Level -> (Position, Cell) -> (Position, Cell) -> (Position, Cell) -> Bool
findPath EmptyLevel _ _ _ = False
findPath (Level cells) start end lastC = 
    if start == end
        then True
    else if (findConnection (Level cells) (fst start) (snd lastC)) /= ((-1, -1), EmptyS) 
        then if findConnection (Level cells) (fst start) (snd lastC) /= lastC
            then findPath (Level cells) (findConnection (Level cells) (fst start) (snd lastC)) end start
        else False
    else False


wonLevel :: Level -> Bool
wonLevel EmptyLevel = False
wonLevel (Level cells) = 
    let cellList = A.assocs cells
        start = findStart cellList
        end = findEnd cellList 
        lastC = start in
        if (findPath (Level cells) start end lastC) == True
            then True
        else False

isCell :: (Position, Cell) -> Bool
isCell cell 
    | snd cell == EmptyS || verifyStart (snd cell) || verifyEnd (snd cell) = False
    | otherwise = True
  

findCells :: Level -> [(Position, Cell)]
findCells EmptyLevel = []
findCells (Level cells) = 
    let cellList = A.assocs cells in
        filter isCell cellList    

canMove :: Level -> (Position, Cell) -> Directions -> Bool
canMove EmptyLevel _ _ = False
canMove (Level cells) cell dir  
    | dir == North = if x-1 >= fst (fst (A.bounds cells))
                        then if upCell == EmptyS
                            then True 
                        else False
                    else False
    | dir == South = if x+1 <= fst (snd (A.bounds cells))
                        then if downCell == EmptyS
                            then True 
                        else False
                    else False
    | dir == West = if y-1 >= snd (fst (A.bounds cells))
                        then if leftCell == EmptyS
                            then True 
                        else False
                    else False
    | dir == East = if y+1 <= snd (snd (A.bounds cells))
                        then if rightCell == EmptyS
                            then True 
                        else False
                    else False
    | otherwise = False

    where (x, y) = fst cell
          upCell = cells A.! (x-1, y) 
          downCell = cells A.! (x+1, y)
          leftCell = cells A.! (x, y-1)
          rightCell = cells A.! (x, y+1)
    
addMove :: Level -> (Position, Cell) -> Directions -> [((Position, Directions), Level)] -> [((Position, Directions), Level)]
addMove EmptyLevel _ _ _ = []
addMove (Level cells) cell dir list = 
    let lvl = moveCell (fst cell) dir (Level cells) in
        if canMove (Level cells) cell dir == True 
            then list ++ [(((fst cell), dir), lvl)]
        else list   


addSuccessors :: Level -> (Position, Cell) -> [((Position, Directions), Level)]
addSuccessors EmptyLevel _ = []
addSuccessors (Level cells) cell = foldr (\dir acc -> addMove (Level cells) cell dir acc) [] [North, South, West, East]
                    

instance ProblemState Level (Position, Directions) where
    successors EmptyLevel = []
    successors (Level cells) = let list = findCells (Level cells) in
                               concat (map (\cell-> addSuccessors (Level cells) cell) list)   
    isGoal EmptyLevel = False
    isGoal (Level cells) = wonLevel (Level cells)
    reverseAction ((_, _), EmptyLevel) = (((-1,-1), NoDir), EmptyLevel)
    reverseAction ((pos, dir), (Level cells)) 
        | dir == North = (((x-1, y), South), moveCell pos South (Level cells))
        | dir == South = (((x+1, y), North), moveCell pos North (Level cells))
        | dir == West = (((x, y-1), East), moveCell pos East (Level cells))
        | dir == East = (((x, y+1), West), moveCell pos West (Level cells))
        | otherwise = (((-1,-1), NoDir), (Level cells))
        where (x, y) = pos
              
              











  
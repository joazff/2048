import Data.List

data Acao = Cima | Baixo | Esquerda | Direita
    deriving(Eq,Show)

type Board = [[Int]]
board :: Board
board = [[0, 0, 0, 0],
         [0, 0, 0, 0],
         [0, 0, 0, 2],
         [0, 0, 0, 2]]

tboard :: Board
tboard = [[0, 2, 0, 2],
         [0, 0, 0, 0],
         [0, 0, 0, 2],
         [0, 0, 0, 0]]

zboard :: Board
zboard = [[4, 2, 4, 2],
         [2, 4, 2, 4],
         [4, 2, 4, 2],
         [2, 4, 2, 4]]              

rotate :: Board -> Board
rotate [[a], [b], [c], [d]] = [[a, b, c, d]]
rotate [(a:as), (b:bs), (c:cs), (d:ds)] = rotate [as, bs, cs, ds] ++ [[a, b, c, d]]

moveEsquerda :: Board -> Board
moveEsquerda xs = map combinaLinha xs
    where 
    combinaLinha [ ] = [ ]
    combinaLinha [x] = [x]
    combinaLinha (x:y:xs) 
        | x == 0 = combinaLinha (y:xs) ++ [0]
        | y == 0 = combinaLinha (x:xs) ++ [0]
        | x == y = (x+y) : combinaLinha xs ++ [0]
        | otherwise = x : combinaLinha (y:xs)

moveDireita :: Board -> Board
moveDireita xs = rotate (rotate (map combinaLinha (rotate (rotate xs))))
    where 
    combinaLinha [ ] = [ ]
    combinaLinha [x] = [x]
    combinaLinha (x:y:xs) 
        | x == 0 = combinaLinha (y:xs) ++ [0]
        | y == 0 = combinaLinha (x:xs) ++ [0]
        | x == y = (x+y) : combinaLinha xs ++ [0]
        | otherwise = x : combinaLinha (y:xs)

moveCima :: Board -> Board
moveCima xs = rotate (rotate (rotate (map combinaLinha (rotate xs))))
    where 
    combinaLinha [ ] = [ ]
    combinaLinha [x] = [x]
    combinaLinha (x:y:xs) 
       | x == 0 = combinaLinha (y:xs) ++ [0]
       | y == 0 = combinaLinha (x:xs) ++ [0]
       | x == y = (x+y) : combinaLinha xs ++ [0]
       | otherwise = x : combinaLinha (y:xs)

moveBaixo :: Board -> Board
moveBaixo xs = rotate (map combinaLinha (rotate (rotate (rotate xs))))
    where 
    combinaLinha [ ] = [ ]
    combinaLinha [x] = [x]
    combinaLinha (x:y:xs) 
       | x == 0 = combinaLinha (y:xs) ++ [0]
       | y == 0 = combinaLinha (x:xs) ++ [0]
       | x == y = (x+y) : combinaLinha xs ++ [0]
       | otherwise = x : combinaLinha (y:xs)       

move :: Acao -> Board -> Board
move Esquerda xs = moveEsquerda xs 
move Direita xs = moveDireita xs 
move Cima xs = moveCima xs
move Baixo xs = moveBaixo xs

check2048 :: Board -> Bool
check2048 xs = [] /= filter (== 2048) (concat xs)

canMove :: Board -> Bool
canMove xs 
 | (xs == (move Esquerda xs)) && (xs == (move Direita xs)) && (xs == (move Cima xs)) && (xs == (move Baixo xs)) = False
 | otherwise = True

{-
main :: IO ()
main = gameLoop start
-}
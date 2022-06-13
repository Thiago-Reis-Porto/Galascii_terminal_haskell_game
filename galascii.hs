-------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------GALASCII---------------------------------------------------------------
------------------------------Space Invaders Clone with charcters - Thiago Reis Porto -----------------------------------
-------------------------------------------------------------------------------------------------------------------------

import System.Console.ANSI
import System.IO
import System.Random
import Control.Concurrent
import Data.Functor
import qualified Data.ByteString.Char8 as C

-------------------------------------------------------------------------------------------------------------------------
-- Game Data Structures -------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------

--Each model has is a sprite with x,y coordenates and a char 
data Sprite = Sprite { x :: Int,
                       y :: Int,
                       c :: Char 
                     }deriving(Eq,Show)  

--Data for the aliens manipulation(moviments, bullets ...)
type Invaders = [Sprite]
data InvadersData = InvadersData { dInvaders   :: Invaders,
                                   dDirection  :: Int,
                                   dMoviment   :: Int,
                                   dInvBullets :: [Sprite]
                                 }deriving(Eq,Show)

--Data for player, and ist bullets
data PlayerData = PlayerData { playerSp :: Sprite,
                               dPBullets :: [Sprite]
                             }deriving(Eq,Show)

--Main game information to render, player, aliens and bullets data are stored here
--also the score and remaining lives
data GameData = GameData { pData :: PlayerData,
                           iData :: InvadersData,
                           lives :: Int,
                           score :: Int
                         }deriving(Eq,Show)

--Frames to render the game screen
type Frame = [[Char]]
start :: Frame
start = ["##########################################\n",
         "#                                        #\n",
         "#                                        #\n",
         "#                                        #\n",
         "#                                        #\n",
         "#                                        #\n",
         "#                                        #\n",
         "#                                        #\n",
         "#                                        #\n",
         "#                                        #\n",
         "#                                        #\n",
         "#                                        #\n",
         "##########################################\n"]

menu :: Frame
menu =  ["##########################################\n",
         "#                                        #\n",
         "#                                        #\n",
         "#     *  *  *  *  *  *  *  *  *  *       #\n",
         "#                                        #\n",
         "#     *         GALASCII         *       #\n",
         "#                                        #\n",
         "#     *  *  *  *  *  *  *  *  *  *       #\n",
         "#                                        #\n",
         "#           PRESS S TO START             #\n",
         "#           PRESS Q TO EXIT              #\n",
         "#   Controls: a=left  d=right s=shoot    #\n",
         "##########################################\n"]

-------------------------------------------------------------------------------------------------------------------------
-- Main Functions and Utils ---------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------

--Main function calls the menu or exit the game
main :: IO ()
main = do 
     putStr (concat menu)
     hSetEcho stdin False; hideCursor;
     inp <- getChar
     case inp of
          's' -> initialize
          'q' -> return ()
          _   -> main

--Initialize the game
initialize :: IO ()
initialize = 
          let 
               aI = InvadersData (invadersArrayInit 4 4 30 30) 1 0 []
               p = PlayerData (Sprite 20 11 'X') []
               gD = GameData p aI 3 0
          in gameLoop gD

--Main game loop
gameLoop :: GameData -> IO ()
gameLoop gD 
     | lives gD == 0 = do putStr ("GAME OVER - FINAL SCORE:" ++ show (score gD)++"\n")
     | null (dInvaders (iData gD)) = do putStr "YOU WIN\n"
     | otherwise = do
                    let nf = buildFrame gD start
                    --terminalClear 
                    C.putStr (C.pack ( frameString (lives gD) (score gD) nf))
                    --threadDelay 1000
                    hFlush stdout
                    pc <- ifReadyDo stdin getChar
                    case pc of
                         Nothing -> do ngD <- prepareData gD{pData = uPlayerChar (pData gD)};threadDelay 101000; gameLoop ngD
                         Just 's' -> do ngD <- prepareData gD{pData = playerShoot (uPlayerChar (pData gD))};threadDelay 101000; gameLoop ngD
                         Just 'a' -> do ngD <- prepareData gD{pData = movePlayerL (uPlayerChar (pData gD))};threadDelay 101000; gameLoop ngD
                         Just 'd' -> do ngD <- prepareData gD{pData = movePlayerR (uPlayerChar (pData gD))};threadDelay 101000; gameLoop ngD               
                         Just 'q' -> return ()
                         Just _ -> do ngD <- prepareData gD{pData = uPlayerChar (pData gD)};threadDelay 101000; gameLoop ngD
                         
--Cria uma String com o frame do jogo pra ser printada
frameString :: Int -> Int -> Frame -> String
frameString lvs scr fr = " SCORE:" ++ show scr ++ "            " ++ "LIVES:" ++ show lvs ++ ('\n':concat fr)
--Constroi um frame com os dados do jogo
buildFrame :: GameData -> Frame -> Frame
buildFrame gD fR = putPlayer(putPlrBullets (putInvaders putInvBullets))
     where
          putInvBullets :: Frame
          putInvBullets = putListOnFrame (dInvBullets (iData gD)) fR
          
          putInvaders :: Frame -> Frame
          putInvaders fRI = putListOnFrame (dInvaders (iData gD)) fRI

          putPlrBullets :: Frame -> Frame
          putPlrBullets fRP = putListOnFrame (dPBullets (pData gD)) fRP
          
          putPlayer :: Frame -> Frame
          putPlayer frPP = putOnFrame (playerSp (pData gD)) frPP

--Coloca uma lista de sprites no frame
putListOnFrame :: [Sprite] -> Frame -> Frame
putListOnFrame sl fr = foldl (flip putOnFrame) fr sl

--This function dont block the execution, if a key was not pressed
ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd xh = hReady hnd >>= f
   where f True = xh Data.Functor.<&> Just
         f _    = return Nothing

uPlayerChar :: PlayerData ->PlayerData
uPlayerChar pD = pD {playerSp = (playerSp pD){c='X'}}

--Coloca um Sprite no frame
putOnFrame :: Sprite -> Frame -> Frame
putOnFrame s = putInPos (x s) (y s) (c s)
          where
               putInPos :: Int -> Int -> Char -> Frame -> Frame
               putInPos _ _ _ [] = []
               putInPos xP yP ch (f:fs)
                    | yP == 0 = pCollumn xP ch f:fs
                    | otherwise = f:putInPos xP (yP-1) ch fs
               pCollumn :: Int -> Char -> [Char] -> [Char]
               pCollumn _ _ [] = [] 
               pCollumn i chr (col:cols)
                    | i == 0 = chr:cols
                    | otherwise = col:pCollumn (i-1) chr cols

--Prepares the nex frame data
prepareData :: GameData -> IO GameData
prepareData gD =  do  
                    --Colisões -> Tiros Aliens -> Movimento
                    let ch = colisionHandler gD
                    sh <- aBulletsHandler (iData ch)
                    
                    return ch{iData = moveInvaders sh{dInvBullets = moveBulletsAlien (dInvBullets sh)}, pData = (pData ch){dPBullets = moveBulletsPlayer (dPBullets (pData ch))}}   

-------------------------------------------------------------------------------------------------------------------------
-- Colissions Handlers / Remove player/alien ---------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------

colisionHandler :: GameData -> GameData
colisionHandler gD = gD{pData = newPData, iData = newIData , lives = cptZ , score = catZ}
     where
          (catX, catY, catZ) = colisionAliens (dPBullets (pData gD)) [] (dInvaders (iData gD)) (score gD)
          (cptX, cptY, cptZ) = colissionPlayer (pData gD, [], lives gD) (dInvBullets (iData gD))
          newPData = cptX{dPBullets = catX}
          newIData = (iData gD){dInvaders = catY, dInvBullets = cptY}

colisionAliens :: [Sprite] -> [Sprite] -> Invaders -> Int-> ([Sprite], Invaders, Int)
colisionAliens [] bul inv scr = (bul, inv, scr)
colisionAliens (bp:bps) bul inv scr
     | rmv = colisionAliens bps bul nInv (scr+10)
     | otherwise = colisionAliens bps (bul++[bp]) inv scr 
     where 
          caAux :: Sprite -> Invaders -> Invaders -> (Bool, Invaders)
          caAux _ [] inA = (False, inA)
          caAux b (i:is) inA
               | bColision b i = (True, inA++is)
               | otherwise = caAux b is (inA++[i])
          (rmv, nInv) = caAux bp inv [] 
colissionPlayer :: (PlayerData, [Sprite], Int) -> [Sprite] -> (PlayerData,[Sprite], Int)
colissionPlayer p [] = p
colissionPlayer (pd, nb, lv) (b:bs)
     | bColision (playerSp pd) b = colissionPlayer (pd{playerSp=(playerSp pd){c=' '}},nb,lv-1) bs
     | otherwise = colissionPlayer (pd, b:nb,lv) bs

-------------------------------------------------------------------------------------------------------
-----Player/Ship Functions ----------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
--TODO
--Move nava para direita
movePlayerR :: PlayerData -> PlayerData
movePlayerR pD = pD{playerSp = moveSprite 1 0 (playerSp pD)}
--Move nava para esquerda
movePlayerL :: PlayerData -> PlayerData
movePlayerL pD = pD{playerSp = moveSprite (-1) 0 (playerSp pD)}
--Player Atira
playerShoot :: PlayerData -> PlayerData
playerShoot pD = pD{dPBullets = Sprite (x(playerSp pD)) (y(playerSp pD)-1) 'I' : dPBullets pD}
   {-  | null (dPBullets pD) = pD{dPBullets = Sprite (x(playerSp pD)) (y(playerSp pD)-1) 'I' : dPBullets pD}
     | x (head (dPBullets pD)) == x (playerSp pD) && (y (head (dPBullets pD)) == (y(playerSp pD)-1)) = pD
     | otherwise = pD{dPBullets = Sprite (x(playerSp pD)) (y(playerSp pD)-1) 'I' : dPBullets pD-}

-------------------------------------------------------------------------------------------------------
-----Bullets Functions --------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
--TODO

--Colisão
bColision :: Sprite -> Sprite -> Bool
bColision s1 s2 = (x s1 == x s2) && (y s1 == y s2)

--Move balas de Alien
moveBulletsAlien :: [Sprite] -> [Sprite]
moveBulletsAlien [] = []
moveBulletsAlien (b:bs) 
     | y nb > 11 = moveBulletsAlien bs
     | otherwise = nb:moveBulletsAlien bs
          where nb = moveSprite 0 1 b

--Move balas do player
moveBulletsPlayer :: [Sprite] -> [Sprite]
moveBulletsPlayer [] = []
moveBulletsPlayer (b:bs) 
     | y nb < 1 = moveBulletsAlien bs
     | otherwise = nb:moveBulletsPlayer bs
          where nb = moveSprite 0 (-1) b
          
--Trata as balas dos aliens
aBulletsHandler :: InvadersData -> IO InvadersData
aBulletsHandler inv 
     | null bA = return inv
     | otherwise = do gen <- newStdGen; return inv{dInvBullets = shootBullet (bA !! fst (randomR (0,length bA-1) gen)) (dInvBullets inv) }
     where bA = bottonAliensPos (dInvaders inv) 

-------------------------------------------------------------------------------------------------------
-----Aliens/Invaders Functions ------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
--TODO tiros

--Movimenta todos Aliens / um pouco ineleg
moveInvaders :: InvadersData -> InvadersData
moveInvaders i
     | null (dInvaders i) = i
     | (dMoviment i == 10) && (yInvData i > 8) = i{dInvaders = map (moveSprite (-1) 0) (dInvaders i), dDirection = -1, dMoviment = 9}
     | dMoviment i == 10 = i{dInvaders = map (moveSprite (-1) 1) (dInvaders i), dDirection = -1, dMoviment = 9}
     | (dMoviment i == -10 ) && (yInvData i > 8 ) = i{dInvaders = map (moveSprite 1 0) (dInvaders i), dDirection = 1, dMoviment = -9}
     | dMoviment i == -10 = i{dInvaders = map (moveSprite 1 1) (dInvaders i), dDirection = 1, dMoviment = -9}
     | otherwise = i{dInvaders = map (moveSprite (dDirection i) 0) (dInvaders i), dDirection = dDirection i, dMoviment = dMoviment i + dDirection i }
     where
          yInvData :: InvadersData -> Int
          yInvData inv = y (head (dInvaders inv))


--Movimenta um alien para direita ou esquerda, e para baixo
moveSprite :: Int-> Int -> Sprite -> Sprite
moveSprite lr d s@Sprite {x=aX, y = aY} = s{x=aX+lr,y=aY+d} 


--Inicializa os aliens
invadersArrayInit :: Int -> Int -> Int -> Int-> Invaders
invadersArrayInit row rows column columns
     | row == (rows - 3) = []
     | column > 10 = Sprite column row 'A' : invadersArrayInit row rows (column -1) columns
     | otherwise = invadersArrayInit (row - 1) rows columns columns


--retorna as pos de alien que podem atirar
bottonAliensPos :: Invaders -> [(Int, Int)]
bottonAliensPos [] = []
bottonAliensPos inv = bottomColumn [] inv
     where
          bottomColumn :: [Int] -> Invaders -> [(Int, Int)]
          bottomColumn _ [] = []
          bottomColumn v (i:is) 
               | x i `notElem` v = (x i, y i) : bottomColumn (x i:v) is
               | otherwise = bottomColumn v is


--alienShoot
shootBullet :: (Int,Int) -> [Sprite] -> [Sprite]
shootBullet (pX, pY) bullets
     | alienBulletColumnCheck (pX, pY) bullets = Sprite pX (pY + 1) '|':bullets
     | otherwise = bullets 
              

--Checka se o alien de uma coluna tem uma bala ainda em movimento
alienBulletColumnCheck :: (Int, Int) -> [Sprite] -> Bool
alienBulletColumnCheck _ [] = True
alienBulletColumnCheck (pX, pY) (b:bs)
     | pX == x b = False
     | otherwise = alienBulletColumnCheck (pX, pY) bs 

-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------

{-
terminalClear :: IO ()
terminalClear = do
                    a <- getTerminalSize
                    case a of
                         Nothing -> return()
                         Just (h,_) -> do C.putStr(C.pack(fillSpace h)) 
     where
          fillSpace :: Int->String
          fillSpace 0 = []
          fillSpace n = "                                          \n" ++ fillSpace (n-1)

terminalClear :: IO ()
terminalClear = C.putStr(C.pack(fillSpace 30)) 
     where
          fillSpace :: Int->String
          fillSpace 0 = []
          fillSpace n = "                                          \n" ++ fillSpace (n-1)
-}
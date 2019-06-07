{-# LANGUAGE LambdaCase #-}


import Control.Monad.State 

--types
type Brd = [[PLAYER]]
type Board = State (WINNER, WINNER) Brd
data WINNER = CW  | ZW  | FW  | ST Int | GA deriving (Eq, Ord)
data PLAYER = EM |CR | ZR deriving (Eq, Ord)

--colors
uX = "\x1b[32m" ++ " X " ++ "\x1b[0m"
u0 = "\x1b[33m" ++ " 0 " ++ "\x1b[0m"
uP = "\x1b[37m" ++ " . " ++ "\x1b[0m"

--steppers
strST= (ST 0, ST 0)
chgST EM True = CR
chgST EM False = ZR
chgST i _ = i
addST (ST x) = ST $ x+1

--summer
sums (x:xs) = foldl (.+.) x xs
            where (.+.) CR CR = CR
                  (.+.) ZR ZR = ZR
                  (.+.) _ _ = EM


startBoard :: Board 
startBoard = do put strST; return $ replicate 3 . replicate 3 $ EM


getBoard s = return $ runState s strST


modify2DarrayS :: Bool -> Int -> Int -> ([[PLAYER]]->[[PLAYER]])
modify2DarrayS me _ _ [] = []
modify2DarrayS me 1 j (x:xs) = modify2Darray me j x : modify2DarrayS me 0 j xs
modify2DarrayS me i j (x:xs) = x : modify2DarrayS me (i-1) j xs

modify2Darray me _ [] = []
modify2Darray me 1 (x:xs) = chgST x me : modify2Darray me 0 xs
modify2Darray me i (x:xs) = x: modify2Darray me (i-1) xs


changeBoard me i j =   \(b, (s1, s2)) ->  do let nb = modify2DarrayS me i j b;
                                             (nb, (addST s1, whatsWrong2 nb . whatsWrong1 $ nb))

whatsWrong1 :: Brd->WINNER
whatsWrong1 s  | l >=9 = FW
               | otherwise = ST l
              where l = length . filter (/=EM) . join $ s

whatsWrong2 :: Brd->WINNER->WINNER
whatsWrong2 s sn = minimum $ sn : res
              where ss =  join s
                    res= [checker1, checker2, checker3] <*> [EM] <*> [0] <*> [ss]

--checkers
checker1 s st ln
           | s==CR = CW
           | s==ZR = ZW
           | st>6  = GA
           | otherwise = checker1 (sums . take 3 . drop st $ ln) (st+3) ln

checker2 s st ln
           | s==CR = CW
           | s==CR = ZW
           | st>2  = GA
           | otherwise = checker2 (sums modLn) (st+1) ln
          where modLn = (take 1 . drop st $ ln) ++(take 1 . drop (st+3) $ ln)++(take 1 . drop (st+6) $ ln)


checker3 s st ln
            | res==CR   = CW
            | res==ZR   = ZW
            | otherwise = GA
                where modLn1 = (take 1 . drop st $ ln) ++(take 1 . drop (st+4) $ ln)++(take 1 . drop (st+8) $ ln)      
                      modLn2 = (take 1 . drop (st+2) $ ln) ++(take 1 . drop (st+4) $ ln)++(take 1 . drop (st+6) $ ln)      
                      res = max (sums modLn1) (sums modLn2)

          

renderLine [] = ""
renderLine (CR :xs) = uX  ++ renderLine xs
renderLine (ZR :xs) = u0  ++ renderLine xs
renderLine (_ :xs)  = uP  ++ renderLine xs
                      

renderLineS [] = ""
renderLineS (x:xs) = renderLine x ++ "\n" ++ renderLineS xs

getLineOfBoard a = return $ renderLineS a

getXY [] = (-1, -1) 
getXY (x:xs) = case (reads . snd $ x) of 
                     [] -> (-1,-1)
                     (xr:xrs) -> (fst x,  fst xr)


cycles brd me  = do
    putStrLn "Начнем пожалуй" 
    putStrLn "Напиши X и Y"
    xy <- getLine 
    let (x,y) = getXY . reads $ xy
    let step = mapState (changeBoard me x y) brd
    stpBrd <- getBoard step 
    toScr <- getLineOfBoard (fst stpBrd)
    let (res1, res2) = snd stpBrd 
    putStrLn toScr
    case res2 of
        FW -> putStrLn "Победила дружба!"
        CW -> putStrLn "Победил крестик!"
        ZW -> putStrLn "Победил нолик!"
        otherwise -> if res1==res2 then cycles step (not me) 
                                   else cycles brd me         

        


main :: IO ()
main = cycles startBoard True 
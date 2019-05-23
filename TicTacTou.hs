
{-# LANGUAGE LambdaCase #-}


import Control.Monad.State 

type Brd = [[Int]] 
type Board = State (Int, Int) Brd
uX = "\x1b[32m" ++ " X " ++ "\x1b[0m"
u0 = "\x1b[33m" ++ " 0 " ++ "\x1b[0m"
uP = "\x1b[37m" ++ " . " ++ "\x1b[0m"

speper 0 True = 1
speper 0 False = 2
speper 2 _ = 2
speper 1 _ = 1

startBoard :: Board 
startBoard = do put (0, 0); return $ replicate 3 . replicate 3 $ 0

getBoard s = return $ runState s (0, 0)

tranceArrayS me _ _ [] = []
tranceArrayS me 1 j (x:xs) = tranceArray me j x : tranceArrayS me 0 j xs
tranceArrayS me i j (x:xs) = x : tranceArrayS me (i-1) j xs

tranceArray me _ [] = []
tranceArray me 1 (x:xs) = speper x me : tranceArray me 0 xs
tranceArray me i (x:xs) = x: tranceArray me (i-1) xs

changeBoard me i j =   \(b, (s1, s2)) ->  do let nb = tranceArrayS me i j b;  
                                             (nb, (s1+1, whatsWrong2 nb . whatsWrong $ nb))                  

whatsWrong s  | l >=9 = -1
              | otherwise = l    
              where l = length . filter (>0) . join $ s   

whatsWrong2 :: Brd->Int->Int
whatsWrong2 s n = minimum [rX, rXX, r0, r00, rXXX, r000, n]
            where sX =  map (\case x | x==1 -> 1; _ -> 0) ss
                  s0 =  map (\case x | x==2 -> 1; _ -> 0) ss
                  ss =  join s
                  rX =  (checker 0 0 sX) $ -2 
                  r0 =  (checker 0 0 s0) $ -3
                  rXX = (checker1 0 0 sX) $ -2 
                  r00 = (checker1 0 0 s0) $ -3
                  rXXX = (checker2 0 0 sX) $ -2 
                  r000 = (checker2 0 0 s0) $ -3


checker s st ln 
           | s==3 = id
           | st>6 = const 100
           | otherwise = checker (sum . take 3 . drop st $ ln) (st+3) ln

checker1 s st ln 
           | s==3 = id
           | st>2 = const 100
           | otherwise = checker1 (sum modLn) (st+1) ln
                where modLn = (take 1 . drop st $ ln) ++(take 1 . drop (st+3) $ ln)++(take 1 . drop (st+6) $ ln)


checker2 s st ln 
            | res==3 = id
            | otherwise = const 100
                where modLn1 = (take 1 . drop st $ ln) ++(take 1 . drop (st+4) $ ln)++(take 1 . drop (st+8) $ ln)      
                      modLn2 = (take 1 . drop (st+2) $ ln) ++(take 1 . drop (st+4) $ ln)++(take 1 . drop (st+6) $ ln)      
                      res = max (sum modLn1) (sum modLn2)

          

changeLine [] = ""
changeLine (1 :xs) = uX  ++ changeLine xs 
changeLine (2 :xs) = u0  ++ changeLine xs 
changeLine (_ :xs) = uP  ++ changeLine xs 
                      

changeLineS [] = ""
changeLineS (x:xs) = changeLine x ++ "\n" ++ changeLineS xs

getLineOfBoard a = return $ changeLineS a

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
        -1 -> putStrLn "Победила дружба!"
        -2 -> putStrLn "Победил крестик!" 
        -3 -> putStrLn "Победил нолик!" 
        otherwise -> if res1==res2 then cycles step (not me) 
                                   else cycles brd me         

        


main :: IO ()
main = cycles startBoard True 

        


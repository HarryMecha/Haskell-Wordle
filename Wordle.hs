{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}


import Data.Char
import Foreign.C.Types
import Base

-----------------------------------------------------------
-- Code Author: Harry McCoy -----------------------
-----------------------------------------------------------

status :: Status -> String
status Here = "Y "
status Nowhere = "- "
status (Elsewhere (Just Before)) = "< "
status (Elsewhere (Just After)) = "> "
status (Elsewhere Nothing) = "y "


showStatus :: [Status] -> String
showStatus xs = do
 x <- xs
 status x

updateAvailable :: [Char] -> [(Char, Status)] -> [Char]
updateAvailable [] [] = []
updateAvailable [] y = []
updateAvailable (x:xs) y = if checkList x y == True then x : updateAvailable xs y
else updateAvailable xs y

leftMargin :: String
leftMargin = do
 let len = length(prompt Start)
 leftMarginAux len

leftMarginAux :: Int -> String
leftMarginAux 0 = []
leftMarginAux n = ' ' : leftMarginAux (n-1)

checkList :: Char -> [(Char,Status)] -> Bool
checkList _ [] = True
checkList x ((a,b):ys) =  if x == a then do
 if b == Nowhere then False 
       else 
         True
   else checkList x ys 



checkGuess :: String -> String -> [(Char, Status)]
checkGuess x y = checkGuessA x y y 1

checkGuessA :: String -> String -> String -> Int -> [(Char, Status)]
checkGuessA (x:xs) (y:ys) z i = do 
  if (x == y) 
    then (x,(Here)) : checkGuessA xs ys z (i+1)
    else do 
    if checkListAux x z 
      then do 
      if ((checkListIndex x z 1) <= (i -1)) 
       then do (x,(Elsewhere (Just Before))) : checkGuessA xs ys z (i+1)
       else do 
        if ((checkListIndex x z 1) >= (i + 1)) then (x,(Elsewhere (Just After))) : checkGuessA xs ys z (i+1)
          else do (x,(Elsewhere Nothing)) : checkGuessA xs ys z (i+1) 
      else do 
      (x,Nowhere) : checkGuessA xs ys z (i+1)
checkGuessA _ _  z _ = []      


checkListAux :: Char -> [Char] -> Bool
checkListAux _ [] = False
checkListAux x (y:ys) =  if x == y then True
   else checkListAux x ys 

checkListIndex :: Char -> [Char] -> Int -> Int
checkListIndex _ [] _ = 1000
checkListIndex x (y:ys) i =  if x == y then i
   else checkListIndex x ys (i+1)

--getGuess :: Int -> IO String
--getGuess f = do
 --case f of
  --  0 -> do 
   --  putStrLn " "
   --  return []
   -- _ -> do
   --  x <- (getChar')
   --  putChar ' '
   --  xs <- getGuess (f-1)
   --  return (x:xs)

getGuess :: Int -> [Char] -> IO String
getGuess f ys = do
 case f of
    0 -> do 
     putStrLn " "
     return []
    _ -> do
     x <- (getChar')
     if x == '.' then do
      if f < 5 then do
       putChar '\b'
       putChar '\b'
       putChar '\b'
       xs <- getGuess (f+1) ys
       return (tail xs)
      else do
       putChar '\b'
       xs <- getGuess f ys
       return (xs)
      else do 
      if checkListAux x ys then do 
        putChar ' '
        xs <- getGuess (f-1) ys
        return (x:xs)
      else do
        putChar '\b'
        xs <- getGuess f ys
        return (xs)

loop :: String -> [Char] -> Int -> IO ()
loop x ys f = do
 case f of
    0 -> do
     putStrLn (prompt Lose)
    _ -> do
     putStrLn ""
     putStrLn ("Attempt "++show(7-f))
     putStrLn (prompt Start)
     i <- getChar'
     if i == 'q' then do
        putChar '\b'
        putStrLn (prompt Quit)
        return ()
       else do
     putChar '\b'
     putStr (prompt Guess)
     y <- getGuess 5 ys
     let guess1 = checkGuess y x
     let guess = map snd (checkGuess y x)
     let status = showStatus guess
     if (status == "Y Y Y Y Y ")
        then do
        putStr leftMargin 
        putStrLn status
        putStrLn (prompt Win)
        else do 
        putStr leftMargin 
        putStrLn status
        xs <- loop x (updateAvailable ys guess1) (f-1) 
        return ()

go :: String -> IO () 
go x = loop x "abcdefghijklmnopqrstuvwxyz" 6
{-
-- EPITECH PROJECT, 2023
-- Parse_arg
-- File description:
-- Wolfram
-}

module Wolfram where

import System.Exit (ExitCode (ExitFailure), exitWith)
import Text.Read (readMaybe, read)
import System.IO (hPutStrLn, putStrLn, stderr)
import Data.List (length)

data Tabl = Tab Int Int Int deriving (Show)

data Wolfram = Empty | Wol Int Int Int Int Int deriving (Show)

startTab :: Int -> Int -> String
startTab 0 _ = " "
startTab w c | c == w = '*':(startTab (w - 1) c)
             | otherwise = ' ':(startTab (w - 1) c)

getLastElem :: [a] -> a
getLastElem (x:[]) = x
getLastElem (x:xs) = getLastElem xs

appendW :: [a] -> a -> [a]
appendW l s = reverse (s:(reverse l))

setChar30 :: Char -> Char -> Char -> Char
setChar30 ' ' ' ' ' ' = ' '
setChar30 ' ' ' ' '*' = '*'
setChar30 ' ' '*' ' ' = '*'
setChar30 ' ' '*' '*' = '*'
setChar30 '*' ' ' ' ' = '*'
setChar30 '*' ' ' '*' = ' '
setChar30 '*' '*' ' ' = ' '
setChar30 '*' '*' '*' = ' '

setChar90 :: Char -> Char -> Char -> Char
setChar90 ' ' ' ' ' ' = ' '
setChar90 ' ' ' ' '*' = '*'
setChar90 ' ' '*' ' ' = ' '
setChar90 ' ' '*' '*' = '*'
setChar90 '*' ' ' ' ' = '*'
setChar90 '*' ' ' '*' = ' '
setChar90 '*' '*' ' ' = '*'
setChar90 '*' '*' '*' = ' '

setChar110 :: Char -> Char -> Char -> Char
setChar110 ' ' ' ' ' ' = ' '
setChar110 ' ' ' ' '*' = '*'
setChar110 ' ' '*' ' ' = '*'
setChar110 ' ' '*' '*' = '*'
setChar110 '*' ' ' ' ' = ' '
setChar110 '*' ' ' '*' = '*'
setChar110 '*' '*' ' ' = '*'
setChar110 '*' '*' '*' = ' '

setStr30 :: String -> String
setStr30 (x:y:[]) = [setChar30 x y ' ']
setStr30 (x:y:z:xs) = (setChar30 x y z):(setStr30 (y:z:xs))

setStr90 :: String -> String
setStr90 (x:y:[]) = [setChar90 x y ' ']
setStr90 (x:y:z:xs) = (setChar90 x y z):(setStr90 (y:z:xs))

setStr110 :: String -> String
setStr110 (x:y:[]) = [setChar110 x y ' ']
setStr110 (x:y:z:xs) = (setChar110 x y z):(setStr110 (y:z:xs))

getTab :: [String] -> Tabl -> [String]
getTab [] (Tab r s w) =
    getTab [(startTab w ((div w 2)))] (Tab r s w)
getTab x (Tab _ 0 _) = x
getTab x (Tab 30 s w) | s > 0 = getTab (appendW x
                      (' ':(setStr30 (getLastElem x))))
                      (Tab 30 (s - 1) w)
getTab x (Tab 90 s w) | s > 0 = getTab (appendW x
                      (' ':(setStr90 (getLastElem x))))
                      (Tab 90 (s - 1) w)
getTab x (Tab 110 s w) | s > 0 = getTab (appendW x
                       (' ':(setStr110 (getLastElem x))))
                       (Tab 110 (s - 1) w)

parseAction :: [String] -> [(String, Int)]
parseAction [] = []
parseAction (_:[]) = []
parseAction (x:y:ls) = case readMaybe y :: Maybe Integer of
                            Just _ -> (x, read y):(parseAction ls)
                            Nothing -> []

makeWol :: [(String, Int)] -> Wolfram -> Wolfram
makeWol [] res = res
makeWol (("--rule", val):xs) (Wol _ s l w m) = makeWol xs (Wol val s l w m)
makeWol (("--start", val):xs) (Wol r _ l w m) = makeWol xs (Wol r val l w m)
makeWol (("--lines", val):xs) (Wol r s _ w m) = makeWol xs (Wol r s val w m)
makeWol (("--window", val):xs) (Wol r s l _ m) = makeWol xs (Wol r s l val m)
makeWol (("--move", val):xs) (Wol r s l w _) = makeWol xs (Wol r s l w val)
makeWol _ _ = Empty

moveTab :: String -> Int -> Int -> String
moveTab _ _ 0 = ""
moveTab "" 0 c = ' ':(moveTab "" 0 (c - 1))
moveTab "" v c | v > 0 = ' ':(moveTab "" (v - 1) (c - 1))
                | otherwise = ' ':(moveTab "" (v + 1) (c - 1))
moveTab (x:xs) 0 c = x:(moveTab xs 0 (c - 1))
moveTab (x:xs) v c | v > 0 = ' ':(moveTab (x:xs) (v - 1) (c - 1))
                   | otherwise = moveTab xs (v + 1) c

starterTab :: [String] -> Int -> Int -> Int -> [String]
starterTab [] _ _ _ = []
starterTab (x:xs) 0 v c = (moveTab x v c):(starterTab xs 0 v c)
starterTab (_:xs) i v c = starterTab xs (i - 1) v c

parseArg :: Wolfram -> IO ()
parseArg Empty = hPutStrLn stderr "Bad function" >>
    exitWith (ExitFailure 84)
parseArg (Wol 0 0 15000 80 0) = hPutStrLn stderr "Empty argument" >>
    exitWith (ExitFailure 84)
parseArg (Wol 0 _ _ _ _) = hPutStrLn stderr "No rule" >>
    exitWith (ExitFailure 84)
parseArg (Wol 30 s l w m) = mapM_ putStrLn (starterTab
    (getTab [] (Tab 30 (s + l - 1) (2 * (s + l) + 1)))
    s ((div (w - (2 * (s + l))) 2) + m - 1) (w))
parseArg (Wol 90 s l w m) = mapM_ putStrLn (starterTab
    (getTab [] (Tab 90 (s + l - 1) (2 * (s + l) + 1)))
    s ((div (w - (2 * (s + l))) 2) + m - 1) (w))
parseArg (Wol 110 s l w m) = mapM_ putStrLn (starterTab
    (getTab [] (Tab 110 (s + l - 1) (2 * (s + l) + 1)))
    s ((div (w - (2 * (s + l))) 2) + m - 1) (w))
parseArg _ = hPutStrLn stderr "Bad rule"
                        >> exitWith (ExitFailure 84)

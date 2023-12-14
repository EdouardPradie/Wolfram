{-
-- EPITECH PROJECT, 2023
-- Main
-- File description:
-- Wolfr
-}

import System.Environment (getArgs)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Wolfram (parseArg, Wolfram(Wol), makeWol, parseAction)

main :: IO ()
main = parseArg (makeWol
    (parseAction (unsafeLocalState getArgs)) (Wol 0 0 20000 80 0))
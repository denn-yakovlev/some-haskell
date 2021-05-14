{-# LANGUAGE BlockArguments #-}
module Main where

import AsmInterpreter

main :: IO ()
main = print $ interpret ["mov A 5", "inc A", "mov B 100", "dec B", "mov A B"]
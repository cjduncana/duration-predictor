module Main where

import Wizard (wizard)

main :: IO ()
main = do
  putStrLn "Welcome to Duration Predictor!"
  wizard

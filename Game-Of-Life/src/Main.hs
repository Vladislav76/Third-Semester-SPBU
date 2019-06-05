module Main where

import Flat
import Tape

startRandom2D :: Int -> Int -> IO () 
startRandom2D width height = Flat.run $ Flat.loadRandomUniverse width height

startPrepared2D :: String -> IO ()
startPrepared2D fileName = Flat.run $ Flat.loadUniverse fileName

startRandom1D :: Int -> IO()
startRandom1D length = Tape.run $ Tape.loadRandomUniverse length
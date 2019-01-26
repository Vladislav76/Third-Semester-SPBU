module Tape where

import Control.Concurrent
import System.IO.Unsafe
import System.Random

data Universe = Universe {getLength :: Int, getCells :: [Int]} deriving Show

nextState :: Universe -> Universe
nextState Universe {getLength = len, getCells = cells} = 
    checkRightEdge $ checkLeftEdge $ Universe len newCells
    where
      checkLeftEdge state@(Universe len cells) = 
          if head cells == 1 then Universe (len+1) (0:cells) else state
      checkRightEdge state@(Universe len cells) = 
          if last cells == 1 then Universe (len+1) (cells++[0]) else state          
      
      newCells = map getChangedCell [(i,x) | (i,x) <- zip [0..] cells]
          where 
            getChangedCell (p,x)
              | liveNeighbors == 1 = if x == 0 then 1 else 0
              | otherwise          = x
                where
                  liveNeighbors = length list
                  positions = map (`mod` len) [p - 1, p + 1]
                  list = [(i,x) | (i,x) <- zip [0..] cells, x == 1, i /= p, i `elem` positions]  

loadRandomUniverse :: Int -> Universe
loadRandomUniverse length = Universe {getLength = length, getCells = cells}
    where 
      list = randomRs (0,1) (unsafePerformIO newStdGen)
      cells = take length list

render :: Universe -> IO ()
render Universe {getLength = length, getCells = cells} = helper cells
    where 
      helper [] = putStrLn ""
      helper (x:list) 
        | x == 0 =
          do 
            putStr "."
            helper list
        | otherwise =
          do
            putStr "o"
            helper list

run :: Universe -> IO ()
run universe = 
    do
      putStr "\ESC[2J"
      render universe
      threadDelay 200000
      run $ nextState universe
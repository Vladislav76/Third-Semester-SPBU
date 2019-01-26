module Flat where

import Control.Concurrent
import System.IO  
import System.IO.Unsafe
import System.Random
import Data.List

data Universe = Universe {getWidth :: Int, getHeight :: Int, getCells :: [Int]} deriving Show

nextState :: Universe -> Universe
nextState sourceState@Universe {getWidth = width, getHeight = height, getCells = cells} = 
    checkRightEdge $ checkLeftEdge $ checkBottomEdge $ checkTopEdge $ Universe width height newCells
    where   
      checkTopEdge state@(Universe width height cells) = 
          if not (null list) then Universe width (height+1) (replicate width 0 ++ cells) else state
          where
            list = [(i,x) | (i,x) <- zip [0..] cells, x == 1, i >= 0, i < width]
      
      checkBottomEdge state@(Universe width height cells) = 
          if not (null list) then Universe width (height+1) (cells ++ replicate width 0) else state
          where
            list = [(i,x) | (i,x) <- zip [0..] cells, x == 1, i >= width*(height-1), i < width*height]
      
      checkLeftEdge state@(Universe width height cells) = 
          if not (null list) then Universe (width+1) height newList else state
          where
            list = [(i,x) | (i,x) <- zip [0..] cells, x == 1, i `mod` width == 0]
            newList = concat [if i `mod` width == 0 then [0,x] else [x] | (i,x) <- zip [0..] cells]
      
      checkRightEdge state@(Universe width height cells) = 
          if not (null list) then Universe (width+1) height newList else state
          where
            list = [(i,x) | (i,x) <- zip [0..] cells, x == 1, i `mod` width == width-1]
            newList = concat [if i `mod` width == width-1 then [x,0] else [x] | (i,x) <- zip [0..] cells]

      newCells = map getChangedCell [(i,x) | (i,x) <- zip [0..] cells]
          where
            getChangedCell (p,x)
              | x == 1 && (liveNeighbors < 2 || liveNeighbors > 3) = 0
              | x == 0 && liveNeighbors == 3                       = 1
              | otherwise                                          = x            
                where
                  liveNeighbors = length list
                  pi = p `div` width
                  pj = p `mod` width
                  getPosition (i,j) = i * width + j
                  positions = map getPosition (nub [((pi+di) `mod` height, (pj+dj) `mod` width) | di <- [-1..1], dj <- [-1..1]])
                  list = [(i,x) | (i,x) <- zip [0..] cells, x == 1, i /= p, i `elem` positions]       

loadUniverse :: String -> Universe
loadUniverse fileName = unsafePerformIO load
    where
      load = 
        do  
          handle <- openFile fileName ReadMode
          contentLine <- hGetLine handle
          let singlewords = words contentLine
              firstLine = f singlewords
              width = length firstLine  
          content <- hGetContents handle
          let singleWords = words content
              otherLines = f singleWords
              list = firstLine ++ otherLines
              cellsNumber = length list
              height = cellsNumber `div` width
          print $ "Universe is loaded. Area: " ++ (show cellsNumber) ++ " cells."
          hClose handle 
          return Universe {getWidth = width, getHeight = height, getCells = list}
        where
          f :: [String] -> [Int]
          f = map read

loadRandomUniverse :: Int -> Int -> Universe
loadRandomUniverse width height = Universe {getWidth = width, getHeight = height, getCells = cells}
    where 
      list = randomRs (0,1) (unsafePerformIO newStdGen)
      cells = take (width * height) list

render :: Universe -> IO ()
render Universe {getWidth = width, getHeight = height, getCells = cells} = 
    helper 0
      where 
        helper index = 
          do
            renderLine index 0
            if index < height - 1
              then helper (index + 1)
              else putStr ""
        renderLine index j = 
          if j < width
            then case cells !! (index * width + j) of 
                  0 -> do
                          putStr "."
                          renderLine index (j+1)
                  _ -> do 
                          putStr "o"
                          renderLine index (j+1)
            else putStrLn ""
      
run :: Universe -> IO ()
run universe = 
    do
      putStr "\ESC[2J"
      render universe
      threadDelay 300000
      run $ nextState universe
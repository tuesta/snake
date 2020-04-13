module Logic
  ( handleEvent,
    generateNewFood,
    animation,
  )
where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import System.Random

import Game

handleEvent :: Event -> Game -> Game
handleEvent event game@(Game ps d _ std state) = case event of
  EventKey (SpecialKey KeyUp) Down _ _ -> changeDirection UP
  EventKey (SpecialKey KeyDown) Down _ _ -> changeDirection DOWN
  EventKey (SpecialKey KeyRight) Down _ _ -> changeDirection RIGHT
  EventKey (SpecialKey KeyLeft) Down _ _ -> changeDirection LEFT
  EventKey (SpecialKey KeySpace) Down _ _ -> case state of
    Running -> game {gameState = Pause}
    Pause -> game {gameState = Running}
    GameOver _ -> initialGame $ generateNewFood initialSnake std
  _ -> game
  where
    changeDirection d' = if opposite d == d' then game else game {direction = d'}
    opposite d =
      (\(Just d) -> d) . lookup d $
        zip
          [UP, DOWN, RIGHT, LEFT]
          [DOWN, UP, LEFT, RIGHT]

move :: Direction -> Position -> Position
move d (x, y) = case d of
  UP -> (x, y + 1)
  DOWN -> (x, y - 1)
  RIGHT -> (x + 1, y)
  LEFT -> (x - 1, y)

checkGameOver :: [Position] -> Bool
checkGameOver snake =
  headC <= 0 || headC >= cols ||
  headR <= 1 || headR >= rows ||
  head' `elem` tail'
  where
    head' = head snake
    (headC, headR) = head'
    tail' = tail snake

generateNewFood :: [Position] -> StdGen -> (Position, StdGen)
generateNewFood ps s
  | newFood `elem` ps = generateNewFood ps s2
  | otherwise = (newFood, s2)
  where
    (x, s1) = randomR (1, cols - 1) s
    (y, s2) = randomR (1, rows - 1) s1
    newFood = (x, y)

animation :: Float -> Game -> Game
animation _ game@(Game _ _ _ _ Pause) = game
animation _ game@(Game ps d p s state)
  | checkGameOver ps = game {gameState = GameOver $ length ps - length initialSnake}
  | p == newP = game { positions = pps
                     , position = fst fS
                     , getRandomStdGen = snd fS
                     }
  | otherwise = game {positions = init pps}
  where
    newP = move d (head ps)
    fS = generateNewFood ps s
    pps = newP : ps

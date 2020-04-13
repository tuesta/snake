module Game where

import System.Random (StdGen)

type Position = (Int, Int)

data Direction = UP | DOWN | RIGHT | LEFT
  deriving (Eq, Enum, Show)

data State = GameOver Int | Pause | Running deriving Eq

data Game
  = Game
      { positions :: [Position],
        direction :: Direction,
        position :: Position,
        getRandomStdGen :: StdGen,
        gameState :: State
      }

screenWidth = 640 :: Int--640-960
screenHeight = 480 :: Int--480-720

[cols, rows] = (`div` 10) <$> [screenWidth, screenHeight]

initialSnake =
  [ (cols `div` 2, rows `div` 2),
    (cols `div` 2, rows `div` 2 + 1),
    (cols `div` 2, rows `div` 2 + 2)
  ]

initialGame :: (Position, StdGen) -> Game
initialGame (p, std) =
  Game
    { positions = initialSnake,
      direction = DOWN,
      position = p,
      getRandomStdGen = std,
      gameState = Running
    }

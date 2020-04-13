module Main where

import System.Random
import Graphics.Gloss

import Game
import Rendering
import Logic

window :: Display
window = InWindow "snake" (screenWidth, screenHeight) (100, 100)

main :: IO ()
main = do
  std <- getStdGen
  play
    window
    black
    30
    (initialGame $ generateNewFood initialSnake std)
    gameAsPicture
    handleEvent
    animation

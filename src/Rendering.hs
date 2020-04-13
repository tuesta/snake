module Rendering (gameAsPicture) where

import Graphics.Gloss

import Game

(sW, sH) = (screenWidth, screenHeight)
half = (`div` 2)

fillRectangle :: (Int, Int) -> Position -> Picture
fillRectangle (w, h) (c, r) =
      color white
    $ translate x y
    $ rectangleSolid w' h'
  where
    [w', h', x, y] = fromIntegral <$> [w, h, c * 10 - half sW, r * 10 - half sH]

gameOverPicture :: Int -> Picture
gameOverPicture p = pictures . (color white <$>) $ [gameOver, points p, tryAgain]
  where
    gameOver =
          translate (-180) 35
        $ scale 0.5 0.5
        $ text "Game Over"
    points p =
          translate (-100) (-30)
        $ scale 0.35 0.35
        $ text $ "Points: " <> show p
    tryAgain =
          translate (-175) (-80)
        $ scale 0.2 0.2
        $ text "Press SPACE to try again."

gameAsPicture :: Game -> Picture
gameAsPicture game = case gameState game of
  GameOver p -> gameOverPicture p
  _ -> pictures $ walls <> snake <> [food]
  where
    snake = fillRectangle (10, 10) <$> positions game
    food = fillRectangle (10, 10) $ position game
    walls =
      zipWith
        fillRectangle
        [(sW, 10), (sW, 10), (10, sH), (10, sH)]
        [(half cols, 0), (half cols, rows), (0, half rows), (cols, half rows)]

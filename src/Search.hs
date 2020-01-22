module Search where

import           Data.List
import           Graphics.Gloss

boardSize = 100
unseenColor = white

data BoardState =  Start | Seen | Goal deriving (Show, Eq)

data Coord a = Coord a a deriving (Show, Eq, Ord)

instance Functor Coord where
  fmap f (Coord x y) = Coord (f x) (f y)

instance Applicative Coord where
  pure x = Coord x x
  (Coord f f') <*> (Coord x y) = Coord (f x) (f' y)


btc :: BoardState -> Color
btc Start = blue
btc Seen  = black
btc Goal  = red

bts :: BoardState -> Float -> Float -> Picture
bts Start = rectangleSolid
bts Seen  = rectangleWire
bts Goal  = rectangleSolid


pathToVisual :: Float -> BoardState -> Coord Int -> Picture
pathToVisual scale colo (Coord x y) = translate
  (fromIntegral x * scale)
  (fromIntegral y * scale)
  (color (btc colo) (bts colo scale scale))

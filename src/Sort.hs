module Sort where

import           Data.List
import           Graphics.Gloss
import qualified Data.Set                      as S
import           Control.Monad.State

boardSize = 100
unseenColor = white

data BFS = BFS {    start :: BoardStateSet,
                    seen :: BoardStateSet,
                    neighbors :: BoardStateSet,
                    goal :: BoardStateSet} deriving Show

emptyBFS = BFS S.empty S.empty S.empty S.empty

data BoardState =  Start | Seen | Goal deriving (Show, Eq)
type BoardStateSet = S.Set (Coord Int)

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

visualizePath :: Float -> (Bool, BFS) -> Picture
visualizePath scale (_, bfs) =
  pictures $ startPictures ++ seenPictures ++ goalPictures
 where
  startPictures = map (pathToVisual scale Start) ((S.elems . start) bfs)
  seenPictures  = map (pathToVisual scale Seen) ((S.elems . seen) bfs)
  goalPictures  = map (pathToVisual scale Goal) ((S.elems . goal) bfs)

coordNeighbors :: [Coord Int] -> Coord Int -> [Coord Int]
coordNeighbors ds c = map (\d -> (+) <$> c <*> d) ds

cardinalDeltas = [Coord 1 0, Coord (-1) 0, Coord 0 1, Coord 0 (-1)]

cardinalNeighbors :: Coord Int -> S.Set (Coord Int) -> S.Set (Coord Int)
cardinalNeighbors c r =
  let newNeighbors = coordNeighbors cardinalDeltas c
  in  foldr S.insert r newNeighbors

-- Marks all current neighbors as seen. Does not change the neighbor list.
-- Returns True if a goal has been seen, and False otherwise.
bfsMarkSeen :: State BFS Bool
bfsMarkSeen = state
  (\s ->
    let newSeen   = foldr S.insert (seen s) (neighbors s)
        foundGoal = newSeen `S.disjoint` goal s
    in  (foundGoal, s { seen = newSeen })
  )

-- Updates the neighbor list with all new adjacent unseen neighbors
bfsGetNeighbors :: State BFS ()
bfsGetNeighbors = state
  (\s ->
    let newNeighbors = S.foldr cardinalNeighbors S.empty (neighbors s)
    in  ((), s { neighbors = newNeighbors })
  )

bfsStep :: State BFS Bool
bfsStep = do
  continue <- bfsMarkSeen
  when continue bfsGetNeighbors
  pure continue

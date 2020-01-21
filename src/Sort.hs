module Sort where

import           Data.List
import           Graphics.Gloss
import qualified Data.Map                      as M
import           Control.Monad.State

boardSize = 100
unseenColor = greyN 0.5

data BFS = BFS {    seen :: BoardStateMap,
                    neighbors :: [Coord Int],
                    goal :: [Coord Int]} deriving Show

emptyBFS = BFS M.empty [] []

data BoardState =  Seen | Goal deriving (Show, Eq)
type BoardStateMap = M.Map (Coord Int) BoardState

data Coord a = Coord a a deriving (Show, Eq, Ord)

instance Functor Coord where
  fmap f (Coord x y) = Coord (f x) (f y)

instance Applicative Coord where
  pure x = Coord x x
  (Coord f f') <*> (Coord x y) = Coord (f x) (f' y)


btc :: BoardState -> Color
btc Seen = black
btc Goal = red

pathToVisual :: Float -> BoardState -> Coord Int -> Picture
pathToVisual scale colo (Coord x y) = translate
  (fromIntegral x * scale)
  (fromIntegral y * scale)
  (color (btc colo) (rectangleSolid scale scale))

visualizePath :: Float -> (Bool, BFS) -> Picture
visualizePath scale (_, bfs) = pictures $ exploredPictures ++ goalPictures
 where
  goalPictures     = map (pathToVisual scale Goal) (goal bfs)
  exploredPictures = map (pathToVisual scale Seen) (M.keys (seen bfs))

coordNeighbors :: [Coord Int] -> Coord Int -> [Coord Int]
coordNeighbors ds c = map (\d -> (+) <$> c <*> d) ds

cardinalDeltas = [Coord 1 0, Coord (-1) 0, Coord 0 1, Coord 0 (-1)]

cardinalNeighbors :: Coord Int -> [Coord Int]
cardinalNeighbors = coordNeighbors cardinalDeltas

-- Marks all current neighbors as seen. Does not change the neighbor list.
-- Returns True if a goal has been seen, and False otherwise.
bfsMarkSeen :: State BFS Bool
bfsMarkSeen = state
  (\s ->
    let markSeen c = M.insert c Seen
        newSeen   = foldr markSeen (seen s) (neighbors s)
        foundGoal = [] /= intersect (goal s) (M.keys newSeen)
    in  (foundGoal, s { seen = newSeen })
  )

-- Updates the neighbor list with all new adjacent unseen neighbors
bfsGetNeighbors :: State BFS ()
bfsGetNeighbors = state
  (\s ->
    let newNeighbors =
            concatMap cardinalNeighbors (neighbors s) \\ M.keys (seen s)
    in  ((), s { neighbors = newNeighbors })
  )

bfsStep :: State BFS Bool
bfsStep = do
  done <- bfsMarkSeen
  unless done bfsGetNeighbors
  pure done

{- Simple game loop example. -}

import System.IO
import System.Timeout
import Data.Fixed

inputTimeout = 50000
stepLength = 0.05
rotationStep = 0.05
mapSize = (10,10)
totalMapSquares = (fst mapSize) * (snd mapSize)

squareEmpty = 0                     -- map square enums
squareWall = 1

type MapSquare = Int

gameMap1 = 
  [
    0,0,0,0,0,0,1,1,1,0,
    0,0,0,1,0,0,1,0,0,0,
    0,0,0,1,0,0,1,0,0,0,
    0,0,0,1,0,0,1,0,0,0,
    0,0,0,1,0,0,1,0,0,0,
    0,0,0,1,1,1,1,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0
  ]

data GameState = GameState
  {
    playerPos :: (Float,Float),     -- x, y, starting top left (map AND square)
    playerRot :: Float,             -- rotation in radians, CCW, 0 = facing right
    gameMap :: [Int]
  } deriving (Show)

initialGameState = GameState
  {
    playerPos = (0.0,0.0),
    playerRot = 0.0,
    gameMap = gameMap1
  }

-----------------------------------------------   Converts 2D map coords to 1D array coords.

mapToArrayCoords :: (Int, Int) -> Int
mapToArrayCoords coords =
  snd coords * (fst mapSize) + fst coords

-----------------------------------------------   Converts 1D array coords to 2D map coords.

arrayToMapCoords :: Int -> (Int, Int)
arrayToMapCoords coords =
  (mod coords (fst mapSize),div coords (fst mapSize))

-----------------------------------------------   Renders the game state into string, simple version.

renderGameStateSimple :: GameState -> String
renderGameStateSimple gameState =

  concat
    (
      map
        (   
          \square ->
              (
                if mod (snd square) (fst mapSize) == 0
                  then "\n"
                  else ""
              )
                ++
              (
                if floor (fst (playerPos gameState)) == fst (arrayToMapCoords (snd square)) &&
                   floor (snd (playerPos gameState)) == snd (arrayToMapCoords (snd square))
                   then case round (4.0 * (playerRot gameState) / pi)  of
                     0 -> "->"
                     1 -> "/^"
                     2 -> "|^"
                     3 -> "^\\"
                     4 -> "<-"
                     5 -> "./"
                     6 -> ".|"
                     7 -> "\\."
                     8 -> "->"
                  else if fst square == squareEmpty
                    then "  "
                    else "[]"
              )
        ) (zip (gameMap gameState) [0..])
    )
  ++
  "\npos: " ++ (show (playerPos gameState)) ++ "\nrot: " ++ (show (playerRot gameState)) ++ "\n-----"

-----------------------------------------------   Returns map square at given corrds.

mapSquareAt :: GameState -> (Int, Int) -> MapSquare
mapSquareAt gameState coords =
   (gameMap gameState) !! (mapToArrayCoords coords)

-----------------------------------------------   Checks if given player position is walid (collisions).

positionIsWalkable :: GameState -> (Float, Float) -> Bool
positionIsWalkable gameState position =
  not (floor (fst position) < 0) &&
  not (floor (fst position) >= (fst mapSize)) &&
  not (floor (snd position) < 0) &&
  not (floor (snd position) >= (snd mapSize)) &&
  (mapSquareAt gameState (floor (fst position),floor (snd position))) == squareEmpty

-----------------------------------------------   Moves the player forward by given distance, with collisions.

movePlayer :: GameState -> Float -> GameState
movePlayer previousGameState distance =
  previousGameState
    {
      playerPos =
        (
          let plusX = cos (playerRot previousGameState) * distance in
          fst (playerPos previousGameState) + 
          if positionIsWalkable previousGameState ((fst (playerPos previousGameState)) + plusX,snd (playerPos previousGameState))
            then plusX
            else 0,

          let plusY = -1 * (sin (playerRot previousGameState) * distance) in
          snd (playerPos previousGameState) + 
          if positionIsWalkable previousGameState (fst (playerPos previousGameState),(snd (playerPos previousGameState)) + plusY)
            then plusY
            else 0
        )
    }

-----------------------------------------------   Computes the next game state.

nextGameState :: GameState -> Char -> GameState
nextGameState previousGameState inputChar =

  case inputChar of
    'w' -> movePlayer previousGameState stepLength
    's' -> movePlayer previousGameState (-1 * stepLength)
    'a' -> previousGameState { playerRot = mod' ((playerRot previousGameState) + rotationStep) (2 * pi) }
    'd' -> previousGameState { playerRot = mod' ((playerRot previousGameState) - rotationStep) (2 * pi) }
    _   -> previousGameState

-----------------------------------------------   Main game loop.

loop :: GameState -> IO ()
loop gameState =
  do
    putStrLn (renderGameStateSimple gameState)

    c <- timeout inputTimeout getChar -- wait for input, with timeout

    case c of
      -- no input given
      Nothing -> do loop gameState

      -- quit on 'q'
      Just 'q' -> do putStrLn "quitting"                     

      -- input was given
      Just input -> do loop (nextGameState gameState input)

-----------------------------------------------
        
main = 
  do
    hSetBuffering stdin NoBuffering   -- to read char without [enter]
    loop initialGameState

{- Simple game loop example. -}

import System.IO
import System.Timeout
import Data.Fixed
import Debug.Trace

inputTimeout = 50000
stepLength = 0.05
rotationStep = 0.05
mapSize = (10,10)
screenSize = (30,15)
fieldOfView = pi / 4.0

totalMapSquares = (fst mapSize) * (snd mapSize)
rayAngleStep = fieldOfView / fromIntegral (fst mapSize)


type MapSquare = Int
squareEmpty = 0                     -- map square enums
squareWall = 1

type Normal = Int
normalNorth = 0
normalEast = 1
normalSouth = 2
normalWest = 3

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
    playerPos = (0.5,0.5),
    playerRot = 0.0,
    gameMap = gameMap1
  }

fst3 (x, _, _) = x
snd3 (_, x, _) = x
thd3 (_, _, x) = x

-----------------------------------------------

addCouples :: (Num a) => (Num b) => (a, b) -> (a, b) -> (a, b)
addCouples first second =
  ((fst first) + (fst second),(snd first) + (snd second))

-----------------------------------------------

floorCouple :: (RealFrac a) => (RealFrac b) => (a, b) -> (Int, Int)
floorCouple couple =
  (floor (fst couple),floor (snd couple))

-----------------------------------------------   Converts 2D map coords to 1D array coords.

mapToArrayCoords :: (Int, Int) -> Int
mapToArrayCoords coords =
  snd coords * (fst mapSize) + fst coords

-----------------------------------------------   Converts 1D array coords to 2D map coords.

arrayToMapCoords :: Int -> (Int, Int)
arrayToMapCoords coords =
  (mod coords (fst mapSize),div coords (fst mapSize))

-----------------------------------------------

angleTo02Pi :: Float -> Float
angleTo02Pi angle =
  mod' angle (2 * pi)

-----------------------------------------------

draw3Dview :: [(Float, Normal)] -> Int -> String
draw3Dview drawInfo height =
  let
    middle = div height 2 + 1
    halfHeight = (fromIntegral height) / 2
  in
    concat[
           map (
                \item -> if abs (middle - i) < floor (  (5.0 / ((fst item) + 5.0)) * halfHeight  )
                           then
                             if (snd item) == normalNorth then      'n'
                             else if (snd item) == normalEast then  'e'
                             else if (snd item) == normalSouth then 's'
                             else                                   'w'
                           else ' ') drawInfo ++ "\n"
           | i <- [1..height]]

-----------------------------------------------   Renders the game in 3D.

renderGameState3D :: GameState -> String
renderGameState3D gameState =
  let
    drawInfo = (renderDrawInfo gameState)
  in
    (renderGameStateSimple gameState)
    ++
    "\n"
    ++
    draw3Dview drawInfo (snd screenSize)

-----------------------------------------------

renderDrawInfo :: GameState -> [(Float, Normal)]
renderDrawInfo gameState =
  [
    castRay gameState (playerPos gameState) (floorCouple (playerPos gameState)) ((playerRot gameState) - fieldOfView / 2 + x * rayAngleStep) 10
    | x <- [0..(fst screenSize) - 1]
  ]

-----------------------------------------------   Casts a ray and returns an information (distance, normal) about a wall it hits.

castRay :: GameState -> (Float, Float) -> (Int, Int) -> Float -> Int ->  (Float, Normal)
castRay gameState rayOrigin square rayDirection maxIterations =
  let
    squareCoords = floorCouple rayOrigin
    angle = angleTo02Pi rayDirection
  in
    if (mapSquareAt gameState square) /= squareEmpty || maxIterations == 0
      then (0,normalNorth)
      else
        let
          squareCastResult = castRaySquare square rayOrigin angle
        in
          let
            recursionResult = castRay gameState (fst squareCastResult) (addCouples square (snd squareCastResult)) angle (maxIterations - 1)
          in
            (
              pointPointDistance rayOrigin (fst squareCastResult) + (fst recursionResult),
              if (fst recursionResult) /= 0
                then (snd recursionResult)
                else
                  case (snd squareCastResult) of
                    (1,0)  -> normalEast
                    (0,1)  -> normalSouth
                    (-1,0) -> normalWest
                    _      -> normalNorth
            )

-----------------------------------------------   Casts a ray inside a single square.

castRaySquare :: (Int, Int) -> (Float, Float) -> Float -> ((Float, Float),(Int, Int)) -- result = (intersection,next square offset)
castRaySquare squareCoords rayPosition rayAngle =
  let
    angle = 2 * pi - rayAngle
  in
    let
      boundX = (fst squareCoords) + if angle < (pi / 2) || angle > (pi + pi / 2) then 1 else 0
      boundY = (snd squareCoords) + if angle < pi then 1 else 0
    in
      let intersection1 = lineLineIntersection rayPosition angle (fromIntegral boundX,fromIntegral (snd squareCoords)) (pi / 2)
          intersection2 = lineLineIntersection rayPosition angle (fromIntegral (fst squareCoords),fromIntegral boundY) 0
      in
        if (pointPointDistance rayPosition intersection1) <= (pointPointDistance rayPosition intersection2)
          then (intersection1,(if boundX == (fst squareCoords) then -1 else 1,0))
          else (intersection2,(0,if boundY == (snd squareCoords) then -1 else 1))

-----------------------------------------------   Gets distance of two points.

pointPointDistance :: (Float, Float) -> (Float, Float) -> Float
pointPointDistance point1 point2 =
  let
    dx = (fst point1) - (fst point2)
    dy = (snd point1) - (snd point2)
  in
    sqrt (dx * dx + dy * dy)

-----------------------------------------------   Makes the angle safe for tan function.

safeAngle :: Float -> Float
safeAngle angle =
  if mod' angle (pi / 2) == 0.0
    then angle + 0.0001
    else angle

-----------------------------------------------   Computes an intersection point of two lines.

lineLineIntersection :: (Float, Float) -> Float -> (Float, Float) -> Float -> (Float, Float)
lineLineIntersection position1 angle1 position2 angle2 =
  let
    tan1 = tan (safeAngle angle1)
    tan2 = tan (safeAngle angle2)
    p1x  = fst position1
    p1y  = snd position1
    p2x  = fst position2
    p2y  = snd position2
    denominator = tan1 - tan2
  in
    let x = (p2y - tan2 * p2x - p1y + tan1 * p1x) / denominator
    in (x,if abs tan1 < abs tan2 then tan1 * x + (p1y - tan1 * p1x) else tan2 * x + (p2y - tan2 * p2x))

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
  "\npos: " ++ (show (playerPos gameState)) ++ "\nrot: " ++ (show (playerRot gameState)) ++ "\n"

-----------------------------------------------   Returns map square at given corrds.

mapSquareAt :: GameState -> (Int, Int) -> MapSquare
mapSquareAt gameState coords =
   if ((fst coords) < (fst mapSize)) && ((fst coords) >= 0) && ((snd coords) < (snd mapSize)) && ((snd coords) >= 0)
    then (gameMap gameState) !! (mapToArrayCoords coords)
    else squareWall

-----------------------------------------------   Checks if given player position is walid (collisions).

positionIsWalkable gameState position =
  (mapSquareAt gameState (floorCouple position)) == squareEmpty

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
    'a' -> previousGameState { playerRot = angleTo02Pi ((playerRot previousGameState) + rotationStep) }
    'd' -> previousGameState { playerRot = angleTo02Pi ((playerRot previousGameState) - rotationStep) }
    _   -> previousGameState

-----------------------------------------------   Main game loop.

loop :: GameState -> IO ()
loop gameState =
  do
    putStrLn (renderGameState3D gameState)

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

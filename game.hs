{- Simple game loop example. -}

import System.IO
import System.Timeout
import Data.Fixed
import Debug.Trace
import Control.Concurrent

inputTimeout = 50000
stepLength = 0.1
rotationStep = 0.06
mapSize = (15,15)
screenSize = (150,35)
fieldOfView = pi / 1.5
focalLength = 0.5
maxRaycastIterations = 20

totalMapSquares = (fst mapSize) * (snd mapSize)
rayAngleStep = fieldOfView / (fst screenSize)

type MapSquare = Int
squareEmpty = 0                     -- map square enums
squareWall = 1

type Normal = Int                   -- possible wall normals
normalNorth = 0
normalEast = 1
normalSouth = 2
normalWest = 3

gameMap1 = 
  [
    0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,
    0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,1,0,0,0,0,0,1,0,1,
    0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,
    0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ]

data GameState = GameState
  {
    playerPos :: (Double,Double),     -- x, y, starting top left (map AND square)
    playerRot :: Double,             -- rotation in radians, CCW, 0 = facing right
    gameMap :: [Int]
  } deriving (Show)

initialGameState = GameState
  {
    playerPos = (0.5,0.5),
    playerRot = 0.0,
    gameMap = gameMap1
  }

grayscaleMap = [                    -- characters sorted by brigtness
    'M','$','o','?','/','!',';',':','\'','.','-']

fst3 (x, _, _) = x
snd3 (_, x, _) = x
thd3 (_, _, x) = x

-----------------------------------------------   Ensures given values is in given interval by clamping it.

clamp :: (Ord a) => a -> a -> a -> a
clamp value minimum maximum
  | value < minimum = minimum
  | value > maximum = maximum
  | otherwise       = value

-----------------------------------------------   Adds two 2-item couples tuples, itemwise.

addCouples :: (Num a) => (Num b) => (a, b) -> (a, b) -> (a, b)
addCouples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-----------------------------------------------   Applies floor function to both items of a 2 item tuple.

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

angleTo02Pi :: Double -> Double
angleTo02Pi angle =
  mod' angle (2 * pi)

-----------------------------------------------   Maps normalized intensity to ASCII character.

intensityToChar :: Double -> Char
intensityToChar intensity =
  grayscaleMap !! (clamp (floor (intensity * fromIntegral (length grayscaleMap))) 0 ((length grayscaleMap) - 1))

-----------------------------------------------   Returns an intensity addition (possibly negative) cause by distance.

distanceToIntensity :: Double -> Double
distanceToIntensity distance =
  (min (distance / 7.0) 1.0) * (-0.3)

-----------------------------------------------   Renders the 3D player view into String.

render3Dview :: [(Double, Normal)] -> Int -> String
render3Dview drawInfo height =
  let
    middle = div height 2 + 1
    heightDouble = (fromIntegral height)
  in
    concat
      [
        let
          distanceFromMiddle = abs (middle - i)
        in
          map
            (
              \item ->
                let
                  columnHeight = floor ((1.0 / ((fst item) + 1.0)) * heightDouble)
                in
                  if distanceFromMiddle < columnHeight
                    then
                      if (snd item) == normalNorth then      intensityToChar $ 0.25 + distanceToIntensity (fst item)
                      else if (snd item) == normalEast then  intensityToChar $ 0.50 + distanceToIntensity (fst item)
                      else if (snd item) == normalSouth then intensityToChar $ 0.75 + distanceToIntensity (fst item)
                      else                                   intensityToChar $ 1.00 + distanceToIntensity (fst item)
                  else ' ' --intensityToChar ( 5 *  (fromIntegral distanceFromMiddle) / heightFrac )
            ) drawInfo ++ "\n"
           
        | i <- [1..height]
      ]

-----------------------------------------------   Renders the game in 3D.

renderGameState3D :: GameState -> String
renderGameState3D gameState =
  let
    drawInfo = castRays gameState
  in
  --  (renderGameStateSimple gameState)
  --  ++
  --  "\n"
  --  ++
    render3Dview drawInfo (snd screenSize)

-----------------------------------------------   Gets the distance from projection origin to projection plane.

distanceToProjectionPlane :: Double -> Double -> Double
distanceToProjectionPlane focalDistance angleFromCenter =
  focalDistance * (tan angleFromCenter)

-----------------------------------------------   Casts all rays needed to render player's view, returns a list of ray cast results.

castRays :: GameState -> [(Double, Normal)]
castRays gameState =
  [
    let
      rayDirection = ((playerRot gameState) + fieldOfView / 2 - x * rayAngleStep)
      rayResult = castRay gameState (playerPos gameState) (floorCouple (playerPos gameState)) rayDirection maxRaycastIterations
    in
      (
        max
          (
            (fst rayResult) - (distanceToProjectionPlane focalLength (abs $ (playerRot gameState) - rayDirection))
          ) 0.0,
        snd rayResult
      )

    | x <- [0..(fst screenSize) - 1]
  ]

-----------------------------------------------   Casts a ray and returns an information (distance, normal) about a wall it hits.

castRay :: GameState -> (Double, Double) -> (Int, Int) -> Double -> Int ->  (Double, Normal)
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

-----------------------------------------------   Casts a ray inside a single square, returns (intersection point with square bounds,next square offset)

castRaySquare :: (Int, Int) -> (Double, Double) -> Double -> ((Double, Double),(Int, Int))
castRaySquare squareCoords rayPosition rayAngle =
  let
    angle = 2 * pi - rayAngle
    boundX = (fst squareCoords) + if angle < (pi / 2) || angle > (pi + pi / 2) then 1 else 0
    boundY = (snd squareCoords) + if angle < pi then 1 else 0
    intersection1 = lineLineIntersection rayPosition angle (fromIntegral boundX,fromIntegral (snd squareCoords)) (pi / 2)
    intersection2 = lineLineIntersection rayPosition angle (fromIntegral (fst squareCoords),fromIntegral boundY) 0
  in
    if (pointPointDistance rayPosition intersection1) <= (pointPointDistance rayPosition intersection2)
      then (intersection1,(if boundX == (fst squareCoords) then -1 else 1,0))
      else (intersection2,(0,if boundY == (snd squareCoords) then -1 else 1))

-----------------------------------------------   Gets distance of two points.

pointPointDistance :: (Double, Double) -> (Double, Double) -> Double
pointPointDistance point1 point2 =
  let
    dx = (fst point1) - (fst point2)
    dy = (snd point1) - (snd point2)
  in
    sqrt (dx * dx + dy * dy)

-----------------------------------------------   Makes the angle safe for tan function.

tanSafeAngle :: Double -> Double
tanSafeAngle angle =
  if mod' angle (pi / 2) == 0.0
    then angle + 0.00001
    else angle

-----------------------------------------------   Computes an intersection point of two lines.

lineLineIntersection :: (Double, Double) -> Double -> (Double, Double) -> Double -> (Double, Double)
lineLineIntersection position1 angle1 position2 angle2 =
  let
    tan1 = tan (tanSafeAngle angle1)
    tan2 = tan (tanSafeAngle angle2)
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

-----------------------------------------------   Returns map square at given coords.

mapSquareAt :: GameState -> (Int, Int) -> MapSquare
mapSquareAt gameState coords =
   if ((fst coords) < (fst mapSize)) && ((fst coords) >= 0) && ((snd coords) < (snd mapSize)) && ((snd coords) >= 0)
    then (gameMap gameState) !! (mapToArrayCoords coords)
    else squareWall

-----------------------------------------------   Checks if given player position is valid (collisions).

positionIsWalkable gameState position =
  (mapSquareAt gameState (floorCouple position)) == squareEmpty

-----------------------------------------------   Moves player by given distance in given direction, with collisions.

movePlayerInDirection :: GameState -> Double -> Double -> GameState
movePlayerInDirection previousGameState angle distance =
  let
    plusX = cos angle * distance
    plusY = -1 * (sin angle * distance)
  in
    previousGameState
      {
        playerPos =
          (
            fst (playerPos previousGameState) + 
            if positionIsWalkable previousGameState ((fst (playerPos previousGameState)) + plusX,snd (playerPos previousGameState))
              then plusX
              else 0,

            snd (playerPos previousGameState) + 
            if positionIsWalkable previousGameState (fst (playerPos previousGameState),(snd (playerPos previousGameState)) + plusY)
              then plusY
              else 0
          )
      }    

-----------------------------------------------   Moves the player forward by given distance, with collisions.

movePlayerForward :: GameState -> Double -> GameState
movePlayerForward previousGameState distance =
  movePlayerInDirection previousGameState (playerRot previousGameState) distance

-----------------------------------------------   Strafes the player left by given distance (with collisions).

strafePlayer :: GameState -> Double -> GameState
strafePlayer previousGameState distance =
  movePlayerInDirection previousGameState (angleTo02Pi ((playerRot previousGameState) + pi / 2)) distance

-----------------------------------------------   Computes the next game state.

nextGameState :: GameState -> Char -> GameState
nextGameState previousGameState inputChar =
  case inputChar of
    'w' -> movePlayerForward previousGameState stepLength
    's' -> movePlayerForward previousGameState (-1 * stepLength)
    'a' -> previousGameState { playerRot = angleTo02Pi ((playerRot previousGameState) + rotationStep) }
    'd' -> previousGameState { playerRot = angleTo02Pi ((playerRot previousGameState) - rotationStep) }
    'q' -> strafePlayer previousGameState stepLength
    'e' -> strafePlayer previousGameState (-1 * stepLength)
    _   -> previousGameState

-----------------------------------------------   Main game loop.

loop :: GameState -> IO ()
loop gameState =
  do
    putStrLn (renderGameState3D gameState)
    hFlush stdout

    c <- timeout inputTimeout getChar             -- wait for input, with timeout

    case c of
      -- no input given
      Nothing -> do loop gameState

      -- quit on 'q'
      Just 'x' -> do putStrLn "quitting"                     

      -- input was given
      Just input -> do loop (nextGameState gameState input)

-----------------------------------------------
        
main = 
  do
    hSetBuffering stdin NoBuffering                     -- to read char without [enter]
    hSetBuffering stdout (BlockBuffering (Just 20000))  -- to read flickering
    hSetEcho stdout False                               
    loop initialGameState

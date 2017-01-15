{- 
  raycasting game in Haskell
  
  Miloslav Číž, 2017
-}

import System.IO
import System.Timeout
import Data.Fixed
import Data.Char
import Debug.Trace
import Control.Concurrent
import System.CPUTime

inputTimeout :: Int
inputTimeout = 50000

stepLength :: Double
stepLength = 0.1

rotationStep :: Double
rotationStep = 0.06

mapSize :: (Int,Int)
mapSize = (15,15)

screenSize :: (Int,Int)
screenSize = (150,40)

fieldOfView :: Double
fieldOfView = pi / 2

focalLength :: Double
focalLength = 0.5

maxRaycastIterations :: Int
maxRaycastIterations = 20

spriteSize :: (Int,Int)
spriteSize = (15,10)

spriteScale :: Double
spriteScale = fromIntegral (snd screenSize) / fromIntegral (snd spriteSize) * 2

totalMapSquares :: Int
totalMapSquares = (fst mapSize) * (snd mapSize)

rayAngleStep :: Double
rayAngleStep = fieldOfView / fromIntegral (fst screenSize)

infinity = 1.0 / 0.0

backgroundChar = ' '
transparentChar = 'X'               -- marks transparency in sprites

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
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ]

type Sprite = Int
spriteNone = -1
spriteTree = 0
spriteZombie = 1
spriteDemon = 2
spriteGun = 3
spriteUzi = 4
spriteMedkit = 5

spriteList =
  [
    [
      "XXXXXXXXXXXXXXX",
      "XXXX/'''''\\XXXX",
      "XX/'       ''\\X",
      "X|   O        |",
      "X|        O   |",
      "XX\\_  O      /X",
      "XXXX\\_  ____/XX",
      "XXXXXX||XXXXXXX",
      "XXXXXX||XXXXXXX",
      "XX-=#/__\\#=-XXX"
    ],
    [
      "XXXXXXXXXXXXXXX",
      "XXXXX/`\\XXXXXXX",
      "XXXX(o o)XXXXXX",
      "XXXX|===|XXXXXX",
      "X/''    ''')XXX",
      "(==/     \\==)XX",
      "XXX(_____)XXXXX",
      "XXX|  _  |XXXXX",
      "XXX| |X| |XXXXX",
      "X-(__]#\\__)-XXX"
    ],
    [
      "|\\XX/''`''\\XX/|",
      "\\ ;`       `; /",
      "X/  _     _  \\X",
      "X| [WW   WW] |X",
      "XX\\_`  A  `_/XX",
      "XXXX\\ ,_, /XXXX",
      "XXXX| [_] |XXXX",
      "XXXXX\\___/XXXXX",
      "XXXXXXXXXXXXXXX",
      "XX-==#####==-XX"
    ],
    [
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "Xl_/_\\\\\\^^^^^^]",
      "X/  P\\_______/X",
      "/    (__)XXXXXX",
      "|   /XXXXXXXXXX",
      "(___)XXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      " --==######==--"
    ],
    [
      "|^\\XXXXXXXXX/^|",
      "|  ^^^^^^^^^ _|",
      "|  -o-- [[[(_o)",
      "|___________  |",
      "XX\\  __|X[_]\\_|",
      "XX/ /_]XX| |XXX",
      "X/ /XXXXX| |XXX",
      "|_/XXXXXX|_|XXX",
      "XXXXXXXXXXXXXXX",
      "--===#####===--"
    ],
    [
      "XXXXXXXXXXXXXXX",
      "XX/`````````\\XX",
      "X| ..  _  .. |X",
      "X|   _| |_   |X",
      "X|  [  +  ]  |X",
      "X|   `|_|`   |X",
      "X| ..     .. |X",
      "XX\\_________/XX",
      "XXXXXXXXXXXXXXX",
      " --==#####==-- "
    ]
  ]

data GameState = GameState
  {
    playerPos :: (Double,Double),          -- x, y, starting top left (map AND square)
    playerRot :: Double,                   -- rotation in radians, CCW, 0 = facing right
    gameMap :: [Int],
    sprites :: [((Double,Double),Sprite)]  -- list of sprites with world position
  } deriving (Show)

initialGameState = GameState
  {
    playerPos = (7.5,8.5),
    playerRot = 0.0,
    gameMap = gameMap1,
    sprites =
      [
        ((7.5,7.5),spriteTree),
        ((6.5,7.5),spriteZombie),
        ((7.5,2.5),spriteTree),
        ((3.0,6.0),spriteDemon),
        ((8.1,8.5),spriteDemon),
        ((9.0,6.5),spriteGun),
        ((2.0,8.0),spriteUzi),
        ((12.0,13.0),spriteMedkit)
      ]
  }

grayscaleMap = ['M','$','o','?','/','!',';',':','\'','.','-']          -- characters sorted by brigtness

-----------------------------------------------   Functions for 3-item tuples.

fst3 (x, _, _) = x
snd3 (_, x, _) = x
thd3 (_, _, x) = x

-----------------------------------------------   Alternative version of trace for debugging.

trace2 :: a -> (a -> String) -> a
trace2 what func =
  trace (func what) what

-----------------------------------------------   Ensures given values is in given interval by clamping it.

clamp :: (Ord a) => a -> (a, a) -> a
clamp value (minimum, maximum) =
  (min maximum . max minimum) value

-----------------------------------------------   Adds two 2-item pair tuples, itemwise.

addPairs :: (Num a) => (Num b) => (a, b) -> (a, b) -> (a, b)
addPairs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-----------------------------------------------   Applies floor function to both items of a pair.

floorPair :: (RealFrac a) => (RealFrac b) => (a, b) -> (Int, Int)
floorPair couple =
  (floor (fst couple),floor (snd couple))

-----------------------------------------------   Makes the angle safe for tan function.

tanSafeAngle :: Double -> Double
tanSafeAngle angle
  | mod' angle (pi / 2) == 0.0 = angle + 0.00001
  | otherwise                  = angle

-----------------------------------------------

vectorAngle :: (Double,Double) -> Double
vectorAngle vector =
  atan2 (-1 * (snd vector)) (fst vector)

-----------------------------------------------   Returns the result of angle1 - angle2 closest to 0.

angleAngleDifference :: Double -> Double -> Double
angleAngleDifference angle1 angle2 =
  let
   difference = angleTo02Pi (angle1 - angle2)
  in
   if difference > pi
     then difference - 2 * pi
     else difference
   
-----------------------------------------------

angleTo02Pi :: Double -> Double
angleTo02Pi angle =
  mod' angle (2 * pi)

-----------------------------------------------   Gets distance of two points.

pointPointDistance :: (Double, Double) -> (Double, Double) -> Double
pointPointDistance point1 point2 =
  let
    dx = (fst point1) - (fst point2)
    dy = (snd point1) - (snd point2)
  in
    sqrt (dx * dx + dy * dy)
     
-----------------------------------------------   Converts 2D map coords to 1D array coords.

mapToArrayCoords :: (Int, Int) -> Int
mapToArrayCoords coords =
  snd coords * (fst mapSize) + fst coords

-----------------------------------------------   Converts 1D array coords to 2D map coords.

arrayToMapCoords :: Int -> (Int, Int)
arrayToMapCoords coords =
  (mod coords (fst mapSize),div coords (fst mapSize))

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

-----------------------------------------------   Maps normalized intensity to ASCII character.

intensityToChar :: Double -> Char
intensityToChar intensity =
  grayscaleMap !! (clamp (floor (intensity * fromIntegral (length grayscaleMap))) (0,((length grayscaleMap) - 1)))

-----------------------------------------------   Returns an intensity addition (possibly negative) cause by distance.

distanceToIntensity :: Double -> Double
distanceToIntensity distance =
  (min (distance / 7.0) 1.0) * (-0.3)

-----------------------------------------------   Maps worldspace distance to normalized screenspace size (caused by perspective).

distanceToSize :: Double -> Double
distanceToSize distance =
  1.0 / (distance + 1.0)
  
-----------------------------------------------   Projects sprites to screen space, returns a list representing screen, each
                                             --   pixel has (sprite id,sprite x pixel,distance), sprite id = -1 => empty.

projectSprites :: GameState -> [(Sprite,Int,Double)]
projectSprites gameState =
  let
    -- project all sprites to screenspace first:
    screenspaceSprites =                                         -- [(sprite id,sprite x pixel,distance)]
      [
        (
          snd sprite,                                            -- sprite id
            0.5 +                                                -- sprite center in screenspace, normalized
            (
              angleAngleDifference (playerRot gameState) ( vectorAngle ( fst (fst sprite) - fst (playerPos gameState), snd (fst sprite) - snd (playerPos gameState) ) )
            )
            / fieldOfView
            ,
          pointPointDistance (playerPos gameState) (fst sprite)  -- sprite distance
        )
        | sprite <- (sprites gameState)
      ]
      
    projectOneSprite :: (Sprite,Double,Double) -> [(Sprite,Int,Double)] -> [(Sprite,Int,Double)]  -- [(sprite id,sprite x pixel,distance)]
    projectOneSprite =                  -- projects a single sprite to screen list
      (
        \spriteInfo screenList ->
          let
            spritePos = (snd3 spriteInfo) * fromIntegral ((length screenList) - 1)
            spriteLength = (distanceToSize (thd3 spriteInfo)) * fromIntegral (fst spriteSize) * spriteScale
            spriteInterval = ( floor (spritePos - spriteLength / 2) , floor (spritePos + spriteLength / 2) )
          in
            map
              (
                \item ->
                  if (snd item) >= (fst spriteInterval) && (snd item) <= (snd spriteInterval)
                    then
                      (
                        (fst3 spriteInfo),
                        round $ ((fromIntegral ( (snd item) - (fst spriteInterval) )) / spriteLength) * fromIntegral ((fst spriteSize) - 1),
                        (thd3 spriteInfo)
                      )
                    else (fst item)
              )
              (zip screenList [0..])
      )
      
    emptyScreenlList = [(spriteNone,0,infinity) | i <- [0..(fst screenSize) - 1]]
  in
    foldl
      (
        \screenList1 screenList2 ->
          map
            (
              \itemPair ->                  -- compare depths
                if (thd3 (fst itemPair)) <= (thd3 (snd itemPair))
                  then (fst itemPair)
                  else (snd itemPair)
            )
            (zip screenList1 screenList2)
      )
             
      emptyScreenlList
          
      [projectOneSprite spriteItem emptyScreenlList | spriteItem <- screenspaceSprites]
      
-----------------------------------------------   

sampleSprite :: Sprite -> (Int,Int) -> Char
sampleSprite spriteId coordinates =
  let
    safeCoords =
      (
        clamp (fst coordinates) (0,(fst spriteSize) - 1),
        clamp (snd coordinates) (0,(snd spriteSize) - 1)
      )
  in
    ((spriteList !! spriteId) !! (snd safeCoords)) !! (fst safeCoords)

-----------------------------------------------   Renders the 3D player view into String.

render3Dview :: [(Double, Normal)] -> [(Sprite,Int,Double)] -> Int -> String
render3Dview wallMap spriteMap height =
  let
    middle = div height 2 + 1
    heightDouble = (fromIntegral height)
  in
    concat
      [
        let
          distanceFromMiddle = middle - i
          absDistanceFromMiddle = abs distanceFromMiddle
        in
          map
            (
              \item ->
                let                  
                  normal = (snd (fst item))
                  distance = (fst (fst item))
                  columnHeight = floor ((distanceToSize distance) * heightDouble)
                  spriteInfo = (snd item)
                  
                  wallSample =
                    if absDistanceFromMiddle < columnHeight
                      then
                        if normal == normalNorth then      intensityToChar $ 0.25 + distanceToIntensity distance
                        else if normal == normalEast then  intensityToChar $ 0.50 + distanceToIntensity distance
                        else if normal == normalSouth then intensityToChar $ 0.75 + distanceToIntensity distance
                        else                               intensityToChar $ 1.00 + distanceToIntensity distance
                      else backgroundChar
                  
                  spriteHalfHeight = floor ( spriteScale * distanceToSize (thd3 spriteInfo) * fromIntegral (snd spriteSize) / 2 )
                  sampleX = snd3 spriteInfo
                  sampleY = round (((1 - (1 + (fromIntegral distanceFromMiddle) / (fromIntegral spriteHalfHeight)) / 2)) * fromIntegral ((snd spriteSize) - 1))
                  spriteSample = sampleSprite (fst3 spriteInfo) (sampleX,sampleY)
                in
                  if (thd3 spriteInfo) >= distance      -- is wall closer than sprite?
                    then wallSample
                  else
                    if absDistanceFromMiddle <= spriteHalfHeight
                      then
                        if spriteSample /= transparentChar
                          then spriteSample
                          else wallSample
                      else wallSample
            )
            (zip wallMap spriteMap) ++ "\n"
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
    render3Dview drawInfo (projectSprites gameState) (snd screenSize)

-----------------------------------------------   Gets the distance from projection origin to projection plane.

distanceToProjectionPlane :: Double -> Double -> Double
distanceToProjectionPlane focalDistance angleFromCenter =
  focalDistance * (cos angleFromCenter)

-----------------------------------------------   Casts all rays needed to render player's view, returns a list of ray cast results.

castRays :: GameState -> [(Double, Normal)]
castRays gameState =
  [
    let
      rayDirection = ((playerRot gameState) + fieldOfView / 2 - (fromIntegral x) * rayAngleStep)
      rayResult = castRay gameState (playerPos gameState) (floorPair (playerPos gameState)) rayDirection maxRaycastIterations
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
    squareCoords = floorPair rayOrigin
    angle = angleTo02Pi rayDirection
  in
    if (mapSquareAt gameState square) /= squareEmpty || maxIterations == 0
      then (0,normalNorth)
      else
        let
          squareCastResult = castRaySquare square rayOrigin angle
          recursionResult = castRay gameState (fst squareCastResult) (addPairs square (snd squareCastResult)) angle (maxIterations - 1)
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
                then
                  case round (4.0 * (playerRot gameState) / pi)  of
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
mapSquareAt gameState coords 
  | (fst coords) < (fst mapSize) && (fst coords) >= 0 && (snd coords) < (snd mapSize) && (snd coords) >= 0 = (gameMap gameState) !! (mapToArrayCoords coords)
  | otherwise = squareWall

-----------------------------------------------   Checks if given player position is valid (collisions).

positionIsWalkable gameState position =
  (mapSquareAt gameState (floorPair position)) == squareEmpty

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

gameLoop :: GameState -> IO ()
gameLoop gameState =
  do
    t1 <- getCPUTime                                          -- for profiling, comment out otherwise
    
    putStrLn (renderGameState3D gameState)
    
    t2 <- getCPUTime                                          -- for profiling, comment out otherwise
    putStrLn (show (fromIntegral (t2 - t1) / 10e9) ++ " ms")  -- for profiling, comment out otherwise
    
    hFlush stdout
    
    c <- timeout inputTimeout getChar             -- wait for input, with timeout
    
    case c of
      -- no input given
      Nothing -> do gameLoop gameState
      -- quit on 'q'
      Just 'x' -> do putStrLn "quitting"                     
      -- input was given
      Just input -> do gameLoop (nextGameState gameState input)

-----------------------------------------------
        
main = 
  do
    hSetBuffering stdin NoBuffering                     -- to read char without [enter]
    hSetBuffering stdout (BlockBuffering (Just 20000))  -- to read flickering
    hSetEcho stdout False                               
    gameLoop initialGameState

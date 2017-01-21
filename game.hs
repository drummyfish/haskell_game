{----------------------------------------

  raycasting game in Haskell
  
  Miloslav Číž, 2017
  
----------------------------------------}

import System.IO
import System.Timeout
import Data.Fixed
import Data.Char
import Debug.Trace
import Control.Concurrent
import System.CPUTime
import Data.List

-- key mapping:

keyForward     = 'w'
keyBackward    = 's'
keyTurnLeft    = 'a'
keyTurnRight   = 'd'
keyStrafeLeft  = 'q'
keyStrafeRight = 'e'
keyFire        = 'p'
keyWeapon1     = '1'
keyWeapon2     = '2'
keyWeapon3     = '3'
keyQuit        = 'x'

frameDelayMs = 16                         -- in milliseconds
frameDelayUs = frameDelayMs * 1000        -- in microseconds
stepLength = 0.1
zombieStepLength = 0.01
demonStepLength = 0.09
rotationStep = 0.06
mapSize = (15,15)
infoBarHeight = 5
screenSize = (150,45)
viewSize = ( (fst screenSize) , (snd screenSize) - infoBarHeight )
fieldOfView = pi / 2
focalLength = 0.5
maxRaycastIterations = 20
spriteSize = (15,10)
spriteScale = fromIntegral (snd viewSize) / fromIntegral (snd spriteSize) * 2
totalMapSquares = (fst mapSize) * (snd mapSize)
rayAngleStep = fieldOfView / fromIntegral (fst viewSize)
infinity = 1.0 / 0.0
animationFrameStep = 4
spriteDepthBias = 1                 -- so that sprites don't disappear in walls
backgroundChar = ' '
transparentChar = 'X'               -- marks transparency in sprites
emptyLines = 15                     -- number of empty lines added before each rendered frame
emptyLineString = ['\n' | i <- [1..emptyLines]]
recomputeAIin = 64

fireRateKnife = 6
fireRateGun = 10
fireRateUzi = 4

monsterHealthZombie = 100           -- initial health amounts
monsterHealthDemon = 50

weaponSpritePosition = ((fst viewSize) - (fst viewSize) `div` 3,1 + snd viewSize - snd spriteSize)

type Position2D = (Double,Double)   -- in squares, starting top left
data MonsterType = Zombie | Demon deriving(Show, Eq)
data Normal = NormalNorth | NormalWest | NormalSouth | NormalEast deriving(Show, Eq)
data Weapon = Knife | Gun | Uzi deriving(Show, Eq)
data MapSquare = SquareEmpty | SquareWall deriving(Show, Eq)

sE = SquareEmpty                    -- short aliases
sW = SquareWall

gameMap1 :: [MapSquare]
gameMap1 = 
  [
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sW,sW,sE,sE,
    sE,sE,sE,sW,sE,sE,sW,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sW,sE,sE,sW,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sW,sE,sE,sW,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sW,sE,sE,sW,sE,sE,sE,sE,sE,sW,sE,sW,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sW,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sW,sW,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sW,sW,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,
    sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE,sE
  ]

type SpriteType = Int
spriteNone = -1
spriteTree = 0
spriteZombie = 1   -- animated, 2 frames
-- skip
spriteDemon = 3    -- animated, 2 frames
-- skip
spriteGun = 5
spriteUzi = 6
spriteMedkit = 7
-- skip
spriteFPKnife = 8  -- animated, 2 frames
-- skip
spriteFPGun = 10   -- animated, 2 frames
-- skip
spriteFPUzi = 12   -- animated, 2 frames

grayscaleMap = ['M','$','o','?','/','!',';',':','\'','.','-']          -- characters sorted by brigtness

animatedSpriteIds = [1,3,8,10,12]     -- list of sprite IDs that are animated

spriteList =
  [
    [ -- 0
      "XXXXXXXXXXXXXXX",
      "XXXX/'''''\\XXXX",
      "XX/'       ''\\X",
      "X|   O        |",
      "X|        O   |",
      "XX\\_  O      /X",
      "XXXX\\_  ____/XX",
      "XXXXXX||XXXXXXX",
      "XXXXXX||XXXXXXX",
      "XX-=#/__\\#=-XXX"],
    [ -- 1
      "XXXXXXXXXXXXXXX",
      "XXXXX/```\\XXXXX",
      "XXXXX[o.o]XXXXX",
      "XXXXX\\II]/XXXXX",
      "XXX/^^````(III)",
      "X(III]    /XXXX",
      "XXXX(_____)XXXX",
      "XXXX(  _  )XXXX",
      "XXXX| |X\\ |XXXX",
      "X--(__]=|__)--X"],
    [ -- 2
      "XXXXXXXXXXXXXXXX",
      "XXXXX/```\\XXXXX",
      "XXXXX[o.o]XXXXX",
      "XXXXX\\[II/XXXXX",
      "X(III]```^^\\XXX",
      "XXXX\\    (III)X",
      "XXXX|_____)XXXX",
      "XXXX(  _  )XXXX",
      "XXXX/ | | |XXXX",
      "X--(__/=|__)--X"],
    [ -- 3
      "X|\\X-''`''-X/|X",
      "X\\;`       `;/X",
      "X/ ,_     _, \\X",
      "X( \\0) , (0/ )X",
      "XX\\_:  ^  :_/XX",
      "XXXX\\ |^| /XXXX",
      "XXXX| [_] |XXXX",
      "XXXXX\\___/XXXXX",
      "XXXXXXXXXXXXXXX",
      "X--==#####==--X"],
    [ -- 4
      "|\\XX-'```'-XX/|",
      "\\ ;`       `; /",
      "X/ ,_ \\ / _, \\X",
      "X( (0)   (0) )X",
      "XX\\_   A   _/XX",
      "XXXX) ___ (XXXX",
      "XXXX( ' ' )XXXX",
      "XXXXX'---'XXXXX",
      "XXXXXXXXXXXXXXX",
      "X--==#####==--X"],
    [ -- 5
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "Xl_/_\\\\\\^^^^^^]",
      "X/  P\\_______/X",
      "/    ({_)XXXXXX",
      "|   /XXXXXXXXXX",
      "(___)XXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "X--==#####==--X"],
    [ -- 6
      "|^\\XXXXXXXXX/^|",
      "|  ^^^^^^^^^ _|",
      "|  -o-- [[[(_o)",
      "|___________  |",
      "XX\\  __|X[_]\\_|",
      "XX/ /_]XX| |XXX",
      "X/ /XXXXX| |XXX",
      "|_/XXXXXX|_|XXX",
      "XXXXXXXXXXXXXXX",
      "--===#####===--"],
    [ -- 7
      "XXXXXXXXXXXXXXX",
      "XX/`````````\\XX",
      "X| ..  _  .. |X",
      "X|   _| |_   |X",
      "X|  [  +  ]  |X",
      "X|   `|_|`   |X",
      "X| ..     .. |X",
      "XX\\_________/XX",
      "XXXXXXXXXXXXXXX",
      " --==#####==-- "],
    [ -- 8
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXX/|XXXX",                   
      "XXXXXXXX/ |XXXX",
      "XXXXXXX( ,|XXXX",
      "XXXXXXX| |<XXXX",
      "XXXXXXX| |<XXXX",
      "XXXXXXX| |<XXXX",
      "XXXXXXX| |<XXXX"],
     [ -- 9
      "XXXXXXXXXXXXXXX",
      "XXXXXXXXXXXXXXX",
      "XX|\\XXXXXXXXXXX",
      "XX| \\XXXXXXXXXX",
      "XX( ,\\XXXXXXXXX",
      "XXX\\ \\LXXXXXXXX",
      "XXXX\\ \\LXXXXXXX",
      "XXXXX\\ \\LX/\\XXX",
      "XXXXXX\\ \\V /XXX",
      "XXXX\\^^ ,  \\XXX"],
     [ -- 10
      "X/^^\\XXXXXXXXXX",
      "[:\\ Y\\XXXXXXXXX",
      "X\\:\\__`\\XXXXXXX",
      "XX\\:\\ \\ \\XXXXXX",
      "XXX\\:\\`  `\\XXXX",
      "XXX/\\:\\____\\XXX",
      "XX(_|::\\ P |\\XX",
      "XX( \\_:|   | )X",
      "XX(\\_ )-___/ /X",
      "XXX\\ \\      |XX"],
     [ -- 11
      "XWWWWWWWXXXXXXX",
      "WW/^^\\WWXXXXXXX",
      "WW|:) Y\\WXXXXXX",
      "XW(:( _ \\XXXXXX",
      "XXX|:( \\ \\XXXXX",
      "XXX\\::)`  \\XXXX",
      "XXXX\\:(    \\XXX",
      "XXXX(::\\____|\\X",
      "XXX/|::|  P | |",
      "XX(  \\_|    | /"],
     [ -- 12
      "X[\\^^\\XXXXXXXXX",
      "X|:\\__\\XXXXXXXX",
      "C|::|__|XXXXXXX",
      "X|: :\\ \\XXXXXXX",
      "X|:  :\\ \\XXXXXX",
      "X|::\\ :\\ `\\XXXX",
      "XX\\_:\\ :\\  \\XXX",
      "XXX|\\:\\ :\\__\\XX",
      "XX/|:\\:O :\\  `\\",
      "X( |:|\\:::|^^^|"],
     [ -- 13
      "WXXXXXXXXXXXXXX",
      "WWXXXXXXXXXXXXX",
      "WWW[\\^^\\XXXXXXX",
      "XWW|:\\__\\XXXXXX",
      "XWC|::|__|XXXXX",
      "XXW|: :\\ \\XXXXX",
      "XXX|:  :\\ \\XXXX",
      "XXX|::\\ :\\ `\\XX",
      "XXXX\\_:\\ :\\  \\X",
      "XXXX/ \\:\\ :\\__\\"
     ]
  ]

data GameState = GameState
  {
    playerPos ::      Position2D,
    playerRot ::      Double,                          -- rotation in radians, CCW, 0 = facing right
    frameNumber ::    Int,
    currentWeapon ::  Weapon,
    currentLevel ::   Int,
    currentScore ::   Int,
    gameMap ::        [MapSquare],
    monsters ::       [Monster],                       -- list of monsters
    sprites ::        [Sprite],                        -- list of sprites
    fireCountdown ::  Int                              -- counter for implementing different fire rates
  } deriving (Show)
  
data Sprite = Sprite
  {
    spriteType ::      SpriteType,
    spritePos ::       Position2D
  } deriving (Show)
  
data Monster = Monster
  {
    monsterType ::    MonsterType,
    monsterPos ::     Position2D,
    health ::         Int,
    countdownAI ::    Int,
    monsterRot ::     Double
  } deriving (Show)
  
newMonster :: MonsterType -> Position2D -> Monster
newMonster monsterType initialPosition = Monster
  {
    monsterType = monsterType,
    monsterPos = initialPosition,
    health = 
      if monsterType == Zombie
        then monsterHealthZombie
        else monsterHealthDemon,
    countdownAI = 0,
    monsterRot = 0
  }
  
initialGameState :: GameState
initialGameState = GameState
  {
    playerPos       = (7.5,8.5),
    playerRot       = 0.0,
    frameNumber     = 0,
    currentWeapon   = Knife,
    currentLevel    = 1,
    currentScore    = 0,
    gameMap         = gameMap1,
    monsters        =
      [
        newMonster Zombie (6,7),
        newMonster Demon (8,5)
      ],
    sprites = [],
    fireCountdown   = 0
  }

-----------------------------------------------   Functions for 3-item tuples.

fst3 (x, _, _) = x
snd3 (_, x, _) = x
thd3 (_, _, x) = x

-----------------------------------------------   Splits given list to chunks of size n.

splitChunks _ [] = []
splitChunks n list = first : (splitChunks n rest)
  where
    (first,rest) = splitAt n list

-----------------------------------------------   Fills given string with spaces to given length.

toLength :: String -> Int -> String
toLength what outputLength =
  what ++ [' ' | i <- [1.. outputLength - length(what)]]

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

-----------------------------------------------

substractPairs :: (Num a) => (Num b) => (a, b) -> (a, b) -> (a, b)
substractPairs (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

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

vectorAngle :: Position2D -> Double
vectorAngle vector =
  atan2 (-1 * (snd vector)) (fst vector)

-----------------------------------------------   Returns the result of angle1 - angle2 closest to 0.

angleAngleDifference :: Double -> Double -> Double
angleAngleDifference angle1 angle2 
  | difference > pi = difference - 2 * pi
  | otherwise       = difference
  where 
    difference = angleTo02Pi (angle1 - angle2)
    
-----------------------------------------------

angleTo02Pi :: Double -> Double
angleTo02Pi angle =
  mod' angle (2 * pi)

-----------------------------------------------   Gets distance of two points.

pointPointDistance :: Position2D -> Position2D -> Double
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

lineLineIntersection :: Position2D -> Double -> Position2D -> Double -> Position2D
lineLineIntersection (x1,y1) angle1 (x2,y2) angle2 = (x,y)
  where
    tan1 = tan (tanSafeAngle angle1)
    tan2 = tan (tanSafeAngle anggitle2)
    x = (y2 - tan2 * x2 - y1 + tan1 * x1) / (tan1 - tan2)
    y = if abs tan1 < abs tan2 then tan1 * x + (y1 - tan1 * x1) else tan2 * x + (y2 - tan2 * x2)

-----------------------------------------------   Maps normalized intensity to ASCII character.

intensityToChar :: Double -> Char
intensityToChar intensity =
  let
    safeIndex = clamp (floor (intensity * fromIntegral (length grayscaleMap))) (0,(length grayscaleMap) - 1)
  in
    grayscaleMap !! safeIndex 
    
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

projectSprites :: GameState -> [(SpriteType,Int,Double)]
projectSprites gameState =
  let
    -- project all sprites to screenspace first:
    screenspaceSprites =                                         -- [(sprite id,sprite x pixel,distance)]
      [
        (
          spriteType sprite,                                     
            0.5 +                                                -- sprite center in screenspace, normalized
            (
              angleAngleDifference (playerRot gameState) ( vectorAngle ( fst (spritePos sprite) - fst (playerPos gameState), snd (spritePos sprite) - snd (playerPos gameState) ) )
            )
            / fieldOfView
            ,
          (pointPointDistance (playerPos gameState) (spritePos sprite)) - spriteDepthBias   -- sprite distance
        )
        | sprite <- (sprites gameState)
      ]
      
    -- projects one sprite (sprite,x,y) to a screen list [(sprite id,sprite x pixel,distance)]
    projectOneSprite :: (SpriteType,Double,Double) -> [(SpriteType,Int,Double)] -> [(SpriteType,Int,Double)]  
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
      
    emptyScreenlList = [(spriteNone,0,infinity) | i <- [0..(fst viewSize) - 1]]
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
      
-----------------------------------------------   Samples given sprite.

sampleSprite :: SpriteType -> (Int,Int) -> Int -> Char
sampleSprite spriteId coordinates animationFrame =
  let
    safeCoords =
      (
        clamp (fst coordinates) (0,(fst spriteSize) - 1),
        clamp (snd coordinates) (0,(snd spriteSize) - 1)
      )
  in
    ((spriteList !! (spriteId + animationFrame)) !! (snd safeCoords)) !! (fst safeCoords)
    
-----------------------------------------------   Gets animation frame for current frame number.

animationFrameForSprite :: SpriteType -> Int -> Int
animationFrameForSprite spriteId frameNumber
  | ((frameNumber `div` animationFrameStep) `mod` 2 == 1) && (spriteId `elem` animatedSpriteIds) = 1
  | otherwise = 0

-----------------------------------------------   Renders the 3D player view (no bar or weapon) into String.

render3Dview :: [(Double, Normal)] -> [(SpriteType,Int,Double)] -> Int -> Int -> String
render3Dview wallMap spriteMap height frameNumber =
  let
    middle = div height 2 + 1                     -- middle line of the view
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
                        if normal == NormalNorth then      intensityToChar $ 0.25 + distanceToIntensity distance
                        else if normal == NormalEast then  intensityToChar $ 0.50 + distanceToIntensity distance
                        else if normal == NormalSouth then intensityToChar $ 0.75 + distanceToIntensity distance
                        else                               intensityToChar $ 1.00 + distanceToIntensity distance
                      else backgroundChar
                  
                  spriteHalfHeight = floor ( spriteScale * distanceToSize (thd3 spriteInfo) * fromIntegral (snd spriteSize) / 2 )
                  sampleX = snd3 spriteInfo
                  sampleY = round (((1 - (1 + (fromIntegral distanceFromMiddle) / (fromIntegral spriteHalfHeight)) / 2)) * fromIntegral ((snd spriteSize) - 1))
                  spriteSample = sampleSprite (fst3 spriteInfo) (sampleX,sampleY) (animationFrameForSprite (fst3 spriteInfo) frameNumber)
                in
                  if (thd3 spriteInfo) >= distance                  -- is wall closer than sprite?
                    then wallSample                                 
                    else                                            -- sprite is closer  
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

-----------------------------------------------   Renders the lower info bar to String.

renderInfoBar :: GameState -> String
renderInfoBar gameState =
  let
    separatorPositions = [0,15,31,63]
    separator = "+" ++ [if i `elem` separatorPositions then '+' else '~' | i <- [3..(fst viewSize)]] ++ "+"
    emptyLine = "|" ++ [if i `elem` separatorPositions then '|' else ' ' | i <- [3..(fst viewSize)]] ++ "|\n"
    infoLine = "|  level: " ++ (toLength (show (currentLevel gameState)) 3) ++ "|  score: " ++ (toLength (show (currentScore gameState)) 6) ++ "|  health: 100/100  ##########  |  ammo: 100/100"
  in
    separator ++ "\n" ++
    emptyLine ++
    (toLength infoLine ((fst screenSize) - 1)) ++ "|\n" ++
    emptyLine ++
    separator
      
-----------------------------------------------   Overlays a string image over another

overlay :: String -> String -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Char -> String
overlay background foreground position backgroundResolution foregroundResolution transparentChar =
  let
    backgroundLines = splitChunks (fst backgroundResolution) background
    
    (firstLines,restLines) = splitAt (snd position) backgroundLines
    (secondLines,thirdLines) = splitAt (snd foregroundResolution) restLines
    
    foregroundLines =
      [
        take (fst position) (snd item) ++
          [
            if (fst chars) == transparentChar then (snd chars) else (fst chars)
            | chars  <- zip (fst item) ( take (fst foregroundResolution)  (drop (fst position) (snd item)))
          ] ++
        drop (fst position + fst foregroundResolution) (snd item)
        | item <- zip (splitChunks (fst foregroundResolution) foreground) secondLines
      ]
  in   
    concat (firstLines) ++
    concat (foregroundLines) ++
    concat (thirdLines)

-----------------------------------------------

weaponFireRate :: Weapon -> Int
weaponFireRate weaponId
  | weaponId == Knife = fireRateKnife
  | weaponId == Gun   = fireRateGun
  | weaponId == Uzi   = fireRateUzi
  | otherwise         = 1
 
-----------------------------------------------

weaponSprite :: Weapon -> Int
weaponSprite weaponId
  | weaponId == Knife = spriteFPKnife
  | weaponId == Gun   = spriteFPGun
  | weaponId == Uzi   = spriteFPUzi
  | otherwise         = spriteFPKnife
  
-----------------------------------------------   Renders the game in 3D.

renderGameState :: GameState -> String
renderGameState gameState =
  let
    wallDrawInfo = castRays gameState
    gunSprite = weaponSprite (currentWeapon gameState) +
      if (fireCountdown gameState) /= 0
        then 1
        else 0
  in
    (
      overlay
        (render3Dview wallDrawInfo (projectSprites gameState) (snd viewSize) (frameNumber gameState))
        (concat (spriteList !! gunSprite))
        weaponSpritePosition
        (addPairs viewSize (1,0))
        spriteSize
        transparentChar
    )
    ++
    renderInfoBar gameState 
    
-----------------------------------------------   Renders the game state into string, simple version.

renderMap :: GameState -> String
renderMap gameState =
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
                else if fst square == SquareEmpty
                  then "  "
                  else "[]"
            )
        ) (zip (gameMap gameState) [0..])
    )
    
-----------------------------------------------   Gets the distance from projection origin to projection plane.

distanceToProjectionPlane :: Double -> Double -> Double
distanceToProjectionPlane focalDistance angleFromCenter =
  focalDistance * (cos angleFromCenter)

-----------------------------------------------   Casts all rays needed to render player's view, returns a list of ray cast results.

castRays :: GameState -> [(Double, Normal)]
castRays gameState =
  [
    let
      rayDirection = (playerRot gameState) + fieldOfView / 2 - (fromIntegral x) * rayAngleStep
      rayResult = castRay gameState (playerPos gameState) (floorPair (playerPos gameState)) rayDirection maxRaycastIterations
    in
      (
        max
          ( (fst rayResult) - (distanceToProjectionPlane focalLength (abs $ (playerRot gameState) - rayDirection)) )
          0.0,
        snd rayResult
      )

    | x <- [0..(fst viewSize) - 1]
  ]

-----------------------------------------------   Casts a ray and returns an information (distance, normal) about a wall it hits.

castRay :: GameState -> Position2D -> (Int, Int) -> Double -> Int ->  (Double, Normal)
castRay gameState rayOrigin square rayDirection maxIterations =
  let
    squareCoords = floorPair rayOrigin
    angle = angleTo02Pi rayDirection
  in
    if (mapSquareAt gameState square) /= SquareEmpty || maxIterations == 0
      then (0,NormalNorth)
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
                  (1,0)  -> NormalEast
                  (0,1)  -> NormalSouth
                  (-1,0) -> NormalWest
                  _      -> NormalNorth
          )

-----------------------------------------------   Casts a ray inside a single square, returns (intersection point with square bounds,next square offset)

castRaySquare :: (Int, Int) -> Position2D -> Double -> (Position2D,(Int, Int))
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

-----------------------------------------------   Returns map square at given coords.

mapSquareAt :: GameState -> (Int, Int) -> MapSquare
mapSquareAt gameState coords 
  | (fst coords) < (fst mapSize) && (fst coords) >= 0 && (snd coords) < (snd mapSize) && (snd coords) >= 0 = (gameMap gameState) !! (mapToArrayCoords coords)
  | otherwise = SquareWall

-----------------------------------------------   Checks if given player position is valid (collisions).

positionIsWalkable :: GameState -> Position2D -> Bool
positionIsWalkable gameState position =
  (mapSquareAt gameState (floorPair position)) == SquareEmpty

-----------------------------------------------

monsterSprite :: MonsterType -> Int
monsterSprite monsterId
  | monsterId == Zombie = spriteZombie
  | otherwise                  = spriteDemon
  
-----------------------------------------------

monsterStepLength :: MonsterType -> Double
monsterStepLength monsterId
  | monsterId == Zombie = zombieStepLength
  | otherwise                  = demonStepLength
  
-----------------------------------------------   Creates sprites and places them on the map depending on current state of things.

updateSprites :: GameState -> GameState
updateSprites gameState =
  gameState
    {
      sprites =
        [
          Sprite {spriteType = monsterSprite (monsterType monster), spritePos = (monsterPos monster)}
          | monster <- (monsters gameState)
        ]
    }

-----------------------------------------------   

monsterAI :: GameState -> Monster -> Monster
monsterAI gameState whatMonster =
  let
    rotation =
      if (monsterType whatMonster) == Zombie
        then vectorAngle $ substractPairs (playerPos gameState) (monsterPos whatMonster)  -- zombie walks towards the player
        else
          if (countdownAI whatMonster) == 0
            then angleTo02Pi ((fst (monsterPos whatMonster)) + (snd (monsterPos whatMonster)) + (fromIntegral (frameNumber gameState)) / 100.0)
            else (monsterRot whatMonster)
  in
    whatMonster
      {
        monsterPos = moveWithCollision gameState (monsterPos whatMonster) (monsterRot whatMonster) (monsterStepLength (monsterType whatMonster))
      }
      {
        monsterRot = rotation
      }
      {
        countdownAI = 
          if (countdownAI whatMonster) <= 0
            then recomputeAIin
            else (countdownAI whatMonster) - 1
      }
    
-----------------------------------------------   Runs the AI for each monster, updating their positions etc.

updateMonsters :: GameState -> GameState
updateMonsters gameState =
  gameState
    {
      monsters =
        [
          monsterAI gameState monster
          | monster <- filter (\m -> (health m) > 0) (monsters gameState)  -- health = 0 => monster is dead, filter it out
        ]
    }
    
-----------------------------------------------

moveWithCollision :: GameState -> Position2D -> Double -> Double -> Position2D
moveWithCollision gameState positionFrom angle distance =
  let
    plusX = cos angle * distance
    plusY = -1 * (sin angle * distance)
  in
    (
      (fst positionFrom) + 
      if positionIsWalkable gameState ((fst positionFrom) + plusX,snd positionFrom)
        then plusX
        else 0,
      
      (snd positionFrom) + 
      if positionIsWalkable gameState (fst positionFrom,(snd positionFrom) + plusY)
        then plusY
        else 0
    )
      
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
  previousGameState
    {
      playerPos = moveWithCollision previousGameState (playerPos previousGameState) (playerRot previousGameState) distance
    }

-----------------------------------------------   Strafes the player left by given distance (with collisions).

strafePlayer :: GameState -> Double -> GameState
strafePlayer previousGameState distance =
  previousGameState
    {
      playerPos = moveWithCollision previousGameState (playerPos previousGameState) (angleTo02Pi ((playerRot previousGameState) + pi / 2)) distance
    }

-----------------------------------------------

fire :: GameState -> GameState
fire gameState =
  if (fireCountdown gameState) == 0
    then gameState { fireCountdown = weaponFireRate (currentWeapon gameState) }
    else gameState
  
-----------------------------------------------   Computes the next game state.

nextGameState :: GameState -> Char -> GameState
nextGameState previousGameState inputChar =
  let
    newGameState =
      case () of _ -- case with expressions hack
                   | inputChar == keyForward     -> movePlayerForward previousGameState stepLength
                   | inputChar == keyBackward    -> movePlayerForward previousGameState (-1 * stepLength)
                   | inputChar == keyTurnLeft    -> previousGameState { playerRot = angleTo02Pi ((playerRot previousGameState) + rotationStep) }
                   | inputChar == keyTurnRight   -> previousGameState { playerRot = angleTo02Pi ((playerRot previousGameState) - rotationStep) }
                   | inputChar == keyStrafeLeft  -> strafePlayer previousGameState stepLength
                   | inputChar == keyStrafeRight -> strafePlayer previousGameState (-1 * stepLength)
                   | inputChar == keyFire        -> fire previousGameState
                   | inputChar == keyWeapon1     -> previousGameState { currentWeapon = Knife }
                   | inputChar == keyWeapon2     -> previousGameState { currentWeapon = Gun }
                   | inputChar == keyWeapon3     -> previousGameState { currentWeapon = Uzi }
                   | otherwise                   -> previousGameState
  in
    (
      updateMonsters $ updateSprites newGameState
    )
    {
      frameNumber = (frameNumber newGameState) + 1
    }
    {
      fireCountdown = max ((fireCountdown newGameState) - 1) 0
    }
  
-----------------------------------------------   Reads all available chars on input and returns the last one, or ' ' if not available.
  
getLastChar :: IO Char
getLastChar =
  do
    isInput <- hWaitForInput stdin 1
    
    if isInput
      then do
        c1 <- getChar
        c2 <- getLastChar
             
        if c2 == ' '
          then return c1
          else return c2
               
      else do
        return ' '
    
-----------------------------------------------   Main game loop.

gameLoop :: GameState -> IO ()
gameLoop gameState =
  do
    t1 <- getCPUTime
    
    putStrLn (emptyLineString ++ renderGameState gameState)
    
    c <- getLastChar
    
    t2 <- getCPUTime
    threadDelay (frameDelayUs - ( (fromIntegral (t2 - t1)) `div` 1000000) )       -- wait for the rest of frame time
    
--    t3 <- getCPUTime
--    putStrLn (show (fromIntegral (t3 - t1) / 10e9) ++ " ms")  -- for profiling, comment out otherwise
    
    hFlush stdout
    
    if c == keyQuit
      then do putStrLn "quitting"
      else do gameLoop (nextGameState gameState c)
      
-----------------------------------------------
        
main = 
  do
    hSetBuffering stdin NoBuffering                     -- to read char without [enter]
    hSetBuffering stdout (BlockBuffering (Just 20000))  -- to read flickering
    hSetEcho stdout False                               
    gameLoop initialGameState

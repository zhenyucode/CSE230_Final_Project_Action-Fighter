
module Model where
import Linear.V2 (V2(..))

type Name = ()
data Tick = Tick

-- This section defines the game's core data structures
data Cell = EmptyCell | PlayershipCell | EnemyCell | PlayerShotCell | EnemyShotCell | Road | RoadWithStripe | Curb | Tree
type Coord = V2 Int
type Playership = Coord

-- Data structure for game state, including players, enemies, shots, and level details.
data Game = Game
  { lives0       :: Int
  , lives1       :: Int
  , level       :: Level
  , score       :: Int
  , dead        :: Bool
  , playership0  :: Playership
  , playership1  :: Playership
  , playerShots :: [Coord]
  , enemies     :: Enemies
  , enemiesShots:: [Coord]
  , curstep     :: Int
  } deriving (Show)

-- Level data with number, attack frequency, and shot limit.
data Level = Level
  { levelNumber :: Int
  , attackFrequency :: Int
  , lShots  :: Int
  } deriving (Show)

-- Enum for representing directional movement.
data Direction = L | R | U | D
  deriving (Show, Eq)

-- Initializes game state with default settings.
game ::Int -> Int -> Int -> Level -> Game
game s li0 li1 l@(Level _ af sf) = Game
        { lives0        = li0
        , lives1        = li1
        , level        = l
        , score        = s
        , dead         = False
        , playership0   = V2 (width `div` 2 - 3) 3
        , playership1   = V2 (width `div` 2 + 3) 3
        , playerShots        = []
        , enemies      = initEnemies 5 af L sf
        , enemiesShots = []
        , curstep = 100000000
        }

-- Represents enemy units with their attack pattern and frequency.
data Enemies = Enemies {
    enemyList :: [Enemy]
  , countdown :: Int
  , origPosition :: [Enemy]
  , attackEnemy  :: [Enemy]
  , attackFreq   :: Int
} deriving (Show, Eq)

-- Defines an enemy's position, status, and movement direction.
data Enemy
  = E
    { coord :: Coord
    , edead :: Bool
    , direc :: Direction
    }
  deriving (Show, Eq)

instance Ord Enemy where
  (< ) (E c1 _ _) (E c2 _ _) = c1 <  c2
  (<=) (E c1 _ _) (E c2 _ _) = c1 <= c2
  (> ) (E c1 _ _) (E c2 _ _) = c1 >  c2
  (>=) (E c1 _ _) (E c2 _ _) = c1 >= c2


initEnemies:: Int -> Int -> Direction -> Int -> Enemies
initEnemies n f d sf = Enemies (initEnemyList n d) f [] [] sf
initEnemyList:: Int -> Direction -> [Enemy]
initEnemyList n d = [E (V2 (((width `div` 2) + ((n*2) `div` 2)) - (x*2)) enemyHeight) False d | x <- [1..n]]



setAttackFrequency :: Int -> Int
setAttackFrequency = (100-)

-- Move enemy according to directions
moveEnemy :: Direction -> Enemy -> Enemy
moveEnemy L (E (V2 x y) e _) = E (V2 (x-1) y) e L
moveEnemy R (E (V2 x y) e _) = E (V2 (x+1) y) e R
moveEnemy D (E (V2 x y) e d) = E (V2 x (y-1)) e d
moveEnemy U (E (V2 x y) e d) = E (V2 x (y+1)) e d

enemyCoords:: Game -> [Coord]
enemyCoords (Game _ _ _ _ _ _ _ _ (Enemies el _ _ ae _) _ _) = map coord (el++ae)

updateShots :: [Coord] -> Direction -> [Coord]
updateShots s U = map (\(V2 x y) -> (V2 x (y+1))) s 
updateShots s D = map (\(V2 x y) -> (V2 x (y-1))) s 
updateShots s L = map (\(V2 x y) -> (V2 (x-1) y)) s 
updateShots s R = map (\(V2 x y) -> (V2 (x+1) y)) s 


initGame :: IO Game
initGame = do
            let l = getLevel 0
            return $ game 0 10 10 l

getLevel :: Int -> Level
getLevel n = Level { levelNumber = n
                  , attackFrequency = max 5 (30 - 2 * n) -- Decreases with level but never goes below 5
                  , lShots = 3   
                  }

-- Initialize screen size
height, width :: Int
height = 20
width = 35

-- Initialize enemy position
enemyHeight :: Int
enemyHeight = height - 2


getCoord :: Enemy -> Coord
getCoord (E c _ _) = c

getCoordPlus :: Enemy -> Coord
getCoordPlus (E (V2 x y) _ _) = V2 x (y+1)

getCoordMinus :: Enemy -> Coord
getCoordMinus (E (V2 x y) _ _) = V2 x (y-1)

getEnemyLocList :: Game -> [Coord]
getEnemyLocList (Game _ _ _ _ _ _ _ _(Enemies el _ _ _ _) _ _ ) = map getCoord el

getAllEnemyLocList :: Game -> [Coord]
getAllEnemyLocList (Game _ _ _ _ _ _ _ _ (Enemies _  _ _ ae _) _ _) = (map getCoord ae) ++ (map getCoordPlus ae) ++ (map getCoordMinus ae)
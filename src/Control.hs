module Control where

import Model
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))


move0 :: (Int -> Int) -> (Int -> Int) -> Game -> Game
move0 fx fy g = if dead g || null (enemyList (enemies g)) then g
               else g {playership0 = V2 newX newY}
  where 
    playershipCoord = playership0 g
    initX = fx (playershipCoord ^. _x) `mod` width
    newX = adjustNewX initX
    newY = adjustNewY (fy (playershipCoord ^. _y)) 

move1 :: (Int -> Int) -> (Int -> Int) -> Game -> Game
move1 fx fy g = if dead g || null (enemyList (enemies g)) then g
               else g {playership1 = V2 newX newY}
  where 
    playershipCoord = playership1 g
    initX = fx (playershipCoord ^. _x) `mod` width
    newX = adjustNewX initX
    newY = adjustNewY (fy (playershipCoord ^. _y)) 

adjustNewX :: Int -> Int
adjustNewX newX
  | newX < 11 = 11
  | newX > 24 = 24
  | otherwise = newX

adjustNewY :: Int -> Int
adjustNewY newY
  | newY >= (height - 5) = height - 5
  | newY <= 3 = 3
  | otherwise = newY

-- | Restart game
restart :: Game -> Game
restart _ = game 0 10 10 (getLevel 0)

continue :: Game -> Game
continue g = if null (enemyList (enemies g)) then game (score g) 10 10 (getLevel (levelNumber (level g) + 1))
             else g
  

-- | Add new shot from the motor to the game
shoot0 :: Game -> Game
shoot0 g = if dead g || length s >= lShots l || null (enemyList (enemies g)) then g
  else g {playerShots = n:s, curstep=0}
    where s = playerShots g
          n = fmap (\(V2 x y)  -> V2 x (y + 1)) playership0 g
          l = level g

shoot1 :: Game -> Game
shoot1 g = if dead g || length s >= lShots l || null (enemyList (enemies g)) then g
  else g {playerShots = n:s, curstep=0}
    where s = playerShots g
          n = fmap (\(V2 x y)  -> V2 x (y + 1)) playership1 g
          l = level g

updateScore :: Level -> Int -> Enemies -> Enemies -> Int
updateScore lev score1 olde newe = do
                                let old_el = enemyList olde
                                let old_ae = attackEnemy olde
                                let new_el = enemyList newe
                                let new_ae = attackEnemy newe
                                let sum1 = (length old_el) - (length new_el) + (length old_ae) - (length new_ae)
                                score1 + sum1 * (20 + (10 * (levelNumber lev)) )

updateLives0 :: Game -> Coord -> [Coord] -> [Enemy] -> Int
updateLives0 g player enemyLasers listAttackingEnemies = if (player `elem` enemyLasers) || (player `elem` map coord listAttackingEnemies)
                                                then lives0 g - 1
                                                else lives0 g

updateLives1 :: Game -> Coord -> [Coord] -> [Enemy] -> Int
updateLives1 g player enemyLasers listAttackingEnemies = if (player `elem` enemyLasers) || (player `elem` map coord listAttackingEnemies)
                                                then lives1 g - 1
                                                else lives1 g

-- TODO: 1. Shoot and being shot.
--       2. Attack frequently


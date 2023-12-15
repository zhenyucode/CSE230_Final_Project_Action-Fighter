module Control where

import Model
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))


move0 :: (Int -> Int) -> (Int -> Int) -> Game -> Game
move0 fx fy g = if dead g || ((null (enemyList (enemies g))) && (null (attackEnemy (enemies g))))  then g
               else g {playership0 = V2 newX newY}
  where 
    playershipCoord = playership0 g
    initX = fx (playershipCoord ^. _x) `mod` width
    newX = adjustNewX initX
    newY = adjustNewY (fy (playershipCoord ^. _y)) 

move1 :: (Int -> Int) -> (Int -> Int) -> Game -> Game
move1 fx fy g = if dead g || ((null (enemyList (enemies g))) && (null (attackEnemy (enemies g))))  then g
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
restart _ = game 0 5 5 (getLevel 0)

continue :: Game -> Game
continue g = if ((null (enemyList (enemies g))) && (null (attackEnemy (enemies g))))  then game (score g) (lives0 g) (lives1 g) (getLevel (levelNumber (level g) + 1))
             else g
  

shoot0 :: Game -> Game
shoot0 g = if dead g || length s >= lShots l || ((null (enemyList (enemies g))) && (null (attackEnemy (enemies g))))  then g
  else g {playerShots = n:s, curstep=0}
    where s = playerShots g
          n = fmap (\(V2 x y)  -> V2 x (y + 1)) playership0 g
          l = level g

shoot1 :: Game -> Game
shoot1 g = if dead g || length s >= lShots l || ((null (enemyList (enemies g))) && (null (attackEnemy (enemies g))))  then g
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

getNewEnemyState :: Game -> Enemies
getNewEnemyState (Game _ _ (Level _ af sf) _ _ (V2 px _) _  _ (Enemies el f op ae s) _ _) = if null (el++ae)
                              
                                          then error "No Enemy!"
                                          else do
                                            
                                            let f'  = frequencyCounter f af
                                            let s'  = frequencyCounter s sf
                                            
                                            let op' = getNewEnemyStateMove op (el++op)
                                            let el' = getNewEnemyStateMove el (el++op)
                                            
                                            let es' = pickNewAttacker (Enemies el' f' op' ae s')
                                            let es''= updateAttackMove px es'
                                    
                                            returnFinishedAttack es''

getNewEnemyStateAfterShots :: Enemies -> [Coord] -> Enemies
getNewEnemyStateAfterShots es@(Enemies el _ _ ae sf) shotsNew = if null (el++ae)
                                    then error "No Enemy!"
                                    else do
                                      let el_ = moveAndKillV (enemyList es) shotsNew
                                      let ae_ = moveAndKillV2 (attackEnemy es) shotsNew
                                      let es' = Enemies el_ (countdown es) (origPosition es) ae_ sf
                                      es'


-- attack enemies shot
updateEenemyShot :: Game -> [Coord]
updateEenemyShot (Game _ _ _ _ _ _ _ _ (Enemies _ _ _ ae s) esh _)
  = do
    if s /= 0 then esh
      else esh ++ map getCoordMinus ae


moveAndKillV :: [Enemy] -> [Coord] -> [Enemy]

moveAndKillV a s = [x | x <- a', True /= edead x] 
      where a' = map (\(E coord' edead' dir) -> if coord' `elem` s then (E coord' True dir) else (E coord' edead' dir)) a  -- check for hits

 
getC :: [Coord] -> [Coord]
getC c = c ++ (map(\(V2 x y) -> (V2 x (y+1))) c) ++ (map(\(V2 x y) -> (V2 x (y-1))) c)

moveAndKillV2 :: [Enemy] -> [Coord] -> [Enemy]

moveAndKillV2 a s = [x | x <- a', True /= edead x] 
      where a' = map (\(E coord' edead' dir) -> if coord' `elem` (getC s) then (E coord' True dir) else (E coord' edead' dir)) a  -- check for hits


      -- where a' = map (\(E (V2 x_ y_) edead dir) -> if (V2 x_ (y)) `elem` s then (E (V2 x_ y_) True dir) else (E (V2 x_ y_) edead dir)) a  -- check for hits

-- Pick new attack enemy
pickNewAttacker :: Enemies -> Enemies
pickNewAttacker es@(Enemies el f op ae sf)
  | f /= 0 || null el = es
  | otherwise = do
      let e_ = head el
      let el'= tail el
      let op'= e_:op
      let ae' = e_:ae
      Enemies el' f op' ae' sf

-- Attack enemies move
updateAttackMove :: Int -> Enemies -> Enemies
updateAttackMove px es@(Enemies _ f _ ae sf)
  = do
    
    let idx = returnEnemyAtBottom ae 0 (enemyHeight+1)
    let (Enemies el' _ op' ae' _) = backToOriginalPosition idx es
    -- update movement
    Enemies el' f op' (map (horizontalMove . moveEnemy D) ae') sf
    where
      horizontalMove e@(E (V2 ex ey) _ _) = do  
        if ey-1 > enemyHeight
          then moveEnemy D e
            else if ex < px
              then moveEnemy R e
              else if ex > px
                then moveEnemy L e
                else e

-- Back to original position if out of bounds
backToOriginalPosition :: Int -> Enemies -> Enemies
backToOriginalPosition idx es@(Enemies el f op ae sf)
  = if idx == -1 then es
      else do
        let op_ = op!!idx
        let el' = el ++ [op_]
        let ae' = removeEnemy idx ae
        let op' = removeEnemy idx op
        Enemies el' f op' ae' sf


returnEnemyAtBottom :: [Enemy] -> Int -> Int -> Int
returnEnemyAtBottom [] _ _ = -1
returnEnemyAtBottom ((E (V2 _ y) _ _): as) idx y_val
  = do
    if y == y_val
      then idx
      else returnEnemyAtBottom as (idx+1) y_val

removeEnemy :: Int -> [Enemy] -> [Enemy]
removeEnemy idx el = case splitAt idx el of
    (lhs, _:rhs) -> lhs ++ rhs
    (lhs, [])    -> lhs


returnFinishedAttack :: Enemies -> Enemies
returnFinishedAttack es@(Enemies el f op ae sf)
  = do
    let idx = returnEnemyAtBottom ae 0 1
    if idx == -1
      then es
      else do
        let (E (V2 x _) e d) = ae!!idx
        let ae' = E (V2 x height) e d:ae
        Enemies el f op ae' sf


-- Update attack frequency
frequencyCounter :: Int -> Int -> Int
frequencyCounter currFreq freq
  | currFreq > 0     = currFreq - 1
  | otherwise = freq

getNewEnemyStateMove :: [Enemy] -> [Enemy] -> [Enemy]
getNewEnemyStateMove e1 e2 = do
                    let (E (V2 lx _) _ _) = minimum e2
                    let (E (V2 rx _) _ _) = maximum e2
                    let  E _ _ d          = head e2
                    if lx == 11
                      then mvEnemies R e1
                    else if rx == 24
                      then mvEnemies L e1
                      else mvEnemies d e1
                    where mvEnemies d = map (moveEnemy d)


shotHandler :: Game ->  [Coord] -> [Coord]
shotHandler g s =  do
      let s1 = [x | x <- s, not (x `elem` getEnemyLocList g) && not (x `elem` getAllEnemyLocList g)] -- remove shots that hit
      [(V2 x y) | (V2 x y)  <- s1, y <= height] -- remove shots that are out of screen                                           
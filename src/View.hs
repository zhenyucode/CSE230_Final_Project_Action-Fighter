module View where

import Model
import Brick
  ( Widget,
    Padding(..),
    AttrMap, AttrName, attrName, withAttr, attrMap
    , vBox, hBox, withBorderStyle, str
    , fg
    , emptyWidget, padRight, padTop, padAll
    , hLimit, (<+>), Padding(..), (<=>), (<+>)
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))


drawApp :: Game -> [Widget Name]
drawApp g =
  [ C.center $ padRight (Pad 3) (drawAllElement g) <+> drawStats g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 20
  $ vBox [(drawScore $ score g) g,  padTop (Pad 3) $ drawGameOver (dead g),  padTop (Pad 6) $ drawGameLevelUp(null (enemyList (enemies g)))]

drawScore :: Int -> Game -> Widget Name
drawScore n g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str ("Level " ++ show (levelNumber (level g))))
  $ C.hCenter
  $ padAll 1
  $ (str "Score: " <+> str (show n)) <=> (str "P0 Lives: " <+> str (show li0)) <=> (str "P1 Lives: " <+> str (show li1))
    where li0 = if lives0 g > 0 then lives0 g else 0
          li1 = if lives1 g > 0 then lives1 g else 0

drawGameOver :: Bool -> Widget Name
drawGameOver isDead =
  if isDead
     then withAttr gameOverAttr $ C.hCenter $ str " -- GAME OVER --"
     else emptyWidget

drawGameLevelUp :: Bool -> Widget Name
drawGameLevelUp isDead =
  if isDead
     then withAttr gameOverAttr $ C.hCenter $ str " -- LEVEL UP --" <=> (str "Press Y to continue ")
     else emptyWidget

drawAllElement :: Game -> Widget Name
drawAllElement g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "-- Action Fighter --")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c == playership0 g               = PlayershipCell
      | c == playership1 g               = PlayershipCell
      | c `elem` enemyCoords g          = EnemyCell
      | c `elem` playerShots g          = PlayerShotCell
      | c `elem` enemiesShots g         = EnemyShotCell
      | c `elem` roadCoords roadLeftBoundary              = Curb
      | c `elem` roadCoords roadRightBoundary             = Curb
      | c `elem` treeOddCoords 2            = Tree 
      | c `elem` treeEvenCoords 7            = Tree 
      | c `elem` treeEvenCoords 27           = Tree 
      | c `elem` treeOddCoords 33            = Tree 
      | otherwise                       = EmptyCell

    roadLeftBoundary = 10
    roadRightBoundary = 25

roadCoords:: Int -> [Coord]
roadCoords x = [V2 x y| y<-[1..40]]

treeOddCoords:: Int -> [Coord]
treeOddCoords x = [V2 x y| y<-[1..40], odd y]

treeEvenCoords:: Int -> [Coord]
treeEvenCoords x = [V2 x y| y<-[1..40], even y]


-- TODO： attribute configuration and drawCell function

-- drawCell :: Cell -> Widget Name
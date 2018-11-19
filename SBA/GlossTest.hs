import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

data Game = Menu Int Float
          | Play Motion [PolyRot] [Float]

data PolyRot = PolyRot (Float,Float) Motion [(Float,Float)]

data Motion = PosVel (Float,Float) (Float,Float)

playerSize :: Float
playerSize = 20

playerSpeed :: Float
playerSpeed = 100

sizeX :: Float
sizeX = 700

sizeY :: Float
sizeY = 500

randomVals :: Float -> [Float]
randomVals seed = randomRs (-100,100) (mkStdGen ((fromInteger . toInteger . floor) seed))

motionTick :: Float -> Motion -> Motion
motionTick f (PosVel (x,y) (vx,vy)) = PosVel (x + f*vx, y + f*vy) (vx,vy)

limSize :: Motion -> Motion
limSize (PosVel (x,y) (vx,vy)) | x > sizeX = limSize (PosVel (700,y) (vx,vy))
                               | y > sizeY = limSize (PosVel (x,500) (vx,vy))
                               | otherwise = PosVel (x,y) (vx,vy)


negPlayerSpeed :: Float
negPlayerSpeed = - playerSpeed

main :: IO ()
--main = display (FullScreen) black (Pictures [Color white (Rotate (360) ((Polygon [(-170,-170),(170,-170),(170,170),(-170,170)]))),(game2Pic (Menu 0 0))])
main = play FullScreen black 60 (Menu 0 0) game2Pic eventHandler worldStepper

game2Pic :: Game -> Picture
game2Pic (Menu x _) = Pictures [Color (menuColorByInt x) (Polygon [(-170,-50),(170,-50),(170,50),(-170,50)]),
                              Translate (-140) (-25) (Scale 0.4 0.4  (Text "Play Game")),
                              Color (menuColorByInt (x + 10)) (Line [(-170,-50),(170,-50),(170,50),(-170,50),(-170,-50)])
                             ]
game2Pic (Play m ps (r:rs)) = Pictures ([motionPic2Pic m (Color blue (Circle playerSize))] ++ fmap convPolyRot ps ++ [Color white (Line [(sizeX,sizeY),(-sizeX,sizeY),(-sizeX,-sizeY),(sizeX,-sizeY),(sizeX,sizeY)])])
game2Pic _ = Blank

convPolyRot :: PolyRot -> Picture
convPolyRot (PolyRot (r,_) m ps) = motionPic2Pic m (Color red (Rotate (r) (Polygon ps)))

motionPic2Pic :: Motion -> Picture -> Picture
motionPic2Pic (PosVel (x,y) (_,_)) = Translate x y

menuColorByInt :: Int -> Color
menuColorByInt 0  = makeColor 0.5 0.5 1 1
menuColorByInt 1  = makeColor 1 0.2 0.2 1
menuColorByInt 10 = makeColor 1 1 1 1
menuColorByInt 11 = makeColor 0.8 0.8 1 1
menuColorByInt _  = makeColor 1 1 1 1

eventHandler :: Event -> Game -> Game
eventHandler (EventKey (Char 's') Down (Modifiers Up Up Up) (_,_)) (Menu x s) = Menu (mod (x + 1) 2) 0
eventHandler (EventKey (SpecialKey KeyEnter) Down (Modifiers Up Up Up) (_,_)) (Menu 1 s) = Play (PosVel (0,0) (0,0)) [] (randomVals s)

eventHandler (EventKey (Char 'w') Down (Modifiers Up Up Up) (_,_)) (Play (PosVel (x,y) (vx,0)) ps r) = Play (PosVel (x,y) (vx,playerSpeed) ) ps r
eventHandler (EventKey (Char 'a') Down (Modifiers Up Up Up) (_,_)) (Play (PosVel (x,y) (0,vy)) ps r) = Play (PosVel (x,y) (-playerSpeed,vy)) ps r
eventHandler (EventKey (Char 's') Down (Modifiers Up Up Up) (_,_)) (Play (PosVel (x,y) (vx,0)) ps r) = Play (PosVel (x,y) (vx,-playerSpeed)) ps r
eventHandler (EventKey (Char 'd') Down (Modifiers Up Up Up) (_,_)) (Play (PosVel (x,y) (0,vy)) ps r) = Play (PosVel (x,y) (playerSpeed,vy) ) ps r
eventHandler (EventKey (Char 'w') Up (Modifiers Up Up Up) (_,_)) (Play (PosVel (x,y) (vx,playerSpeed) ) ps r) = Play (PosVel (x,y) (vx,0)) ps r
eventHandler (EventKey (Char 'a') Up (Modifiers Up Up Up) (_,_)) (Play (PosVel (x,y) ((negPlayerSpeed),vy)) ps r) = Play (PosVel (x,y) (0,vy)) ps r
eventHandler (EventKey (Char 's') Up (Modifiers Up Up Up) (_,_)) (Play (PosVel (x,y) (vx,(negPlayerSpeed))) ps r) = Play (PosVel (x,y) (vx,0)) ps r
eventHandler (EventKey (Char 'd') Up (Modifiers Up Up Up) (_,_)) (Play (PosVel (x,y) (playerSpeed,vy) ) ps r) = Play (PosVel (x,y) (0,vy)) ps r

eventHandler (EventKey (Char 'l') Down (Modifiers Up Up Up) (_,_)) (Play p (ps) r) = Play p (fmap (\(PolyRot (f,fs) m qs) -> PolyRot (f,fs - 1) m qs) ps) r
eventHandler (EventKey (Char 'k') Down (Modifiers Up Up Up) (_,_)) (Play p (ps) r) = Play p (fmap (\(PolyRot (f,fs) m qs) -> PolyRot (f,fs + 1) m qs) ps) r

eventHandler (EventKey (Char 'o') Down (Modifiers Up Up Up) (_,_)) (Play p ps r) = Play p ((PolyRot (0,0) (PosVel (100,100) (0,0)) [(0,0),(100,100),(100,-100),(-100,-100),(-100,100),(100,100)]):ps) r

eventHandler _ x = x


worldStepper :: Float -> Game -> Game

worldStepper x (Play p ps (r:rs)) | (collisionDete p ps) = Menu 0 0
                                  | (floor r) == 0 = Play (limSize (motionTick x p)) (fmap (\(PolyRot (f,fv) m fs) -> PolyRot (f + (falsesqrt fv),fv) (motionTick x m) fs) ((rect):(take 200 ps))) rs'
                                  | otherwise = Play (limSize (motionTick x p)) (fmap (\(PolyRot (f,fv) m fs) -> PolyRot (f + (falsesqrt fv),fv) (motionTick x m) fs) ps) ((((sqrt . abs) r) - x):rs) where
                                                                        (rect,rs') = genRect rs
worldStepper x (Play p ps []) = Play (limSize (motionTick x p)) (fmap (\(PolyRot (f,fv) m fs) -> PolyRot (f + (falsesqrt fv),fv) (motionTick x m) fs) ps) []

worldStepper x (Menu y s) = Menu y (s + x*10000)

worldStepper _ x = x


genRect :: [Float] -> (PolyRot,[Float])
genRect (fs) = ((gPR fs1),fs2) where
  (fs1,fs2) = splitAt 7 fs
  gPR :: [Float] -> PolyRot
  gPR (f1:f2:f3:f5:f6:f7:f8:fs) = PolyRot (f1/10,f2/10) (motionCalc (f3 * (sizeX + sizeY + 400) / 50) f5 f6) [(0,0),(f7,f8),(f7,-f8),(-f7,-f8),(-f7,f8),(f7,f8)]
  gPR _ = (PolyRot (0,0) (PosVel (100,100) (0,0)) [(0,0),(100,100),(100,-100),(-100,-100),(-100,100),(100,100)])

motionCalc :: Float -> Float -> Float -> Motion
motionCalc f3 f5 f6 = PosVel (x,y) (upSpeed (a * (abs f5)) (b * (abs f6))) where
  ((x,y),(a,b)) = findCorrect f3
  findCorrect :: Float -> ((Float,Float),(Float,Float))
  findCorrect x | x > (sizeX + 200) = ((sizeX + 200,(x - sizeX - 200 - ((sizeY + 200) / 2))),(-1, f6 / (abs f6)))
                | x >= 0 = ((x - ((sizeX + 200) / 2), -200 - sizeY),(f5/ (abs f5),1))
                | x < (-200 - sizeY) = ((abs x - sizeY - 200 - ((sizeX + 200) / 2),(sizeY + 200)),(f5 / (abs f5),-1))
                | otherwise = ((-200 - sizeX, abs x - ((sizeY + 200) / 2)),(1,f6 / (abs f6)))

upSpeed :: Float -> Float -> (Float,Float)
upSpeed x y | r < 20    = (x * 100 / r, y * 100 / r)
            | otherwise = (x,y) where
              r = sqrt (x^2 + y^2)

falsesqrt :: Float -> Float
falsesqrt x | x < 0 = (-1) * (sqrt (-x))
            | otherwise = sqrt x

collisionDete :: Motion -> [PolyRot] -> Bool
collisionDete (PosVel (x,y) (vx,vy)) ps = fst (foldr f (False,(x,y)) ps) where
    f :: PolyRot -> (Bool,(Float,Float)) -> (Bool,(Float,Float))
    f (PolyRot (r,_) (PosVel xy (_,_)) (point:points)) (b, p) = ((g r xy p (points) || b),p)
    g :: Float -> (Float,Float) -> (Float,Float) -> [(Float,Float)] -> Bool
    g d (x1,y1) (x2,y2) ps = h (r * (cos mRad),r * (sin mRad)) ps where
      x = x2 - x1
      y = y2 - y1
      r = sqrt (x^2 + y^2)
      dRad = (d * pi)/180
      mRad = nRad + ((sign nRad) * (1 - sign x) * pi / 4) - dRad
      nRad = atan (y / x)
      sign :: Float -> Float
      sign x = x / (abs x)
    h :: (Float,Float) -> [(Float,Float)] -> Bool
    h (x,y) ((x1,y1):ps) = (x - playerSize) < (abs x1)
                        && (x + playerSize) > ((-1)*(abs x1))
                        && (y - playerSize) < (abs y1)
                        && (y + playerSize) > ((-1)*(abs y1))

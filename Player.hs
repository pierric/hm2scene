module Player where

import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.OpenGL
import Data.Maybe

import Utils

data Once = Once | Continue deriving (Eq,Show)
data Dir  = Wstraight | Wleft | Wright deriving Show 
data Run  = Wwalk | Wrun deriving Show

data RoleState = Idle      Int Int
               | Walk      Int Dir Run Once
               | TurnLeft  Int Once
               | TurnRight Int Once
                 deriving Show 

data Role      = Role{ _rstate   :: RoleState
                     , _rpending :: Maybe RoleState
                     , _rpos   :: Vector3 GLdouble
                     , _rdir   :: GLdouble
                     } deriving Show 


initPlayer = Role (Idle 0 0) Nothing (Vector3 0 0 0) 0

startWalk dt r (Role (Walk c t _ o) _ p d)    = evolve dt $ Role (Walk c t r o) Nothing p d
startWalk dt r (Role i@(Idle _ _) _ p d)      = evolve dt $ Role i (Just (Walk 0 Wstraight r Continue)) p d
startWalk dt r (Role t@(TurnLeft _ _) _ p d)  = evolve dt $ Role t (Just (Walk 0 Wstraight Wwalk Once)) p d
startWalk dt r (Role t@(TurnRight _ _) _ p d) = evolve dt $ Role t (Just (Walk 0 Wstraight Wwalk Once)) p d

stopWalk  dt (Role w@(Walk _ _ _ _) _ p d)  = evolve dt $ Role w (Just (Idle 0 0)) p d
stopWalk  dt (Role i@(Idle _ _) n p d)      = evolve dt $ Role i (once n) p d
stopWalk  dt (Role t@(TurnLeft _ _) n p d)  = evolve dt $ Role t (once n) p d
stopWalk  dt (Role t@(TurnRight _ _) n p d) = evolve dt $ Role t (once n) p d

turnLeft  dt (Role (Walk c _ t o) n p d)    = evolve dt $ Role (Walk c Wleft t o) n p d
turnLeft  dt (Role i@(Idle _ _) _ p d)      = evolve dt $ Role i (Just (TurnLeft 0 Continue)) p d
turnLeft  dt (Role i@(TurnLeft _ _) _ p d)  = evolve dt $ Role i Nothing p d
turnLeft  dt (Role i@(TurnRight _ _) _ p d) = evolve dt $ Role i (Just (TurnLeft 0 Continue)) p d

turnRight dt (Role (Walk c _ t o) n p d)    = evolve dt $ Role (Walk c Wright t o) n p d
turnRight dt (Role i@(Idle _ _) _ p d)      = evolve dt $ Role i (Just (TurnRight 0 Continue)) p d
turnRight dt (Role i@(TurnLeft _ _) _ p d)  = evolve dt $ Role i (Just (TurnRight 0 Continue)) p d
turnRight dt (Role i@(TurnRight _ _) _ p d) = evolve dt $ Role i Nothing p d
                
turnDone  dt (Role (Walk c _ t _) n p d)    = evolve dt $ Role (Walk c Wstraight t Continue) n p d
turnDone  dt (Role i@(Idle _ _) n p d)      = evolve dt $ Role i (once n) p d
turnDone  dt (Role i@(TurnLeft _ _) _ p d)  = evolve dt $ Role i (Just (Idle 0 0)) p d
turnDone  dt (Role i@(TurnRight _ _) _ p d) = evolve dt $ Role i (Just (Idle 0 0)) p d

once Nothing               = Nothing
once (Just i@(Idle _ _))   = Just i
once (Just (Walk a b t _)) = Just (Walk a b Wwalk Once)
once (Just (TurnLeft a _)) = Just (TurnLeft a Once)
once (Just (TurnRight a _))= Just (TurnRight a Once)

evolve dt (Role (Idle c t) n p d)    = let max = snd (idle !! t)
                                           c'  = c + dt
                                       in  case n of 
                                             Just ns -> Role ns Nothing p d
                                             Nothing -> if c' >= max 
                                                        then Role (fromMaybe (Idle 0 (mod (t+1) 3)) n) Nothing p d
                                                        else Role (Idle c' t) Nothing p d

evolve dt (Role (Walk c t r o) n p d)= let max = snd walk
                                           c' = c + dt
                                           f  = fromIntegral dt / fromIntegral max
                                           d' = case t of Wstraight -> d
                                                          Wleft     -> d - 30*f
                                                          Wright    -> d + 30*f
                                           spd     = case r of 
                                                       Wwalk -> 0.12
                                                       Wrun  -> 0.2
                                           (sx,sz) = (spd * (cos $ d2r d'), spd * (sin $ d2r d'))
                                           p' = case p of Vector3 px py pz -> Vector3 (px+sx) py (pz+sz)
                                       in  if c' < max 
                                           then Role (Walk c' t r o) n p' d'
                                           else case (n,o) of
                                                  (Nothing, Continue) -> Role (Walk c' t r o) n p' d'
                                                  (Nothing, Once)     -> Role (Idle 0 0) Nothing p d
                                                  (Just ns,_)         -> Role ns Nothing p d
evolve dt (Role (TurnLeft c o) n p d)= let max = snd tleft
                                           c'  = c + dt
                                           d'  = d - 30 * (fromIntegral dt / fromIntegral max)
                                       in  if c' < max
                                           then Role (TurnLeft c' o) n p d'
                                           else case (n,o) of
                                                  (Nothing, Continue) -> Role (TurnLeft c' o) n p d'
                                                  (Nothing, Once)     -> Role (Idle 0 0) Nothing p d
                                                  (Just ns, _)        -> Role ns Nothing p d
evolve dt (Role (TurnRight c o) n p d)=let max = snd tright
                                           c' = c + dt
                                           d' = d + 30 * (fromIntegral dt / fromIntegral max)
                                       in  if c' < max
                                           then Role (TurnRight c' o) n p d'
                                           else case (n,o) of
                                                  (Nothing, Continue) -> Role (TurnRight c' o) n p d'
                                                  (Nothing, Once)     -> Role (Idle 0 0) Nothing p d
                                                  (Just ns, _)        -> Role ns Nothing p d

evolvePly dt (Nothing, _) = evolve (floor dt)
evolvePly dt (Just (False, "Up", mod), _) = let run = if Shift `elem` mod
                                                         then Wrun
                                                         else Wwalk
                                               in  startWalk (floor dt) run
evolvePly dt (Just (True,  "Up",   _), _) = stopWalk  (floor dt) 
evolvePly dt (Just (False, "Left", _), _) = turnLeft  (floor dt)
evolvePly dt (Just (True,  "Left", _), _) = turnDone  (floor dt)
evolvePly dt (Just (False, "Right",_), _) = turnRight (floor dt)
evolvePly dt (Just (True,  "Right",_), _) = turnDone  (floor dt)
evolvePly dt _ = id

walk, run, sit, stand2sit, sit2stand, lying, stand2lying, lying2stand, tleft, tright :: (Int,Int)
idle :: [(Int,Int)]
walk = (1, 800)
run  = (2, 667)
idle = [(0,2667),(8,4000),(9,3000)]
sit  = (10, 2000)
stand2sit = (11, 1000)
sit2stand = (12, 1000)
lying     = (13, 2000)
stand2lying = (14, 1333)
lying2stand = (15, 1334)
tleft  = (1,800)
tright = (1,800)

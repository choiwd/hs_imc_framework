{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveAnyClass            #-}

module SubTrees.Commons

where

-- Data types
import Data.Word                    ( Word16, Word8 )
import qualified Data.ByteString as BS

import qualified IMC.Messages as M
import           IMC.Maneuvers      ( makeReference )
import           IMC.Control.BehaviorTree
                                    ( BStatus(..), FStateBT(ActionSF, ConditionSF) )
import IMC.Control.Utils
                                    ( addLatLon,
                                      distance3D_H,
                                      VState(VState, lon, lat),
                                      Vehicle(state, srcID) )

-- Quality of life
import Control.Applicative          ( Alternative((<|>)) )
import Data.List                    ( sortBy )
import Data.Maybe                   ( catMaybes )

-- Yampa/FRP
import FRP.Yampa
                                    ( SF,
                                      DTime,
                                      Time,
                                      dup,
                                      identity,
                                      loopPre,
                                      returnA,
                                      Arrow((***), arr) )

-- Generics and Lens
import GHC.Generics                 ( Generic )
import Control.Lens                 ( view, (??) )
import Control.DeepSeq              ( NFData )

import Debug.Trace                  ( traceShow, traceShowId )

-- ##########################################
-- Data Types
-- ##########################################

data WorldState = WorldState  { myState           :: Vehicle
                              , vehicleList       :: [Vehicle]
                              , globalTime        :: Time
                              , receivedTask      :: Maybe Task
                              , successfulTrans  :: Maybe Word16
                              } deriving (Eq, Show, Generic, NFData)

instance NFData VState
instance NFData Vehicle
instance NFData Task
instance NFData PointWGS84

data TreeState = TreeState  { taskQueue         :: [(Task, Bool)]             -- Identifier and Flag (indicates if the task has already been dealt with/executed)
                            , myTask            :: Maybe (Task, Float)        -- 0~100%
                            , splitTask         :: [(Vehicle, Task, Word16)]  -- List of messages to vehicles with their subtasks.
                                                                              -- Empty   -> indicates its either a member or has no task to send.
                                                                              -- Stores a snapshot of the task division and sticks to it.
                            , didISendARef      :: Bool
                            , transReqCounter   :: Word16
                            } deriving (Eq, Show)

data Task = Task  { sNumber       :: Word16
                  , taskType      :: Word8
                  , identifier    :: Word16
                  , leader        :: Word16
                  , assignedTo    :: Word16
                  , geometry      :: [PointWGS84]
                  } deriving (Eq, Show, Generic)
instance M.IMCSerialize Task

data PointWGS84 = PointWGS84 Double Double Float deriving (Eq, Show, Generic)
instance M.IMCSerialize PointWGS84

type TreeInput  = (WorldState, TreeState)
type TreeOutput = ((BStatus, [M.MessageF]), TreeState)

-- ##########################################
-- Task functions
-- ##########################################

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _) = Nothing

-- | Function to deserialize Task data type, so that we can keep its IMCSerialize instance
-- generically derived.
deserializeTask :: Word16 -> BS.ByteString -> Maybe Task
deserializeTask snumber bs = do
  -- Overly complex looking function, but actually quite simple. 
  -- f returns Maybe Bool that indicates if the bs starts with the sync number, 
  -- given a deserialization method
  -- g deserializes
  let f be_ = (snumber ==) <$> eitherToMaybe (M.deserialize be_ bs :: Either String Word16)
      g be_ = eitherToMaybe (M.deserialize be_ bs :: Either String Task)
  isBE <- f True
  isLE <- f False
  if isBE then g True else if isLE then g False else Nothing

getTask :: Word16 -> SF M.Message (Maybe Task)
getTask snumber = proc msg -> do
  case view #fields msg of
    M.DevDataBinary (M.DevDataBinaryFields x) -> returnA -< deserializeTask snumber x
    _                                         -> returnA -< Nothing

updateTask :: Word16 -> SF (M.Message, Maybe Task) (Maybe Task, Maybe Task)
updateTask sNumber = proc (m, old_t) -> do
  new_t <- getTask sNumber -< m
  returnA -< dup $ new_t <|> old_t

-- | Simply enqueues the task if its not already there.
-- If this vehicle is the leader of the received task or has been explicitly assigned to it.
acceptTask :: FStateBT TreeInput TreeOutput
acceptTask = ActionSF x
  where x = proc (ws, ts) -> do
          case receivedTask ws of
            Just t -> do
              let myTaskQueue = map fst $ taskQueue ts
                  mySRCid = srcID (myState ws)
              if notElem t myTaskQueue && (leader t == mySRCid || assignedTo t == mySRCid)
                then returnA -< ((Success, []), ts {taskQueue = taskQueue ts ++ [(t, False)]})
                else returnA -< ((Success, []), ts)
            _     -> returnA -< ((Success, []), ts)

-- | Fill a default message with its target and a bytestring.
-- Implementation detail: The request id is expected to be randomized when 
-- sent by the output function given to yampa reactimate.
sendBinaryData :: Word16 -> String -> BS.ByteString -> M.MessageF
sendBinaryData req_id target bs = M.TransmissionRequest $ M.TransmissionRequestFields req_id 0 target (10^20) 0 0 (M.DevDataBinary $ M.DevDataBinaryFields bs) "" BS.empty

getTransmissionStatus :: SF M.Message (Maybe Word16)
getTransmissionStatus = proc msg -> do
  case view #fields msg of
    M.TransmissionStatus (M.TransmissionStatusFields req_id status _ _) -> 
        if status == 1     -- SENT status
          then returnA -< Just req_id
          else returnA -< Nothing
    _       -> returnA -< Nothing

removeSuccTransmission :: FStateBT TreeInput TreeOutput
removeSuccTransmission = ActionSF x
  where x = proc (ws, ts) -> do
          case successfulTrans ws of
            Just req_id -> do
                  let newList = filter f (splitTask ts)
                      f (_, _, _id ) = _id /= req_id
                  returnA -< ((Success, []), ts {splitTask = newList})
            _ ->  returnA -< ((Success, []), ts)

-- ##########################################
-- Distance and positioning functions
-- ##########################################

-- | Default function to calculate distance. Can be changed to other approximations
-- found in the main IMC module. For some yet unknown reason, using distance3D_D does
-- not work. (It probably breaks the interpolation)
distance :: Double -> Double -> Double -> Double -> Double -> Double -> Double
distance = distance3D_H

-- | Adds distance to a WGS84 coordinate
addDistance :: PointWGS84 -> Double -> Double -> PointWGS84
addDistance (PointWGS84 lat lon depth) distX distY = PointWGS84 new_lat new_lon depth
  where (new_lat, new_lon) = addLatLon lat lon distX distY

lineInterp :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> (Double, Double, Double)
lineInterp d x y z x' y' z' = (f x x', f y y', f z z')
  where d' = distance x y z x' y' z'
        f a a' = (a' - a) * (d' - d)/d' + a

-- | Calculates the point at distance d, given the controlled vehicle and the target
pointAtDist :: VState -> Double -> VState -> (Double, Double, Double)
pointAtDist v1 dist v2 = lineInterp dist lat1 lon1 (realToFrac dep1) lat2 lon2 (realToFrac dep1)
  where VState lat1 lon1 dep1 _ _ _ _ = v1
        VState lat2 lon2 dep2 _ _ _ _ = v2

-- | Calculates the distance between two VStates
vstateDistance :: VState -> VState -> Double
vstateDistance v1 v2 = distance lat1 lon1 (realToFrac h1) lat2 lon2  (realToFrac h2)
  where VState lat1 lon1 h1 _ _ _ _ = v1
        VState lat2 lon2 h2 _ _ _ _ = v2

-- | Calculates the distance between two Vehicle
vehicleDistance :: Vehicle -> Vehicle -> Double
vehicleDistance v1 v2 = vstateDistance (state v1) (state v2)

getClosestVehicle :: Vehicle -> [Vehicle] -> Maybe Vehicle
getClosestVehicle reference vlist =
  let l = sortBy f [(ds, v) | v <- vlist, let ds = vstateDistance (state reference) $ state v]
      f (a,_) (b,_) = compare a b
  in case l of
    (a, b):xs -> Just b
    [] -> Nothing

-- | Make a Reference Message at distance d from target vehicle.
-- That is, it makes vehicle 1 go in range of vehicle 2
mkRefAtDistance :: Double -> Vehicle -> Vehicle -> [M.MessageF]
mkRefAtDistance d v1 v2 = ref
  where myState = state v1
        targetState = state v2

        point = pointAtDist myState d targetState
        ref   = [(\ (x, y, _) -> makeReference x y 0) point]

-- ##########################################
-- Behavior tree miscellaneous
-- ##########################################

mkConditionSF :: ((WorldState, TreeState) -> Bool) -> FStateBT TreeInput TreeOutput
mkConditionSF f = ConditionSF c
  where c = proc (exs, ts) -> do
          if f (exs, ts)
            then
              returnA -< ((Success, mempty), ts)
            else
              returnA -< ((Failure, mempty), ts)

-- | Records in the TreeState that this iteration is sending a Reference
recordRef :: TreeOutput -> TreeOutput
recordRef ((btstatus, msglist), ts) = 
  let isRef (M.Reference _) = True
      isRef _               = False
  in if any isRef msglist
      then ((btstatus, msglist), ts {didISendARef = True})
      else ((btstatus, msglist), ts)

-- | Simply sends a reference with its own position every x time.
-- Can be improved, though: If it is underwater, send a point slightly forward, 
-- so that we don't trigger a loiter maneuver. If it is close to the surface,
-- send its own position
keepFollowRefManeuverAlive :: DTime -> FStateBT TreeInput TreeOutput
keepFollowRefManeuverAlive timeout = ActionSF $ loopPre 0 a
  where a = proc ((worldState, ts), lastSentRef) -> do
          let now = globalTime worldState
              _myState = state $ myState worldState
              ref = [makeReference (lat _myState) (lon _myState) 0] -- depth myState)
              new_ts = ts {didISendARef = False} -- Always clear the flag
          if didISendARef ts -- If this iteration output already contains a Reference
            then returnA -< (((Success, []), new_ts), now)
            else 
              if now - lastSentRef > timeout
                then returnA -< (((Running, ref), new_ts), now) --send a ref with my position
                else returnA -< (((Success, []), new_ts), lastSentRef) --Do nothing

printStates :: FStateBT TreeInput TreeOutput
printStates = ActionSF x
  where x = proc (ws, ts) -> do
          returnA -< traceShow (ws, ts) ((Success, []), ts)

-- ##########################################
-- Arrows/Yampa miscellaneous
-- ##########################################

traceA :: Show a => SF a a
traceA = arr traceShowId

-- | Run a specialized arrow every t. Returns the given BStatus when
-- arrow does not run. Use it very carefully, because it brings many potential 
-- problems: 
-- 1. The wrapped node may be skipped, when it needs to update an internal state
-- 2. It may change the semantics of the tree, forcing a BState that does not make
-- sense. For example, outputing Success while the wrapped node is actually Running.
runEveryT :: Time -> BStatus -> SF TreeInput TreeOutput -> SF TreeInput TreeOutput
runEveryT rate defaultState sf = loopPre 0 aux
  where aux = proc (input, lastExec) -> do
          let now = globalTime $ fst input
          if now - lastExec > rate
            then sf *** identity -< (input, now)
            else returnA -< (((defaultState, []), snd input), lastExec)

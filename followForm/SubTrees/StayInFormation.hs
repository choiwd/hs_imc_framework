module SubTrees.StayInFormation(
    stayInFormation
)
where

-- Quality of life
import           Data.Maybe         ( maybeToList )

-- Generics and Lens
import           GHC.Generics       ( Generic )
import           Control.Lens       ( view, (??) )

-- Yampa/FRP
import           FRP.Yampa          ( loopPre, returnA )

import qualified IMC.Messages as M
import           IMC.Control.BehaviorTree
                                    ( BStatus(Success, Running, Failure),
                                      FStateBT(SequenceSF, ActionSF) )

import SubTrees.Commons
                                    ( TreeOutput,
                                      TreeInput,
                                      WorldState(vehicleList, myState),
                                      vehicleDistance,
                                      getClosestVehicle,
                                      mkRefAtDistance )

stayClose :: Double -> Double -> Double -> FStateBT TreeInput TreeOutput
stayClose max_dist target_d overshoot = ActionSF $ loopPre Success a
  where a = proc ((worldState, ts), prevNodeState) -> do
          let me = myState worldState
              closestVehicle = getClosestVehicle me (vehicleList worldState)
              distance = vehicleDistance me <$> closestVehicle

              ref  = concat . maybeToList $ mkRefAtDistance (target_d - overshoot) me <$> closestVehicle
          case prevNodeState of
            Success ->
                case distance of
                  Just d -> if d > max_dist
                    then returnA -< (((Running, ref), ts), Running)
                    else returnA -< (((Success, []), ts), Success)
                  _ -> returnA -< (((Success, []), ts), Success)
            Running -> do
                case distance of
                  Just d -> if d > target_d
                    then returnA -< (((Running, ref), ts), Running)
                    else returnA -< (((Success, []), ts), Success)
                  _ -> returnA -< (((Success, []), ts), Success)
            _ -> returnA -< (((Failure, []), ts), Failure) -- This should be impossible and never happen

dontCollide :: Double -> Double -> Double -> FStateBT TreeInput TreeOutput
dontCollide min_dist target_d overshoot = ActionSF $ loopPre Success a
  where a = proc ((worldState, ts), prevNodeState) -> do
          let me = myState worldState
              closestVehicle = getClosestVehicle me (vehicleList worldState)
              distance = vehicleDistance me <$> closestVehicle

              ref  = concat . maybeToList $ mkRefAtDistance (target_d + overshoot) me <$> closestVehicle
          case prevNodeState of
            Success ->
                case distance of
                  Just d -> if d < min_dist
                    then returnA -< (((Running, ref), ts), Running)
                    else returnA -< (((Success, []), ts), Success)
                  _ -> returnA -< (((Success, []), ts), Success)
            Running -> do
                case distance of
                  Just d -> if d < target_d
                    then returnA -< (((Running, ref), ts), Running)
                    else returnA -< (((Success, []), ts), Success)
                  _ -> returnA -< (((Success, []), ts), Success)
            _ -> returnA -< (((Failure, []), ts), Failure) -- This should be impossible

stayInFormation :: FStateBT TreeInput TreeOutput
stayInFormation = SequenceSF [
    dontCollide 100 150 15
  , stayClose 2000 1500 15
  ]
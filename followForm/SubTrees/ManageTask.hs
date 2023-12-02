module SubTrees.ManageTask (
    manageTask
  , splitGeom
)
where

import qualified IMC.Messages as M
import           IMC.Control.BehaviorTree
                                          ( BStatus(Failure, Running, Success), FStateBT(..) )
import           IMC.Control.Utils        ( Vehicle(srcID, name) )

import FRP.Yampa                          ( returnA )

import SubTrees.Commons
                                          ( TreeOutput,
                                            TreeInput,
                                            PointWGS84(..),
                                            Task(assignedTo, leader, taskType, geometry),
                                            TreeState(taskQueue, transReqCounter, splitTask, myTask),
                                            WorldState(myState, vehicleList),
                                            sendBinaryData,
                                            vehicleDistance,
                                            mkConditionSF,
                                            runEveryT )
import Data.List                            ( find, partition )

-- | Returns 2 vertices corresponding to the diagonal of the 
-- minimum bounding rectangle
boundingRectangle :: [PointWGS84] -> (PointWGS84, PointWGS84)
boundingRectangle l = (max_NW, min_SE)
  where max_NW = PointWGS84 (maximum (map (\(PointWGS84 x _ _)-> x) l))
                              (maximum (map (\(PointWGS84 _ x _)-> x) l)) 0
        min_SE = PointWGS84 (minimum (map (\(PointWGS84 x _ _)-> x) l))
                              (minimum (map (\(PointWGS84 _ x _)-> x) l)) 0

-- | Split the given line that represents the diagonal of a rectangle 
-- and zips it the list of vehicles
-- TODO: match each vehicle to the closest area
splitGeom :: Float -> [vehicle]-> (PointWGS84, PointWGS84) -> [(vehicle, (PointWGS84, PointWGS84))]
splitGeom depth vehicles (PointWGS84 lat1 lon1 _ , PointWGS84 lat2 lon2 _ ) =
  let lat_length = (lat2 - lat1)
      lon_length = (lon2 - lon1)
      n_vehicles = length vehicles
  in if abs lat_length > abs lon_length
      then
        let step = lat_length/realToFrac n_vehicles
            subRectangles = [
              (PointWGS84 (lat1 + step * i) lon1 depth ,
               PointWGS84 (lat1 + step * (i + 1)) lon2 depth)
              | i <- map realToFrac [0..n_vehicles - 1]]
        in zip vehicles subRectangles
      else
        let step = lon_length/realToFrac n_vehicles
            subRectangles = [
              (PointWGS84 lat1 (lon1 + step * i) depth,
               PointWGS84 lat2 (lon1 + step * (i + 1)) depth)
              | i <- map realToFrac [0..n_vehicles - 1]]
        in zip vehicles subRectangles

hasTaskToDistribute :: FStateBT TreeInput TreeOutput
hasTaskToDistribute = mkConditionSF f
  where f (_, ts) = not (null $ splitTask ts)

popTaskToDistribute :: Double -> FStateBT TreeInput TreeOutput
popTaskToDistribute range = ActionSF a
  where a = proc (ws, ts) -> do
          let me = srcID (myState ws)
              (taskToBeDistributed, pending) = partition predicate (taskQueue ts)
              predicate (task, hasBeenDealtWith) =
                        assignedTo task == 0xFFFF && leader task == me && not hasBeenDealtWith
          case taskToBeDistributed of
            (parentTask, _):_ -> do
              case taskType parentTask of
                1 -> do
                  let counterStart = transReqCounter ts
                      vehiclesInRange = myState ws : [ v | v <- vehicleList ws, let d = vehicleDistance (myState ws) v, d < range]
                      l = splitGeom 0 vehiclesInRange
                                            ((\[x, y] -> (x, y)) $ geometry parentTask) -- quite unsafe step
                      vehicleTaskList = 
                              [ (v, parentTask {geometry = [p1, p2], assignedTo = srcID v}, req_id) 
                              | (req_id, (v, (p1, p2))) <- zip [counterStart..] l]

                      newCounter = counterStart + fromIntegral (length vehicleTaskList)
                  returnA -< ((Success, []), ts {taskQueue = pending ++ [(parentTask, True)], splitTask = vehicleTaskList, transReqCounter = newCounter})
            [] -> returnA -< ((Failure, []), ts)   -- No such task

sendSubtasks :: FStateBT TreeInput TreeOutput
sendSubtasks = ActionSF (runEveryT 5 Success x)
  where x = proc (_, ts) -> do
          let _splitTask = splitTask ts
              msgList = [ sendBinaryData req_id (name v) (M.serialize False t) 
                        | (v, t, req_id) <- _splitTask]
          if null _splitTask
            then returnA -< ((Failure, []), ts)
            else returnA -< ((Running, msgList), ts)

-- |Check if has task and if it has been completed
hasTaskToPerform :: FStateBT TreeInput TreeOutput
hasTaskToPerform = ConditionSF x
  where x = proc (_, ts) -> do
          case myTask ts of
            Just _ -> returnA -< ((Success, []), ts)    -- Task in progress
            Nothing -> returnA -< ((Failure, []), ts)   -- No task

popTaskToPerform :: FStateBT TreeInput TreeOutput
popTaskToPerform = ActionSF x
  where x = proc (ws, ts) -> do
          let (taskTobeDone, pending) = partition predicate (taskQueue ts)
              predicate (task, hasBeenDealtWith) =
                        assignedTo task == srcID (myState ws) && not hasBeenDealtWith
          case taskTobeDone of
            (task, _):_ -> do
              let myNewTask = Just (task, 0)
                  newTaskQueue = pending ++ [(task, True)]
              returnA -< ((Success, []), ts {myTask = myNewTask, taskQueue = newTaskQueue}) -- Task in progress
            [] -> returnA -< ((Failure, []), ts)   -- No task

manageTask :: FStateBT TreeInput TreeOutput
manageTask = FallbackSF [
    SequenceSF [
            FallbackSF [
                hasTaskToDistribute
              , popTaskToDistribute 1000
          ]
          , sendSubtasks
          , FallbackSF [
                  hasTaskToPerform
                , popTaskToPerform
          ]
    ]
  , FallbackSF [
          hasTaskToPerform
        , popTaskToPerform
  ]
  ]
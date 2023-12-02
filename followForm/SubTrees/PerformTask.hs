module SubTrees.PerformTask(
    performTask
  , removeTask
)
where

import FRP.Yampa                            ( loopPre, returnA )

import qualified IMC.Messages as M
import           IMC.Control.BehaviorTree
                                            ( BStatus(Success, Failure, Running)
                                            , FStateBT(ActionSF) )
import           IMC.Control.Utils          ( VState(VState), Vehicle(Vehicle) )
import           IMC.Maneuvers              ( makeReference )

import           SubTrees.Commons
                                            ( TreeOutput,
                                              TreeInput,
                                              PointWGS84(..),
                                              Task(geometry),
                                              TreeState(myTask),
                                              WorldState(myState),
                                              distance,
                                              addDistance )
import           Debug.Trace                ( )

referenceFromWGS84 :: PointWGS84 -> M.MessageF
referenceFromWGS84 (PointWGS84 x y z) = makeReference x y z

distanceVehicleWGS84 :: Vehicle -> PointWGS84 -> Double
distanceVehicleWGS84 (Vehicle _ _ vstate) (PointWGS84 x y z) = distance lat lon (realToFrac d) x y (realToFrac z)
  where VState lat lon d _ _ _ _ = vstate

-- | Receives as parameters a step and 2 points that represent the diagonal of a rectangle
-- and returns a list of points that describe an S shaped curve path aligned to the 
-- longest axis that covers the area of the rectangle. The step represents the 
-- distance between each bend of the s shaped curve. All points share at least one
-- coordinate with the vertices of the rectangle.
rowsCover :: Float -> Double -> (PointWGS84, PointWGS84) -> [PointWGS84]
rowsCover depth step (PointWGS84 x1 y1 _, PointWGS84 x2 y2 _) =
  let isWide = abs (x2 - x1) > abs (y2 - y1)
      minX = min x1 x2
      minY = min y1 y2
      maxX = max x1 x2
      maxY = max y1 y2
      PointWGS84 extraMaxX extraMaxY _ = addDistance (PointWGS84 maxX maxY 0) step step -- Guarantee that the whole area will be covered
      bendLines index =
        let inc = (step * realToFrac index)
            p1 = if isWide then addDistance  (PointWGS84 minX minY 0) inc 0
                            else addDistance (PointWGS84 minX minY 0) 0 inc    -- p1 and p2 are points that have the same coordinate along the longestAxis
            p2 = if isWide then addDistance  (PointWGS84 maxX minY 0) inc 0
                            else addDistance (PointWGS84 minX maxY 0) 0 inc
        in if even index then [p1, p2] else [p2, p1]
  in if isWide
    then takeWhile (\(PointWGS84 _ y _) -> y < extraMaxY) $ concatMap bendLines [0..]
    else takeWhile (\(PointWGS84 x _ _) -> x < extraMaxX) $ concatMap bendLines [0..]

removeTask :: FStateBT TreeInput TreeOutput
removeTask = ActionSF x
  where x = proc (ws, ts) -> do
              case myTask ts of
                Just (_, progress) -> 
                  if progress >= 100
                    then returnA -< ((Success, []), ts {myTask = Nothing} )   -- Task completed
                    else returnA -< ((Failure, []), ts)                       -- Task not completed
                _ -> returnA -< ((Success, []), ts)                           -- No task to remove

-- | Depth step and tolerance in meters
performTask :: Float -> Double -> Double -> FStateBT TreeInput TreeOutput
performTask depth step tolerance = ActionSF (loopPre [] x)
  where x = proc ((ws, ts), remainingPoints) -> do
          case myTask ts of
            Just (task, progress) -> do
              let fullTask = rowsCover depth step ((\[x, y] -> (x, y)) $ geometry task)
              if progress == 0    -- Does not have the 1st point
                then do
                  let ref = referenceFromWGS84 $ head fullTask
                  returnA -< (((Running, [ref]), ts {myTask = Just (task, 0.1)}), fullTask)
                else
                  case remainingPoints of
                    target:_ -> do
                      let ref = referenceFromWGS84 target
                          l_fullTask = realToFrac $ length fullTask
                          l_remaining = realToFrac $ length remainingPoints
                          completion = max progress (100 * (l_fullTask - l_remaining) / l_fullTask)
                      if distanceVehicleWGS84 (myState ws) target < tolerance                             -- Started searching, go to next points
                        then returnA -< (((Running, [ref]), ts {myTask = Just (task, completion)}), tail remainingPoints) -- Got point, pop it
                        else returnA -< (((Running, [ref]), ts), remainingPoints)                           -- Keep going
                    []        -> returnA -< (((Success, []), ts {myTask = Just (task, 101)}), [])
            
            
            _ -> returnA -< (((Success, []), ts), []) 
            -- Should it Succeed? Right now it succeeds, because otherwise the tree cant
            -- go further
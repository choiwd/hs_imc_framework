module FollowFormationTree(
    followFormation
)

where

-- Data types
import           Data.Word                ( Word16, Word8 )
import qualified Data.ByteString as BS

-- Quality of life      
import Data.Maybe                         ( maybeToList, listToMaybe, fromMaybe )
import Data.List                          ( partition )

-- Generics and Lens      
import GHC.Generics                       ( Generic )
import Control.Lens                       ( view, (??) )

-- Yampa/FRP
import FRP.Yampa                          ( SF, Time, loopPre, localTime, returnA, (<<<), Arrow(arr) )

import qualified IMC.Messages as M
import           IMC.Control.BehaviorTree
                                          ( runFSBT, BStatus(Running), FStateBT(SequenceSF) )
import           IMC.Control.Utils        ( updateVehicleList, Vehicle(srcID) )
import           IMC.Network              ( IMCnetConf(dst) )

import           SubTrees.Commons         ( TreeOutput,
                                            TreeInput,
                                            TreeState(TreeState),
                                            WorldState(WorldState),
                                            getTask,
                                            acceptTask,
                                            getTransmissionStatus,
                                            removeSuccTransmission,
                                            recordRef,
                                            keepFollowRefManeuverAlive )
import           SubTrees.ManageTask      ( manageTask )
import           SubTrees.PerformTask     ( removeTask, performTask )
import           SubTrees.StayInFormation ( stayInFormation )

taskSyncNumber :: Word16
taskSyncNumber = 10 -- hardcoded 2/2

mainTree :: FStateBT TreeInput TreeOutput
mainTree = SequenceSF [
    acceptTask
  , removeSuccTransmission
  --, printStates
  , recordRef <$> stayInFormation
  , manageTask
  , SequenceSF  
      [
        recordRef <$> performTask 0 50 10
      , removeTask
      ]
  , keepFollowRefManeuverAlive 10
  ]

initialTS :: TreeState
initialTS = TreeState [] Nothing [] False 1

filterRefs :: Time -> SF (([M.MessageF], Time), Time) ([M.MessageF], Time)
filterRefs rate = proc ((msgList, now), lastRef) -> do
  case msgList of
    [M.Reference _] -> 
      if now - lastRef > rate
        then returnA -< (msgList, now)
        else returnA -< ([], lastRef)
    _               -> returnA -< (msgList, lastRef)

followFormation :: Vehicle -> IMCnetConf -> SF M.Message [M.MessageF]
followFormation myInitialState imcNet = proc msg -> do

    (myState, others) <- arr (partition (\x -> srcID x == dst imcNet)) 
                                              <<< loopPre [] updateVehicleList
                                                     -< msg
    newTask <- getTask taskSyncNumber -< msg
    now <- localTime -< ()

    successTransmission <- getTransmissionStatus -< msg
    -- Currently, transmission requests tries to send via WIFI. It would be
    -- worth considering sending via "ANY".
    
    let ws = WorldState (fromMaybe myInitialState (listToMaybe myState)) others now newTask successTransmission
    
    (status, msgList) <- loopPre initialTS (runFSBT mainTree) -< ws
    filteredMsgList <- loopPre 0 (filterRefs 2) -< (msgList, now)
    
    case status of 
      Running -> returnA -< filteredMsgList
      _       -> returnA -< []
module IMC.Maneuvers (
    launchManeuver
  
  , module FollowRef

) where

import FRP.Yampa

import qualified IMC.Messages as M
import qualified IMC.Network  as IMCnetwork
import           IMC.Maneuvers.FollowRef as FollowRef

import Data.Word ( Word8 )

-- | Launch a maneuver using Yampa
launchManeuver :: IMCnetwork.IMCnetConf -> Word8 -> (IMCnetwork.IMCnet -> IO M.Message) -> (IMCnetwork.IMCnetConf -> SF M.Message [M.MessageF]) -> IO ()
launchManeuver configs mysrc_ent initfunc sf = do
  (input, output, imcnet) <- IMCnetwork.makeEndPoints configs mysrc_ent
  reactimate (initfunc imcnet) input output (sf configs)
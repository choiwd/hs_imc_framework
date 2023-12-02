{-# LANGUAGE OverloadedLabels             #-}

module IMC.Maneuvers.FollowRef (
    initFollowRef
  , makeReference
)
where

import qualified IMC.Network  as IMCnetwork
import qualified IMC.Base     as IMCbase
import qualified IMC.Messages as M

-- Types
import Data.Word                        ( Word16, Word32, Word8 )
import FRP.Yampa                        ( DTime )
import qualified Data.ByteString as B

-- Control & Concurrency
import Control.Monad                    ( unless )
import Control.Lens                     ( view )
import Control.Concurrent               ( writeList2Chan )

-- IO related
import Data.Time.Clock.POSIX            ( getPOSIXTime )

initFollowRef :: IMCnetwork.IMCnet -> IO M.Message
initFollowRef imcNetwork = do
  -- Look for the message that indicates the plan has started
  let --_control_src :: Word16, _control_ent :: Word8, _timeout :: Float, _loiter_radius :: Float, _altitude_interval :: Float
      fr = M.FollowReference $ M.FollowReferenceFields 0xFFFF 0xFF 300 0 0
      -- _type :: Word8, _op :: Word8, _request_id :: Word16, _plan_id :: String, _flags :: Word16, _arg :: Message, _info :: String
      request = M.PlanControl $ M.PlanControlFields 0 0 10 "haskell-imc" 0x0002 fr "haskell-imc plan"
    
      waitConfirmation = do
        msg <- IMCnetwork.recvMessage imcNetwork
        case view #fields msg of
          M.PlanControlState pcsFields -> do
            unless (view #_state pcsFields == 3) waitConfirmation
          _ -> waitConfirmation

      waitValidMessage = IMCnetwork.recvMessage imcNetwork
  
  IMCnetwork.sendMessage imcNetwork request
  waitConfirmation
  waitValidMessage

makeReference :: Double -> Double -> Float -> M.MessageF
makeReference lat lon depth = M.Reference $ M.ReferenceFields 0x07 desired_speed desired_z lat lon 10
  where desired_z = M.DesiredZ $ M.DesiredZFields depth 0x01
        --desired_speed = M.DesiredSpeed $ M.DesiredSpeedFields 1 0
        desired_speed = M.DesiredSpeed $ M.DesiredSpeedFields 70 2 -- dont use it. it may be bugged
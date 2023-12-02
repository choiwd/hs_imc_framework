{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE OverloadedLabels          #-}

module Main (
  main
)
where

-- Yampa
import           FRP.Yampa                    ( (<<<), returnA, loopPre
                                              , Arrow(arr), SF, joinE
                                              , maybeToEvent )

-- Types
import           Data.Word                    ( Word16, Word8 )

-- IMC
import qualified IMC.Messages as M
import qualified IMC.Network  as IMCnetwork
import           IMC.Maneuvers                ( launchManeuver, initFollowRef, makeReference )
import           IMC.Network                  ( readAnnounce )
import IMC.Control.Utils                      ( updateVehicleList,
                                                Vehicle(name, state),
                                                VState(lon, lat),
                                                eventToList,
                                                everyT )

-- Control
import Control.Lens                           ( view, set, (??) )

-- Command line argument parsing
import System.Environment                     ( getArgs )
import System.Console.CmdArgs                 ( Data, Typeable, (&=), cmdArgs, argPos, help
                                              , program, summary, typ, Default(def) )
import Data.List                              ( find )

vehicleToRef :: Vehicle -> M.MessageF
vehicleToRef v = makeReference _lat _lon 0
  where _lat = lat $ state v
        _lon = lon $ state v

followSystem :: String -> IMCnetwork.IMCnetConf -> SF M.Message [M.MessageF]
followSystem target _ = proc msg -> do
  vehicleList <- loopPre [] updateVehicleList -< msg
  targetVehicle <- arr (find (\x -> name x == target)) -< vehicleList
  ref <- arr maybeToEvent <<< arr (fmap vehicleToRef) -< targetVehicle
  tick <- everyT 1 id -< ()
  returnA <<< eventToList -< fst <$> joinE ref tick

data FollowForm = FollowSystem  { ipv4    :: String
                                  , port    :: Int
                                  , mainCPU :: Maybe String
                                  , target  :: Maybe String } deriving (Show, Data, Typeable)

followSysOpts :: FollowForm
followSysOpts = FollowSystem {
    ipv4 = "localhost"  &= help "IPV4 Address of the vehicle" 
                        &= typ "STRING"
  , port = 6006         &= help "Port to connect. Defaults to 6006." 
                        &= typ "INT"
  , mainCPU = def       &= typ "MAINCPU"
                        &= argPos 0
  , target = def        &= typ "TARGET"
                        &= argPos 1
                  -- &= help "Name of the system to be followed" 
                  -- &= typ "STRING"
  } &= help "Perform simple manuevers using IMC messages and Yampa" 
    &= program "FollowSystem" 
    &= summary "FollowSystem v0.0.1.0\nPart of haskell-imc package."

-- An important assumption: We receive messages from multiple vehicles
-- but send to only one (the main CPU). Therefore, the arrow that describes
-- the controller will be 'SF M.Message [M.MessageF]', which express
-- this premise: M.Message contains src src_ent dst dst_ent, but M.MessageF does not.
main :: IO ()
main = do
  opts <- cmdArgs followSysOpts
  print opts
  case sequence [mainCPU opts, target opts] of
    Just [mCPU, t] -> do
      putStrLn "\n>>> Starting Main..."
      putStrLn $ ">>> Connecting to " ++ mCPU ++ "..."
      announce <- IMCnetwork.readAnnounce mCPU (ipv4 opts) (fromIntegral $ port opts)
      putStrLn $ ">>> " ++ mCPU ++ " has been found. Launching maneuver: Follow system: " ++ t ++ "."

      let dst = view #src announce
          configs = IMCnetwork.IMCnetConf
                                {
                                  IMCnetwork.host        = ipv4 opts
                                , IMCnetwork.port        = port opts
                                , IMCnetwork.dst         = dst
                                , IMCnetwork.dst_ent     = 0xFF
                                , IMCnetwork.maxWait     = 1 --s
                                }

      launchManeuver configs 0xFF initFollowRef ( followSystem t )
    _ -> putStrLn "ERROR: Insufficient arguments!\nProvide the name of the system to be controlled and of a target system as arguments for this program"
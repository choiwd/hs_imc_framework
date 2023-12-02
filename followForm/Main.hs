{-# LANGUAGE DeriveDataTypeable        #-}

module Main (
  main
)
where

-- IMC
import qualified IMC.Network  as IMCnetwork
import           IMC.Maneuvers            ( launchManeuver, initFollowRef )
import           IMC.Control.Utils        ( Vehicle(..), VState (..), emptyVState, updateState )
import           FollowFormationTree      ( followFormation )

import Control.Lens                       ( view )

-- Command line argument parsing
import System.Console.CmdArgs             ( Data,
                                            Typeable,
                                            (&=),
                                            cmdArgs,
                                            argPos,
                                            help,
                                            program,
                                            summary,
                                            typ,
                                            Default(def) )

import SendTask                           ( sendTaskD )
import Control.Concurrent                 ( forkFinally )
import SubTrees.Commons                   ( Task(..), PointWGS84(..) )

data FollowForm = FollowForm  { ipv4    :: String
                              , port    :: Int
                              , mainCPU :: Maybe String
                              } deriving (Show, Data, Typeable)

followFormOpts :: FollowForm
followFormOpts = FollowForm {
    ipv4 = "localhost"  &= help "IPV4 Address of the vehicle"
                        &= typ "STRING"
  , port = 6006         &= help "Port to connect. Defaults to 6006."
                        &= typ "INT"
  , mainCPU = def       &= typ "MAINCPU"
                        -- &= help "Controlled UAV"
                        &= argPos 0
  } &= help "Perform simple manuevers using IMC messages and Yampa"
    &= program "FollowForm"
    &= summary "FollowForm v0.0.1.0\nPart of haskell-imc package."

testTask :: Task
testTask = Task taskSyncNumber 1 1 30 0xFFFF geom
  where taskSyncNumber = 10 -- hardcoded 1/2
        rad x = x / 180 * pi
        geom =  [ PointWGS84 (rad 41.18474592) (rad $ -8.7098982) 0
                , PointWGS84 (rad 41.18583192) (rad $ -8.70687803) 0 ]

-- An important assumption: We receive messages from multiple vehicles
-- but send to only one (the main CPU). Therefore, the arrow that describes
-- the controller will be 'SF M.Message [M.MessageF]', which express
-- this premise: M.Message contains src src_ent dst dst_ent, but M.MessageF does not.
main :: IO ()
main = do
  opts <- cmdArgs followFormOpts
  case mainCPU opts of
    Just mCPU -> do
      putStrLn "\n>>> Starting Main..."
      putStrLn $ ">>> Connecting to " ++ mCPU ++ "..."
      announce <- IMCnetwork.readAnnounce mCPU (ipv4 opts) (fromIntegral $ port opts)
      putStrLn $ ">>> " ++ mCPU ++ " has been found. Launching maneuver: Follow Formation"

      let dst = view #src announce
          initialState = Vehicle dst mCPU $ updateState 0 announce emptyVState
          configs = IMCnetwork.IMCnetConf
                            {
                              IMCnetwork.host        = ipv4 opts
                            , IMCnetwork.port        = port opts
                            , IMCnetwork.dst         = dst
                            , IMCnetwork.dst_ent     = 0xFF
                            , IMCnetwork.maxWait     = 1 --s
                            }
      
      putStrLn ">>> Forking a thread to send task..."
      _ <- forkFinally (sendTaskD 15 mCPU configs testTask) (const $ putStrLn ">>> Forked thread has been terminated.")
      
      launchManeuver configs 0xFF initFollowRef (followFormation initialState)
      
    _ -> putStrLn "ERROR: Insufficient arguments!\nProvide the name of the system to be controlled for this program."
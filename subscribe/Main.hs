{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE OverloadedLabels          #-}

module Main (
  main
)
where

-- Types
import Data.Word                    ( Word16, Word8 )

-- IMC
import qualified IMC.Messages as M
import qualified IMC.Network  as IMCnetwork
import           IMC.Maneuvers.FollowRef ( makeReference )

-- Control & Monads
import Control.Lens                 ( view, set )
import qualified Control.Monad.State as S
import           Control.Monad      ( when )
import qualified Control.Monad.ST as ST
import Debug.Trace

import Control.Concurrent           ( readChan, writeChan, Chan )

-- Command line argument parsing
import System.Console.CmdArgs       ( Data, Typeable, (&=), cmdArgs, argPos, help, program, summary,
                                      typ, Default(def) )
import IMC.Control.Subscribe        ( subscribe )
import Data.Maybe (listToMaybe)

followSys :: String -> Chan M.MessageF -> Maybe M.Message -> S.StateT (Maybe (Double, Double)) IO ()
followSys target_sys outputChan msg = do
  state <- S.get
  let sendRef = do 
        case state of
          Just (last_lat, last_lon) -> do
            let msg = makeReference last_lat last_lon 0
            writeChan outputChan msg
          _ -> return ()
  case msg of
    Just (M.Message _ _ _ _ (M.Announce fields)) -> do
      let -- Using Lens and OverloadedLabels to access record fields
          current_lat = view #_lat fields
          current_lon = view #_lon fields
          sysName = view #_sys_name fields
      when (sysName == target_sys) (S.put $ Just (current_lat, current_lon))
      S.lift sendRef
    _ -> S.lift sendRef

startFollowRef :: Chan M.MessageF -> IO ()
startFollowRef chan = do
  -- Look for the message that indicates the plan has started
  let --_control_src :: Word16, _control_ent :: Word8, _timeout :: Float, _loiter_radius :: Float, _altitude_interval :: Float
      fr = M.FollowReference $ M.FollowReferenceFields 0xFFFF 0xFF 300 0 0

      -- _type :: Word8, _op :: Word8, _request_id :: Word16, _plan_id :: String, _flags :: Word16, _arg :: Message, _info :: String
      request = M.PlanControl $ M.PlanControlFields 0 0 10 "haskell-imc" 0x0002 fr "haskell-imc plan"
  writeChan chan request

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
  } &= help "Perform simple manuveurs using IMC messages and a subscriber" 
    &= program "FollowSystem - Subscriber" 
    &= summary "FollowSystem - Subscriber v0.0.1.0\nPart of haskell-imc package."

main :: IO ()
main = do
  opts <- cmdArgs followSysOpts
  print opts
  case sequence [mainCPU opts, target opts] of
    Just [mcpu, _target] -> do
      putStrLn "\n>>> Starting Main..."
      putStrLn $ ">>> Connecting to " ++ mcpu ++ "..."
      announce <- IMCnetwork.readAnnounce mcpu (ipv4 opts) (fromIntegral $ port opts)
      putStrLn $ ">>> " ++ mcpu ++ " has been found. Launching maneuver: Follow system: " ++ _target ++ "."

      let dst = view #src announce
          configs = IMCnetwork.IMCnetConf
                                {
                                  IMCnetwork.host        = ipv4 opts
                                , IMCnetwork.port        = port opts
                                , IMCnetwork.dst         = dst
                                , IMCnetwork.dst_ent     = 0xFF
                                , IMCnetwork.maxWait     = 1 --s
                                }

      subscribe configs startFollowRef [(followSys _target, 5*10^6, Nothing, [151])]
    _ -> putStrLn "ERROR: Insufficent arguments!\nProvide the name of the system to be controlled and of a target system as arguments for this program"
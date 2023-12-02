module SendTask (
    sendTaskD
) where

import qualified IMC.Messages as M
import           IMC.Network as IMCNetwork ( IMCnetConf, initIMCnet, killIMCnet, sendMessage )
import           SubTrees.Commons          ( Task, sendBinaryData )

import           Control.Concurrent        ( threadDelay )

import           Data.Word                 ( Word16 )
import           System.Random.Stateful    ( Uniform(uniformM), globalStdGen ) 

-- Delay should be given in seconds
sendTaskD :: Int -> String -> IMCnetConf -> Task -> IO ()
sendTaskD delay sysName configs task = do
  putStrLn $ ">>> Task ready to be sent after delay of " ++ show delay ++ " seconds..."
  threadDelay (delay * 1000000)
  imcNet <- initIMCnet configs 0xFF

  randomID <- uniformM globalStdGen :: IO Word16
  
  sendMessage imcNet $ sendBinaryData randomID sysName (M.serialize True task)
  putStrLn ">>> Task has been sent."
  threadDelay 3000000 -- Wait 3s to ensure its really sent
  killIMCnet imcNet
  putStrLn ">>> IMC Net has been closed."
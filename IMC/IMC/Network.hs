{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use writeList2Chan"     #-}
{-# LANGUAGE OverloadedLabels             #-}

module IMC.Network(
    sendMessage
  , recvMessage
  , makeEndPoints
  , readAnnounce
  
  , initIMCnet
  , killIMCnet

  , readConn
  , writeConn

  , getMySrc
  
  , IMCnetConf (..)
  , IMCnet (..)
)
where

-- Binary & ByteStrings
import qualified Data.ByteString as BS
import Data.Serialize         ( runGetPartial, Get, Result(..) )

-- IMC messages & Networking
import qualified IMC.Messages as M
import Network.Connection       ( connectionPut, connectionGet, Connection, connectTo,
                                  initConnectionContext, ConnectionParams(..), connectionClose )
import Network.Info             ( getNetworkInterfaces, IPv4(IPv4), NetworkInterface(ipv4) )

-- Types
import Data.Word              ( Word16, Word32, Word8 )
import Data.Bits              ( (.|.), (.&.) )
import FRP.Yampa              ( DTime )

-- Control & Monads & IO related
import Control.Concurrent     ( readChan, writeChan, Chan, newChan, forkIO, killThread, ThreadId )
import Data.Time.Clock.POSIX  ( getPOSIXTime )
import Data.IORef             ( newIORef, readIORef, writeIORef )
import System.Timeout         ( timeout )
import IMC.Messages           ( serialize )
import Control.Lens           ( view, set )

data IMCnet = IMCnet {
    con         :: Connection
  , inputChan   :: Chan M.Message
  , outputChan  :: Chan M.Message
  , config      :: IMCnetConf
  , src         :: Word16
  , src_ent     :: Word8
  , threadId1   :: ThreadId
  , threadId2   :: ThreadId
}

data IMCnetConf = IMCnetConf {
    host        :: String
  , port        :: Int
  , dst         :: Word16
  , dst_ent     :: Word8
  , maxWait     :: Int
} deriving (Show, Eq)

-- | Returns the 1st non-zero, non-loopback IPv4 address as a src number as used in
--   IMC messages
getMySrc :: IO Word32
getMySrc = do
  interfaces <- getNetworkInterfaces
  let myPossibleIPv4 = filter (\x ->  ipv4 x /= IPv4 0 || ipv4 x /= IPv4 0x100007f) interfaces
      IPv4 myIPv4 = ipv4 $ head myPossibleIPv4
  return $ 0x4000 .|. (myIPv4 .&. 0xFFFF)

-- | Read given IPV4 address and returns the sys name and src of the vehicle
-- connected to this address
readAnnounce :: String -> String -> Word16 -> IO M.Message
readAnnounce mainCPU ip port = do
  ctx <- initConnectionContext
  con <- connectTo ctx $ ConnectionParams
                    { connectionHostname  = ip
                    , connectionPort      = fromIntegral port 
                    , connectionUseSecure = Nothing
                    , connectionUseSocks  = Nothing
                    }

  mainInputChan <- newChan
  threadid <- forkIO $ readConn' con mainInputChan

  let waitAnnounce = do 
        msg <- readChan mainInputChan
        let M.Message _ _ _ _ f = msg
        case f of
          -- clean up and return value
          --M.Announce m -> print msg >> putStrLn "" >> waitAnnounce
          M.Announce m -> do
            let sys_name = view #_sys_name m
            if sys_name == mainCPU
              then killThread threadid >> connectionClose con >> return msg
              else waitAnnounce
          _ -> waitAnnounce
  waitAnnounce

-- | Continuosly reads from the connection and writes to the channel.
-- TODO: Deal with end of buffer (when connectionGet returns an empty ByteString)
readConn' :: Connection -> Chan M.Message -> IO ()
readConn' con chan = do
  let buffer_size = 8192
      loop :: Result M.NetworkMessage -> BS.ByteString -> IO ()
      loop decoder _data = do
        case decoder of
          -- Writes the message to the channel, and loops creating a new decoder (which will be Partial) 
          -- feeding it with the unconsumed data
          Done msg leftover -> do
            let M.NetworkMessage (M.Header _ _ _ time src src_ent dst dst_ent) fields _ = msg
                msg' = M.Message src src_ent dst dst_ent fields
            writeChan chan msg'
            loop (runGetPartial (M.getIMC True :: Get M.NetworkMessage) leftover) _data
          -- Gets more data, feeds the decoder with existing data and loops
          Partial k -> do
            more_data <- connectionGet con buffer_size
            loop (k _data) more_data
          -- Failure to decode. 
          Fail error_msg leftover -> do 
            loop (runGetPartial (M.getIMC True :: Get M.NetworkMessage) 
                                              (BS.drop 2 leftover)) _data

  x <- connectionGet con buffer_size
  loop (runGetPartial (M.getIMC True :: Get M.NetworkMessage) x) BS.empty

readConn :: Connection -> Chan M.NetworkMessage -> IO ()
readConn con chan = do
  let buffer_size = 8192
      loop :: Result M.NetworkMessage -> BS.ByteString -> IO ()
      loop decoder _data = do
        case decoder of
          -- Writes the message to the channel, and loops creating a new decoder (which will be Partial) 
          -- feeding it with the unconsumed data
          Done msg leftover -> do
            writeChan chan msg
            loop (runGetPartial (M.getIMC True :: Get M.NetworkMessage) leftover) _data
          -- Gets more data, feeds the decoder with existing data and loops
          Partial k -> do
            more_data <- connectionGet con buffer_size
            loop (k _data) more_data
          -- Failure to decode. 
          Fail error_msg leftover -> do 
            loop (runGetPartial (M.getIMC True :: Get M.NetworkMessage) 
                                              (BS.drop 2 leftover)) _data

  x <- connectionGet con buffer_size
  loop (runGetPartial (M.getIMC True :: Get M.NetworkMessage) x) BS.empty

writeConn :: Word16 -> Word8 -> Word16 -> Word8 -> Connection -> Chan M.MessageF -> IO ()
writeConn src src_ent dst dst_ent con chan = do
  fields <- readChan chan
  time <- realToFrac <$> getPOSIXTime

  let m = M.NetworkMessage (M.Header 0 0 0 time src src_ent dst dst_ent) fields 0
      
  connectionPut con $ serialize False m
  writeConn src src_ent dst dst_ent con chan

writeConn' :: Connection -> Chan M.Message -> IO ()
writeConn' con chan = do
  msg <- readChan chan
  time <- realToFrac <$> getPOSIXTime

  let M.Message src src_ent dst dst_ent fields = msg
      m = M.NetworkMessage (M.Header 0 0 0 time src src_ent dst dst_ent) fields 0
      
  connectionPut con $ serialize False m
  writeConn' con chan

initIMCnet :: IMCnetConf -> Word8 -> IO IMCnet
initIMCnet configs my_src_ent = do
  ctx <- initConnectionContext
  con <- connectTo ctx $ ConnectionParams
                    { connectionHostname  = host configs
                    , connectionPort      = fromIntegral $ port configs
                    , connectionUseSecure = Nothing
                    , connectionUseSocks  = Nothing
                    }

  mainInputChan <- newChan
  t1 <- forkIO $ readConn' con mainInputChan

  mainOutputChan <- newChan
  t2 <- forkIO $ writeConn' con mainOutputChan

  src <- fromIntegral <$> getMySrc

  return $ IMCnet con mainInputChan mainOutputChan configs src my_src_ent t1 t2

killIMCnet :: IMCnet -> IO ()
killIMCnet (IMCnet con inChan outChan _ _ _ t1 t2) = connectionClose con >> killThread t1 >> killThread t2

-- | Creates the input/sensing and output functions as required by Yampa.
-- Also returns the IMCnetwork that will be used so that the user can use
-- it to initialize the reactimation.
makeEndPoints :: IMCnetConf -> Word8 -> IO  (
                                              Bool -> IO (DTime, Maybe M.Message)
                                            , Bool -> [M.MessageF] -> IO Bool
                                            , IMCnet
                                            )
makeEndPoints configs src_ent = do

  imcNetwork <- initIMCnet configs src_ent
  lastIter <- newIORef <$> realToFrac =<< getPOSIXTime

  let _timeout = maxWait configs * 10^6 -- convert to microseconds

      input :: Bool -> IO (DTime, Maybe M.Message)
      input _ = do
        _lastIter <- readIORef lastIter
        --msg <- timeout _timeout $ recvMessage imcNetwork

        let ignorePlanControlState = do 
              msg <- timeout _timeout $ recvMessage imcNetwork
              case msg of
                Just (M.Message _ _ _ _ (M.PlanControlState _)) -> return Nothing
                _ -> return msg
        
        msg <- ignorePlanControlState

        now <- realToFrac <$> getPOSIXTime
        writeIORef lastIter now
        return (now - _lastIter, msg)

      output :: Bool -> [M.MessageF] -> IO Bool
      output _ msglist = do

        mapM_ (sendMessage imcNetwork) msglist
        return False

  return (input, output, imcNetwork)

sendMessage :: IMCnet -> M.MessageF -> IO ()
sendMessage imcnet = writeChan outChan . M.Message src src_ent dst dst_ent
  where IMCnet _ _ outChan conf src src_ent _ _ = imcnet
        IMCnetConf _ _ dst dst_ent _ = conf

recvMessage :: IMCnet -> IO M.Message
recvMessage = recvMessage'

sendMessage' :: IMCnet -> M.Message -> IO ()
sendMessage' = writeChan . outputChan

recvMessage' :: IMCnet -> IO M.Message
recvMessage' = readChan . inputChan
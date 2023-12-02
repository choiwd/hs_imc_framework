module IMC.Control.Subscribe
(
  subscribe
)
where

-- Types
import           Data.Word          ( Word16 )
import qualified Data.Map as Map
import qualified Data.Set as Set

import Network.Connection           ( connectTo,
                                      initConnectionContext,
                                      ConnectionParams(ConnectionParams, connectionUseSocks,
                                                      connectionHostname, connectionPort, connectionUseSecure) )

-- Concurrency
import           Control.Concurrent ( Chan, newChan, readChan, writeChan, forkIO )
import           System.Timeout     ( timeout )

-- Control & Monads
import qualified Control.Monad.State as S
import qualified Control.Monad.ST as ST

import           IMC.Messages as M  ( NetworkMessage(NetworkMessage),
                                      Header(Header),
                                      Message(Message),
                                      MessageF )
import IMC.Network as IMCNetwork    ( IMCnetConf(dst_ent, host, port, dst),
                                      getMySrc,
                                      readConn,
                                      writeConn )

-- | Instead of subscribing a function to a message, we will subscribe a channel
--   to a message and each callback will be executed in a different thread to avoid having to
--   explicitly save, pass and save states of each callback. This also allows us to 
--   easily implement periodic functions.
messageBroker :: Chan M.NetworkMessage -> [(Word16, [Chan M.Message])] -> IO ()
messageBroker chan subs = do
  let subs_dict = Map.fromList subs
      loop = do
        recvMsg <- readChan chan
        let M.NetworkMessage _header msg _ = recvMsg
            M.Header _ recvMsgID _ _ _src _src_ent _dst _dst_ent = _header
            chans = Map.lookup recvMsgID subs_dict
            message = M.Message _src _src_ent _dst _dst_ent msg

        mapM_ (mapM_ $ \c -> writeChan c message) chans
        loop
  loop

-- | This function must 1. Wrap the functions so that they return IO 
--   2. Create a channel for each thread 3. Partial apply them with Connections
--   4. Start threads 5. Return a list of channels
callbackStarter :: (Show state_var) => Chan M.MessageF -> [(Chan M.MessageF -> Maybe input -> S.StateT state_var IO (), Int, state_var)] -> IO [Chan input]
callbackStarter outputChan = mapM starter
  where starter (func, _timeout, initial_state) = do
          _newChan <- newChan
          _ <- forkIO $ stateLooper _newChan _timeout (func outputChan) initial_state
          return _newChan
        
        stateLooper chan _timeout stateM oldState = 
              do
                newData <- timeout _timeout (readChan chan)
                nextState <- S.execStateT (stateM newData) oldState
                stateLooper chan _timeout stateM nextState

subscribe :: (Show state_var) => IMCnetConf -> (Chan M.MessageF -> IO ()) -> [(Chan M.MessageF -> Maybe M.Message -> S.StateT state_var IO (), Int, state_var, [Word16])] -> IO ()
subscribe configs initF callbacks = do
  
  ctx <- initConnectionContext
  con <- connectTo ctx $ ConnectionParams
                    { connectionHostname  = host configs
                    , connectionPort      = fromIntegral $ port configs
                    , connectionUseSecure = Nothing
                    , connectionUseSocks  = Nothing
                    }

  inputChan <- newChan
  t1 <- forkIO $ IMCNetwork.readConn con inputChan
  
  mySrc <- fromIntegral <$> IMCNetwork.getMySrc
  outputChan <- newChan
  t2 <- forkIO $ writeConn mySrc 0xFF (dst configs) (dst_ent configs) con outputChan
  
  initF outputChan
  
  chans <- callbackStarter outputChan [ (f, t, i) | (f, t, i, _) <- callbacks]

  let l = [(_ids, chan) | ((_, _, _, _ids), chan) <- zip callbacks chans]
      all_ids = Set.toList . Set.fromList $ foldMap fst l   -- list of all subscribed ids without duplicates
      pairs = [(i, subs) | i <- all_ids, let _subs = filter (elem i . fst) l, let subs = foldMap (\(_, c) -> [c]) _subs]

  messageBroker inputChan pairs
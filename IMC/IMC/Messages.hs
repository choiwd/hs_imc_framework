{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoFieldSelectors          #-}
{-# LANGUAGE OverloadedLabels          #-}

-- Generic
{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, DefaultSignatures #-}

-- Allow function signatures in instance declaration
{-# LANGUAGE InstanceSigs              #-}

module IMC.Messages where

-- Generic programming and OverloadedLabels support
import GHC.Generics
import Data.Generics.Labels

import Debug.Trace

import Data.Serialize

-- Temporary
import Network.Connection       ( connectionPut, connectionGet, Connection, connectTo,
                                  initConnectionContext, ConnectionParams(..) )
import Control.Concurrent     ( readChan, writeChan, Chan, newChan, forkIO )

-- Types
import           Data.Word              ( Word8, Word16, Word32 )
import           Data.Int               ( Int8, Int16, Int32, Int64 )
import qualified Data.ByteString as BS  ( ByteString, length, empty, splitAt, append, drop, readFile, writeFile, appendFile, pack, null )
import IMC.Base                         ( crc16' )
import Data.Char                        ( ord, chr )
import Control.Monad (when)

-- | #########################################
-- | Generics
-- | #########################################

class IMCSerialize a where
  putIMC :: Bool -> Putter a

  default putIMC :: (Generic a, GIMCSerialize (Rep a)) => Bool -> Putter a
  putIMC be = gputIMC be . from

  getIMC :: Bool -> Get a

  default getIMC :: (Generic a, GIMCSerialize (Rep a)) => Bool -> Get a
  getIMC be = to <$> ggetIMC be

instance IMCSerialize Word8 where
  putIMC _ = putWord8
  getIMC _ = getWord8

instance IMCSerialize Word16 where
  putIMC be = if be
                then putWord16be
                else putWord16le
  getIMC be = if be
                then getWord16be
                else getWord16le

instance IMCSerialize Word32 where
  putIMC be = if be
                then putWord32be
                else putWord32le
  getIMC be = if be
                then getWord32be
                else getWord32le

instance IMCSerialize Int8 where
  putIMC _ = putInt8
  getIMC _ = getInt8

instance IMCSerialize Char where
  putIMC _ = putWord8 . fromIntegral . ord
  getIMC _ = chr . fromIntegral <$> getWord8

instance IMCSerialize Int16 where
  putIMC be = if be
                then putInt16be
                else putInt16le
  getIMC be = if be
                then getInt16be
                else getInt16le

instance IMCSerialize Int32 where
  putIMC be = if be
                then putInt32be
                else putInt32le
  getIMC be = if be
                then getInt32be
                else getInt32le

instance IMCSerialize Int64 where
  putIMC be = if be
                then putInt64be
                else putInt64le
  getIMC be = if be
                then getInt64be
                else getInt64le

instance IMCSerialize Float where
  putIMC be = if be
                then putFloat32be
                else putFloat32le
  getIMC be = if be
                then getFloat32be
                else getFloat32le

instance IMCSerialize Double where
  putIMC be = if be
                then putFloat64be
                else putFloat64le
  getIMC be = if be
                then getFloat64be
                else getFloat64le

instance IMCSerialize BS.ByteString where
  putIMC be bs = do putIMC be (fromIntegral $ BS.length bs :: Word16)
                    putByteString bs
  getIMC be = do
                s <- fromIntegral <$> (getIMC be :: Get Word16)
                getByteString s

instance IMCSerialize a => IMCSerialize [a] where
  putIMC be x = do
              putIMC be (fromIntegral $ length x :: Word16)
              mapM_ (putIMC be) x
  getIMC be = do
                let goNtimes as 0 = return $! reverse as
                    goNtimes as i = do  x <- getIMC be
                                        x `seq` goNtimes (x:as) (i - 1)
                goNtimes [] . fromIntegral =<< (getIMC be :: Get Word16)

class GIMCSerialize f where
  gputIMC :: Bool -> Putter (f a)
  ggetIMC :: Bool -> Get (f a)

-- | Meta-information (constructor names, etc.)
instance GIMCSerialize a => GIMCSerialize (M1 i c a) where
    gputIMC :: GIMCSerialize a => Bool -> Putter (M1 i c a a1)
    gputIMC be = gputIMC be . unM1
    {-# INLINE gputIMC #-}
    ggetIMC :: GIMCSerialize a => Bool -> Get (M1 i c a a1)
    ggetIMC be = M1 <$> ggetIMC be
    {-# INLINE ggetIMC #-}

-- | Constants, additional parameters and recursion of kind *
instance IMCSerialize a => GIMCSerialize (K1 i a) where
    gputIMC :: IMCSerialize a => Bool -> Putter (K1 i a a1)
    gputIMC be = putIMC be . unK1
    {-# INLINE gputIMC #-}
    ggetIMC :: IMCSerialize a => Bool -> Get (K1 i a a1)
    ggetIMC be = K1 <$> getIMC be
    {-# INLINE ggetIMC #-}

-- | Unit: used for constructors without arguments
instance GIMCSerialize U1 where
    gputIMC :: Bool -> Putter (U1 a)
    gputIMC _ _ = pure ()
    {-# INLINE gputIMC #-}
    ggetIMC _   = pure U1
    ggetIMC :: Bool -> Get (U1 a)
    {-# INLINE ggetIMC #-}

-- | Products: encode multiple arguments to constructors
instance (GIMCSerialize a, GIMCSerialize b) => GIMCSerialize (a :*: b) where
    gputIMC :: (GIMCSerialize a, GIMCSerialize b) => Bool -> Putter ((:*:) a b a1)
    gputIMC be (a :*: b) = gputIMC be a *> gputIMC be b
    {-# INLINE gputIMC #-}
    ggetIMC :: (GIMCSerialize a, GIMCSerialize b) => Bool -> Get ((:*:) a b a1)
    ggetIMC be = (:*:) <$> ggetIMC be <*> ggetIMC be
    {-# INLINE ggetIMC #-}

-- We do not need a sum generic instance, since message fields won't have sums... Right? Right???

-- | #########################################
-- | Messages
-- | #########################################

data EntityStateFields = EntityStateFields { _state :: Word8, _flags :: Word8, _description :: String } deriving (Show, Eq, Generic)
instance IMCSerialize EntityStateFields

data QueryEntityStateFields = QueryEntityStateFields {  } deriving (Show, Eq, Generic)
instance IMCSerialize QueryEntityStateFields

data EntityInfoFields = EntityInfoFields { _id :: Word8, _label :: String, _component :: String, _act_time :: Word16, _deact_time :: Word16 } deriving (Show, Eq, Generic)
instance IMCSerialize EntityInfoFields

newtype QueryEntityInfoFields = QueryEntityInfoFields { _id :: Word8 } deriving (Show, Eq, Generic)
instance IMCSerialize QueryEntityInfoFields

data EntityListFields = EntityListFields { _op :: Word8, _list :: String } deriving (Show, Eq, Generic)
instance IMCSerialize EntityListFields

data HeartbeatFields = HeartbeatFields {  } deriving (Show, Eq, Generic)
instance IMCSerialize HeartbeatFields

data AnnounceFields = AnnounceFields { _sys_name :: String, _sys_type :: Word8, _owner :: Word16, _lat :: Double, _lon :: Double, _height :: Float, _services :: String } deriving (Show, Eq, Generic)
instance IMCSerialize AnnounceFields

newtype DevDataBinaryFields = DevDataBinaryFields { _value :: BS.ByteString } deriving (Show, Eq, Generic)
instance IMCSerialize DevDataBinaryFields

data EstimatedStateFields = EstimatedStateFields { _lat :: Double, _lon :: Double, _height :: Float, _x :: Float, _y :: Float, _z :: Float, _phi :: Float, _theta :: Float, _psi :: Float, _u :: Float, _v :: Float, _w :: Float, _vx :: Float, _vy :: Float, _vz :: Float, _p :: Float, _q :: Float, _r :: Float, _depth :: Float, _alt :: Float } deriving (Show, Eq, Generic)
instance IMCSerialize EstimatedStateFields

data DesiredZFields = DesiredZFields { _value :: Float, _z_units :: Word8 } deriving (Show, Eq, Generic)
instance IMCSerialize DesiredZFields

data DesiredSpeedFields = DesiredSpeedFields { _value :: Double, _speed_units :: Word8 } deriving (Show, Eq, Generic)
instance IMCSerialize DesiredSpeedFields

data FollowReferenceFields = FollowReferenceFields { _control_src :: Word16, _control_ent :: Word8, _timeout :: Float, _loiter_radius :: Float, _altitude_interval :: Float } deriving (Show, Eq, Generic)
instance IMCSerialize FollowReferenceFields

data ReferenceFields = ReferenceFields { _flags :: Word8, _speed :: MessageF, _z :: MessageF, _lat :: Double, _lon :: Double, _radius :: Float } deriving (Show, Eq, Generic)
instance IMCSerialize ReferenceFields

data FollowRefStateFields = FollowRefStateFields { _control_src :: Word16, _control_ent :: Word8, _reference :: MessageF, _state :: Word8, _proximity :: Word8 } deriving (Show, Eq, Generic)
instance IMCSerialize FollowRefStateFields

data TransmissionRequestFields = TransmissionRequestFields { _req_id :: Word16, _comm_mean :: Word8, _destination :: String, _deadline :: Double, _range :: Float, _data_mode :: Word8, _msg_data :: MessageF, _txt_data :: String, _raw_data :: BS.ByteString } deriving (Show, Eq, Generic)
instance IMCSerialize TransmissionRequestFields

data TransmissionStatusFields = TransmissionStatusFields { _req_id :: Word16, _status :: Word8, _range :: Float, _info :: String } deriving (Show, Eq, Generic)
instance IMCSerialize TransmissionStatusFields

data PlanControlFields = PlanControlFields { _type :: Word8, _op :: Word8, _request_id :: Word16, _plan_id :: String, _flags :: Word16, _arg :: MessageF, _info :: String } deriving (Show, Eq, Generic)
instance IMCSerialize PlanControlFields

data PlanControlStateFields = PlanControlStateFields { _state :: Word8, _plan_id :: String, _plan_eta :: Int32, _plan_progress :: Float, _man_id :: String, _man_type :: Word16, _man_eta :: Int32, _last_outcome :: Word8 } deriving (Show, Eq, Generic)
instance IMCSerialize PlanControlStateFields

data MessageF = EntityState EntityStateFields
  | QueryEntityState QueryEntityStateFields
  | EntityInfo EntityInfoFields
  | QueryEntityInfo QueryEntityInfoFields
  | EntityList EntityListFields
  | Heartbeat HeartbeatFields
  | Announce AnnounceFields
  | DevDataBinary DevDataBinaryFields
  | EstimatedState EstimatedStateFields
  | DesiredZ DesiredZFields
  | DesiredSpeed DesiredSpeedFields
  | FollowReference FollowReferenceFields
  | Reference ReferenceFields
  | FollowRefState FollowRefStateFields
  | TransmissionRequest TransmissionRequestFields
  | TransmissionStatus TransmissionStatusFields
  | PlanControl PlanControlFields
  | PlanControlState PlanControlStateFields
  | Unparsed Word16 BS.ByteString deriving (Show, Eq, Generic)
instance IMCSerialize MessageF where
  putIMC :: Bool -> Putter MessageF
  putIMC be m = case m of
    EntityState f -> putIMC be (1 :: Word16) >> putIMC be f
    QueryEntityState f -> putIMC be (2 :: Word16) >> putIMC be f
    EntityInfo f -> putIMC be (3 :: Word16) >> putIMC be f
    QueryEntityInfo f -> putIMC be (4 :: Word16) >> putIMC be f
    EntityList f -> putIMC be (5 :: Word16) >> putIMC be f
    Heartbeat f -> putIMC be (150 :: Word16) >> putIMC be f
    Announce f -> putIMC be (151 :: Word16) >> putIMC be f
    DevDataBinary f -> putIMC be (274 :: Word16) >> putIMC be f
    EstimatedState f -> putIMC be (350 :: Word16) >> putIMC be f
    DesiredZ f -> putIMC be (401 :: Word16) >> putIMC be f
    DesiredSpeed f -> putIMC be (402 :: Word16) >> putIMC be f
    FollowReference f -> putIMC be (478 :: Word16) >> putIMC be f
    Reference f -> putIMC be (479 :: Word16) >> putIMC be f
    FollowRefState f -> putIMC be (480 :: Word16) >> putIMC be f
    TransmissionRequest f -> putIMC be (515 :: Word16) >> putIMC be f
    TransmissionStatus f -> putIMC be (516 :: Word16) >> putIMC be f
    PlanControl f -> putIMC be (559 :: Word16) >> putIMC be f
    PlanControlState f -> putIMC be (560 :: Word16) >> putIMC be f
    Unparsed _id f -> putIMC be _id >> putByteString f
  getIMC :: Bool -> Get MessageF
  getIMC be = do
    _id <- getIMC be :: Get Word16
    case _id of
      1 -> EntityState <$> (getIMC be :: Get EntityStateFields)
      2 -> QueryEntityState <$> (getIMC be :: Get QueryEntityStateFields)
      3 -> EntityInfo <$> (getIMC be :: Get EntityInfoFields)
      4 -> QueryEntityInfo <$> (getIMC be :: Get QueryEntityInfoFields)
      5 -> EntityList <$> (getIMC be :: Get EntityListFields)
      150 -> Heartbeat <$> (getIMC be :: Get HeartbeatFields)
      151 -> Announce <$> (getIMC be :: Get AnnounceFields)
      274 -> DevDataBinary <$> (getIMC be :: Get DevDataBinaryFields)
      350 -> EstimatedState <$> (getIMC be :: Get EstimatedStateFields)
      401 -> DesiredZ <$> (getIMC be :: Get DesiredZFields)
      402 -> DesiredSpeed <$> (getIMC be :: Get DesiredSpeedFields)
      478 -> FollowReference <$> (getIMC be :: Get FollowReferenceFields)
      479 -> Reference <$> (getIMC be :: Get ReferenceFields)
      480 -> FollowRefState <$> (getIMC be :: Get FollowRefStateFields)
      515 -> TransmissionRequest <$> (getIMC be :: Get TransmissionRequestFields)
      516 -> TransmissionStatus <$> (getIMC be :: Get TransmissionStatusFields)
      559 -> PlanControl <$> (getIMC be :: Get PlanControlFields)
      560 -> PlanControlState <$> (getIMC be :: Get PlanControlStateFields)
      _ -> Unparsed _id <$> (getByteString =<< remaining)

data Message = Message {
    src       :: Word16
  , src_ent   :: Word8
  , dst       :: Word16
  , dst_ent   :: Word8
  , fields    :: MessageF
  } deriving (Show, Eq, Generic)

serialize :: IMCSerialize a => Bool -> a -> BS.ByteString
serialize be = runPut . putIMC be

deserialize :: IMCSerialize a => Bool -> BS.ByteString -> Either String a
deserialize be = runGet $ getIMC be

data Header = Header
  { sync :: Word16,
    mgid :: Word16,
    size :: Word16,
    timestamp :: Double,
    src :: Word16,
    src_ent :: Word8,
    dst :: Word16,
    dst_ent :: Word8
  }
  deriving (Eq, Show, Generic)
instance IMCSerialize Header where
  -- put is generically derived
  getIMC _ = do
    syncNumber <- getWord16be
    case syncNumber of
      0xFE54 -> Header syncNumber <$> getWord16be <*> getWord16be <*> getFloat64be <*> getWord16be <*> getWord8 <*> getWord16be <*> getWord8
      0x54FE -> Header syncNumber <$> getWord16le <*> getWord16le <*> getFloat64le <*> getWord16le <*> getWord8 <*> getWord16le <*> getWord8
      _ -> fail "Invalid sync number"

data NetworkMessage = NetworkMessage {
    header :: Header
  , fields :: MessageF
  , footer :: Word16
} deriving (Eq, Show, Generic)
instance IMCSerialize NetworkMessage where
  putIMC be m = do
    let NetworkMessage (Header _ _ _ t src src_ent dst dst_ent) f _f = m
        (_id, contents) = BS.splitAt 2 (serialize be f)
        eitherID = if be then runGet getWord16be _id else runGet getWord16le _id
    case eitherID of
      Right __id -> do
        let serialized_h = serialize be $ Header 0xFE54 __id (fromIntegral $ BS.length contents) t src src_ent dst dst_ent
        putByteString serialized_h
        putByteString contents
        putIMC be (crc16' $ BS.append serialized_h contents)
      _      -> putIMC be "Failed to get id from message fields" -- This is stupid. It FAILED, but I cannot call a failure here.
  getIMC _ = do
    let getIMCE :: Get (Either String NetworkMessage)
        getIMCE =
          do
            bin_header <- lookAhead $ getByteString 20
            h <- getIMC True :: Get Header -- 'be' parameter is ignored when get'ing the Header
            let Header syncNumber mid _size _ _ _ _ _ = h
                be = syncNumber == 0xFE54
                h' = h {sync = 0xFE54}
            contents <- getByteString $ fromIntegral _size
            msg_crc <- getIMC be :: Get Word16
            if msg_crc == crc16' (BS.append bin_header contents)
              then
                case deserialize be $ BS.append (serialize be mid) contents of
                  Right mf  -> return . Right $ NetworkMessage h' mf msg_crc
                  Left a    -> return $ Left a
              else return $ Left "Invalid CRC"
    
    attempt <- lookAheadE getIMCE
    case attempt of
      Right m -> return m
      Left error -> fail error

-- |Test function that reads a whole .lsf file deserializes, serializes and writes to the
-- |output file
readFile :: String -> String -> IO ()
readFile i_file o_file = do
  let buffer_size = 8192
      loop :: Result NetworkMessage -> BS.ByteString -> IO ()
      loop decoder _data = do
        case decoder of
          Done msg leftover -> do
            BS.appendFile o_file $ serialize False msg
            let x = deserialize False (serialize False msg)
            case x of
              Right y -> when (y /= msg) $ print y >> print msg >> putStrLn ""
              _ -> return ()
            loop (runGetPartial (getIMC True :: Get NetworkMessage) $ BS.append leftover _data) BS.empty
          Partial k -> do
            loop (k _data) BS.empty
          Fail error_msg leftover -> if BS.null leftover 
            then return () 
            else print ("I failed! :(\n" ++ error_msg) >> loop (runGetPartial (getIMC True :: Get NetworkMessage) (BS.drop 2 $ BS.append leftover _data)) BS.empty
  
  x <- BS.readFile i_file
  loop (runGetPartial (getIMC True :: Get NetworkMessage) x) BS.empty
{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE OverloadedLabels          #-}

{-# LANGUAGE DeriveGeneric             #-}

module IMC.Control.Utils (
    VState (..)
  , Vehicle (..)
  , emptyVState
  , updateState
  , updateVehicleList

  , addLatLon
  , distance3D_E
  , distance3D_H
  , distance3D_D
  , displace

  , eventToList
  , everyT
  , alignEvents
)
where

import FRP.Yampa                      ( returnA,
                                        localTime,
                                        loopPre,
                                        Arrow(arr),
                                        SF,
                                        DTime,
                                        Event(..),
                                        dup,
                                        isEvent )
import           Data.Word            ( Word16 )
import qualified IMC.Messages as M
import           Control.Lens         ( view )
import           GHC.Generics         ( Generic )
import           Data.List            ( partition )

data VState = VState  { lat         :: !Double
                      , lon         :: !Double
                      , depth       :: !Float
                      , gvx         :: Maybe Float
                      , gvy         :: Maybe Float
                      , gvz         :: Maybe Float
                      , lastUpdate  :: !DTime
                      } deriving (Show, Eq, Generic)
emptyVState :: VState
emptyVState = VState 0 0 0 Nothing Nothing Nothing 0

data Vehicle = Vehicle { srcID :: !Word16
                       , name :: String
                       , state :: !VState } deriving (Show, Eq, Generic)

updateState :: DTime -> M.Message -> VState -> VState
updateState now input _state = do
  case view #fields input of
    M.EstimatedState esf  -> newState
      where M.EstimatedStateFields _lat _lon _ offN offE _ _ _ _ _ _ _ _gvx _gvy _gvz _ _ _ _depth _ = esf
            newState = _state { lat = new_lat, lon = new_lon, depth = _depth
                              , gvx = Just _gvx, gvy = Just _gvy, gvz = Just _gvz
                              , lastUpdate = now }

            --(new_lat, new_lon) = addLatLon _lat _lon (realToFrac offN) (realToFrac offE)
            (new_lat, new_lon, _) = displace _lat _lon 0 (realToFrac offN) (realToFrac offE) 0
    M.Announce af         -> newState
      where M.AnnounceFields _ _ _ _lat _lon _height _ = af
            newState = _state { lat = _lat, lon = _lon, depth = - _height
                              , lastUpdate = now }
    _                     -> _state

updateVehicleList :: SF (M.Message, [Vehicle]) ([Vehicle], [Vehicle])
updateVehicleList = proc (msg, knownPeers) -> do
  let msgsrc = view #src msg
      (isSRC, others) = partition ((== msgsrc) . srcID) knownPeers
      updateVehicleState now msg v = v {state = updateState now msg (state v)}

  now <- localTime -< ()

  case view #fields msg of
    M.EstimatedState _ ->
            returnA -< dup $ map ( updateVehicleState now msg ) isSRC ++ others
    M.Announce (M.AnnounceFields sysName sysType _ _ _ _ _) ->
      if null isSRC && sysType == 2    -- I don't know who it is, and it is an UUV (unmanned underwater)
        then returnA -< dup $ updateVehicleState now msg (Vehicle msgsrc sysName emptyVState) : knownPeers
        else returnA -< dup $ map ( updateVehicleState now msg ) isSRC ++ others

    _ -> returnA -< dup knownPeers

eventToList :: SF (Event a) [a]
eventToList = proc msgE -> do
  case msgE of
    Event msg -> returnA -< [msg]
    _         -> returnA -< []

append :: SF ([a], [a]) [a]
append = arr $ uncurry (<>)

everyT :: DTime -> (a -> b) -> SF a (Event b)
everyT period f = loopPre 0 $ countdownTimer period f
  where countdownTimer timeLength f = proc (a, lastCall) -> do
          t <- localTime -< ()
          if t - lastCall > timeLength
            then
              returnA -< (Event $ f a, t)
            else
              returnA -< (NoEvent, lastCall)

alignEvents :: SF (Event a, Event b) (Event a)
alignEvents = proc (a, b) -> do
  let merge | isEvent a && isEvent b  = a
            | otherwise               = NoEvent
  returnA -< merge

-- ##########################################
-- Coordinates functions
-- ##########################################

addLatLon :: Double -> Double -> Double -> Double -> (Double, Double)
addLatLon lat lon distX distY = (lat + latChange, lon + lonChange)
  where earthRadius = 6371000.0                       -- Earth's radius in meters
        latChange = distY / earthRadius               -- Change in latitude in radians
        lonChange = distX / (earthRadius * cos lat)   -- Change in longitude in radians


-- | The haversine of an angle.
haversine :: Floating a => a -> a
haversine = (^ 2) . sin . (/ 2)

-- | The approximate distance, in meters, between two points on Earth.
-- The latitude and longitude are assumed to be in rads.
greatCircleDistance :: (Ord a, Floating a) => (a, a) -> (a, a) -> a
greatCircleDistance = distRad 6371000
  where distRad radius (lat1, lng1) (lat2, lng2) =
          (2 * radius) * asin ( min 1.0
                              ( sqrt $ haversine (lat2 - lat1) + ( (cos lat1 * cos lat2) * haversine (lng2 - lng1) ) ) )

-- | Approximate WGS84 coordinates distance to m, using Euclidean distance
distance3D_E :: (Show a, Floating a) => a -> a -> a -> a -> a -> a -> a
distance3D_E x y z x' y' z'= sqrt (((x-x')*r)^2 + ((y-y')*r)^2 + (z-z')^2)
  where r = 6371*10^3 -- Earth radius in m

-- | Approximate WGS84 coordinates distance to m, using Haversine formula
distance3D_H :: (Show a, Ord a, Floating a) => a -> a -> a -> a -> a -> a -> a
distance3D_H x y z x' y' z'= greatCircleDistance (x, y) (x', y')

-- | Approximate WGS84 coordinates distance to m, using Dune function
distance3D_D :: Double -> Double -> Double -> Double -> Double -> Double -> Double
distance3D_D lat1 lon1 h1 lat2 lon2 h2 = d
  where (x1, y1, z1) = toECEF lat1 lon1 (realToFrac h1)
        (x2, y2, z2) = toECEF lat2 lon2 (realToFrac h2)
        d = sqrt ( (x1 - x2) ** 2 + (y1 - y2) ** 2 + (realToFrac z1 - realToFrac z2) ** 2)

c_wgs84_a :: Double
c_wgs84_a = 6378137.0 -- Semi-major axis
c_wgs84_b :: Double
c_wgs84_b = 6356752.3142 -- Semi-minor axis
c_wgs84_e2 :: Double
c_wgs84_e2 = 0.00669437999013 -- First eccentricity squared
c_wgs84_ep2 :: Double
c_wgs84_ep2 = 0.00673949674228 -- Second (prime) eccentricity squared
c_wgs84_f :: Double
c_wgs84_f = 0.0033528106647475 -- Flattening

toECEF :: Double -> Double -> Float -> (Double, Double, Float)
toECEF lat lon hae' = (x_, y_, z_)
  where hae = realToFrac hae'
        x_ = (rn + hae) * cos lat * cos lon
        y_ = (rn + hae) * cos lat * sin lon
        z_ = realToFrac ((((1.0 - c_wgs84_e2) * rn) + hae) * sin lat)

        rn = computeRn lat

fromECEF :: Double -> Double -> Float -> (Double, Double, Float)
fromECEF x y z = (lat, lon, hae)
  where p = sqrt (x * x + y * y)
        lon = atan2 y x
        theta = atan2 (c_wgs84_a * realToFrac z) ( p * c_wgs84_b)
        num = realToFrac z + c_wgs84_ep2 * c_wgs84_b * (sin theta ** 3.0)
        den = p - c_wgs84_e2 * c_wgs84_a * (cos theta ** 3.0)
        lat = atan2 num (realToFrac den)
        hae = realToFrac (p / cos (realToFrac lat) - computeRn (realToFrac lat))

computeRn :: Double -> Double
computeRn lat = c_wgs84_a / sqrt (1 - c_wgs84_e2 * (sin lat  * sin lat))

displace :: Double -> Double -> Float -> Double -> Double -> Double-> (Double, Double, Float)
displace lat lon hae n e d = (new_lat, new_lon, new_hae)
  where (x, y, z) = toECEF lat lon hae
        p = sqrt (x * x + y * y)
        bigN = computeRn lat
        phi = atan2 (realToFrac z) (p * (1 - c_wgs84_e2 * bigN / (bigN + realToFrac hae)))

        x_aux = x + (- sin lon * e - cos lon * sin phi * n - cos lon * cos phi * d)
        y_aux = y + (cos lon * e - sin lon * sin phi * n - sin lon * cos phi * d)
        z_aux = z + realToFrac (cos phi * n - sin phi * d)

        (new_lat, new_lon, new_hae) = fromECEF x_aux y_aux (realToFrac z_aux)
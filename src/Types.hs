{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Prelude hiding (id)
import Data.Aeson (FromJSON)
import Data.Aeson.TH (deriveFromJSON, defaultOptions, fieldLabelModifier)
import Data.List (intercalate)
import GHC.Generics
import Numeric (showFFloat)

newtype Coords = Coords (Float, Float)
  deriving (Show)
newtype Origin = Origin Coords
  deriving (Show)
newtype Destination = Destination Coords
  deriving (Show)

newtype LongName = LongName String
  deriving (Show, Eq, Generic, FromJSON)
newtype ShortName = ShortName String
  deriving (Show, Eq, Generic, FromJSON)
newtype GTFSId = GTFSId String
  deriving (Show, Eq, Generic, FromJSON)
newtype Point = Point String
newtype DistanceMeters = DistanceMeters Int
newtype StartTime = StartTime Int
newtype EndTime = EndTime Int

class MyShow a where
  myshow :: a -> String

data MethodId = MethodId GTFSId ShortName LongName
  deriving (Eq)
instance MyShow MethodId where
  myshow (MethodId _ (ShortName sn) (LongName ln)) =
    sn ++ " " ++ ln

newtype DistanceKilometers = DistanceKilometers Float
instance MyShow DistanceKilometers where
  myshow (DistanceKilometers x) =
    showFFloat (Just 1) x " km"

data Details = Details Point DistanceKilometers TimeMinutes
instance MyShow Details where
  myshow (Details (Point pt) dkm tm) =
    pt ++ " for " ++ myshow tm ++ ", " ++ myshow dkm 

data Method
  = Walk Details
  | Bus MethodId Details
  | Tram MethodId Details
  | Train MethodId Details
  | Mystery String Details

newtype Step = Step Method
instance MyShow Step where
  myshow (Step method) =
    case method of
      Walk details ->
        "walk to " ++ myshow details
      Bus id details ->
        "take bus " ++ myshow id ++ " to " ++ myshow details
      Tram id details ->
        "take tram " ++ myshow id ++ " to " ++ myshow details
      Train id details ->
        "take train " ++ myshow id ++ " to " ++ myshow details
      Mystery x details ->
        "take ??? " ++ x ++ " to " ++ myshow details

newtype TimeMinutes = TimeMinutes Int
instance MyShow TimeMinutes where
  myshow (TimeMinutes x) =
    show x ++ " min"

newtype WalkingDistance = WalkingDistance DistanceMeters
instance MyShow WalkingDistance where
  myshow (WalkingDistance (DistanceMeters x)) =
    show x ++ " m"

data Summary = Summary WalkingDistance TimeMinutes
instance MyShow Summary where
  myshow (Summary wd tm) =
    "walking: " ++ myshow wd ++ "\n" ++
    "time: " ++ myshow tm

data Route = Route Summary [Step]
instance MyShow Route where
  myshow (Route summary steps) =
    myshow summary ++
    "\nsteps:\n" ++
    steps'
    where
      steps' = intercalate "\n" $ myshow <$> steps

newtype Home = Home String
  deriving (Show, Generic, FromJSON)

data TimeTable = TimeTable MethodId TimeMinutes

data Config = Config
  { home :: Home
  } deriving (Show, Generic, FromJSON)

type Error = String

data AddressLookupGeometry = AddressLookupGeometry
  { coordinates :: [Float]
  } deriving (Show, Generic, FromJSON)

data AddressLookupFeature = AddressLookupFeature
  { geometry :: AddressLookupGeometry
  } deriving (Show, Generic, FromJSON)

data AddressLookup = AddressLookup
  { features :: [AddressLookupFeature]
  } deriving (Show, Generic, FromJSON)

data ItineraryLookupTo = ItineraryLookupTo
  { name :: String
  } deriving (Show, Generic, FromJSON)

data ItineraryLookupRoute = ItineraryLookupRoute
  { longName :: LongName
  , shortName :: ShortName
  , gtfsId :: GTFSId
  } deriving (Show, Generic, FromJSON)

newtype TimestampMillis = TimestampMillis Int
  deriving (Show, Generic, FromJSON)

data ItineraryLookupLeg = ItineraryLookupLeg
  { startTime :: TimestampMillis
  , endTime :: TimestampMillis
  , mode :: String
  , route :: Maybe ItineraryLookupRoute
  , to :: ItineraryLookupTo
  , distance :: Float
  } deriving (Show, Generic, FromJSON)

data ItineraryLookupItinerary = ItineraryLookupItinerary
  { startTime :: TimestampMillis
  , endTime :: TimestampMillis
  , walkDistance :: Float
  , duration :: Int
  , legs :: [ItineraryLookupLeg]
  } deriving (Show, Generic, FromJSON)

data ItineraryLookupPlan = ItineraryLookupPlan
  { itineraries :: [ItineraryLookupItinerary]
  } deriving (Show, Generic, FromJSON)

data ItineraryLookupData = ItineraryLookupData
  { plan :: ItineraryLookupPlan
  } deriving (Show, Generic, FromJSON)

data ItineraryLookup = ItineraryLookup
  { _data :: ItineraryLookupData
  } deriving (Show)
deriveFromJSON defaultOptions{fieldLabelModifier = drop 1} ''ItineraryLookup

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib where

import Prelude hiding (readFile)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (readFile)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.List (sortOn, intercalate)
import Data.Maybe (fromMaybe)
import System.Process (readProcess)

import Types
import Query

extractDestination :: String -> Either Error Destination
extractDestination s = do
  result :: AddressLookup <- eitherDecode $ fromString s
  feature <- case features result of
    [] -> error "address lookup returned empty features list"
    x:_ -> return x
  coords <- case coordinates . geometry $ feature of
    x:y:_ -> return (y, x) -- why does the API give these flipped...?
    _ -> error "coordinates were malformed in empty features list"
  return $ Destination $ Coords coords

getHomeCoords :: Home -> IO (Either Error Destination)
getHomeCoords (Home home) =
  extractDestination <$> readProcess
    "curl"
    [ "-s"
    , "http://api.digitransit.fi/geocoding/v1/search?text=" ++ (format =<< home) ++ "&size=1"
    ]
    ""
  where
    format ' ' = return '+'
    format c = return c

extractRoutes :: String -> Either Error [Route]
extractRoutes s = do
  result :: ItineraryLookup <- eitherDecode $ fromString s
  let itns = itineraries . plan . _data $ result
  mapM makeRoute itns
  where
    makeRoute ItineraryLookupItinerary {duration, walkDistance, legs}= do
      let time = duration `quot` 60
      let dist = DistanceMeters $ round walkDistance
      let summary = Summary (WalkingDistance dist) (TimeMinutes time)
      return $ Route summary $ makeStep <$> legs
    makeStep ItineraryLookupLeg
      { startTime = TimestampMillis st
      , endTime = TimestampMillis et
      , mode
      , route
      , to
      , distance
      } = do
      let details = Details
            (Point $ name to)
            (DistanceKilometers (distance / 1000))
            (TimeMinutes $ (et - st) `div` 1000 `div` 60)
      let id' = case route of
            Just ItineraryLookupRoute {gtfsId, shortName, longName} ->
              Just $ MethodId gtfsId (fromMaybe (ShortName "") shortName) longName
            Nothing -> Nothing
      Step $ case (mode, id') of
        ("WALK", _) ->
          Walk details
        ("BUS", Just x) ->
          Bus x details
        ("TRAM", Just x) ->
          Tram x details
        ("TRAIN", Just x) ->
          Train x details
        ("SUBWAY", Just x) ->
          Subway x details
        (_, Just x) ->
          Mystery (mode ++ ": " ++ myshow x) details
        _ ->
          Mystery (mode ++ ": " ++ "???") details

getRoutes :: Origin -> Destination -> IO (Either Error [Route])
getRoutes origin dest = do
  let query = prepareItineraryQuery origin dest
  result <- readProcess
    "curl"
    [ "-s"
    , "-XPOST"
    , "-H"
    , "Content-Type:application/json"
    , "https://api.digitransit.fi/routing/v1/routers/finland/index/graphql"
    , "-d"
    , query
    ] ""
  return $ extractRoutes result

getWalkingDistance :: Route -> Int
getWalkingDistance (Route (Summary (WalkingDistance (DistanceMeters x)) _) _) = x

run :: Origin -> IO ()
run origin = do
  Config {home} :: Config <- right . eitherDecode <$> readFile "config.json"
  dest <- right <$> getHomeCoords home
  routes <- (getWalkingDistance `sortOn`) . right <$> getRoutes origin dest
  putStrLn $ "Found " ++ show (length routes) ++ " routes.\n"
  putStrLn $ intercalate "\n\n" $ map myshow routes
  where
    right = either error id

module Query where

import Text.Printf (printf)

import Types

prepareItineraryQuery :: Origin -> Destination -> String
prepareItineraryQuery (Origin origin) (Destination dest) =
  printf queryJSON query
  where
    prepare :: Coords -> String
    prepare (Coords (x, y)) = printf "{lat: %f, lon: %f}" x y
    query :: String
    query = printf itineraryQueryString (prepare origin) (prepare dest)

queryJSON :: String
queryJSON = "{\"query\": \"%s\" ,\"variables\": null}"

itineraryQueryString :: String
itineraryQueryString = "\
\{\
\  plan(\
\    from: %s\
\    to: %s\
\    modes: \\\"TRAM,BUS,SUBWAY,RAIL,FERRY,WALK\\\"\
\    walkReluctance: 1\
\    numItineraries: 3\
\  ) {\
\    itineraries{\
\      startTime\
\      endTime\
\      walkDistance,\
\      duration,\
\      legs {\
\        startTime\
\        endTime\
\        mode\
\        route {\
\          gtfsId\
\          longName\
\          shortName\
\        },\
\        to {\
\          name\
\        },\
\        distance\
\      }\
\    }\
\  }\
\}"

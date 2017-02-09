{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.Aeson
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock.POSIX
import Horizon
import System.Locale
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Text.Read
import qualified Data.ByteString.Lazy.UTF8 as BLL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map

data Time = Time {
    startPeriodName :: [String],
    startValidTime :: [String],
    tempLabel :: [String]
} deriving (Show)

data CurrentObservation = CurrentObservation {
    date :: String,
    temp :: String,
    dewp :: String,
    relh :: String,
    windspeed :: String,
    winddir :: String,
    cWeather :: String,
    visibility :: String,
    pressure :: String
} deriving (Show)

data Data = Data {
    temperature :: [String],
    pop :: [Maybe String],
    fWeather :: [String]
} deriving (Show)

data Weather = Weather {
    time :: Time,
    dat :: Data,
    currentobservation :: CurrentObservation,
    creationDate :: String
} deriving (Show)

instance FromJSON Weather where
    parseJSON (Object o) = do
        creationDate <- o .: "creationDate"
        time <- o .: "time" >>= parseJSON
        dat <- o .: "data" >>= parseJSON
        curObs <- o .: "currentobservation" >>= parseJSON
        return $ Weather time dat curObs creationDate
    parseJSON _ = empty

instance FromJSON Data where
    parseJSON (Object v) = do 
        temp <- v .: "temperature" >>= mapM parseJSON
        precipChance <- v .: "pop"
        weatherDesc <- v .: "weather"
        return $ Data temp precipChance weatherDesc
    parseJSON _ = empty
    
instance FromJSON CurrentObservation where
    parseJSON (Object c) = CurrentObservation <$>
        c .: "Date" <*>
        c .: "Temp" <*>
        c .: "Dewp" <*>
        c .: "Relh" <*>
        c .: "Winds" <*>
        c .: "Windd" <*>
        c .: "Weather" <*>
        c .: "Visibility" <*>
        c .: "SLP"
    parseJSON _ = empty

instance FromJSON Time where
    parseJSON (Object t) = Time <$>
        t .: "startPeriodName" <*>
        t .: "startValidTime" <*>
        t .: "tempLabel"
    parseJSON _ = empty

amWeatherIcons = Map.fromList [
    ("Mostly Cloudy", "d"),
    ("Increasing Clouds", "d"),
    ("Sunny", "a"),
    ("Fair", "a"),
    ("Clear", "a"),
    ("A Few Clouds", "b"),
    ("Partly Sunny", "b"),
    ("Partly Cloudy", "c"),
    ("Overcast", "e"),
    ("Fog", "0"),
    ("Smoke", "0"),
    ("Freezing Rain", "t"),
    ("Freezing Drizzle", "t"),
    ("Ice Pellets", "u"),
    ("Hail", "u"),
    ("Freezing Rain Snow", "y"),
    ("Freezing Drizzle Snow", "y"),
    ("Snow Freezing Rain", "y"),
    ("Snow Freezing Drizzle", "y"),
    ("Rain Ice Pellets", "v"),
    ("Drizzle Ice Pellets", "v"),
    ("Ice Pellets Rain", "v"),
    ("Ice Pellets Drizzle", "v"),
    ("Rain Snow", "x"),
    ("Snow Rain", "x"),
    ("Drizzle Snow", "x"),
    ("Snow Drizzle", "x"),
    ("Showers", "j"),
    ("Thunderstorm", "m"),
    ("T-storm", "m"),
    ("Snow", "q"),
    ("Windy", "6"),
    ("Breezy", "6"),
    ("Light Rain", "h"),
    ("Drizzle", "h"),
    ("Sprinkles", "h"),
    ("Rain", "i"),
    ("Funnel Cloud", "1"),
    ("Tornado", "1"),
    ("Dust", "7"),
    ("Sand", "7"),
    ("Haze", "9"),
    ("Hot", "5"),
    ("Cold", "E"),
    ("Frost", "E") ]
pmWeatherIcons = ["A", "B", "C", "D", "G", "K", "O"]

cardinalDirections = ["N", "NbE", "NNE", "NEbN", "NE", "NEbE", "ENE", "EbN",
    "E", "EbS", "ESE", "SEbE", "SE", "SEbS", "SSE", "SbE", "S", "SbW", "SSW",
    "SWbS", "SW", "SWbW", "WSW", "WbS", "W", "WbN", "WNW", "NWbW", "NW", "NWbN",
    "NNW", "NbW", "N"]

-- Latitude and longitude for your location
latitudeN = 0.00000 :: LatitudeNorth
longitudeW = 0.00000 :: LongitudeWest

-- For determining cardinal directions:
-- Take floor((windd + 11.25) / 22.5) = index in list.
-- E.g., (80 + 15) / 30 = 3.16 -> floor = 3
-- Positions: 0: N, 1: NNE, 2: NE, 3: ENE, 4: E, 5: ESE, ..., 12: N
-- So 80° should be displayed as E.
cardinalDirectionify :: (Real a, Fractional a, RealFrac a) => Maybe a -> String
cardinalDirectionify (Just direction) = cardinalDirections !! (floor ((direction + 5.625) / 11.25))
cardinalDirectionify Nothing = ""

windspeedDisplay :: String -> String -> String
windspeedDisplay "NA" "0" = "Calm"
windspeedDisplay "0" "0" = "Calm"
windspeedDisplay _ speed = speed ++ "mph"

-- Kind of lifts the auto-generated functions for the data types above to be able to work with Maybe.
test' :: (a -> b) -> Maybe a -> Maybe b
test' extract (Just (currentWeather)) = Just $ extract currentWeather
test' extract Nothing = Nothing

-- Might have to double check the format string every so often (primarily the day and hour).
rayify :: CurrentObservation -> [String]
rayify (CurrentObservation "Data is Old" _ _ _ _ _ _ _ _) = ["" | _ <- [1..11]]
rayify (CurrentObservation d t dew rh ws wd w v p) =
    let pm = let tod = todHour $ fromMaybe (TimeOfDay 0 0 0) ((parseTime defaultTimeLocale "%e %b %l:%M %p %Z" d) :: Maybe TimeOfDay)
                in if tod >= 18 || tod <= 6 then True else False
        lutime = formatTime defaultTimeLocale "%m/%d %H:%M" $ ((readTime defaultTimeLocale "%e %b %l:%M %p %Z" d) :: LocalTime)
    in [lutime, find_icon pm w, w, t, cardinalDirectionify $ readMaybe wd, wd, windspeedDisplay wd ws, rh ++ "%", dew, p ++ " in", v ++ " mi"]

-- Take just the first 13 forecasts 'cause that's how the Conky config file
-- is set up. Any more than that and the Sun/Moon Rise and Set things get
-- messed up and display forecast info instead of celestial info.
aggregate :: Weather -> [[String]]
aggregate (Weather t d _ _) = take 13 $ agg (startPeriodName t) (temperature d) (fWeather d)
    where agg [] [] [] = []
          agg (p:[]) (t:[]) (w:[]) = [[p] ++ [t] ++ [w]]
          agg (p:ps) (t:ts) (w:ws) = [[p] ++ [t] ++ [w]] ++ (agg ps ts ws)

-- Should be the same thing as Data.List.isPrefixOf (I wrote it as an exercise)
testp :: [Char] -> [Char] -> Bool
testp [] [] = True
testp [] (y:ys) = True
testp (x:xs) [] = False
testp (x:xs) (y:ys)
    | x == y = True && testp xs ys
    | otherwise = False

-- Should be the same thing as Data.List.isInfixOf (I wrote it as an exercise)
testeq :: [Char] -> [Char] -> Bool
testeq [] [] = True
testeq [] (i:n_here) = True
testeq (f:ind) [] = False
testeq s@(f:ind) (i:n_here)
    | f == i = True && testp ind n_here
    | otherwise = testeq s n_here
          
-- For use with Map.foldrWithKey in find_icon. Easier for me to write it out like this so
-- I remember what I'm trying to do (and remember the inputs and outputs)
-- Method: figure out if key is in statement. Store resulting key as result. In future
-- iterations, find out if result is shorter than key (if statement matches). In that case,
-- replace result with new key
find_icon' :: Bool -> String -> String -> String -> (String, String) -> (String, String)
find_icon' pm statement key icon result
    | key `isInfixOf` statement = cNight $ check result (key, icon)
    | otherwise = cNight result
    where check r ki = if (length $ fst r) > (length $ fst ki) then r else ki
          cNight ico = let letter = map toUpper (snd ico)
                       in if pm && letter `elem` pmWeatherIcons
                            then (fst ico, letter)
                            else ico

find_icon :: Bool -> String -> String
find_icon pm statement = snd $ Map.foldrWithKey (find_icon' pm statement) ("", "") amWeatherIcons

-- Get today's Modified Julian Day. (Ineffectively) gets the current POSIX time, converts it to
-- an IO UTCTime, then to an IO String, and finally to an IO Day.
getTodaysMJD :: IO Day
getTodaysMJD = do
    posTime <- liftM posixSecondsToUTCTime getPOSIXTime
    let sTime = formatTime defaultTimeLocale "%Y-%m-%d" posTime
    return $ (readTime defaultTimeLocale "%Y-%m-%d" sTime) :: IO Day

-- unsugared edition
getTodaysMJD' :: IO Day
getTodaysMJD' = liftM posixSecondsToUTCTime getPOSIXTime >>= (\posTime ->
                    let sTime = formatTime defaultTimeLocale "%Y-%m-%d" posTime
                    in return $ (readTime defaultTimeLocale "%Y-%m-%d" sTime))

mjdToJD :: Day -> Int
mjdToJD d = floor ((fromIntegral $ toModifiedJulianDay d) + 2400000.5)

-- Uses Horizon package to calculate sunrise/sunset times. Doesn't do
-- moonrise/set.Luckily, the sunrise and sunset functions both take Day as their
-- input type.
calculateRiseSet :: Day -> TimeZone -> [String]
calculateRiseSet d tz = [sr, "", ss, ""]
    where sr = formatTime defaultTimeLocale "%T" $ utcToLocalTime tz $ sunrise d longitudeW latitudeN
          ss = formatTime defaultTimeLocale "%T" $ utcToLocalTime tz $ sunset d longitudeW latitudeN

-- Does what it says on the box, and it also formats each string so that it displays better
-- e.g., Monday Night becomes Mon PM and Chance Showers becomes Chance
--                                                              Showers
-- 7 characters max per line.
-- The (ampm day (False, True)) argument to find_icon tells whether or not it should find
-- icons for night (i.e., with moon and stars instead of sun). The (ampm day ("H", "L")) argument
-- says what to prepend to the temperature (i.e., High or Low for the day).
add_icons :: [[String]] -> [[String]]
add_icons weatherStatements = map icons_to weatherStatements
    where icons_to [] = []
          icons_to ws@(day:t:statement:[]) = [(shorten day)] ++ 
            [find_icon (ampm day (False, True)) statement] ++
            (shorten' statement) ++ 
            [ampm day ("H", "L") ++ " " ++ t]
          icons_to ws = ws

-- Decides whether or not to append AM or PM
ampm :: String -> (a, a) -> a
ampm s (a, p)
    | "This Afternoon" == s = a
    | "Late Afternoon" == s = a 
    | " Night" `isInfixOf` s = p
    | "Overnight" == s = p
    | "Tonight" == s = p
    | "Today" == s = a
    | otherwise = a
        
-- Shortens days
shorten :: [Char] -> String
shorten "Tonight" = "Tonight"
shorten "Today" = "Today"
shorten "This Afternoon" = "Today"
shorten "Late Afternoon" = "Today"
shorten s = (take 3 s) ++ " " ++ (ampm s ("AM", "PM"))

-- Shortens weather statements
shorten' :: [Char] -> [String]
shorten' s = let dist = let svn = ' ' `elemIndex` s
                        in if svn == Nothing || svn >= Just 8
                            then (7, 7)
                            else (fromMaybe 0 svn, (fromMaybe 0 svn + 1))
             in [take (fst dist) s] ++ [take 7 $ drop (snd dist) s]
             
main :: IO ()
main = do
    content <- readFile "/home/me/.conky_weather/fetched"
    let test = decode $ BLL.fromString content :: Maybe Weather
    
    
    -- This line returns the first (returned by list indexing, i.e., x !! 0) weather forecast
    -- description (returned by fWeather $ dat test). We need to do \x -> Just x because
    -- (>>=) expects the return type of this function (a lambda, in this case) to be the same as
    -- the type of the first argument (in this case, a Maybe (which Just and Nothing are), because
    -- test' fWeather $ test' dat test = Maybe [String])
    -- 
    -- weathDesc <- return $ (test' fWeather $ test' dat test) >>= (\x -> Just $ x !! 0)
    
    -- liftM makes the function "aggregate" work on Maybe Weather instead of just Weather
    -- return turns the Maybe String that is returned by "liftM aggregate" into an IO Maybe String
    -- weath <- return $ liftM ((maybeToList (liftM rayify (liftM currentobservation test))) ++) (liftM aggregate test)
    
    -- Alternatively,
    co <- return $ liftM currentobservation test           -- co :: Maybe CurrentObservation
    fcst <- return $ liftM aggregate test                  -- fcst :: Maybe [[String]]
    let curObs = liftM rayify co >>= (\cb -> return [cb])  -- curObs :: Maybe [[String]]
    -- or sequence [liftM rayify co] or maybeToList $ liftM rayify co
    let weath = curObs >>= (\corn -> fcst >>= (\cob -> return (corn ++ cob))) -- weath :: Maybe [[String]]
    -- or liftM2 (++) curObs fcst
    
    -- For each element in weather, map the 3rd element of each element to a weather icon and tack
    -- on said weather icon to the end. To map elements to a weather icon, use foldlWithKey or
    -- foldrWithKey or filterWithKey. Not sure why I can't find something that matches lists in
    -- lists, but... oh well. Edit: Actually, there is. See Data.List.isInfixOf
    -- let weatherForecast = return $ liftM add_icons weath
    
    -- Since we're calculating sunrise and sunset based on the current time with liftM,
    -- which returns an IO [String], we need to do a bit of binding to combine it with the
    -- Maybe [[String]] that is weath. Basically, bind the result of the riseset calculation
    -- to get the [String], then pass that to a lambda function that binds the Maybe [[String]]
    -- that is weath, and pass both to a function that concatenates the two arrays, turns them
    -- into a Maybe [[String]], and then use return again after adding icons to the Maybe [[String]]
    -- that is forecast to turn the List into an IO (Maybe [[String]]) to satisfy the compiler.
    -- I suppose it could also be a Maybe (IO [[String]]) if we'd started out by binding weath.
    let weatherForecast = (liftM2 calculateRiseSet getTodaysMJD' getCurrentTimeZone) >>= 
                           (\rs ->
                               let forecast = weath >>= (\w -> return (w ++ [rs]))
                               in return $ liftM add_icons forecast) -- IO (Maybe [[String]])

    -- Convert the inner List into lines and then the outer one into lines. Convert Maybe String
    -- into a String for writing to file.
    weatherForecast >>= (\x -> let y = x >>= (\z -> return $ unlines $ map unlines z) in writeFile "/home/me/.conky_weather/parsed" $ fromMaybe "" y)
    
    -- Need x -> let y = x >>= instead of just x -> x >>= possibly because the let statement lets
    -- the compiler infer the return type of x >>= (\z -> ...), whereas without the let, the compiler
    -- expects an IO type. That's my current hypothesis. Could be wrong, but it makes sense in my
    -- head.
    
    -- weath <- return $ test' test  -- Line a
    -- print weath                   -- Line b
    
    -- These two lines each do the same as lines a and b, above, but using bind (i.e., >>=)
    -- instead of do-notation
    -- 
    -- (return (test' test)) >>= (\y -> print y)
    -- return test >>= (\x -> (liftM test') (return x)) >>= (\y -> print y)

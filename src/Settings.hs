module Settings
    ( Settings
    , fromOk
    , strToCode, codeToStr, settingsFile, annotFile
    , readSettings, writeUserSettings, getCode

    , createSettings
    , get_replicates, get_counts_file, get_counts_skip, get_user_settings
    , get_id_column, get_info_columns, get_ec_column
    , is_locked, get_csv_format
    ) where

import Control.Applicative
import Data.Time
import Data.List
import System.FilePath
import Text.JSON
import qualified Data.ByteString.Lazy as BS

import Utils

newtype Code = Code {codeToStr :: String} deriving (Eq,Show)

data Settings = Settings { code :: Code
                         , remote_addr :: String
                         , created :: String
                         , user_settings :: UserSettings
                         } deriving Show

data UserSettings = UserSettings { id_col :: Int -- ^ Column number of ID column (0-based)
                                 , replicates :: [(Int, [Int])] -- ^ Definition of replicates
                                 , ec_col :: Maybe Int -- ^ Optional EC number column (0-based)
                                 , info_cols :: [Int] -- ^ Optional list of columns to include in display

                                 , skip :: Int -- ^ Number of columns to skip
                                 , csv_format :: Bool  -- ^ True for CSV, False for TAB
                                 , locked :: Bool -- ^ True to allow a public example
                                 , col_names :: [String]  -- ^ Not used by the backend, set from the csv file
                                 , replicate_names :: [String] -- ^ Not used by the backend, set by the user
                                 } deriving Show

instance JSON Settings where
    -- readJSON :: JSValue -> Result Settings
    readJSON (JSObject obj) = Settings <$> (strToCode <$> valFromObj "code" obj)
                                       <*> valFromObj "remote_addr" obj
                                       <*> valFromObj "created" obj
                                       <*> valFromObj "user_settings" obj
    readJSON _ = Error "Expect object"

    -- showJSON :: Settings -> JSValue
    showJSON s = makeObj [("code", showJSON . codeToStr . code $ s)
                          ,("remote_addr", showJSON $ remote_addr s)
                          ,("created", showJSON $ created s)
                          ,("user_settings",showJSON $ user_settings s)
                          ]

instance JSON UserSettings where
    -- readJSON :: JSValue -> Result Settings
    readJSON (JSObject obj) = do id_col <- valFromObj "id_column" obj
                                 ec_col <- return $ resToMaybe $ valFromObj "ec_column" obj
                                 info_cols <- valFromObj "info_columns" obj
                                 reps <- valFromObj "replicates" obj
                                 names <-  valFromObj "column_names" obj
                                 rep_names <- valFromObj "replicate_names" obj
                                 fmt <- valFromObj "csv_format" obj
                                 locked <- valFromObj "locked" obj
                                 skip <- valFromObj "skip" obj
                                 return $ UserSettings { id_col = id_col
                                                       , ec_col = ec_col
                                                       , info_cols = info_cols
                                                       , replicates = reps
                                                       , col_names = names
                                                       , replicate_names = rep_names
                                                       , csv_format = fmt
                                                       , skip = skip
                                                       , locked = locked
                                                       }
    readJSON _ = Error "Expect object"

    -- showJSON :: Settings -> JSValue
    showJSON s = makeObj $ [("id_column", showJSON . id_col $ s)
                           ,("info_columns", showJSON . info_cols $ s)
                           ,("replicates", showJSON . replicates $ s)
                           ,("column_names", showJSON . col_names $ s)
                           ,("replicate_names", showJSON . replicate_names $ s)
                           ,("csv_format", showJSON . csv_format $ s)
                           ,("skip", showJSON . skip $ s)
                           ,("locked", showJSON . locked $ s)
                           ] ++ maybe [] (\v -> [("ec_column", showJSON v)]) (ec_col s)


-- | Smart constructor for 'Code' to ensure it is a safe FilePath
strToCode :: String -> Code
strToCode s | badChars || null s = error $ "Bad code :"++s
            | otherwise = Code s
  where
    badChars = length (intersect "/." s) > 0

codeToFilePath :: Code -> FilePath
codeToFilePath (Code s) = user_dir </> s

settingsFile,countsFile :: Code -> String
settingsFile code = codeToFilePath code ++ "-settings.js"
countsFile code = codeToFilePath code ++ "-counts.csv"
annotFile code = codeToFilePath code ++ "-annot.csv"

user_dir :: FilePath
user_dir = "user-files"

-- globalSettings :: MVar Settings
-- globalSettings = unsafePerformIO $ newEmptyMVar

readSettings :: Code -> IO Settings
readSettings code = do
  str <- Prelude.readFile $ settingsFile code
  let ss = decode str
  return $ fromOk ss

writeUserSettings :: Settings -> UserSettings -> IO ()
writeUserSettings settings userSettings = do
    writeSettings (code settings) $ settings { user_settings = userSettings}

writeSettings :: Code -> Settings -> IO ()
writeSettings code settings
    | not valid = error "Invalid settings"
    | otherwise = Prelude.writeFile (settingsFile code) $ encode settings
  where
    valid = True

createSettings ::  BS.ByteString -> String -> UTCTime -> IO Code
createSettings dat remote_ip now = do
  (file, _) <- newRandFile user_dir
  let code = strToCode (takeFileName file)
  BS.writeFile (countsFile code) dat
  writeSettings code $ (initSettings code) {remote_addr=remote_ip, created=show now}
  return code

----------------------------------------------------------------------

getCode :: Settings -> Code
getCode settings = code settings

fromOk :: Show t => Result t -> t
fromOk (Ok r) = r
fromOk x = error $ "json parse failure : "++show x

resToMaybe :: Result t -> Maybe t
resToMaybe (Ok r) = Just r
resToMaybe _ = Nothing

----------------------------------------------------------------------
initSettings :: Code -> Settings
initSettings code = let userSettings = UserSettings { id_col = -1
                                                    , ec_col = Nothing
                                                    , info_cols = []
                                                    , replicates = []
                                                    , col_names = []
                                                    , replicate_names = []
                                                    , csv_format = True
                                                    , skip = 0
                                                    , locked = False
                                                    }
                    in Settings { code = code
                                , remote_addr = ""
                                , created = error "Must set created"
                                , user_settings = userSettings
                                }

get_id_column :: Settings -> Int
get_id_column settings = id_col $ user_settings settings

get_ec_column :: Settings -> Maybe Int
get_ec_column settings = ec_col $ user_settings settings

get_info_columns :: Settings -> [Int]
get_info_columns settings = info_cols $ user_settings settings

get_replicates :: Settings -> [(Int,[Int])]
get_replicates settings = replicates $ user_settings settings

is_locked :: Settings -> Bool
is_locked settings = locked $ user_settings settings

get_csv_format :: Settings -> String
get_csv_format settings = if csv_format $ user_settings settings then "CSV" else "TAB"

get_counts_file :: Settings -> FilePath
get_counts_file s = countsFile $ getCode s

get_counts_skip :: Settings -> Int
get_counts_skip s = skip $ user_settings s

get_user_settings :: Settings -> UserSettings
get_user_settings s = user_settings s

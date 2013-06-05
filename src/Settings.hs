{-# LANGUAGE TemplateHaskell, RankNTypes #-}

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
import Data.Maybe
import System.FilePath
import Text.JSON
import Text.JSON.Types (set_field)
import qualified Data.ByteString.Lazy as BS
import Control.Lens
import Control.Monad (foldM)

import Utils

newtype Code = Code {codeToStr :: String} deriving (Eq,Show)

data Settings = Settings { code :: Code
                         , remote_addr :: String
                         , created :: String
                         , user_settings :: UserSettings
                         } deriving Show

data UserSettings = UserSettings { _id_col :: Int -- ^ Column number of ID column (0-based)
                                 , _replicates :: [(Int, [Int])] -- ^ Definition of replicates
                                 , _ec_col :: Maybe Int -- ^ Optional EC number column (0-based)
                                 , _info_cols :: [Int] -- ^ Optional list of columns to include in display
                                 , _init_select :: [Int] -- ^ Replicates to initially select in UI

                                 , _skip :: Int -- ^ Number of columns to skip
                                 , _csv_format :: Bool  -- ^ True for CSV, False for TAB
                                 , _locked :: Bool -- ^ True to allow a public example
                                 , _col_names :: [String]  -- ^ Not used by the backend, set from the csv file
                                 , _replicate_names :: [String] -- ^ Not used by the backend, set by the user
                                 , _hide :: [Int] -- ^ Used in the frontend to hide some columns
                                 , _title :: String -- ^ Title to use in UI
                                 } deriving Show
makeLenses ''UserSettings

defUserSettings = UserSettings { _id_col = -1
                               , _ec_col = Nothing
                               , _info_cols = []
                               , _replicates = []
                               , _col_names = []
                               , _replicate_names = []
                               , _init_select = []
                               , _hide = []
                               , _csv_format = True
                               , _skip = 0
                               , _locked = False
                               , _title = ""
                               }

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
    readJSON (JSObject obj) = foldM (\u (get,_) -> get u obj) defUserSettings user_settings_cols
    showJSON u = JSObject $ foldl' (\obj (_,set) -> set u obj) (toJSObject []) user_settings_cols

user_settings_cols :: [(UserSettings -> JSObject JSValue -> Result UserSettings
                       ,UserSettings -> JSObject JSValue -> JSObject JSValue)]
user_settings_cols = [simple "id_column" id_col
                     ,simple "info_columns" info_cols
                     ,(get_simple_f "ec_column" ec_col id, set_maybe "ec_column" ec_col)
                     ,simple "replicates" replicates
                     ,simple "column_names" col_names
                     ,simple "replicate_names" replicate_names
                     ,simple "csv_format" csv_format
                     ,simple "locked" locked
                     ,simple "skip" skip
                     ,simple_with_def "init_select" init_select []
                     ,simple_with_def "hide_columns" hide []
                     ,simple_with_def "name" title ""
                     ]
  where
    simple :: JSON a => String -> (Lens' UserSettings a)
           -> (UserSettings -> JSObject JSValue -> Result UserSettings
              ,UserSettings -> JSObject JSValue -> JSObject JSValue)
    simple name lens = (get_simple name lens, set_simple name lens)
    get_simple name lens u obj = valFromObj name obj >>= \v -> return $ set lens v u
    set_simple name lens u obj = set_field obj name (showJSON $ u ^. lens)

    simple_with_def :: JSON a => String -> (Lens' UserSettings a) -> a
                    -> (UserSettings -> JSObject JSValue -> Result UserSettings
                       ,UserSettings -> JSObject JSValue -> JSObject JSValue)
    simple_with_def name lens def = (get_simple_f name lens (fromMaybe def), set_simple name lens)

    -- | get_simple_f is like get_simple, but allows the user to specify what to do with a missing key
    get_simple_f name lens f u obj = Ok $ case valFromObj name obj of
                                            (Ok v) -> set lens (f $ Just v) u
                                            _      -> set lens (f $ Nothing) u

    -- | set_maybe is like set_simple, but Nothing will delete the key
    set_maybe name lens u obj = case u ^. lens of
                                  Nothing -> del_field obj name
                                  Just v  -> set_field obj name (showJSON v)

-- | Delete a field from a JSObject.  To complement get_field and set_field
del_field :: JSObject a -> String -> JSObject a
del_field obj name = toJSObject . filter ((/=name).fst) $ fromJSObject obj

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
initSettings code = Settings { code = code
                             , remote_addr = ""
                             , created = error "Must set created"
                             , user_settings = defUserSettings
                             }

get_id_column :: Settings -> Int
get_id_column settings = user_settings settings ^. id_col

get_ec_column :: Settings -> Maybe Int
get_ec_column settings = user_settings settings ^. ec_col

get_info_columns :: Settings -> [Int]
get_info_columns settings = user_settings settings ^. info_cols

get_replicates :: Settings -> [(Int,[Int])]
get_replicates settings = user_settings settings ^. replicates

is_locked :: Settings -> Bool
is_locked settings = user_settings settings ^. locked

get_csv_format :: Settings -> String
get_csv_format settings = if user_settings settings ^. csv_format then "CSV" else "TAB"

get_counts_file :: Settings -> FilePath
get_counts_file s = countsFile $ getCode s

get_counts_skip :: Settings -> Int
get_counts_skip s = user_settings s ^. skip

get_user_settings :: Settings -> UserSettings
get_user_settings s = user_settings s

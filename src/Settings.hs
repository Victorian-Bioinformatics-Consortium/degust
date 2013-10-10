{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Settings
    ( Settings
    , fromOk
    , strToCode, codeToStr, settingsFile, annotFile
    , readSettings, writeUserSettings, getCode

    , createSettings
    , get_replicates, get_counts_file, get_counts_skip, get_user_settings
    , get_info_columns, get_ec_column
    , is_locked, get_csv_format
    , get_js_user_settings, get_min_counts
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
                         , locked :: Bool -- ^ True to allow a public example
                        } deriving Show

data UserSettings =
  UserSettings { _skip :: Int -- ^ Number of rows to skip (not used frontend?)
               , _csv_format :: Bool  -- ^ True for CSV, False for TAB

               , _ec_col :: Maybe String -- ^ Optional EC number column (0-based)
               , _info_cols :: [String] -- ^ Optional list of columns to include in display

               -- Only used on the frontent
               , _hide :: [String] -- ^ Used in the frontend to hide some columns
               , _title :: String -- ^ Title to use in UI

               , _analyze_server_side :: Bool -- ^ To be analysed server side?

               -- Used for server-side analysis
               , _init_select :: [String] -- ^ Replicates to initially select in UI
               , _replicates :: [(String, [String])] -- ^ Definition of replicates
               , _min_counts :: Maybe Int -- ^ Minimum number of reads across all replicates otherwise ignored

               -- Used for data already analysed by user
               , _fc_cols :: [String] -- ^ Columns containing FC info
               , _primary :: String   -- ^ Name for primary condition
               , _avg_col :: String   -- ^ Column name for average expression
               , _fdr_col :: String   -- ^ Column name for False Discovery Rate
               } deriving Show
makeLenses ''UserSettings

defUserSettings = UserSettings { _ec_col = Nothing
                               , _info_cols = []
                               , _replicates = []
                               , _init_select = []
                               , _hide = []
                               , _csv_format = True
                               , _skip = 0
                               , _title = ""
                               , _min_counts = Nothing
                               , _fc_cols = []
                               , _primary = ""
                               , _avg_col = ""
                               , _analyze_server_side = True
                               , _fdr_col = ""
                               }

instance JSON Settings where
    -- readJSON :: JSValue -> Result Settings
    readJSON (JSObject obj) = Settings <$> (strToCode <$> valFromObj "code" obj)
                                       <*> valFromObj "remote_addr" obj
                                       <*> valFromObj "created" obj
                                       <*> valFromObj "user_settings" obj
                                       <*> valFromObj "locked" obj
    readJSON _ = Error "Expect object"

    -- showJSON :: Settings -> JSValue
    showJSON s = makeObj [("code", showJSON . codeToStr . code $ s)
                          ,("remote_addr", showJSON $ remote_addr s)
                          ,("created", showJSON $ created s)
                          ,("user_settings",showJSON $ user_settings s)
                          ,("locked",showJSON $ locked s)
                          ]

instance JSON UserSettings where
    readJSON (JSObject obj) = chkUserSettingsValid =<< foldM (\u (get,_) -> get u obj) defUserSettings user_settings_cols
    showJSON u = JSObject $ foldl' (\obj (_,set) -> set u obj) (toJSObject []) user_settings_cols

chkUserSettingsValid :: UserSettings -> Result UserSettings
chkUserSettingsValid s = if any invalidChar allColumns
                           then Error "Invalid character in column name"
                           else return s
  where
    invalidChar :: String -> Bool
    invalidChar str = not . null $ intersect "\\'\"" str
    allColumns :: [String]
    allColumns = (map fst $ s ^. replicates) ++ (concatMap snd $ s ^. replicates)
                 ++ maybeToList (s ^. ec_col)
                 ++ s ^. info_cols
                 ++ s ^. init_select


user_settings_cols :: [(UserSettings -> JSObject JSValue -> Result UserSettings
                       ,UserSettings -> JSObject JSValue -> JSObject JSValue)]
user_settings_cols = [simple "info_columns" info_cols
                     ,(get_simple_f "ec_column" ec_col id, set_maybe "ec_column" ec_col)
                     ,simple "replicates" replicates
                     ,simple "csv_format" csv_format
                     ,simple "skip" skip
                     ,simple_with_def "init_select" init_select []
                     ,simple_with_def "hide_columns" hide []
                     ,simple_with_def "name" title ""
                     ,(get_simple_f "min_counts" min_counts id, set_maybe "min_counts" min_counts)
                     ,simple_with_def "fc_columns" fc_cols []
                     ,simple_with_def "primary_name" primary ""
                     ,simple_with_def "avg_column" avg_col ""
                     ,simple_with_def "analyze_server_side" analyze_server_side True
                     ,simple_with_def "fdr_column" fdr_col ""
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
                             , locked = False
                             }

get_ec_column :: Settings -> Maybe String
get_ec_column settings = user_settings settings ^. ec_col

get_info_columns :: Settings -> [String]
get_info_columns settings = user_settings settings ^. info_cols

get_replicates :: Settings -> [(String,[String])]
get_replicates settings = user_settings settings ^. replicates

is_locked :: Settings -> Bool
is_locked settings = locked settings

get_csv_format :: Settings -> String
get_csv_format settings = if user_settings settings ^. csv_format then "CSV" else "TAB"

get_counts_file :: Settings -> FilePath
get_counts_file s = countsFile $ getCode s

get_counts_skip :: Settings -> Int
get_counts_skip s = user_settings s ^. skip

get_min_counts :: Settings -> Int
get_min_counts s = fromMaybe 0 (user_settings s ^. min_counts)

get_user_settings :: Settings -> UserSettings
get_user_settings s = user_settings s

-- | Turn the user settings into JS.  Mostly just 'encode' but adds the 'locked' field from
-- the outer object
get_js_user_settings :: Settings -> String
get_js_user_settings s = -- encode $ get_user_settings s
    case showJSON $ get_user_settings s of
      JSObject obj -> encode $ set_field obj "locked" (showJSON $ is_locked s)

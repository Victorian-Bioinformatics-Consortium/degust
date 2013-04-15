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
                         , user_settings :: JSObject JSValue
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

writeUserSettings :: Settings -> JSObject JSValue -> IO ()
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

makeObj' :: JSON a => [(String, a)] -> JSObject JSValue
makeObj' lst = toJSObject $ map (\(a,b) -> (a,showJSON b)) lst

----------------------------------------------------------------------

getCode :: Settings -> Code
getCode settings = code settings

fromOk :: Show t => Result t -> t
fromOk (Ok r) = r
fromOk x = error $ "json parse failure : "++show x

fromResult :: t -> Result t -> t
fromResult _ (Ok r) = r
fromResult x _ = x

----------------------------------------------------------------------
initSettings :: Code -> Settings
initSettings code = let userSettings = [("replicates", showJSON ([] :: [String]))
                                       ,("skip", showJSON (0::Int))
                                       ]
                    in Settings { code = code
                                , remote_addr = ""
                                , created = error "Must set created"
                                , user_settings = makeObj' userSettings
                                }

get_id_column :: Settings -> String
get_id_column settings = fromOk $ valFromObj "id_column" $ user_settings settings

get_ec_column :: Settings -> Maybe String
get_ec_column settings = fromResult Nothing $ valFromObj "ec_column" $ user_settings settings

get_info_columns :: Settings -> [String]
get_info_columns settings = fromResult [] $ valFromObj "info_columns" $ user_settings settings

get_replicates :: Settings -> [(String,[String])]
get_replicates settings = fromJSObject $ fromOk $ valFromObj "replicates" $ user_settings settings

is_locked :: Settings -> Bool
is_locked settings = fromResult False $ valFromObj "locked" $ user_settings settings

get_csv_format :: Settings -> String
get_csv_format settings = fromResult "CSV" $ valFromObj "fmt" $ user_settings settings

get_counts_file :: Settings -> FilePath
get_counts_file s = countsFile $ getCode s

get_counts_skip :: Settings -> Int
get_counts_skip s = fromOk $ valFromObj "skip" $ user_settings s

get_user_settings :: Settings -> JSObject JSValue
get_user_settings s = user_settings s

module Settings
    ( Settings
    , fromResult
    , strToCode, codeToStr
    , readSettings

    , createSettings
    , get_replicates, counts_file, counts_skip
    ) where

import Data.List
import System.FilePath
import Text.JSON
import qualified Data.ByteString.Lazy as BS

import Utils

newtype Code = Code {codeToStr :: String}

type Settings = JSObject JSValue

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

user_dir :: FilePath
user_dir = "user-files"

-- globalSettings :: MVar Settings
-- globalSettings = unsafePerformIO $ newEmptyMVar

readSettings :: Code -> IO Settings
readSettings code = do
  str <- Prelude.readFile $ settingsFile code
  let ss = decode . dropWhile (' '==) . unlines . tail . lines $ str
  return $ fromResult ss

writeSettings :: Code -> Settings -> IO ()
writeSettings code settings
    | not valid = error "Invalid settings"
    | otherwise = do
        let str = "window.settings = \n" ++ encode settings
        Prelude.writeFile (settingsFile code) str
  where
    valid = True

createSettings ::  BS.ByteString -> [(String,String)] -> IO Code
createSettings dat extra = do
  (file, _) <- newRandFile user_dir
  let code = strToCode (takeFileName file)
  BS.writeFile (countsFile code) dat
  writeSettings code (mergeSettings (initSettings code) (makeObj' extra))
  return code

makeObj' lst = toJSObject $ map (\(a,b) -> (a,showJSON b)) lst

mergeSettings :: Settings -> Settings -> Settings
mergeSettings a b = toJSObject $ fromJSObject a ++ fromJSObject b

----------------------------------------------------------------------

getCode :: Settings -> Code
getCode settings = strToCode $ fromResult $ valFromObj "code" settings

fromResult (Ok r) = r
fromResult x = error $ "json parse failure : "++show x

----------------------------------------------------------------------
initSettings :: Code -> Settings
initSettings (Code str) = toJSObject [("replicates", showJSON ([] :: [String]))
                                     ,("code", showJSON str)
                                     ,("skip", showJSON (0::Int))
                                     ]

get_replicates :: Settings -> [(String,[String])]
get_replicates settings = fromResult $ valFromObj "replicates" settings


counts_file :: Settings -> FilePath
counts_file s = countsFile $ getCode s

counts_skip :: Settings -> Int
counts_skip s = fromResult $ valFromObj "skip" s



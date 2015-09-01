#!/usr/bin/env runghc

{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, BangPatterns #-}

{- Test on the command line:
      QUERY_STRING='code=d742e14cdf68a896d426376604046227&query=settings' ./r-json
      QUERY_STRING=query=dge ./r-json.hs
      QUERY_STRING=query=kegg_titles ./r-json.hs
-}

import Prelude hiding (catch)
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe,maybeToList)
import Data.List
import Data.List.Split
import Network.CGI
import Control.Monad (when,filterM)
import Control.Concurrent (forkIO)
import Control.Exception (try, IOException, catch)
import System.Exit (ExitCode(..))
import qualified System.IO.Strict as SIO (run, hGetContents, readFile)
import System.IO (openTempFile, hClose, hPutStr, hPutStrLn, stderr)
import System.Process
import System.Directory (removeFile, doesFileExist, getModificationTime, getDirectoryContents)
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BS
import Text.Regex.PCRE
import Text.JSON (decode,Result(..))
import Data.Time (getCurrentTime)
import Text.Printf

import Data.ByteString.Lazy (pack)
import Data.Digest.Pure.MD5 (md5)

import R_Functions
import Utils
import Settings

debug :: Bool
debug = False

_log_stderr :: (MonadIO m) => String -> m ()
_log_stderr = liftIO . hPutStrLn stderr

main :: IO ()
main = do
  runCGI $ catchCGI doQuery
                 (\ex -> do logMsg $ "Exception : "++show ex
                            outputError 400 "Bad request" []
                 )

doQuery :: CGI CGIResult
doQuery = do query <- getInput "query"
             case query of
               Nothing -> redirectMainPage -- Support old style links
               Just "settings" -> getJSSettings
               Just "csv" -> getAllCSV
               Just "partial_csv" -> getPartialCSV
               Just "dge" -> cached "text/csv" getDGE
               Just "dge_r_code" -> getDGERCode
               Just "annot" -> cached "text/csv" getAnnot
               Just "kegg_titles" -> cached "text/csv" getKeggTitles
               Just "clustering" -> cached "text/csv" getClustering
               Just "upload" -> doUpload
               Just "save" -> saveSettings
               x -> let msg = "Unknown query : "++show x in logMsg msg >> outputNotFound msg

hash :: String -> String
hash key = show . md5 . pack . map (fromIntegral . fromEnum) $ key

newest_file_time :: IO String
newest_file_time = do
  ts1 <- mapM getModificationTime =<< filterM doesFileExist =<< getDirectoryContents "."
  ts2 <- mapM getModificationTime =<< filterM doesFileExist =<< getDirectoryContents "r-templates"
  -- hPrintf stderr "%s\n" (show . maximum $ ts)
  return $ show . maximum $ ts1 ++ ts2

cached :: String -> CGI String -> CGI CGIResult
cached typ act = do
  inputs <- getInputs
  settings <- findSettings
  newest <- liftIO newest_file_time
  let cache_file = "cached/"++hash (show inputs ++ show settings ++ newest)
  out <- liftIO $ getFile cache_file
  out2 <- case out of
            Right out -> do -- liftIO $ hPutStrLn stderr $ "using cache"
                            return out
            Left _err -> do -- liftIO $ putStrLn $ "cache : "++show err
                     out <- act
                     liftIO $ when (not $ null out) $ writeFile cache_file out
                     return out
  setHeader "Content-type" typ
  output out2
  where
    getFile :: FilePath -> IO (Either IOException String)
    getFile = try . SIO.run . SIO.readFile

redirectMainPage :: CGI CGIResult
redirectMainPage = do
    code <- strToCode . fromMaybe (error "No Code") <$> getInput "code"
    let url = "compare.html?code="++codeToStr code
    setHeader "Content-type" "text/html"
    output $ printf "Redirecting...<br>Click <a href='%s'>here</a> \
                    \if it doesn't happen automatically.\
                    \<meta http-equiv=\"refresh\" content=\"0;URL='%s'\">" url url
    -- setStatus 302 "Found"
    -- redirect $ url


-- | Return a string with the first 20 lines of the csv counts file
getPartialCSV :: CGI CGIResult
getPartialCSV = do
    settings <- findSettings
    setHeader "Content-type" "text/csv"
    counts <- liftIO $ BS.readFile (get_counts_file settings)
    outputFPS . bsUnlines . take 20 . bsLines $ counts
  where
    bsLines = BS.split (BS.head "\n")
    bsUnlines = BS.intercalate "\n"

-- | Return complete CSV file
getAllCSV :: CGI CGIResult
getAllCSV = do
    settings <- findSettings
    setHeader "Content-type" "text/csv"
    counts <- liftIO $ BS.readFile (get_counts_file settings)
    outputFPS counts

getJSSettings :: CGI CGIResult
getJSSettings = do
    settings <- findSettings
    setHeader "Content-type" "application/javascript"
    let settingStr = get_js_user_settings settings
    setHeader "Content-type" "text/json"
    output settingStr

type DGEMethod = Settings -> [String] -> FilePath -> IO String

getDGEMethod :: CGI DGEMethod
getDGEMethod = do
    typ <- getInput "method"
    return $ case typ of
               Nothing -> voomR
               Just "voom" -> voomR
               Just "voom-weights" -> voomWeightsR
               Just "edgeR" -> edgeR
               x -> error $ "Unknown method : "++show x

getDGERCode :: CGI CGIResult
getDGERCode = do
    method <- getDGEMethod
    s <- getRCodeWithFields method
    setHeader "Content-type" "text/plain"
    output s

getDGE :: CGI String
getDGE = do
    method <- getDGEMethod
    getWithFields method

getClustering :: CGI String
getClustering = getWithFields clusteringR

getAnnot :: CGI String
getAnnot = do
    settings <- findSettings
    liftIO $ Prelude.readFile $ annotFile $ getCode settings

getWithFields :: DGEMethod -> CGI String
getWithFields act = do jsonString <- getInput "fields"
                       let flds = decode $ fromMaybe (error "No fields") jsonString
                       case flds of
                         Error e -> logMsg ("ERR:"++e) >> return ""
                         Ok [] -> return ""
                         Ok [_] -> return ""
                         Ok flds -> do (res,_) <- runR (\s -> act s flds)
                                       return res

getRCodeWithFields :: DGEMethod -> CGI String
getRCodeWithFields act = do jsonString <- getInput "fields"
                            let flds = decode $ fromMaybe (error "No fields") jsonString
                            case flds of
                              Error e -> logMsg ("ERR:"++e) >> return ""
                              Ok [] -> return ""
                              Ok [_] -> return ""
                              Ok flds -> do s <- findSettings
                                            str <- liftIO $ act s flds "out-file.csv"
                                            ver_cmd <- liftIO $ versions
                                            (_,ver_str) <- runR (\_ _ -> versions)
                                            return $ str ++ ver_cmd ++ ver_str
  where
    versions =  render "versions.R" []

findSettings :: CGI Settings
findSettings = do
  code <- strToCode . fromMaybe (error "No Code") <$> getInput "code"
  liftIO $ readSettings code


getKeggTitles :: CGI String
getKeggTitles =  liftIO $ do
    ls <- map (splitOn "\t") . lines <$> Prelude.readFile "kegg/pathway/map_title.tab"
    withEC <- mapM lookupEC ls
    return $ unlines (map (intercalate "\t") $ header : withEC)
  where
    header = ["code","title","ec"]
    lookupEC [] = return []
    lookupEC line@(mp:_) = do
        xml <- catch (Prelude.readFile $ "kegg/kgml/map/map"++mp++".xml")
                     ((\_ -> return "" ) :: IOException -> IO String)
        let ecs = xml =~ ("name=\"ec:([.\\d]+)\"" ::String) :: [[String]]
        return $ line ++ [intercalate " " $ map (!!1) ecs]

-- | Portable count lines.  Handle just LF (unix), just CR+LF (windows), just CR (macos)
-- Actually not exact since won't count last line correctly if doesn't finish with a eol marker
countLines :: T.Text -> Int
countLines txt = snd $ T.foldl' go (False,0) txt
  where
    go (True, !num)  '\n' = (False,num)    -- Don't increment on CR+LF (we incremented on the CR already)
    go (False, !num) '\n' = (False, num+1) -- Increment LF
    go (_, !num) '\r' = (True, num+1)      -- Increment on CR
    go (_, !num) _ = (False, num)          -- Any other non-line charater

doUpload :: CGI CGIResult
doUpload = do
    dat <- getInputFPS "filename"
    case dat of
      Nothing -> error "No input data"
      Just dat -> case isValid dat of
                    Nothing -> save dat
                    Just msg -> do setHeader "Content-type" "text/html"; output msg
  where
    chkLines txt = let n = countLines txt
                   in if n<10
                      then Just "Too few lines in the file"
                      else if n>70000
                           then Just "Too many lines in the file"
                           else Nothing

    isValid dat = case decodeUtf8' dat of
                    Left _ex  -> Just "File does not appear to be text"
                    Right txt -> chkLines txt

    save dat = do remote_ip <- remoteAddr
                  now <- liftIO getCurrentTime
                  code <- liftIO $ createSettings dat remote_ip now
                  logMsg $ "New upload from "++remote_ip++" : "++codeToStr code
                  let url = "config.html?code="++codeToStr code
                  setHeader "Content-type" "text/html"
                  output $ printf "Redirecting...<br>Click <a href='%s'>here</a> \
                                  \if it doesn't happen automatically.\
                                  \<meta http-equiv=\"refresh\" content=\"0;URL='%s'\">" url url

                  -- setStatus 302 "Found"
                  -- redirect $ url

saveSettings :: CGI CGIResult
saveSettings = do
    oldSettings <- findSettings
    if is_locked oldSettings
    then logMsg ("ERR: settings are locked") >> outputMethodNotAllowed [""]
    else do
        jsonString <- getInput "settings"
        let mNew = decode $ fromMaybe (error "No data") jsonString
        case mNew of
          Error e -> logMsg ("ERR:"++e) >> outputMethodNotAllowed [""]
          Ok new -> do liftIO $ writeUserSettings oldSettings new
                       setHeader "Content-type" "text/json"
                       output "{\"result\": \"ok!\"}"

-- Takes a function "gen_script".  gen_script should take 2 parameters
--  settings and an output filename.  It should create an R script that will write
--  to that file.
-- This returns a pair.  The first is the contents of the output file, the second is stdout
runR :: (Settings -> FilePath -> IO String) -> CGI (String,String)
runR gen_script = do
  settings <- findSettings
  liftIO $ do
    (inF, hIn) <- openTempFile "tmp" "Rin.tmp"
    (outF, hOut) <- openTempFile "tmp" "Rout.tmp"
    let errFname = outF ++ "-err"

    script <- gen_script settings outF
    hPutStr hIn script
    hClose hIn
    (_,stdoutH,stderrH,pid) <- runInteractiveProcess "Rscript" ["--vanilla",inF] Nothing (Just [])
    forkIO $ do str <- SIO.run $ SIO.hGetContents stderrH
                case str of {"" -> return (); x -> writeFile errFname x}
                hClose stderrH

    stdout <- SIO.run $ SIO.hGetContents stdoutH

    exCode <- waitForProcess pid

    res <- SIO.run $ SIO.hGetContents hOut

    when (exCode == ExitSuccess && not debug) $
       mapM_ delFile [inF, outF, errFname]

    return (res,stdout)
  where
    delFile :: FilePath -> IO (Either IOException ())
    delFile = try . removeFile

-- instance ToText Int where toText = toText . show

-- | Convert the array of string columns to R array of string names
colsToRList :: [String] -> String
colsToRList ls = intercalate "," . map (\c -> "'"++c++"'") $ ls

-- | Build an R list of the columns
columns :: Settings -> String
columns settings = let columns = concatMap snd $ get_replicates settings
                   in "c("++colsToRList columns++")"

-- | Build the R design matrix
design :: Settings -> String
design settings = "matrix(data=c("++intercalate "," (concat allCols)++")"
                  ++ ", nrow="++show (length columns)++", ncol="++show (length reps)
                  ++ ", dimnames = list(c(), c("++colsToRList conditions++"))"
                  ++ ")"
  where
    conditions = map fst $ get_replicates settings
    columns = concatMap snd $ get_replicates settings
    reps = get_replicates settings
    oneCol vs = map (\c -> if c `elem` vs then "1" else "0") columns
    allCols = map (oneCol . snd) reps

-- | Build an R contrast matrix
contMatrix :: Settings -> [String] -> String
contMatrix _ [] = error "Unexpected empty fields to contMatrix"
contMatrix settings (c1:cs) =  "matrix(data=c("++intercalate "," (concat allCols)++")"
                  ++ ", nrow="++show (length conditions)++", ncol="++show (length cs)
                  ++ ", dimnames = list(c(), c("++colsToRList cs++")))"
  where
    conditions = map fst $ get_replicates settings
    oneCol col = map (\c -> if c==col then "1" else if c==c1 then "-1" else "0") conditions
    allCols = map oneCol cs

commonVars :: Settings -> [(String, String)]
commonVars settings =
    [("sep_char", case get_csv_format settings of {"TAB" -> "\\t" ; "CSV" -> ","; _ -> ","})
    ,("counts_file", get_counts_file settings)
    ,("counts_skip", show $ get_counts_skip settings)
    ,("columns", columns settings)
    ,("min_counts", show $ get_min_counts settings)
    ,("design", design settings)
    ]


voomR :: DGEMethod
voomR = voom_or_edgeR "voom.R"

voomWeightsR :: DGEMethod
voomWeightsR = voom_or_edgeR "voom-weights.R"

edgeR :: DGEMethod
edgeR = voom_or_edgeR "edgeR.R"

voom_or_edgeR :: String -> DGEMethod
voom_or_edgeR r_file settings cs outFile = render r_file vars
  where
    vars = commonVars settings ++
           [("file", outFile)
           ,("cont_matrix", contMatrix settings cs)
           ,("export_cols", colsToRList export_cols)
           ]
    export_cols =  get_info_columns settings
                     ++ concatMap snd (get_replicates settings)
                     ++ maybeToList (get_ec_column settings)


clusteringR :: Settings -> [String] -> FilePath -> IO String
clusteringR settings cs outFile = render "clustering.R" vars
  where
    vars = commonVars settings ++
           [("file", outFile)
           ,("columns", colsToRList cs)
           ]

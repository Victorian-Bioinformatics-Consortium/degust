#!/usr/bin/env runghc

{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, BangPatterns #-}

{- Test on the command line:
      QUERY_STRING=query=code=2456dfc6c20a6ff4b43129fd6ac00ac5&query=counts ./r-json.hs
      QUERY_STRING=query=dge ./r-json.hs
      QUERY_STRING=query=kegg_titles ./r-json.hs
-}

import Prelude hiding (catch)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust,fromMaybe,maybeToList)
import Data.List
import Data.List.Split
import Network.CGI
import Control.Monad (when,filterM)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,readMVar,MVar)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (try, handle, IOException, catch)
import System.Exit (ExitCode(..))
import qualified System.IO.Strict as SIO (run, hGetContents, readFile)
import System.IO (openTempFile, hClose, hPutStr, hPutStrLn, stderr)
import System.Process
import System.Directory (removeFile, doesFileExist, getModificationTime, getDirectoryContents)
import Text.Shakespeare.Text
import Text.Hamlet
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.ByteString.Lazy as BS
import Text.Regex.PCRE
import Text.JSON (encode,decode,Result(..))
import Data.Time (getCurrentTime)
import Text.Printf

import Data.ByteString.Lazy (pack)
import Data.Digest.Pure.MD5 (md5)

import System.Environment (getProgName)

import Utils
import Settings

debug = False

_log_stderr :: (MonadIO m) => String -> m ()
_log_stderr = liftIO . hPutStrLn stderr

main = do
  runCGI $ catchCGI doQuery
                 (\ex -> do logMsg $ "Exception : "++show ex
                            outputError 400 "Bad request" []
                 )

fromRight (Right r) = r
fromRight (Left e) = error e

--doQuery :: CGI String
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
  ts <- mapM getModificationTime =<< filterM doesFileExist =<< getDirectoryContents "."
  -- hPrintf stderr "%s\n" (show . maximum $ ts)
  return $ show . maximum $ ts

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
            Left err -> do -- liftIO $ putStrLn $ "cache : "++show err
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

getDGERCode :: CGI CGIResult
getDGERCode = do s <- getRCodeWithFields dgeR
                 setHeader "Content-type" "text/plain"
                 output s

getDGE :: CGI String
getDGE = getWithFields dgeR

getClustering :: CGI String
getClustering = getWithFields clusteringR

getAnnot :: CGI String
getAnnot = do
    settings <- findSettings
    liftIO $ Prelude.readFile $ annotFile $ getCode settings

getWithFields :: (Settings -> [String] -> FilePath -> String) -> CGI String
getWithFields act = do jsonString <- getInput "fields"
                       let flds = decode $ fromMaybe (error "No fields") jsonString
                       case flds of
                         Error e -> logMsg ("ERR:"++e) >> return ""
                         Ok [] -> return ""
                         Ok [_] -> return ""
                         Ok flds -> runR (\s -> act s flds)

getRCodeWithFields :: (Settings -> [String] -> FilePath -> String) -> CGI String
getRCodeWithFields act = do jsonString <- getInput "fields"
                            let flds = decode $ fromMaybe (error "No fields") jsonString
                            case flds of
                              Error e -> logMsg ("ERR:"++e) >> return ""
                              Ok [] -> return ""
                              Ok [_] -> return ""
                              Ok flds -> do s <- findSettings
                                            return $ act s flds "out-file.csv"

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
    lookupEC line@(mp:_) = do xml <- catch (Prelude.readFile $ "kegg/kgml/map/map"++mp++".xml")
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
    go (_, !num) c = (False, num)          -- Any other non-line charater

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

runR :: (Settings -> FilePath -> String) -> CGI String
runR script = do
    settings <- findSettings
    liftIO $ do
    (inF, hIn) <- openTempFile "tmp" "Rin.tmp"
    (outF, hOut) <- openTempFile "tmp" "Rout.tmp"
    let errFname = outF ++ "-err"

    hPutStr hIn (script settings outF)
    hClose hIn
    (_,out,err,pid) <- runInteractiveProcess "Rscript" ["--vanilla",inF] Nothing
                             (Just [("R_LIBS_SITE","/bio/sw/R:")])
    forkIO $ do str <- SIO.run $ SIO.hGetContents err
                case str of {"" -> return (); x -> writeFile errFname x}
                hClose err

    exCode <- waitForProcess pid

    out <- SIO.run $ SIO.hGetContents hOut

    when (exCode == ExitSuccess && not debug) $
       mapM_ delFile [inF, outF, errFname]

    return out
  where
    delFile :: FilePath -> IO (Either IOException ())
    delFile = try . removeFile

-- instance ToText Int where toText = toText . show

getCountsR :: Settings -> FilePath -> String
getCountsR settings file =
    let extra_cols = nub $ get_info_columns settings ++
                           maybeToList (get_ec_column settings)
    in
  T.unpack . toLazyText $ [text|
  #{initR settings}
  counts <- cbind(x[,c(#{colsToRList extra_cols})], counts)
  write.csv(counts, file="#{file}", row.names=FALSE)
 |] ()

-- | Convert the array of string columns to R array of string names
colsToRList :: [String] -> String
colsToRList ls = intercalate "," . map (\c -> "'"++c++"'") $ ls

-- | Build an R list of the columns
columns settings = let columns = concatMap snd $ get_replicates settings
                   in "c("++colsToRList columns++")"

-- | Build the R design matrix
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
contMatrix settings (c1:cs) =  "matrix(data=c("++intercalate "," (concat allCols)++")"
                  ++ ", nrow="++show (length conditions)++", ncol="++show (length cs)
                  ++ ", dimnames = list(c(), c("++colsToRList cs++")))"
  where
    conditions = map fst $ get_replicates settings
    oneCol col = map (\c -> if c==col then "1" else if c==c1 then "-1" else "0") conditions
    allCols = map oneCol cs

-- | Common R setup code
initR settings =
    let sep_char = case get_csv_format settings of {"TAB" -> "\\t" ; "CSV" -> ","; _ -> ","} :: String
    in T.unpack . toLazyText $ [text|
  library(limma)
  library(edgeR)

  x<-read.delim('#{get_counts_file settings}',skip=#{get_counts_skip settings}, sep="#{sep_char}", check.names=FALSE)
  counts <- x[,#{columns settings}]
  keep <- apply(counts, 1, max) >= #{get_min_counts settings}
  x <- x[keep,]
  counts <- counts[keep,]
  design <- #{design settings}
 |] ()

dgeR :: Settings -> [String] -> FilePath -> String
dgeR settings cs file =
  let export_cols =  get_info_columns settings
                     ++ concatMap snd (get_replicates settings)
                     ++ maybeToList (get_ec_column settings)
  in T.unpack . toLazyText $ [text|
  #{initR settings}

  nf <- calcNormFactors(counts)
  y<-voom(counts, design, plot=FALSE,lib.size=colSums(counts)*nf)

  cont.matrix <- #{contMatrix settings cs}

  fit <- lmFit(y,design)
  fit2 <- contrasts.fit(fit, cont.matrix)
  fit2 <- eBayes(fit2)

  out <- topTable(fit2, n=Inf, sort.by='none')

  out2 <- cbind(fit2$coef,
                out[, c('adj.P.Val','AveExpr')],
                x[, c(#{colsToRList export_cols})] )

  write.csv(out2, file="#{file}", row.names=FALSE,na='')
 |] ()

clusteringR settings cs file =
    T.unpack . toLazyText $ [text|
  #{initR settings}

  nf <- calcNormFactors(counts)
  y<-voom(counts, design, plot=FALSE,lib.size=colSums(counts)*nf)

  fit <- lmFit(y,design)

  # Cluster ordering...
  library(seriation)
  d <- dist(fit$coefficients[,c(#{colsToRList cs})])
  c <- list(hclust = hclust(d))
  s <- seriate(d, method='OLO', control=c)
  order <- get_order(s[[1]])
  write.csv(list(id=order), file="#{file}", row.names=FALSE)
  |] ()

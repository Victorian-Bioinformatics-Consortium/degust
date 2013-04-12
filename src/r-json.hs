#!/usr/bin/env runghc

{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}

{- Test on the command line:
      QUERY_STRING=query=counts ./r-json.hs
      QUERY_STRING=query=dge ./r-json.hs
      QUERY_STRING=query=kegg_titles ./r-json.hs
-}

import Prelude hiding (catch)
import Control.Applicative ((<$>))
import Data.Maybe (fromJust,fromMaybe)
import Data.List
import Data.List.Split
import Network.CGI
import Control.Monad (when)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,readMVar,MVar)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (try, handle, IOException, catch)
import System.Exit (ExitCode(..))
import System.IO.Strict (hGetContents, readFile)
import System.IO (openTempFile, hClose, hPutStr, hPutStrLn, stderr)
import System.Process
import System.Directory (removeFile)
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

import Utils
import Settings

debug = False

main = do
  runCGI $ catchCGI doQuery
                 (\ex -> do logMsg $ "Exception : "++show ex
                            outputError 400 "Bad request" []
                 )

fromRight (Right r) = r
fromRight (Left e) = error e

urlForCode code = "r-json.cgi?code="++codeToStr code

--doQuery :: CGI String
doQuery = do query <- getInput "query"
             case query of
               Nothing -> getPage "compare.html"
               Just "settings" -> getJSSettings
               Just "config" -> getPage "config.html"
               Just "csv" -> getCSV
               Just "dge" -> cached "text/csv" getDGE
               Just "counts" -> cached "text/csv" getCounts
               Just "annot" -> cached "text/csv" getAnnot
               Just "kegg_titles" -> cached "text/csv" getKeggTitles
               Just "clustering" -> cached "text/csv" getClustering
               Just "upload" -> doUpload
               Just "save" -> saveSettings
               x -> let msg = "Unknown query : "++show x in logMsg msg >> outputNotFound msg

hash key = show . md5 . pack . map (fromIntegral . fromEnum) . show $ key

cached :: (MonadIO m, MonadCGI m) => String -> m String -> m CGIResult
cached typ act = do
  inputs <- getInputs
  let cache_file = "cached/"++hash inputs
  out <- liftIO $ getFile cache_file
  out2 <- case out of
            Right out -> return out
            Left err -> do -- liftIO $ putStrLn $ "cache : "++show err
                     out <- act
                     liftIO $ when (not $ null out) $ writeFile cache_file out
                     return out
  setHeader "Content-type" typ
  output out2
  where
    getFile :: FilePath -> IO (Either IOException String)
    getFile = try . System.IO.Strict.readFile

-- | Return an HTML page with a substitution for SETTINGS
getPage :: FilePath -> CGI CGIResult
getPage file = do
    html <- liftIO $ Prelude.readFile file
    settings <- findSettings
    setHeader "Content-type" "text/html"
    output $ replace "##SETTINGS##" (urlForCode (getCode settings) ++ "&query=settings") html
  where
    replace s1 s2 str = intercalate s2 $ splitOn s1 str

-- | Return a string with the first 20 lines of the csv counts file
getCSV :: CGI CGIResult
getCSV = do
    settings <- findSettings
    setHeader "Content-type" "text/csv"
    counts <- liftIO $ BS.readFile (get_counts_file settings)
    outputFPS . bsUnlines . take 20 . bsLines $ counts
  where
    bsLines = BS.split (BS.head "\n")
    bsUnlines = BS.intercalate "\n"

getJSSettings :: CGI CGIResult
getJSSettings = do
    settings <- findSettings
    setHeader "Content-type" "application/javascript"
    let settingStr = encode . get_user_settings $ settings
    output $ "window.settings = "++settingStr++"; window.my_code='"++codeToStr (getCode settings)++"';"

getDGE :: CGI String
getDGE = getWithFields dgeR

getClustering :: CGI String
getClustering = getWithFields clusteringR

getCounts :: CGI String
getCounts = runR getCountsR

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

doUpload :: CGI CGIResult
doUpload = do
    dat <- getInputFPS "filename"
    case dat of
      Nothing -> error "No input data"
      Just dat -> case isValid dat of
                    Nothing -> save dat
                    Just msg -> do setHeader "Content-type" "text/html"; output msg
  where
    chkLines txt = let n = length $ T.lines txt
                   in if n<10
                      then Just "Too few lines in the file"
                      else if n>40000
                           then Just "Too many lines in the file"
                           else Nothing

    isValid dat = case decodeUtf8' dat of
                    Left _ex  -> Just "File does not appear to be text"
                    Right txt -> chkLines txt

    save dat = do remote_ip <- remoteAddr
                  now <- liftIO getCurrentTime
                  code <- liftIO $ createSettings dat remote_ip now
                  logMsg $ "New upload from "++remote_ip++" : "++codeToStr code
                  let url = urlForCode code ++ "&query=config"
                  setHeader "Content-type" "text/html"
                  output $ printf "Redirecting...<br>Click <a href='%s'>here</a> \
                                  \if it doesn't happen automatically.\
                                  \<meta http-equiv=\"refresh\" content=\"0;URL='%s'\">" url url

                  -- setStatus 302 "Found"
                  -- redirect $ url

saveSettings :: CGI CGIResult
saveSettings = do
    oldSettings <- findSettings
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
    forkIO $ do str <- hGetContents err
                case str of {"" -> return (); x -> writeFile errFname x}
                hClose err

    exCode <- waitForProcess pid

    out <- hGetContents hOut

    when (exCode == ExitSuccess && not debug) $
       mapM_ delFile [inF, outF, errFname]

    return out
  where
    delFile :: FilePath -> IO (Either IOException ())
    delFile = try . removeFile

instance ToText Int where toText = toText . show

getCountsR :: Settings -> FilePath -> String
getCountsR settings file =
    let extra_cols = nub $ get_id_column settings : get_ec_column settings : get_info_columns settings
    in
  T.unpack . toLazyText $ [text|
  #{initR settings}
  counts <- cbind(x[,c(#{toRStringList extra_cols})], counts)
  write.csv(counts, file="#{file}", row.names=FALSE)
 |] ()

-- | Convert the array of column names into an R list (remember to replace magic characters in the column name)
toRStringList :: [String] -> String
toRStringList ls = intercalate "," . map (\col -> "'"++repl col++"'") $ ls
  where
    repl str = map (\c -> if c `elem` "-:" then '.' else c) str

-- | Build an R list of the columns
columns settings = let columns = concatMap snd $ get_replicates settings
                   in "c("++toRStringList columns++")"

-- | Build the R design matrix
design settings = "matrix(data=c("++intercalate "," (concat allCols)++")"
                  ++ ", nrow="++show (length columns)++", ncol="++show (length reps)
                  ++ ", dimnames = list(c(), c("++toRStringList (map fst reps)++")))"
  where
    columns = concatMap snd $ get_replicates settings
    reps = get_replicates settings
    oneCol vs = map (\c -> if c `elem` vs then "1" else "0") columns
    allCols = map (oneCol . snd) reps

-- | Build an R contrast matrix
contMatrix settings (c1:cs) =  "matrix(data=c("++intercalate "," (concat allCols)++")"
                  ++ ", nrow="++show (length conditions)++", ncol="++show (length cs)
                  ++ ", dimnames = list(c(), c("++toRStringList cs++")))"
  where
    conditions = map fst $ get_replicates settings
    oneCol col = map (\c -> if c==col then "1" else if c==c1 then "-1" else "0") conditions
    allCols = map oneCol cs

-- | Common R setup code
initR settings =
  T.unpack . toLazyText $ [text|
  library(limma)
  library(edgeR)

  x<-read.delim('#{get_counts_file settings}',skip=#{get_counts_skip settings})
  counts <- x[,#{columns settings}]
  design <- #{design settings}
 |] ()

dgeR :: Settings -> [String] -> FilePath -> String
dgeR settings cs file =
    T.unpack . toLazyText $ [text|
  #{initR settings}

  nf <- calcNormFactors(counts)
  y<-voom(counts, design, plot=FALSE,lib.size=colSums(counts)*nf)
  y$genes <- x[c('Feature', 'product', 'gene')]

  cont.matrix <- #{contMatrix settings cs}

  fit <- lmFit(y,design)
  fit2 <- contrasts.fit(fit, cont.matrix)
  fit2 <- eBayes(fit2)

  out <- topTable(fit2, n=Inf)

  abs_vals <- fit$coefficients[,c(#{toRStringList cs})]
  colnames(abs_vals) <- paste("ABS", colnames(abs_vals))
  abs_vals <- data.frame(Feature=fit$genes$Feature, abs_vals)

  out2 <- merge(abs_vals, out[, c('Feature', 'product', 'gene', 'AveExpr', 'adj.P.Val')])

  write.csv(out2, file="#{file}", row.names=FALSE,na='')
 |] ()

clusteringR settings cs file =
    T.unpack . toLazyText $ [text|
  #{initR settings}

  nf <- calcNormFactors(counts)
  y<-voom(counts, design, plot=FALSE,lib.size=colSums(counts)*nf)
  y$genes <- x[c('Feature', 'product')]

  fit <- lmFit(y,design)

  # Cluster ordering...
  library(seriation)
  d <- dist(fit$coefficients[,c(#{toRStringList cs})])
  c <- list(hclust = hclust(d))
  s <- seriate(d, method='OLO', control=c)
  order <- get_order(s[[1]])
  write.csv(list(id=fit$genes$Feature[order]), file="#{file}", row.names=FALSE)
  |] ()

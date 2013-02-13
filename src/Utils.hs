module Utils
    ( newRandFile
    , logMsg
    ) where

import Network.CGI

import System.IO (Handle, hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as BS
import Data.Digest.Pure.MD5 (md5)
import System.Random (randomIO)
import Control.Exception (catch,throw)
import System.Posix.IO (openFd, fdToHandle, OpenMode(..), defaultFileFlags, OpenFileFlags(..))
import Data.Char (ord)
import System.IO.Error (isAlreadyExistsError)

newRandFile :: FilePath -> IO (FilePath, Handle)
newRandFile dir = do
  n <- randomIO :: IO Int
  let str = show . md5 . BS.pack . map (fromIntegral . ord) . show $ n
      fname = dir ++ "/" ++ str
  mfd <- Control.Exception.catch
          (openFd fname WriteOnly (Just 0o600) defaultFileFlags {exclusive=True} >>= return . Just)
          (\e -> if isAlreadyExistsError e then return Nothing else throw e)
  case mfd of
    Nothing -> newRandFile dir
    Just fd -> fdToHandle fd >>= \h -> return (fname, h)

logMsg :: MonadIO m => String -> m ()
logMsg msg = liftIO (hPutStrLn stderr msg)


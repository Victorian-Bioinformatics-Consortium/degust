module R_Functions
    ( render
    ) where

import Control.Applicative
import Data.Maybe
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import Text.Hastache
import Text.Hastache.Context

template_dir :: String
template_dir = "r-templates/"

hConfig :: MuConfig IO
hConfig = MuConfig  emptyEscape (Just template_dir) (Just ".R") (\f -> Just <$> T.readFile f)

render :: FilePath -> [(String, String)] -> IO String
render file ctxt = do
    template <- T.readFile $ template_dir ++ file
    res <- hastacheStr hConfig template (mkStrContext ctx_f)
    return $ LT.unpack res
  where
    ctx_f :: String -> MuType IO
    ctx_f str = MuVariable $ fromMaybe (error $ "Missing variable '"++str++"' for :"++file)
                           $ lookup str ctxt

-- bs2T :: BS.ByteString -> T.Text
-- bs2T bs = T.pack . map w2c . BS.unpack $ bs

{-# LANGUAGE TemplateHaskell #-}

module R_Functions
    ( render
    , countsR
    ) where

import Data.Maybe
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Hastache
import Text.Hastache.Context

import Data.FileEmbed

hConfig :: MuConfig IO
hConfig = MuConfig  emptyEscape Nothing Nothing (\_ -> return Nothing)

render :: BS.ByteString -> [(T.Text, T.Text)] -> IO LT.Text
render bs ctxt = hastacheStr hConfig (bs2T bs) (mkStrContext ctx_f)
  where
    ctx_f :: String -> MuType IO
    ctx_f str = MuVariable $ maybe "" T.unpack $ lookup (T.pack str) ctxt

bs2T :: BS.ByteString -> T.Text
bs2T bs = T.pack . map w2c . BS.unpack $ bs


commonR :: BS.ByteString
commonR = $(embedFile "app/backend/common.R")

countsR :: BS.ByteString
countsR = commonR `BS.append` $(embedFile "app/backend/counts.R")

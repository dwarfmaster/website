{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Data where

import Data.Default
import Data.List
import Data.List.Split    (wordsBy)
import Yesod
import Yesod.Default.Util

import Foundation

getDataR :: String -> Handler (ContentType,Content)
getDataR = getDataFromR "data"

getDataFromR :: String -> String -> Handler (ContentType,Content)
getDataFromR dir media = do
    sendFile tpe $ dir ++ "/" ++ media
 where tpe = case head $ reverse $ wordsBy (== '.') media of
              "png" -> typePng
              "svg" -> typeSvg
              "jpg" -> typeJpeg
              "ogv" -> typeOgv
              _     -> typeOctet

getArticleDataR :: String -> String -> Handler (ContentType,Content)
getArticleDataR art = getDataFromR $ "articles/" ++ art


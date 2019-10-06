{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Query where

import Data.Default
import Yesod
import Yesod.Default.Util

import           Foundation
import qualified Blog         as Blg
import           Articles
import           Handler.Blog

getQueryFromR :: String -> Int -> Handler Html
getQueryFromR query from = if null query then getQueryFromNullR from else do
        let articles = Blg.query blog query
        getBlogR from query articles

getQueryR :: String -> Handler Html
getQueryR = (flip getQueryFromR) 0

getQueryFromNullR :: Int -> Handler Html
getQueryFromNullR from = do
        let (_,articles) = blog
        getBlogR from "" articles

getQueryNullR :: Handler Html
getQueryNullR = getQueryFromNullR 0


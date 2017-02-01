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

getQueryR :: String -> Handler Html
getQueryR query = if null query then getQueryNullR else do
        let articles = Blg.query blog query
        getBlogR articles

getQueryNullR :: Handler Html
getQueryNullR = do
        let (_,articles) = blog
        getBlogR articles


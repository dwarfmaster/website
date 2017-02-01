{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Article where

import Data.Default
import Yesod
import Yesod.Default.Util

import Foundation
import Blog
import Articles
import Handler.Utility

getArticleR :: String -> Handler Html
getArticleR ref = getRealArticleR $ article_from_reference blog ref

getRealArticleR :: Article -> Handler Html
getRealArticleR art = makeHandler $ do
        let tags = getTags blog
        $(widgetFileNoReload def "tagbar")
        let article = art
        $(widgetFileNoReload def "article")


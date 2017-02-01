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

getArticleR :: String -> Handler Html
getArticleR art = defaultLayout $ do
        setTitle "DwarfMaster website"
        $(widgetFileNoReload def "topbar")
        let tags = getTags blog
        $(widgetFileNoReload def "tagbar")
        let article = art
        $(widgetFileNoReload def "article")


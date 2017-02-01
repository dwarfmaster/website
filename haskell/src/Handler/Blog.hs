{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Blog where

import Data.Default
import Yesod
import Yesod.Default.Util

import Foundation
import Blog
import Articles
import Handler.Utility

getBlogR :: Articles -> Handler Html
getBlogR arts = makeHandler $ do
        let tags = getTags blog
        $(widgetFileNoReload def "tagbar")
        let articles = take 6 arts
        $(widgetFileNoReload def "blog")


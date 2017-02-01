{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Blog where

import Data.Default
import Control.Monad
import Yesod
import Yesod.Default.Util

import Foundation
import Blog
import Articles

getBlogR :: Articles -> Handler Html
getBlogR arts = defaultLayout $ do
        setTitle "DwarfMaster website"
        $(widgetFileNoReload def "topbar")
        let tags = getTags blog
        $(widgetFileNoReload def "tagbar")
        let articles = take 6 arts
        $(widgetFileNoReload def "blog")


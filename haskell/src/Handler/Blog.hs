{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Blog where

import Data.Default
import Yesod
import Yesod.Default.Util

import Foundation

getBlogR :: Handler Html
getBlogR = defaultLayout $ do
        setTitle "DwarfMaster website"
        $(widgetFileNoReload def "blog")


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Utility where

import Data.Default
import Yesod
import Yesod.Default.Util

import Foundation

makeHandler :: Widget -> Handler Html
makeHandler widget = defaultLayout $ do
    setTitle "DwarfMaster website"
    $(widgetFileNoReload def "topbar")
    widget
    $(widgetFileNoReload def "footer")


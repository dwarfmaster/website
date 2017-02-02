{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Data.Default
import Yesod
import Yesod.Default.Util

import Foundation
import Handler.Utility

getHomeR :: Handler Html
getHomeR = do
    makeHandler $ $(widgetFileNoReload def "home")


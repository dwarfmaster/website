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
    let filenames = ["readme.txt", "report.pdf", "music.wav"] :: [String]
    makeHandler $ do
        $(widgetFileNoReload def "home")


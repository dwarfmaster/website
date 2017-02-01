{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Data.Default
import Text.Hamlet
import Yesod
import Yesod.Default.Util

data DwarvenSite = DwarvenSite
instance Yesod DwarvenSite where
    defaultLayout widget = do
        pc <- widgetToPageContent $ $(widgetFileNoReload def "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

mkYesodData "DwarvenSite" $(parseRoutesFile "config/routes")


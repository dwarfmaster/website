{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Foundation where

import Yesod
import Blog

data DwarvenSite = DwarvenSite Blog
instance Yesod DwarvenSite

mkYesodData "DwarvenSite" $(parseRoutesFile "config/routes")


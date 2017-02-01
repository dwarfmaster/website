{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Yesod

data DwarvenSite = DwarvenSite
instance Yesod DwarvenSite

mkYesodData "DwarvenSite" $(parseRoutesFile "config/routes")


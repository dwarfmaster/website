{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dispatch where

import Yesod
import Foundation
import Handler.Home
import Handler.About
import Handler.Blog

mkYesodDispatch "DwarvenSite" resourcesDwarvenSite


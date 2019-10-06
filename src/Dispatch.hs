{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Dispatch where

import Yesod
import Foundation
import Handler.Home
import Handler.About
import Handler.Query
import Handler.Article
import Handler.Data

mkYesodDispatch "DwarvenSite" resourcesDwarvenSite


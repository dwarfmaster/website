{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Dispatch where

import Yesod
import Foundation
import Handler.Home
import Handler.About
import Handler.Blog
import Handler.Query
import Handler.Article

mkYesodDispatch "DwarvenSite" resourcesDwarvenSite


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod
import Dispatch
import Foundation
import Blog

main :: IO ()
main = warpDebug 3001 $ DwarvenSite $ mkBlog []


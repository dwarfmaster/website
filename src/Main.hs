{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod
import Dispatch
import Foundation

main :: IO ()
main = warp 3001 DwarvenSite


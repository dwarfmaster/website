{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Articles (blog) where

import Yesod
import Yesod.Default.Util
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar

import Blog

readDate :: Int -> Int -> Integer -> UTCTime
readDate day month year = UTCTime
    { utctDay     = fromGregorian year month day
    , utctDayTime = secondsToDiffTime 0
    }

articles :: Articles
articles =
    [ Article
        "My Awesome title"
        (toWidget $ $(whamletFile "articles/article1/summary.hamlet"))
        (toWidget $ $(whamletFile "articles/article1/content.hamlet"))
        ["first", "test", "I'm having fun"]
        (readDate 1 2 2017)
    , Article
        "Another article"
        (toWidget $ $(whamletFile "articles/article2/summary.hamlet"))
        (toWidget $ $(whamletFile "articles/article2/content.hamlet"))
        ["test", "both"]
        (readDate 2 2 2017)
    ]


blog :: Blog
blog = mkBlog articles


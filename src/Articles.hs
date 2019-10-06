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
import Foundation

readDate :: Int -> Int -> Integer -> UTCTime
readDate day month year = UTCTime
    { utctDay     = fromGregorian year month day
    , utctDayTime = secondsToDiffTime 0
    }

staging :: Articles
staging =
    [ Article
        "My Awesome title"
        (toWidget $ $(whamletFile "articles/article1/summary.hamlet"))
        (toWidget $ $(whamletFile "articles/article1/content.hamlet"))
        ["first", "test", "I'm having fun"]
        (readDate 1 2 2017)
        "article1"
    , Article
        "Another article"
        (toWidget $ $(whamletFile "articles/article2/summary.hamlet"))
        (toWidget $ $(whamletFile "articles/article2/content.hamlet"))
        ["test", "both"]
        (readDate 2 2 2017)
        "article2"
    ]

prod :: Articles
prod =
    [ Article
        "Hacking Hurd : setting up the workflow"
        (toWidget $ $(whamletFile "articles/hacking-hurd-workflow/summary.hamlet"))
        (toWidget $ $(whamletFile "articles/hacking-hurd-workflow/content.hamlet"))
        ["system", "hurd", "qemu", "prog"]
        (readDate 28 2 2017)
        "hacking-hurd-workflow"
    , Article
        "Understanding the GNU Mach API"
        (toWidget $ $(whamletFile "articles/gnu-mach-principles/summary.hamlet"))
        (toWidget $ $(whamletFile "articles/gnu-mach-principles/content.hamlet"))
        ["system", "hurd", "mach", "prog"]
        (readDate 31 3 2017)
        "gnu-mach-principles"
    ]

blog :: Blog
blog = mkBlog $ prod


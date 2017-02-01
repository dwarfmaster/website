{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Blog where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Mp
import           Data.Set           (Set, (\\))
import qualified Data.Set           as St
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.Calendar
import           Data.List          (sort)
import           Data.List.Split    (wordsBy)
import           Data.Default
import           Yesod
import           Yesod.Default.Util
import           Foundation

data Article = Article
    { article_title   :: String   -- The title of the article
    , article_summary :: Widget   -- The name of the hamlet summary file
    , article_content :: Widget   -- The name of the hamlet content file
    , article_tags    :: [String] -- The tags of the article
    , article_date    :: UTCTime  -- The article time
    }

article404 ref = Article
    { article_title = "Error 404 : no article for " ++ ref
    , article_summary = $(widgetFileNoReload def "article404")
    , article_content = $(widgetFileNoReload def "article404")
    , article_tags    = ["404", "You Lost", "error"]
    , article_date    = UTCTime
                        { utctDay     = fromGregorian 1970 1 1
                        , utctDayTime = secondsToDiffTime 0
                        }
    }

-- What matters here is the Ord instance
instance Eq Article where
    a1 == a2 = article_date a1 == article_date a2
instance Ord Article where
    a1 <= a2 = article_date a1 >= article_date a2

-- A tag is a set of the articles which have this tag
type Tag = Set Article

-- The tags are a mapping from the tag name to the tag itself
type Tags = Map String Tag

-- The list of all articles
type Articles = [Article]

-- The stored status
type Blog = (Tags,Articles)

-- Create the Tags structure from a list of articles
mkBlog :: Articles -> Blog
mkBlog arts = (Mp.fromSet mkTag allTags, arts)
 where allTags = foldl St.union St.empty
                 $ map (St.fromList . article_tags) arts
       mkTag name = St.fromList $ filter ((elem name) . article_tags) arts

getTags :: Blog -> [String]
getTags (tgs,_) = Mp.keys tgs

queryArticles :: Blog     -- The Blog instance
              -> [String] -- The tags we want
              -> [String] -- The tags we don't want
              -> Articles -- The articles satisfying the query, sorted by
                          -- date in descending order
queryArticles (tags,articles) tags_wanted tags_removed =
    sort $ St.elems $ articles_wanted \\ articles_removed
 where articles_wanted     = articles_by_tag tags_wanted
       articles_removed    = articles_by_tag tags_removed
       articles_by_tag tgs = Mp.foldl St.union St.empty
                             $ Mp.filterWithKey (\k -> \_ -> elem k tgs) tags

-- Parse a query of the form +tag1,+tag2,-tag3,+tag4 into [tag1,tag2,tag4]
-- and [tag3] to be used by queryArticles
parseQuery :: String -> ([String],[String])
parseQuery query = (get_queries '+', get_queries '-')
 where queries = wordsBy (== ',') query
       get_queries char = map tail $ filter (\s -> head s == char) queries

-- Parse the query and return the accepted articles
query :: Blog -> String -> Articles
query blog query = (uncurry $ queryArticles blog) $ parseQuery query

reference_format = iso8601DateFormat $ Just "%H:%M:%S"
reference_locale = defaultTimeLocale

article_reference :: Article -> String
article_reference article = formatTime reference_locale reference_format
                            $ article_date article

article_from_reference :: Blog -> String -> Article
article_from_reference (_,articles) ref =
    let result = parseTimeM True reference_locale reference_format ref in
    case result of
     Nothing   -> article404 ref
     Just time -> case filter (\a -> article_date a == time) articles of
                   []        -> article404 ref
                   [article] -> article
                   _         -> article404 ref -- Should not happen


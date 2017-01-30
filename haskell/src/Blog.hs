
module Blog where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import           Data.Set        (Set, (\\))
import qualified Data.Set        as St
import           Data.Time.Clock
import           Data.List       (sort)
import           Data.List.Split (wordsBy)

data Article = Article
    { article_title   :: String   -- The title of the article
    , article_summary :: String   -- The name of the hamlet summary file
    , article_content :: String   -- The name of the hamlet content file
    , article_tags    :: [String] -- The tags of the article
    , article_date    :: UTCTime  -- The article time
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


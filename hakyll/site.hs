--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Debug.Trace


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "about.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "articles/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pat -> do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll pat
            let postCtx = postCtxWithTags tags
                articlesField = listField "articles" postCtx (pure articles)
                titleField = constField "title" ("Posts tagged \""++tag++"\"")
                indexCtx = articlesField <> titleField <> defaultContext
            makeItem "" >>= loadAndApplyTemplate "templates/tag.html" indexCtx
                        >>= loadAndApplyTemplate "templates/default.html" indexCtx
                        >>= relativizeUrls

    match "articles/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/article.html" (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll "articles/*"
            let archiveCtx =
                    listField "articles" (postCtxWithTags tags) (return articles)
                 <> constField "title" "Archive"
                 <> tagsListField tags
                 <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = (postCtxWithTags tags) `mappend` bodyField "description"
            articles <- take 10 <$> (recentFirst =<< loadAllSnapshots "articles/*" "content")
            renderAtom feedConfiguration feedCtx articles

    match "index.html" $ do
        route idRoute
        compile $ do
            articles <- take 5 <$> (recentFirst =<< loadAll "articles/*")
            let indexCtx =
                    listField "articles" (postCtxWithTags tags) (return articles) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
    tagsField "tags" tags
 <> tagsListField tags
 <> postCtx

tagsListField :: Tags -> Context String
tagsListField tags = listField "tagsL" tagsCtx getAllTags
    where getAllTags :: Compiler [Item (String,[Identifier])]
          getAllTags = pure . map mkItem $ tagsMap tags
           where mkItem :: (String,[Identifier]) -> Item (String,[Identifier])
                 mkItem x@(t,_) = Item (tagsMakeId tags t) x

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
 <> defaultContext

tagsCtx :: Context (String,[Identifier])
tagsCtx = listFieldWith "articles" postCtx getPosts
       <> metadataField
       <> urlField "url"
       <> pathField "path"
       <> titleField "title"
       <> missingField
 where getPosts :: Item (String,[Identifier]) -> Compiler [Item String]
       getPosts (itemBody -> (_,is)) = mapM load is

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
                  { feedTitle       = "DwarfMaster blog"
                  , feedDescription = "Random but mostly computer-sciency posts"
                  , feedAuthorName  = "Luc Chabassier aka DwarfMaster"
                  , feedAuthorEmail = "dwarfmaster@dwarfmaster.net"
                  , feedRoot        = "https://dwarfmaster.net"
                  }


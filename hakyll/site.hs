--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


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

    match "articles/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/article.html" postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll "articles/*"
            let archiveCtx =
                    listField "articles" postCtx (return articles) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            articles <- take 10 <$> (recentFirst =<< loadAllSnapshots "articles/*" "content")
            renderAtom feedConfiguration feedCtx articles

    match "index.html" $ do
        route idRoute
        compile $ do
            articles <- take 5 <$> (recentFirst =<< loadAll "articles/*")
            let indexCtx =
                    listField "articles" postCtx (return articles) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
                  { feedTitle       = "DwarfMaster blog"
                  , feedDescription = "Random but mostly computer-sciency posts"
                  , feedAuthorName  = "Luc Chabassier aka DwarfMaster"
                  , feedAuthorEmail = "dwarfmaster@dwarfmaster.net"
                  , feedRoot        = "https://dwarfmaster.net"
                  }

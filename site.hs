--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import           Data.Monoid         (mappend)
import           Hakyll
import           Debug.Trace
import           Text.Pandoc.Options


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
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate defaultTemplate defaultContext
            >>= relativizeUrls

    tags <- buildTags articlesPat (fromCapture "tags/*.html")

    tagsRules tags $ \tag pat -> do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll pat
            let postCtx = postCtxWithTags tags
                articlesField = listField "articles" postCtx (pure articles)
                titleField = constField "title" ("Posts tagged \""++tag++"\"")
                indexCtx = articlesField <> titleField <> defaultContext
            makeItem "" >>= loadAndApplyTemplate "templates/tag.html" indexCtx
                        >>= loadAndApplyTemplate defaultTemplate indexCtx
                        >>= relativizeUrls

    match articlesPat $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/article.html" (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate defaultTemplate (postCtxWithTags tags)
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll articlesPat
            let archiveCtx =
                    listField "articles" (postCtxWithTags tags) (return articles)
                 <> constField "title" "Archive"
                 <> tagsListField tags
                 <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" archiveCtx
                >>= loadAndApplyTemplate defaultTemplate archiveCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = (postCtxWithTags tags) <> bodyField "description"
            articles <- take 10 <$> (recentFirst =<< loadAllSnapshots articlesPat "content")
            renderAtom feedConfiguration feedCtx articles

    match "index.html" $ do
        route idRoute
        compile $ do
            articles <- take 5 <$> (recentFirst =<< loadAll articlesPat)
            let indexCtx =
                    listField "articles" (postCtxWithTags tags) (return articles) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate defaultTemplate indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
 where articlesPat :: Pattern
       articlesPat = "articles/*"
       defaultTemplate :: Identifier
       defaultTemplate = "templates/default.html"


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

pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr enableExtension defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions


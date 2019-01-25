--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "contact.html" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= loadAndApplyTemplate "templates/burrito.html" defaultContext
                >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = tag
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                      listField "posts" postCtx (return posts) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/burrito.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/burrito.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "impure pics"         <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/burrito.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = tagCloudCtx tags <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/burrito.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

tagCloudCtx :: Tags -> Context String
tagCloudCtx = tagCloudField "tags" 100 100

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagCtx tags <> postCtx

tagCtx :: Tags -> Context String
tagCtx = mapContext (filter (/= ',')) . tagsField "tags"

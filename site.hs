{-# LANGUAGE OverloadedStrings #-}

import GHC.Base (liftM)
import Hakyll

main :: IO ()
main = hakyll $ do
    -- [TODO ] Static files
    -- match ("images/*.jpg" .||. "images/*.png")
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "404.html" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= loadAndApplyTemplate "templates/burrito.html" defaultContext
                >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*/page/1.html")

    tagsRules tags $ \tag tagPattern -> do
        let pageToId pageNum = fromCaptures "tags/*/page/*.html" [tag, show pageNum]
        paginate <- buildPaginateWith asPage tagPattern pageToId
        paginateRules paginate $ \pageNum pattern -> do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern
                let paginateCtx = paginateContext paginate pageNum
                    ctx =
                        listField "posts" postCtx (return posts)
                            <> constField "title" tag
                            <> paginateCtx
                            <> defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" ctx
                    >>= loadAndApplyTemplate "templates/burrito.html" ctx
                    >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/burrito.html" (postCtxWithTags tags)
                >>= relativizeUrls

    paginate <- buildPaginateWith asPage "posts/*" $ \pageNum ->
        fromCapture "posts/page/*.html" (show pageNum)

    paginateRules paginate $ \pageNum pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let paginateCtx = paginateContext paginate pageNum
                ctx =
                    listField "posts" postCtx (return posts)
                        <> constField "title" "impure pics"
                        <> paginateCtx
                        <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/burrito.html" ctx
                >>= relativizeUrls

    -- Not explictily used right now
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts)
                        <> constField "title" "Archive"
                        <> defaultContext

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

    create ["atom.xml"] $ do
        route idRoute
        compile (feedCompiler renderAtom)

    create ["rss.xml"] $ do
        route idRoute
        compile (feedCompiler renderRss)
  where
    feedCompiler renderer = do
        let feedCtx = postCtx <> bodyField "description"
        snapshots <- loadAllSnapshots "posts/*" "content"
        posts <- take 10 <$> recentFirst snapshots
        renderer feedConfiguration feedCtx posts

    asPage = fmap (paginateEvery 15) . sortRecentFirst

tagCloudCtx :: Tags -> Context String
tagCloudCtx = tagCloudField "tags" 100 100

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagCtx tags <> postCtx

tagCtx :: Tags -> Context String
tagCtx = mapContext (filter (/= ',')) . tagsField "tags"

feedConfiguration :: FeedConfiguration
feedConfiguration =
    FeedConfiguration
        { feedTitle = "Impure Pics"
        , feedDescription = "Distilling functional programming for the good of all"
        , feedAuthorName = "Impure Pics"
        , feedAuthorEmail = "Impure Pics"
        , feedRoot = "https://impurepics.com"
        }

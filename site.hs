{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (fromMaybe)
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

    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    match "404.html" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= loadAndApplyTemplate "templates/burrito.html" defaultContext
                >>= relativizeUrls

    match "license.html" $ do
        route idRoute
        compile $ do
            let licenseCtx = constField "title" "License" <> defaultContext
            getResourceBody
                >>= loadAndApplyTemplate "templates/burrito.html" licenseCtx
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
            posts <- recentFirst =<< loadAll "posts/*"
            featuredCtx <- featuredPicCtx posts
            let indexCtx =
                    featuredCtx
                        <> listField "featuredPosts" postCtx (return posts)
                        <> tagCloudCtx tags
                        <> defaultContext

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

-- Shove every post into context, so we can pick a random one from JS on main
featuredPicCtx :: [Item String] -> Compiler (Context String)
featuredPicCtx [] =
    pure $
        constField "featuredTitle" ""
            <> constField "featuredThumbnail" ""
            <> constField "featuredUrl" "/posts/page/1.html"
featuredPicCtx (post : _) = do
    let featured = itemIdentifier post
    title <- fromMaybe "" <$> getMetadataField featured "title"
    thumbnail <- fromMaybe "" <$> getMetadataField featured "thumbnail"
    route <- getRoute featured
    pure $
        constField "featuredTitle" title
            <> constField "featuredThumbnail" thumbnail
            <> constField "featuredUrl" (maybe "/posts/page/1.html" toUrl route)

tagCloudCtx :: Tags -> Context String
tagCloudCtx = tagCloudField "tags" 100 100 . sortTagsBy compareTagsByImportance

-- the more the merrier
compareTagsByImportance :: (String, [Identifier]) -> (String, [Identifier]) -> Ordering
compareTagsByImportance (tagA, postsA) (tagB, postsB) =
    compare (length postsB) (length postsA) <> compare tagA tagB

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

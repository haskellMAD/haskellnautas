{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid          (mappend)
import qualified Data.Set             as Set
import           Hakyll
import           Haskellnautas.Routes
import           Text.Pandoc.Options

config :: Configuration
config = defaultConfiguration { providerDirectory = "static/"
                              , destinationDirectory = "docs"
                              , inMemoryCache = True
                              }

main :: IO ()
main = hakyllWith config $ do
  match "templates/*.html" $ compile templateCompiler

  match "css/**.css" $ do
    route idRoute
    compile compressCssCompiler

  match "public/**" $ do
    route $ stripRoute "public/"
    compile copyFileCompiler

  match "pages/**.md" $ do
    route $ stripRoute "pages/" `composeRoutes` setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" defaultContext
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= relativizeUrls

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
      >>= loadAndApplyTemplate "templates/base.html" (postCtxWithTags tags)
      >>= relativizeUrls

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \""++tag++"\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title
                `mappend` listField "posts" postCtx (return posts)
                `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag-list.html" ctx
        >>= loadAndApplyTemplate "templates/base.html" ctx
        >>= relativizeUrls

  match "drafts/*" $ do
    route $ setExtension "html"
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/draft.html" postCtx
      >>= loadAndApplyTemplate "templates/base.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx = listField "posts" postCtx (return posts) `mappend`
                       constField "title" "Archives"            `mappend`
                       defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/base.html" archiveCtx
        >>= relativizeUrls

  match "pages/index.html" $ do
    route $ stripRoute "pages/"
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let homeCtx = listField "posts" postCtx (return posts) `mappend`
                    defaultContext

      getResourceBody
        >>= applyAsTemplate homeCtx
        >>= loadAndApplyTemplate "templates/home.html" homeCtx
        >>= loadAndApplyTemplate "templates/base.html" homeCtx
        >>= relativizeUrls


--     match "css/*.css" $ do
--       route   idRoute
--       compile compressCssCompiler

--     match "css/*.hs" $ do
--       route $ setExtension "css"
--       compile $ getResourceString >>= withItemBody (unixFilter "stack" ["runghc"])

--     match (fromList ["about.rst", "contact.markdown"]) $ do
--       route   $ setExtension "html"
--       compile $ pandocCompiler
--         >>= loadAndApplyTemplate "templates/default.html" defaultContext
--         >>= relativizeUrls

--     match "posts/*" $ do
--       route $ setExtension "html"
--       compile $ pandocCompiler
--         >>= loadAndApplyTemplate "templates/post.html"    postCtx
--         >>= loadAndApplyTemplate "templates/default.html" postCtx
--         >>= relativizeUrls

--     create ["archive.html"] $ do
--       route idRoute
--       compile $ do
--         posts <- recentFirst =<< loadAll "posts/*"
--         let archiveCtx =
--                 listField "posts" postCtx (return posts) `mappend`
--                 constField "title" "Archives"            `mappend`
--                 defaultContext

--         makeItem ""
--           >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
--           >>= loadAndApplyTemplate "templates/default.html" archiveCtx
--           >>= relativizeUrls


--     match "templates/*" $ compile templateCompiler


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
  let mathExtensions = [ Ext_tex_math_dollars
                       , Ext_tex_math_double_backslash
                       , Ext_latex_macros
                       ]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = foldr Set.insert defaultExtensions mathExtensions
      writerOptions = defaultHakyllWriterOptions { writerExtensions = newExtensions
                                                 , writerHTMLMathMethod = MathJax ""
                                                 }
  in
    pandocCompilerWith defaultHakyllReaderOptions writerOptions


postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

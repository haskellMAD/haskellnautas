{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid          (mappend)
import qualified Data.Set             as Set
import           Hakyll
import           Haskellnautas.Html
import           Haskellnautas.Routes
import           Haskellnautas.Time
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

  match "favicon/*" $ do
    route $ stripRoute "favicon/"
    compile copyFileCompiler

  match "pages/autores/*.md" $ do
    route $ stripRoute "pages/"
      `composeRoutes` setExtension "html"
      `composeRoutes` cleanRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/author.html" defaultContext
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= relativizeUrls

  match "pages/comunidad.md" $ do
    route $ stripRoute "pages/"
      `composeRoutes` setExtension "html"
      `composeRoutes` cleanRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/simple.html" defaultContext
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= relativizeUrls
      >>= useCleanUrls

  tags <- buildTags "articulos/*" (fromCapture "tags/*.html")

  match "articulos/*" $ do
    route $ setExtension "html"
      `composeRoutes` cleanRoute
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
      >>= loadAndApplyTemplate "templates/base.html" (postCtxWithTags tags)
      >>= relativizeUrls
      >>= useCleanUrls

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

  match "borradores/*" $ do
    route $ setExtension "html"
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/draft.html" postCtx
      >>= loadAndApplyTemplate "templates/base.html" postCtx
      >>= relativizeUrls

  match "CNAME" $ do
    route $ idRoute
    compile copyFileCompiler

  create ["archivo.html"] $ do
    route $ cleanRoute
    compile $ do
      posts <- recentFirst =<< loadAll "articulos/*"
      let archiveCtx = listField "posts" postCtx (return posts) `mappend`
                       defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/base.html" archiveCtx
        >>= relativizeUrls

  create ["borradores.html"] $ do
    route $ cleanRoute
    compile $ do
      posts <- recentFirst =<< loadAll "borradores/*"
      let archiveCtx = listField "posts" postCtx (return posts) `mappend`
                       defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/drafts.html" archiveCtx
        >>= loadAndApplyTemplate "templates/base.html" archiveCtx
        >>= relativizeUrls

  match "pages/index.html" $ do
    route $ stripRoute "pages/"
    compile $ do
      posts <- recentFirst =<< loadAll "articulos/*"
      let homeCtx = listField "posts" postCtx (return posts) `mappend`
                    defaultContext

      getResourceBody
        >>= applyAsTemplate homeCtx
        >>= loadAndApplyTemplate "templates/home.html" homeCtx
        >>= loadAndApplyTemplate "templates/base.html" homeCtx
        >>= relativizeUrls
        >>= useCleanUrls

postCtx :: Context String
postCtx =
    dateFieldWith timeLocale "date" "%e de %B de %Y" `mappend`
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

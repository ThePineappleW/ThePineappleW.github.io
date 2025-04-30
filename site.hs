--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Hakyll.Favicon (faviconsField, faviconsRules)
import Text.Pandoc.Highlighting (Style, haddock, styleToCss)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))

--------------------------------------------------------------------------------
pandocCodeStyle :: Style
pandocCodeStyle = haddock

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle = Just pandocCodeStyle
      }

defaultContext' :: Context String
defaultContext' = faviconsField `mappend` defaultContext

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext'

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith config $ do
  faviconsRules "images/epsilon.svg"

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ do
      makeItem $ styleToCss pandocCodeStyle

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["about.md"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler'
        >>= loadAndApplyTemplate "templates/default.html" defaultContext'
        >>= relativizeUrls

  match "posts/**.md" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler'
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  create ["blog.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext'

      makeItem ""
        >>= loadAndApplyTemplate "templates/blog.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext'

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
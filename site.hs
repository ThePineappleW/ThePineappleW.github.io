--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Exception (SomeException, evaluate, try)
import Crossword (puzzleCompiler, puzzleCtx)
import Data.Aeson (Value, decode, encode, withObject)
import Data.Array (elems)
import qualified Data.Base64.Types as B64
import Data.Bits (Bits (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isAlpha, ord)
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.List as L
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import qualified Data.Text as T
import qualified Data.Text.Encoding.Base64 as B64
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Yaml (FromJSON (..), ParseException, decodeEither', (.!=), (.:), (.:?))
import GHC.Generics
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Hakyll
import Hakyll.Favicon (faviconsField, faviconsRules)
import Search (buildSearchMetadata, searchIndexCompiler, searchMetadataCompiler)
import System.FilePath (takeBaseName)
import System.IO.Unsafe (unsafePerformIO)
import Text.Jasmine
import Text.Pandoc.Highlighting (Style, haddock, styleToCss)
import Text.Pandoc.Options (Extension (..), HTMLMathMethod (..), ReaderOptions (..), WriterOptions (..), disableExtension, enableExtension)
import Text.Printf (printf)
import Text.Regex.TDFA (MatchArray, (=~))

--------------------------------------------------------------------------------
-- Extension management
-- Thanks to https://laurentrdc.xyz/posts/making-this-website.html

styleExtensions =
  [ Ext_multiline_tables,
    Ext_raw_attribute
  ]

mathExtensions =
  [ Ext_tex_math_dollars,
    Ext_tex_math_double_backslash,
    Ext_latex_macros
  ]

codeExtensions =
  [ Ext_fenced_code_blocks,
    Ext_backtick_code_blocks,
    Ext_fenced_code_attributes
    -- Ext_inline_code_attributes
  ]

defaultExtensions = writerExtensions defaultHakyllWriterOptions

newExtensions =
  foldr
    enableExtension
    defaultExtensions
    (styleExtensions <> mathExtensions <> codeExtensions)

-- newExtensions =
--------------------------------------------------------------------------------

pandocCodeStyle :: Style
pandocCodeStyle = haddock

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHTMLMathMethod = MathJax "",
        writerHighlightStyle = Just pandocCodeStyle,
        writerSectionDivs = True,
        writerExtensions = newExtensions
      }

pandocCompiler'' :: (String -> String) -> Compiler (Item String)
pandocCompiler'' f =
  let writerOptions =
        defaultHakyllWriterOptions
          { writerHTMLMathMethod = MathJax "",
            writerHighlightStyle = Just pandocCodeStyle,
            writerSectionDivs = True,
            writerExtensions = newExtensions
          }
   in getResourceBody
        >>= withItemBody (return . f)
        >>= readPandocWith defaultHakyllReaderOptions
        <&> writePandocWith writerOptions

defaultContext' :: Context String
defaultContext' = faviconsField `mappend` defaultContext

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext'

-- hjsmin doesn't support >=ES6. If we can't minify the file, just return it unchanged.
safeMinify :: BL.ByteString -> BL.ByteString
safeMinify script =
  case unsafePerformIO (try (evaluate (minify script)) :: IO (Either SomeException BL.ByteString)) of
    Right minified -> minified
    Left _ -> script

-- From https://codethoughts.io/posts/2016-05-10-compiling-scss-and-js-in-hakyll/
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . safeMinify . C.pack . itemBody
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s

--------------------------------------------------------------------------------
-- Crossword puzzle setup
--------------------------------------------------------------------------------

puzzleCtx' = puzzleCtx postCtx

puzzleCompiler' = puzzleCompiler defaultContext'

--------------------------------------------------------------------------------
-- Project functions
--------------------------------------------------------------------------------

sortByRank :: [Item a] -> Compiler [Item a]
sortByRank items = do
  withRank <- mapM addRank items
  return $ map snd $ sortOn fst withRank
  where
    addRank item = do
      rankStr <- getMetadataField (itemIdentifier item) "rank"
      let n = fromMaybe maxBound (rankStr >>= readMaybe)
      return (n :: Int, item)

    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

projectCompiler :: Compiler (Item String)
projectCompiler = do
  pandocCompiler'
    >>= loadAndApplyTemplate "templates/projects/project.html" postCtx
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= relativizeUrls

--------------------------------------------------------------------------------
-- MTG card embedding functions
--------------------------------------------------------------------------------

regexReplace :: String -> (String -> String) -> String -> String
regexReplace pat f input = rebuild 0 matches
  where
    matches :: [(Int, Int)] -- (offset, length) of each match, in order
    matches = [m | ma <- input =~ pat :: [MatchArray], m <- take 1 (elems ma)]

    rebuild pos [] = drop pos input
    rebuild pos ((off, len) : rest) =
      take (off - pos) (drop pos input)
        ++ f (take len (drop off input))
        ++ rebuild (off + len) rest

data MTGCard = Card
  { name :: String,
    set :: Maybe String,
    number :: Maybe String
  }

parseMTG :: String -> Maybe MTGCard
parseMTG str = case splitOneOf "|_" stripped of
  [name, set, number] -> Just (Card name (Just set) (Just number))
  [name, set] -> Just (Card name (Just set) Nothing)
  [name] -> Just (Card name Nothing Nothing)
  [] -> Nothing
  where
    stripped = filter (\ch -> (ch /= '[') && (ch /= ']')) str

formatScryfallParam :: String -> Maybe String -> String
formatScryfallParam label (Just val) = printf "data-%s=\"%s\" " label val
formatScryfallParam _ Nothing = ""

formatScryfallLink :: MTGCard -> String
formatScryfallLink (Card name set number) =
  "`<a class=\"mtg-link\" target=\"_blank\" "
    ++ printf "data-name=\"%s\" " name
    ++ formatScryfallParam "set" set
    ++ formatScryfallParam "number" number
    ++ "href=\"#\">"
    ++ L.dropWhile (== '!') name
    ++ "<img class=\"mtg-popup\" src=\"/images/placeholder_card.png\"></a>`{=html}"

generateMTGEmbed :: String -> String
generateMTGEmbed str =
  maybe str formatScryfallLink (parseMTG str)

embedAllMTG :: String -> String
embedAllMTG = regexReplace "\\[\\[[^]]+\\]\\]" generateMTGEmbed

--------------------------------------------------------------------------------

main :: IO ()
main = do
  setLocaleEncoding utf8
  hakyllWith config $ do
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

    match "js/*" $ do
      route idRoute
      compile compressJsCompiler

    match (fromList ["about.md", "404.md"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler'
          >>= loadAndApplyTemplate "templates/default.html" defaultContext'
          >>= relativizeUrls

    match "projects/**.md" $ do
      route $ setExtension "html"
      compile projectCompiler

    match "projects/**.pdf" $ do
      route idRoute
      compile copyFileCompiler

    create ["projects.html"] $ do
      route idRoute
      compile $ do
        projects <- sortByRank =<< loadAll "projects/**.md"
        let archiveCtx =
              listField "projects" defaultContext' (return projects)
                `mappend` constField "title" "Project list"
                `mappend` defaultContext'

        makeItem ""
          >>= loadAndApplyTemplate "templates/projects/projects.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    match "posts/**.md" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler'' embedAllMTG
          >>= loadAndApplyTemplate "templates/blog/post.html" postCtx
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
          >>= loadAndApplyTemplate "templates/blog/blog.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    match "puzzles/**.yaml" $ do
      route $ setExtension "html"
      compile puzzleCompiler'

    create ["puzzles.html"] $ do
      route idRoute
      compile $ do
        -- We want to sort puzzles by date.
        -- However, Hakyll can't automatically recognize a YAML date field unless it's in the file name.
        -- But we don't want the date in the label when we have it on the site.
        -- So we have to do a little bit of meshugas to get rid of that part.
        puzzles <- recentFirst =<< loadAll "puzzles/**.yaml"
        let puzzleArchiveCtx =
              listField "puzzles" puzzleCtx' (return puzzles)
                `mappend` constField "title" "Puzzle Archives"
                `mappend` defaultContext'
        makeItem ""
          >>= loadAndApplyTemplate "templates/puzzles/puzzles.html" puzzleArchiveCtx
          >>= loadAndApplyTemplate "templates/default.html" puzzleArchiveCtx
          >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        -- Only take the first 5 of each type on the main page.
        posts <- fmap (take 5) (recentFirst =<< loadAll "posts/*.md")
        projects <- fmap (take 5) (sortByRank =<< loadAll "projects/**.md")
        puzzles <- fmap (take 5) (recentFirst =<< loadAll "puzzles/**.yaml")
        let indexCtx =
              listField "posts" postCtx (return posts)
                <> listField "projects" defaultContext' (return projects)
                <> listField "puzzles" puzzleCtx' (return puzzles)
                <> defaultContext'

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    create ["search-index.json"] $ do
      route idRoute
      compile searchIndexCompiler

    create ["search-meta.json"] $ do
      route idRoute
      compile searchMetadataCompiler

    create ["search.html"] $ do
      route idRoute
      compile $ do
        makeItem ""
          >>= loadAndApplyTemplate "templates/search/search-results.html" defaultContext'
          >>= loadAndApplyTemplate "templates/default.html" defaultContext'
          >>= relativizeUrls

    match "templates/**" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson (Value, encode, decode, withObject)
import Data.Yaml (decodeEither', FromJSON(..), (.:), (.:?), (.!=), ParseException)
import Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Monoid (mappend)
import Hakyll
import Hakyll.Favicon (faviconsField, faviconsRules)
import Text.Jasmine
import Text.Pandoc.Highlighting (Style, haddock, styleToCss)
import Text.Pandoc.Options (Extension (..), HTMLMathMethod (..), ReaderOptions (..), WriterOptions (..), disableExtension, enableExtension)
import Control.Exception (SomeException, try, evaluate)
import System.IO.Unsafe (unsafePerformIO)
import Data.Int (Int32)
import Data.Bits (Bits(..))
import Data.Char (ord, isAlpha)
import qualified Data.Base64.Types as B64
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.FilePath (takeBaseName)
import Data.Maybe (fromMaybe)

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
-- Crossword puzzle functions
--------------------------------------------------------------------------------

data Puzzle =
    Puzzle  { title   :: T.Text
            , date    :: T.Text
            , kind    :: T.Text
            , comment :: Maybe T.Text
            , board   :: T.Text
            , across  :: [T.Text]
            , down    :: [T.Text]
    }

instance FromJSON Puzzle where
  parseJSON = withObject "Puzzle" $ \o ->
    Puzzle <$> o .: "title"
           <*> o .: "date"
           <*> o .:? "kind" .!= "standard"
           <*> o .:? "comment"
           <*> o .: "board"
           <*> o .: "across"
           <*> o .: "down"

puzzleDimensions :: T.Text -> (Int, Int)
puzzleDimensions str =
  let lines = T.lines str in
  case lines of
    [] -> (0, 0)
    first:_ -> (length lines, T.length first)

wallPositions :: T.Text -> [[Int]]
wallPositions str =
  let lines = T.lines str in
    foldl (\acc (i, row) -> acc ++ map (\col_id -> [i, col_id]) (L.elemIndices '#' (T.unpack row)))
     []
     (zip [0..] lines)

b64Hash :: T.Text -> String
b64Hash txt =
  let onlyAlpha = T.toUpper $ T.filter isAlpha txt in
    T.unpack $ B64.extractBase64 (B64.encodeBase64 onlyAlpha)

puzzleCtx :: Context String
puzzleCtx =
  let stripDate = L.drop 11 in
  let enspace = map (\c -> if c == '_' then ' ' else c) in
  field "title" (\item -> do
    let path = toFilePath (itemIdentifier item)
    let title = enspace $ stripDate $ takeBaseName path
    return title)
  `mappend` postCtx

puzzleCompiler :: Compiler (Item String)
puzzleCompiler = do
    item <- getResourceLBS
    let yamlBytes = BS.toStrict $ itemBody item

    case decodeEither' yamlBytes of
        Left err -> fail $ "YAML parse error: " ++ show err
        Right puzzle -> do
            let (nrows, ncols) = puzzleDimensions (board puzzle)
            let walls = wallPositions (board puzzle)
            let puzzleHash = b64Hash (board puzzle)
            let cluesCtx =
                    constField "title" (T.unpack (title puzzle)) <>
                    constField "date" (T.unpack (date puzzle)) <>
                    constField "rows" (show nrows) <>
                    constField "columns" (show ncols) <>
                    constField "hash" (show puzzleHash) <>
                    constField "walls" (show walls) <>
                    constField "across" (TL.unpack (TLE.decodeUtf8 (encode (across puzzle)))) <>
                    constField "down" (TL.unpack (TLE.decodeUtf8 (encode (down puzzle)))) <>
                    defaultContext'

            makeItem ""
                >>= loadAndApplyTemplate "templates/puzzles/crossword.html" cluesCtx
                >>= loadAndApplyTemplate "templates/default.html" cluesCtx
                >>= relativizeUrls

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
        _         -> Nothing

projectCompiler :: Compiler (Item String)
projectCompiler = do
    pandocCompiler'
          >>= loadAndApplyTemplate "templates/projects/project.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls


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

    match (fromList ["about.md"]) $ do
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
        pandocCompiler'
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
      compile puzzleCompiler

    create ["puzzles.html"] $ do
      route idRoute
      compile $ do
        -- We want to sort puzzles by date.
        -- However, Hakyll can't automatically recognize a YAML date field unless it's in the file name.
        -- But we don't want the date in the label when we have it on the site.
        -- So we have to do a little bit of meshugas to get rid of that part.
        puzzles <- recentFirst =<< loadAll "puzzles/**.yaml"        
        let puzzleArchiveCtx =
              listField "puzzles" puzzleCtx (return puzzles)
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
        posts    <- fmap (take 5) (recentFirst =<< loadAll "posts/*.md")
        projects <- fmap (take 5) (sortByRank =<< loadAll "projects/**.md")
        puzzles  <- fmap (take 5) (recentFirst =<< loadAll "puzzles/**.yaml")
        let indexCtx =
              listField "posts" postCtx (return posts) <>
              listField "projects" defaultContext' (return projects) <>
              listField "puzzles" puzzleCtx (return puzzles) <>
              defaultContext'

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    match "templates/**" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
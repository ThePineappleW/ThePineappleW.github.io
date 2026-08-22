{-# LANGUAGE OverloadedStrings #-}

module Crossword where

import Data.Aeson (Value, decode, encode, withObject)
import qualified Data.Base64.Types as B64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isAlpha)
import Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding.Base64 as B64
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Yaml (FromJSON (..), ParseException, decodeEither', (.!=), (.:), (.:?))
import Hakyll (Compiler, Context, Item (itemBody, itemIdentifier), constField, field, getResourceLBS, loadAndApplyTemplate, makeItem, relativizeUrls, toFilePath)
import System.FilePath (takeBaseName)

--------------------------------------------------------------------------------
-- Crossword puzzle functions
--------------------------------------------------------------------------------

data Puzzle
  = Puzzle
  { title :: T.Text,
    date :: T.Text,
    kind :: T.Text,
    comment :: Maybe T.Text,
    board :: T.Text,
    across :: [T.Text],
    down :: [T.Text]
  }

instance FromJSON Puzzle where
  parseJSON = withObject "Puzzle" $ \o ->
    Puzzle
      <$> o .: "title"
      <*> o .: "date"
      <*> o .:? "kind" .!= "standard"
      <*> o .:? "comment"
      <*> o .: "board"
      <*> o .: "across"
      <*> o .: "down"

puzzleDimensions :: T.Text -> (Int, Int)
puzzleDimensions str =
  let lines = T.lines str
   in case lines of
        [] -> (0, 0)
        first : _ -> (length lines, T.length first)

wallPositions :: T.Text -> [[Int]]
wallPositions str =
  let lines = T.lines str
   in foldl
        (\acc (i, row) -> acc ++ map (\col_id -> [i, col_id]) (L.elemIndices '#' (T.unpack row)))
        []
        (zip [0 ..] lines)

b64Hash :: T.Text -> String
b64Hash txt =
  let onlyAlpha = T.toUpper $ T.filter isAlpha txt
   in T.unpack $ B64.extractBase64 (B64.encodeBase64 onlyAlpha)

puzzleCtx :: Context String -> Context String
puzzleCtx baseCtx =
  let stripDate = L.drop 11
   in let enspace = map (\c -> if c == '_' then ' ' else c)
       in field
            "title"
            ( \item -> do
                let path = toFilePath (itemIdentifier item)
                let title = enspace $ stripDate $ takeBaseName path
                return title
            )
            `mappend` baseCtx

puzzleCompiler :: Context String -> Compiler (Item String)
puzzleCompiler defaultContext = do
  item <- getResourceLBS
  let yamlBytes = BS.toStrict $ itemBody item

  case decodeEither' yamlBytes of
    Left err -> fail $ "YAML parse error: " ++ show err
    Right puzzle -> do
      let (nrows, ncols) = puzzleDimensions (board puzzle)
      let walls = wallPositions (board puzzle)
      let puzzleHash = b64Hash (board puzzle)
      let cluesCtx =
            constField "title" (T.unpack (title puzzle))
              <> constField "date" (T.unpack (date puzzle))
              <> constField "rows" (show nrows)
              <> constField "columns" (show ncols)
              <> constField "hash" (show puzzleHash)
              <> constField "walls" (show walls)
              <> constField "across" (TL.unpack (TLE.decodeUtf8 (encode (across puzzle))))
              <> constField "down" (TL.unpack (TLE.decodeUtf8 (encode (down puzzle))))
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/puzzles/crossword.html" cluesCtx
        >>= loadAndApplyTemplate "templates/default.html" cluesCtx
        >>= relativizeUrls

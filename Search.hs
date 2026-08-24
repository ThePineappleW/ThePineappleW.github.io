{-# LANGUAGE OverloadedStrings #-}

module Search where

import Data.Aeson (ToJSON (..), ToJSONKey, encode, object, (.=))
import Data.Char (isAlphaNum, toLower)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Hakyll
  ( Compiler,
    Item (itemBody, itemIdentifier),
    Pattern,
    fromGlob,
    getMetadataField,
    getMetadataField',
    getRoute,
    loadAll,
    makeItem,
    recentFirst,
    toFilePath,
    toUrl,
  )

type ID = Int

-- (line, col)
type TokenInfo = (Int, Int)

data Page = Page {pageID :: ID, pageText :: T.Text}

data PageMeta = PageMeta {pmTitle :: T.Text, pmURL :: T.Text}

instance ToJSON PageMeta where
  toJSON (PageMeta title url) =
    object ["title" .= title, "url" .= url]

type PositionalIndex = M.Map T.Text (Set.Set (ID, TokenInfo))

tokenize :: T.Text -> [(T.Text, TokenInfo)]
tokenize text =
  concatMap
    ( \(lineNum, line) ->
        zipWith
          (\wordNum word -> (T.map (\ch -> if isAlphaNum ch then toLower ch else ' ') word, (lineNum, wordNum)))
          [0 ..]
          (T.words line)
    )
    (zip [0 ..] (T.lines text))

buildPositionalIndex :: [Page] -> PositionalIndex
buildPositionalIndex pages =
  let tokenized = [(pageID page, tokenize . pageText $ page) | page <- pages]
   in let assocIndex = concatMap (\(id, tokens) -> map (\(text, info) -> (text, Set.singleton (id, info))) tokens) tokenized
       in M.fromListWith Set.union assocIndex

mapToJson :: (ToJSONKey a, ToJSON b) => M.Map a b -> String
mapToJson = TL.unpack . TLE.decodeUtf8 . encode

searchIndexCompiler :: Compiler (Item String)
searchIndexCompiler = do
  posts <- recentFirst =<< loadAll "posts/**.md"
  pages <-
    mapM
      ( \(p, numID) -> do
          let body = itemBody p
          let itemId = itemIdentifier p
          title <- getMetadataField' itemId "title"
          route <- getRoute itemId
          let url = maybe "" toUrl route
          pure (Page {pageID = numID, pageText = T.pack (title <> " " <> body)})
      )
      (zip posts [0 ..])

  makeItem (mapToJson $ buildPositionalIndex pages)

-- The index metadata is a list of (link, title), where the `i`th entry corresponds to the page with ID `i`.
-- Corresponding orders are managed by building this and the search index in chronological order.
buildSearchMetadata :: [Item String] -> Compiler [PageMeta]
buildSearchMetadata = mapM toEntry
  where
    toEntry :: Item String -> Compiler PageMeta
    toEntry item = do
      let ident = itemIdentifier item
      route' <- getRoute ident
      let url = case route' of
            Just r -> T.pack (toUrl r)
            Nothing -> error ("buildMetadata: no route for " <> show ident)

      title <- T.pack <$> getMetadataField' ident "title"

      pure (PageMeta title url)

searchMetadataCompiler :: Compiler (Item String)
searchMetadataCompiler = do
  pages <- recentFirst =<< loadAll "posts/*"
  meta <- buildSearchMetadata pages
  makeItem (TL.unpack . TLE.decodeUtf8 . encode $ meta)

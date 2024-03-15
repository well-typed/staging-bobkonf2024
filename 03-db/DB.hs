{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module DB where

-- Further reading:
--
-- Matthew Pickering, Andres Löh, Nicolas Wu
-- Staged Sums of Products
--
-- Tiark Rompf, Nada Amin
-- A SQL to C Compiler in 500 Lines of Code

import Data.Text
import Database.SQLite3

data Talk =
  MkTalk
    { time     :: Text
    , duration :: Int
    , track    :: Maybe Text
    , title    :: Text
    , speakers :: Text
    , language :: Text
    }
  deriving Show

-- This is reasonably good code for fetching all the talks from
-- an sqlite database.

fetchTalkRow :: Statement -> IO Talk
fetchTalkRow stmt =
  pure MkTalk
  <*> columnText stmt 0
  <*> (fromIntegral <$> columnInt64 stmt 1)
  <*> (do
        t <- columnType stmt 2
        case t of
          NullColumn -> pure Nothing
          _ -> Just <$> columnText stmt 2
      )
  <*> columnText stmt 3
  <*> columnText stmt 4
  <*> columnText stmt 5

target :: Database -> IO [Talk]
target db =
  let
    q = "SELECT * FROM talks"

    loop stmt = do
      result <- step stmt
      case result of
        Done -> pure []
        Row -> do
          row <- fetchTalkRow stmt
          rows <- loop stmt
          pure (row : rows)
  in
    withStatement db q loop

-- What if we want to employ datatype-generic programming to generate
-- the above code?

data SchemaDesc a where
  Table      :: Text -> a -> SchemaDesc a
  WithColumn :: SchemaDesc (a -> b) -> ColumnDesc a -> SchemaDesc a

data ColumnDesc a where
  IntC  :: ColumnDesc Int
  TextC :: ColumnDesc Text
  NullC :: ColumnDesc a -> ColumnDesc (Maybe a)

talkSchema :: SchemaDesc Talk
talkSchema =
  undefined

descSize :: SchemaDesc a -> Int
descSize =
  undefined

getTable :: SchemaDesc a -> Text
getTable =
  undefined

fetchField :: ColumnIndex -> ColumnDesc a -> Statement -> IO a
fetchField =
  undefined

fetchRow :: SchemaDesc a -> Statement -> IO a
fetchRow =
  undefined

fetch :: SchemaDesc a -> Database -> IO [a]
fetch =
  undefined

-- To reduce the overhead, let's employ staging!

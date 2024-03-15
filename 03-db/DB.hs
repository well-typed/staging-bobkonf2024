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
import Data.Coerce
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

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

data SchemaDesc m a where
  Table      :: Text -> Code m a -> SchemaDesc m a
  WithColumn :: SchemaDesc m (a -> b) -> ColumnDesc a -> SchemaDesc m b

data ColumnDesc a where
  IntC  :: ColumnDesc Int
  TextC :: ColumnDesc Text
  NullC :: ColumnDesc a -> ColumnDesc (Maybe a)

talkSchema :: Quote m => SchemaDesc m Talk
talkSchema =
  Table "talks" [|| MkTalk ||]
  `WithColumn` TextC
  `WithColumn` IntC
  `WithColumn` NullC TextC
  `WithColumn` TextC
  `WithColumn` TextC
  `WithColumn` TextC

descSize :: SchemaDesc m a -> Int
descSize (Table _ _) = 0
descSize (WithColumn d _) = 1 + descSize d

getTable :: SchemaDesc m a -> Text
getTable (Table tn _) = tn
getTable (WithColumn d _) = getTable d

fetchField :: Quote m => ColumnIndex -> ColumnDesc a -> Code m Statement -> Code m (IO a)
fetchField cix IntC stmt =
  [|| fromIntegral <$> columnInt64 $$stmt $$(liftTyped cix) ||]
fetchField cix TextC stmt =
  [|| columnText $$stmt cix ||]
fetchField cix (NullC d) stmt = [|| do
    t <- columnType $$stmt $$(liftTyped cix)
    case t of
      NullColumn -> pure Nothing
      _ -> Just <$> $$(fetchField cix d stmt)
  ||]

deriving instance Lift ColumnIndex

fetchRow :: Quote m => SchemaDesc m a -> Code m Statement -> Code m (IO a)
fetchRow d stmt =
  fetchRow' (coerce (descSize d)) d stmt

fetchRow' :: Quote m => ColumnIndex -> SchemaDesc m a -> Code m Statement -> Code m (IO a)
fetchRow' cix (Table _tn constr) _stmt =
  [|| pure $$constr ||]
fetchRow' cix (WithColumn d cd) stmt =
  [|| $$(fetchRow' (cix - 1) d stmt) <*> $$(fetchField (cix - 1) cd stmt) ||]

fetch :: Quote m => SchemaDesc m a -> Code m Database -> Code m (IO [a])
fetch d db =
  [||
    let
      q = "SELECT * FROM talks"

      loop stmt = do
        result <- step stmt
        case result of
          Done -> pure []
          Row -> do
            row <- $$(fetchRow d [|| stmt ||])
            rows <- loop stmt
            pure (row : rows)
    in
      withStatement $$db q loop
  ||]

-- To reduce the overhead, let's employ staging!

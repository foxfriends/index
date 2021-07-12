module Note (readNote, Note (Note), name, tags, body) where

import System.FilePath
import System.Directory
import Path
import Tags

data Note = Note
    { name :: String
    , tags :: [String]
    , body :: String }
    deriving (Show)

parseNote :: String -> String -> Note
parseNote name contents = Note (tagify name) tags body
    where (body, tags) = getTags contents

readNote :: FilePath -> IO Note
readNote fp = parseNote name <$> readFile fp
    where name = takeBaseName fp

module Main where

import Lib
import Args
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Map (Map, (!))
import Data.Maybe

groupBy :: Foldable t => Ord k => (a -> k) -> t a -> Map k a
groupBy f = foldl insert Map.empty
    where insert m x = Map.insert (f x) x m

appendToGroup :: Ord k => k -> a -> Map k [a] -> Map k [a]
appendToGroup k a m = Map.insertWith (++) k [a] m

multiGroupBy :: Foldable t => Ord k => (a -> [k]) -> t a -> Map k [a]
multiGroupBy f = foldl appendAll Map.empty
    where
        appendAll m x = foldl (append x) m (f x)
        append x m k = appendToGroup k x m

emptyNote :: String -> Note
emptyNote name = Note name [] ("# Tag: " ++ name ++ "\n")

getNote :: Map String Note -> String -> Note
getNote m s = fromMaybe (emptyNote s) $ Map.lookup s m

appendNote (Note n t b) a = (Note n t (b ++ a))

addReferencers :: Map String [Note] -> Note -> Note
addReferencers m note = appendNote note referencedByString
    where
        referencedBy = Map.findWithDefault [] (name note) m
        listLinkItem n = "*   [" ++ n ++ "](./" ++ n ++ ".md)"
        list = listLinkItem . name <$> referencedBy
        referencedByString = if null referencedBy then "" else "\n\nReferenced by:\n" ++ List.unlines list

main :: IO ()
main = do
    Args notesDir outDir <- parseArgs
    notes <- parseNotes notesDir
    let
        grouped = groupBy name notes
        referencedBy = multiGroupBy tags notes
        allTags = Set.fromList $ Map.keys grouped ++ (notes >>= tags)
        allNotes = addReferencers referencedBy <$> getNote grouped <$> Set.toList allTags
        in writeNotes outDir allNotes

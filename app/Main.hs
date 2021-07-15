module Main where

import Lib
import Args
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Map (Map, (!))
import Data.List (sortOn)
import Data.Maybe
import Data.Ord

groupBy :: Foldable t => Ord k => (a -> k) -> t a -> Map k a
groupBy f = foldl insert Map.empty
    where insert m x = Map.insert (f x) x m

appendToGroup :: Ord k => k -> a -> Map k [a] -> Map k [a]
appendToGroup k a = Map.insertWith (++) k [a]

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

tagList :: [String] -> String
tagList = List.unlines . fmap listLinkItem
    where listLinkItem n = "*   [" ++ n ++ "](./" ++ n ++ ".md)"

referencedByString :: [String] -> String
referencedByString [] = ""
referencedByString names = "\nReferenced by:\n" ++ tagList names

addReferencers :: Map String [Note] -> Note -> Note
addReferencers m note@(Note "__index" _ _) = appendNote note (referencedByString indexReferences)
    where
        indexReferences = (fmap fst . sortOn (Down . snd) . Map.toList . fmap length) m
addReferencers m note = appendNote note (referencedByString referencedBy)
    where
        referencedBy = name <$> Map.findWithDefault [] (name note) m

main :: IO ()
main = do
    Args notesDir outDir <- parseArgs
    notes <- parseNotes notesDir
    let
        grouped = groupBy name notes
        referencedBy = multiGroupBy tags notes
        allTags = Set.insert "__index" $ Set.fromList $ Map.keys grouped ++ (notes >>= tags)
        allNotes = addReferencers referencedBy . getNote grouped <$> Set.toList allTags
        in writeNotes outDir allNotes

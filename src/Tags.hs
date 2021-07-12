{-# LANGUAGE RecursiveDo, ViewPatterns, ScopedTypeVariables #-}

module Tags (tagify, getTags) where

import Prelude hiding ((<>))
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Regex
import Text.Parsers.Frisby
import Control.Monad.State.Lazy

type TagWriter t = State (Set String) t

putTag :: String -> String -> TagWriter String
putTag str (tagify -> "") = putTag str str
putTag str (tagify -> tag) = do
    modify (Set.insert tag)
    return $ "[" ++ str ++ "](./" ++ tag ++ ".md)"

replace reg rep s = subRegex reg s rep

slugSpaces = replace (mkRegex "[ ]+") "-"
removeExtra = replace (mkRegex "[^ 0-9A-Za-z_-]") ""
strip = f . f where f = reverse . dropWhile isSpace

tagify :: String -> String
tagify =  fmap toLower . slugSpaces . removeExtra . strip

fromPair :: (t, t) -> [t]
fromPair (a, b) = [a, b]

singleton c = [c]

parser :: PM s (P s (TagWriter String))
parser = mdo
    anyCharStr :: P s String <- newRule $ anyChar ## singleton

    top :: P s (TagWriter String) <- newRule $ between bof eof body
    body :: P s (TagWriter String) <- newRule $ str <> many tagAndStr ## \(s1, ts) -> concat <$> sequence (s1 : ts)
    tagAndStr :: P s (TagWriter String) <- newRule $ tag <> str ## \(t, s) -> concat <$> sequence [t, s]

    at :: P s Char <- newRule $ char '@'

    str :: P s (TagWriter String) <- newRule $ many (doesNotMatch at ->> strChar) ## return . concat
    strChar :: P s String  <- newRule $ text "\\@" // anyCharStr

    tag :: P s (TagWriter String) <- newRule $ at ->> tagPart <> tagPart ## uncurry putTag
    tagPart :: P s String <- newRule $ between open close tagStr
    tagStr :: P s String <- newRule $ many (tagChar // tagNest) ## concat
    tagNest :: P s String <- newRule $ open <> tagStr <> close ## (\((o, s), c) -> o ++ s ++ c)

    tagChar :: P s String <- newRule $ doesNotMatch bracket ->> tagCharChar
    tagCharChar :: P s String <- newRule $ text "\\[" // text "\\]" // anyCharStr

    open :: P s String <- newRule $ char '[' ## singleton
    close :: P s String <- newRule $ char ']' ## singleton
    bracket :: P s String <- newRule $ oneOf "[]" ## singleton

    return top

extractTags :: String -> TagWriter String
extractTags = runPeg parser

getTags :: String -> (String, [String])
getTags text = (node, Set.toList tags)
    where (node, tags) = runState (extractTags text) Set.empty

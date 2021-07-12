module Notes (Notes, parseNotes, writeNotes) where

import System.Directory
import Path hiding ((</>), addExtension)
import System.FilePath
import Note

type Notes = [Note]

relativeTo d = (d </>)

listDirectoryAbsolute :: FilePath -> IO [FilePath]
listDirectoryAbsolute path = fmap (relativeTo path) <$> listDirectory path

parseNotes :: Path Abs Dir -> IO Notes
parseNotes notesDir = do
    files <- listDirectoryAbsolute notesfp
    mapM readNote files
    where notesfp = toFilePath notesDir

md name = addExtension name "md"

writeNote :: Path Abs Dir -> Note -> IO ()
writeNote outDir (Note name _ body) = writeFile outFile body
    where
        outDirFp = toFilePath outDir
        outFile = outDirFp </> md name

writeNotes :: Path Abs Dir -> Notes -> IO ()
writeNotes outDir notes = do
    createDirectoryIfMissing True (toFilePath outDir)
    mapM (writeNote outDir) notes
    return ()

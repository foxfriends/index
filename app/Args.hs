module Args (Args (Args), parseArgs) where
import System.Environment
import System.Directory
import System.Exit
import Path

data Args = Args
    { notesDir :: Path Abs Dir
    , outDir :: Path Abs Dir }

fix :: Path Abs Dir -> SomeBase Dir -> Path Abs Dir
fix _ (Abs dir) = dir
fix base (Rel dir) = base </> dir

parseResolvedDir :: FilePath -> IO (Path Abs Dir)
parseResolvedDir f = canonicalizePath f >>= parseAbsDir

parseArgs :: IO Args
parseArgs = do
    args <- getArgs
    case args of
        [notes, out] -> Args
            <$> parseResolvedDir notes
            <*> parseResolvedDir out
        _ -> usage

usage :: IO a
usage = die "usage:\n    index <notes_path> <out_path>"

{-# LANGUAGE TupleSections #-}

module RecurseFile where

import qualified Data.ByteString as BS
import Data.List.Extra (drop1, isPrefixOf)
import System.Directory.Extra (doesDirectoryExist, doesFileExist, listFilesInside)
import System.Exit
import System.FilePath (isPathSeparator, takeExtension, takeFileName, (</>))
import System.FilePattern
import System.IO (hPutStrLn, stderr)

-- | SEE: https://github.com/ndmitchell/hlint/blob/086bf4430362ccced79ba2363840952eec67382a/src/CmdLine.hs#L245
(<\>) :: String -> FilePath -> FilePath
"." <\> x = x
x <\> y = x </> y

-- SEE: https://github.com/ndmitchell/hlint/blob/086bf4430362ccced79ba2363840952eec67382a/src/Util.hs#L30-L33
exitMessage :: String -> IO a
exitMessage msg = do
  hPutStrLn stderr msg
  exitWith $ ExitFailure 1

-- | SEE: https://github.com/ndmitchell/hlint/blob/086bf4430362ccced79ba2363840952eec67382a/src/CmdLine.hs#L249-L263
resolveFile :: [FilePattern] -> [FilePattern] -> Maybe FilePath -> FilePath -> IO [FilePath]
resolveFile includeGlobs ignoreGlobs =
  getFile
    (toPredicate includeGlobs)
    (toPredicate ignoreGlobs)
    ["."]
    ["dhall"]
  where
    toPredicate :: [FilePattern] -> FilePath -> Bool
    toPredicate [] = const False
    toPredicate globs = \x -> not $ null $ m [((), cleanup x)]
      where
        m = matchMany (map ((),) globs)

    cleanup :: FilePath -> FilePath
    cleanup ('.' : x : xs) | isPathSeparator x, not $ null xs = xs
    cleanup x = x

-- | SEE: https://github.com/ndmitchell/hlint/blob/086bf4430362ccced79ba2363840952eec67382a/src/CmdLine.hs#L266-L288
getFile :: (FilePath -> Bool) -> (FilePath -> Bool) -> [FilePath] -> [String] -> Maybe FilePath -> FilePath -> IO [FilePath]
getFile _ _ _ _ (Just tmpfile) "-" =
  -- make sure we don't reencode any Unicode
  BS.getContents >>= BS.writeFile tmpfile >> pure [tmpfile]
getFile _ _ _ _ Nothing "-" = pure ["-"]
getFile _ _ [] _ _ file = exitMessage $ "Couldn't find file: " ++ file
getFile include ignore (p : ath) exts t file = do
  isDir <- doesDirectoryExist $ p <\> file
  if isDir
    then do
      let ignoredDirectories = ["dist", "dist-newstyle"]
          avoidDir x = let y = takeFileName x in "_" `isPrefixOf` y || ("." `isPrefixOf` y && not (all (== '.') y)) || y `elem` ignoredDirectories || ignore x
          avoidFile x = let y = takeFileName x in "." `isPrefixOf` y || ignore x
      xs <- listFilesInside (pure . not . avoidDir) $ p <\> file
      pure [x | x <- xs, drop1 (takeExtension x) `elem` exts, not $ avoidFile x, include x]
    else do
      isFil <- doesFileExist $ p <\> file
      if isFil
        then pure [p <\> file | not $ ignore $ p <\> file]
        else getFile include ignore ath exts t file

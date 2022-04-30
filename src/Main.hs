{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (FilePath)

import Turtle
import System.Environment (getArgs)
import Data.List (isPrefixOf, (\\))
import Data.Foldable (for_, foldl', traverse_)
import qualified Control.Foldl as Fold
import Data.Either.Extra (fromEither)
import Data.Traversable (for)
import qualified Data.List.NonEmpty as NE


files :: FilePath -> IO [FilePath]
files = reduce Fold.list . ls

moveFiles :: FilePath -> FilePath -> IO ()
moveFiles fromdir todir = sh $ do
  curf <- ls fromdir
  mv curf (todir </> filename curf)

fpLine :: FilePath -> Line
fpLine = unsafeTextToLine . fromEither . toText

usage :: IO ()
usage = traverse_ echo 
  [ "usage: dedup (dirname|option)*"
  , "--help - Prints this usage guide."
  , "--diff - Allows inner directories with differing basenames."
  , ""
  , "If there are no arguments given, use CWD as the target."
  , "'dedups' every *valid* directory in the argument list and ignores errors from previous directories until the end. Exits successfully *only if* every given directory was successfully 'deduped'."
  ]

main :: IO ()
main = do
  args <- getArgs

  -- First, we only accept a single directory to dedup
  let actualFiles = filter (not . isPrefixOf "--") args
  dirs <- case actualFiles of
    [] -> NE.singleton <$> pwd
    filenames -> return $ decodeString <$> NE.fromList filenames

  -- Now, handle options
  let options = map (drop 2) $ filter (isPrefixOf "--")  args

  when ("help" `elem` options) $ do
    usage
    exit ExitSuccess

  let innerDirNameDoesNotHaveToMatch = "diff" `elem` options

  let unknownOptions = (\\ ["help", "diff"]) options
  unless (null unknownOptions) $ do
    err $ "Unrecognized options: " <> fromString (show unknownOptions)
    usage
    exit (ExitFailure 1)

  results <- for dirs $ \dir -> do
    dirStat <- stat dir

    if isDirectory dirStat then do
      filenames <- files dir
      case filenames of
        [single] | innerDirNameDoesNotHaveToMatch || (basename single == basename dir) -> do
          file <- stat single
          if isDirectory file then do
            -- Actual deduping.
            moveFiles single dir
            rmdir single
            return ExitSuccess
          else do
            err $ fpLine single <> " is not a directory."
            return (ExitFailure 1)

        -- When a single directory (or file, because it's before a `isDirectory` check)
        -- does not have the same basename.
        [single] | not innerDirNameDoesNotHaveToMatch -> do
          err $ fpLine (basename single) <> " does not have the same basename as parent directory " <> fpLine (basename dir) <> "."
          err "Use --diff option to allow this."
          return (ExitFailure 1)

        [] -> do
          err $ fpLine dir <> "is empty."
          return (ExitFailure 1)

        fs -> do
          err $ "There's more than one file in " <> fpLine dir <> " -> possible duplicates."
          return (ExitFailure 1)

    else do
      err $ fpLine dir <> " is not a directory."
      return (ExitFailure 1)

  -- Aggregate all of the exit codes and either terminate successfully or
  -- exit with the leftmost exit code.
  exit $ foldl1 (\b a -> if b == ExitSuccess then a else b) results
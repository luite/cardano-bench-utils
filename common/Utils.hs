{-# LANGUAGE ScopedTypeVariables #-}
{-
    Some utilities that are not specific to a single program
 -}
module Utils where

import Control.Monad
import System.Exit
import System.Process
import System.Directory
import Control.Exception
import System.IO
import Data.Char
import qualified Control.Exception as E
import qualified System.IO.Error as E

removeItemIfExists :: FilePath -> IO ()
removeItemIfExists file =
    removePathForcibly file `E.catch` \(e::E.IOError) ->
      if E.isDoesNotExistError e then pure () else E.throwIO e

runIn :: FilePath -> FilePath -> Maybe FilePath -> [String] -> IO ()
runIn working_dir pgm mb_stdout args = do -- pure ()
  putStrLn ("running " ++ pgm ++ " " ++ show args ++ " in " ++ working_dir)
  let run_and_wait mb_h = do
          ph <- runProcess pgm args (Just working_dir) Nothing Nothing mb_h Nothing
          ec <- waitForProcess ph
          case ec of
            ExitSuccess -> pure ()
            _           -> throwIO ec
  case mb_stdout of
     Nothing -> run_and_wait Nothing
     Just file -> withFile file WriteMode (\h -> run_and_wait (Just h))

runIn' :: FilePath -> FilePath -> [String] -> IO ()
runIn' working_dir pgm args = runIn working_dir pgm Nothing args

requireDirExists :: FilePath -> String -> IO ()
requireDirExists fp descr = do
    exists <- doesDirectoryExist fp
    unless exists (die $ "directory `" ++ fp ++ "' does not exist:\n    " ++ descr)

requireExists :: FilePath -> String -> IO ()
requireExists fp descr = do
    exists <- doesPathExist fp
    unless exists (die $ "path `" ++ fp ++ "' does not exist:\n    " ++ descr)

requireExists' :: [FilePath] -> String -> IO ()
requireExists' fps descr = forM_ fps $ \fp -> requireExists fp descr

writeIfNotExists :: FilePath -> (Handle -> IO ()) -> IO ()
writeIfNotExists fp fill = do
    exists <- doesPathExist fp
    unless exists (withFile fp WriteMode fill)


trim :: String -> String
trim = let f = dropWhile isSpace . reverse in f . f

renameIfExists :: FilePath -> FilePath -> IO ()
renameIfExists from to = do
    exists <- doesPathExist from
    when exists (renamePath from to)

removeIfExists :: FilePath -> IO ()
removeIfExists fp = do
    exists <- doesPathExist fp
    when exists (removePathForcibly fp)

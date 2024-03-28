{-
  Generate library configuration for cardano-node benchmark, building a local
  copy of the cardano-node dependencies and generating corresponding
  cabal project configuration files.

  This needs to be run once before the run-bench program can be used.

  Usage:

     cabal run localize-dependencies

  This generates some data:

     - lib.orig: local copy of dependencies of the cardano-node project
          (intended to contain an unmodified version)
     - lib.opt: local copy of dependencies of the cardano-node project
          (intended to contain experimental updates. a git repository is
           created here to make it easy to track the changes)
      - configs/generated/libs-orig.project: project file pulling everything
          from libs.orig into the cabal project
      - configs/generated/libs-opt.project: project file pulling everything
          from libs.opt into the cabal project
 -}
module Main where

import System.Directory
import System.Exit
import System.Process
import System.IO
import Control.Monad
import System.FilePath
import Control.Exception
import System.Environment
import Data.Maybe
import qualified Data.List as L
import Data.Set(Set)
import qualified Data.Set as S
import Utils

tmpDir :: FilePath
tmpDir = "cabal-dir.tmp"

projectDir :: String
projectDir = "configs"

{-
  We exclude some packages that cause cyclic dependencies or are incompatible
  with all the GHC versions we want to test.

  Excluding a package here will just exclude it from the cabal project, it will
  still be installed as a regular dependency. A different version may be
  selected depending on the configuration.
 -}
excludedPackages :: Set String
excludedPackages = S.fromList [ "splitmix"
                              , "split"
                              , "tasty"
                              , "tasty-expected-failure"
                              , "libsystemd-journal"
                              , "megaparsec"
                              , "happy"
                              , "hsc2hs"
                              ]

runCabal :: FilePath -> FilePath -> [String] -> IO ()
runCabal config_dir working_dir args = do
  env0 <- getEnvironment
  let env1 = env0 ++ [("CABAL_DIR", config_dir)]
      run_and_wait = do
          ph <- runProcess "cabal" args (Just working_dir) (Just env1) Nothing Nothing Nothing
          ec <- waitForProcess ph
          case ec of
            ExitSuccess -> pure ()
            _           -> throwIO ec
  run_and_wait


findFilesWithExtension :: FilePath -> String -> IO [FilePath]
findFilesWithExtension dir ext = do
    entries <- listDirectory dir
    concat <$> forM entries processEntry
    where
        processEntry rel_entry = do
            let entry = dir </> rel_entry
            is_dir <- doesDirectoryExist entry
            is_file <- doesFileExist entry
            if is_dir
              then findFilesWithExtension entry ext
              else if is_file
                   then (pure (if ext `isExtensionOf` entry then [entry] else []))
                   else pure []

main :: IO ()
main = do
    -- check that we have everything in the right location
    cabal_file_exists <- doesFileExist "cardano-bench-utils.cabal"
    unless cabal_file_exists (die "please run this in the root of the cardano-bench-utils source tree")
    cardano_node_exists <- doesFileExist "cardano-node/cardano-node/cardano-node.cabal"
    unless cardano_node_exists (die "please initialize a cardano-node source repository in the cardano-node directory")

    removePathForcibly tmpDir
    createDirectoryIfMissing True tmpDir

    -- download all the dependencies of cardano-node to a fresh cabal config dir    
    cabalDir <- makeAbsolute tmpDir
    cardanoDir <- makeAbsolute "cardano-node"

    runCabal cabalDir cardanoDir ["update"]
    -- XXX perhaps we should do this with an empty package db to be sure we get everything?
    runCabal cabalDir cardanoDir ["build", "exe:cardano-node", "--only-download", "--force-reinstalls", "--builddir=dist-newstyle.empty-builddir"]
    
    {-
       "cabal get" doesn't appear to find the packages we just downloaded
       perhaps it's not project file aware?

       instead we just scan for all tar.gz files in our temp directory.
       unfortunately this means that we miss out on cabal file updates
       that may exist in the repositories
     -}
    pkg_archives <- findFilesWithExtension tmpDir ".tar.gz"
    
    {-
       extract all dependencies to the lib and lib.orig subdirs
       the lib.orig subdir should remain unmodified and serve as a baseline
       for benchmarks
     -}
    _ <- catMaybes <$> forM pkg_archives (extractPackage "lib.orig")
    pkg_names <- filter (`S.notMember` excludedPackages) . L.sort . catMaybes <$>
         forM pkg_archives (extractPackage "lib.opt")
    removeItemIfExists "lib.opt/.git"
    forM_ ["lib.orig", "lib.opt"] $ \lib -> do
      runIn' lib "git" ["init"]
      runIn' lib "git" ["add", "."]
      runIn' lib "git" ["commit", "-m", "import"]

    -- generate the project files that pull the dependencies into the project
    createDirectoryIfMissing True projectDir
    writeProject (projectDir </> "generated" </> "libs-orig.project") "../lib.orig" pkg_names
    writeProject (projectDir </> "generated" </> "libs-opt.project") "../lib.opt" pkg_names


writeProject :: FilePath -> String -> [String] -> IO ()
writeProject project_file prefix pkgs = withFile project_file WriteMode $ \h -> do
    hPutStrLn h "packages:"
    forM_ pkgs $ \pkg -> hPutStrLn h ("    " ++ prefix ++ "/" ++ pkg)

extractPackage :: FilePath -> FilePath -> IO (Maybe String)
extractPackage lib_dir archive
  | Just (pkgname, version) <- getPkgNameVersion archive
  = do
      putStrLn ("Extracting `" ++ pkgname ++ "' version " ++ version ++ " (" ++ archive ++ ")")
      let target = lib_dir </> pkgname
      createDirectoryIfMissing True target
      _ <- rawSystem "tar" ["-C", target, "--strip-components", "1", "-xzf", archive]
      pure (Just pkgname)
  | otherwise = do
      putStrLn ("Skipping " ++ archive)
      pure Nothing

getPkgNameVersion :: FilePath -> Maybe (String, String)
getPkgNameVersion archive
  | isVersion rver && not (null rname) = Just (reverse rname, reverse rver)
  | otherwise = Nothing
    where
      isVersion xs = not (null xs) && all isVersionChar xs
      isVersionChar c = c `elem` "0123456789."

      basename = dropExtension (takeBaseName archive)
      (rver, rname0) = break (=='-') (reverse basename)
      rname = drop 1 rname0 -- get rid of the dash

{-

Old version, this required a cabal.project.freeze file generated by
cabal freeze

  Usage:
    - build cabal project to populate the cache
    - run cabal freeze to generate the cabal.project.freeze file

    - customize the prefix and repositories variables to match your local
      cabal cache
    - run this script to extract the dependencies to the lib subdirectory
    - add the output of this script to the packages list in the 
      cabal.project fil

-}

{-

import System.IO
import Data.List
import Data.Maybe
import System.Directory
import System.Process
import Control.Exception

prefix = "/Users/luite/.cabal/packages/"
repositories = [ prefix ++ "cardano-haskell-packages"
               , prefix ++ "hackage.haskell.org"
               ]

main :: IO ()
main = do
    putStrLn "Localizing dependencies"
    packages <- getPackages "cabal.project.freeze"
    mapM_ print packages
    pkgs <- mapM (uncurry extractPackage) packages
    putStrLn "optional-packages:"
    mapM_ putStrLn pkgs

extractPackage :: String -> String -> IO String
extractPackage name version = go repositories
  where
    package = name ++ "-" ++ version
    go [] = pure ("  -- " ++ package ++ " (not found)")
    go (repo:xs) = do
      let path = repo ++ "/" ++ name ++ "/" ++ version ++ "/" ++ package ++ ".tar.gz"
      exists <- doesFileExist path
      case exists of
        True -> do
          -- putStrLn $ "  found " ++ path
          runExtract name path
          return ("  -- lib/" ++ name)
        False -> go xs

runExtract :: String -> FilePath -> IO ()
runExtract name path = do
  let target = "lib/" ++ name
  exists <- doesDirectoryExist target
  case exists of
    True -> putStrLn $ "  " ++ target ++ " already exists"
    False -> do
      putStrLn $ "  extracting " ++ path ++ " to " ++ target
      createDirectory target
      _ <- rawSystem "tar" ["-C", target, "--strip-components", "1", "-xzf", path]
      return ()


{-
  We should really use the cabal library to parse the freeze file, but
  this works for now
 -}
getPackages :: FilePath -> IO [(String, String)]
getPackages freezeFile = do
  freeze <- lines <$> readFile freezeFile
  return (mapMaybe processLine freeze)

processLine :: String -> Maybe (String, String)
processLine line
  | Just line' <- stripPrefix "constraints:" line = processLine line'
  | Just line' <- stripPrefix " " line = processLine line'
  | Just line' <- stripPrefix "any." line = processPackage line'
processLine _ = Nothing

processPackage :: String -> Maybe (String, String)
processPackage line
  = let (name, constraint) = break (== ' ') line
    in case stripPrefix " ==" constraint of
        Just version -> Just (name, takeWhile (/=',') version)
        _ -> Nothing
-}
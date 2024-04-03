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

   TODO:

       Some of this file is a hack around `cabal fetch` not being project file
       aware. We should really implement the functionality in cabal-install
       so that cabal fetch` can unpack all dependencies of the current project
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
import qualified Data.List as L
import Data.Set(Set)
import qualified Data.Set as S
import Utils
import qualified Cabal.Plan as Plan
import qualified Data.Map as M
import qualified Data.Text as T

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
excludedPackages = S.fromList [ -- GHC boot libraries
                               "Cabal"
                              , "Cabal-syntax"
                              , "array"
                              , "base"
                              , "binary"
                              , "bytestring"
                              , "containers"
                              , "deepseq"
                              , "directory"
                              , "exceptions"
                              , "filepath"
                              , "ghc"
                              , "ghc-bignum"
                              , "ghc-boot"
                              , "ghc-boot-th"
                              , "ghc-compact"
                              , "ghc-heap"
                              , "ghc-prim"
                              , "ghci"
                              , "haskeline"
                              , "hpc"
                              , "integer-gmp"
                              , "integer-simple"
                              , "libiserv"
                              , "mtl"
                              , "parsec"
                              , "pretty"
                              , "process"
                              , "rts"
                              , "stm"
                              , "system-cxx-std-lib"
                              , "template-haskell"
                              , "terminfo"
                              , "text"
                              , "time"
                              , "transformers"
                              , "unix"
                              , "xhtml"
                              -- other packages to prevent dependency cycles
                              , "splitmix"
                              , "split"
                              , "tasty"
                              , "tasty-expected-failure"
                              , "libsystemd-journal"
                              , "megaparsec"
                              , "happy"
                              , "hsc2hs"
                              , "Win32-network"
                              -- cardano packages from cardano-node/cabal.project
                              , "cardano-node"
                              , "cardano-node-capi"
                              , "cardano-node-chairman"
                              , "cardano-submit-api"
                              , "cardano-testnet"
                              , "cardano-tracer"
                              , "cardano-topology"
                              , "locli"
                              , "plutus-scripts-bench"
                              , "tx-generator"
                              , "trace-dispatcher"
                              , "trace-resources"
                              , "trace-forward"
                              ]

{-
This appends cardano-node/cabal.project to our temporary
cabal configuration file. It's a workaround for `cabal get` not supporting
project files.

Not everything from the project file is supported in the cabal configuration,
so this results in some (harmless) warnings.
 -}
addRepositories :: FilePath -> FilePath -> IO ()
addRepositories cardanoNodePath cabalConfig = do
  config <- readFile' cabalConfig
  unless ("cardano-haskell-packages" `L.isInfixOf` config) $ do
    project <- readFile (cardanoNodePath </> "cabal.project")
    appendFile cabalConfig ('\n':project)

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
    let buildDir = "dist-newstyle.empty-builddir"
    removePathForcibly (cardanoDir </> buildDir)

    runCabal cabalDir cardanoDir ["update"]
    addRepositories cardanoDir (cabalDir </> "config")
    runCabal cabalDir cardanoDir ["update"]

    runCabal cabalDir cardanoDir ["build", "cardano-node", "--only-download", "--force-reinstalls", "--builddir=" ++ buildDir]

    let planFile = cardanoDir </> "dist-newstyle.empty-builddir/cache/plan.json"
    plan <- Plan.findAndDecodePlanJson (Plan.ExactPath planFile)
    let pkgs0 = getPackageList plan
        pkgs  = filter (\pkg -> pkgNameString pkg `S.notMember` excludedPackages) pkgs0
        pkg_names = map pkgNameString pkgs
        cabalExtractPackage tgtDir pkg = do
          createDirectoryIfMissing True tgtDir
          runCabal cabalDir cardanoDir ["get", pkgString pkg, "-d", ".." </> tgtDir]
          renameDirectory (tgtDir </> pkgString pkg) (tgtDir </> pkgNameString pkg) -- drop version

    forM_ ["lib.orig", "lib.opt"] $ \lib -> do
      forM_ pkgs (cabalExtractPackage lib)
      runIn' lib "git" ["init"]
      runIn' lib "git" ["add", "."]
      runIn' lib "git" ["commit", "-m", "import"]

    writeProject (projectDir </> "generated" </> "libs-orig.project") "../lib.orig" pkg_names
    writeProject (projectDir </> "generated" </> "libs-opt.project") "../lib.opt" pkg_names

pkgNameString :: Plan.PkgId -> String
pkgNameString (Plan.PkgId (Plan.PkgName pkgName) _ ) = T.unpack pkgName

pkgString :: Plan.PkgId -> String
pkgString (Plan.PkgId (Plan.PkgName pkgName) (Plan.Ver pkgVer)) =
  T.unpack pkgName ++ "-" ++ L.intercalate "." (map show pkgVer)

getPackageList :: Plan.PlanJson -> [Plan.PkgId]
getPackageList plan = S.toList . S.fromList $ map Plan.uPId (M.elems (Plan.pjUnits plan))

writeProject :: FilePath -> String -> [String] -> IO ()
writeProject project_file prefix pkgs = withFile project_file WriteMode $ \h -> do
    hPutStrLn h "packages:"
    forM_ pkgs $ \pkg -> hPutStrLn h ("    " ++ prefix ++ "/" ++ pkg)

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

{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-
  Configuration generator and event runner.

  This generates the cabal.config files and adjust cardano-node-service.nix to
  run different benchmarking configurations. It also runs postprocessing steps
  and renames the result directory so we can find back the results more easily

  Usage:

  runbench.hs <test-profile> [configuration]

  Test profiles are: solo-xs, solo, default

 -}
module Main where

import Control.Monad (when, forM_)
import System.Environment (getArgs)
import System.FilePath
import System.Directory
import System.IO
import System.Exit
import qualified Data.List as L
import Data.Maybe
import qualified Control.Exception as E

import Utils

-- XXX prefix
cfgdir :: FilePath
cfgdir = "configs"

getEventlog2html :: IO (Maybe FilePath)
getEventlog2html = findExecutable "eventlog2html"

data Libs = Orig
          | Opt
    deriving (Eq, Show, Ord, Enum)

data GhcVer = Ghc8107
            | Ghc964
    deriving (Eq, Show, Ord, Enum, Bounded)

-- location of the ghc executable (symlink) for this GHC version
ghcLoc :: GhcVer -> FilePath
ghcLoc Ghc8107 = "ghc/ghc-8.10.7"
ghcLoc Ghc964  = "ghc/ghc-9.6.4"

-- location of the ghc-pkg for this GHC version
ghcPkgLoc :: GhcVer -> FilePath
ghcPkgLoc Ghc8107 = "ghc/ghc-pkg-8.10.7"
ghcPkgLoc Ghc964  = "ghc/ghc-pkg-9.6.4"

-- shortened GHC name used in .project file names and configuration names
ghcName :: GhcVer -> String
ghcName Ghc8107 = "ghc8107"
ghcName Ghc964  = "ghc964"

data Flavour = Vanilla    -- ^ standard build with optimization, for validating timings
             | Perf       -- ^ use the `perf` profiler
             | ProfLate   -- ^ use late cost centre profiling (and generate a JSON report for speedscope)
             | StgStacks  -- ^ dump STG call stacks on GC
             | Ticky      -- ^ tickyticky profiling, generate report with eventlog2html
             | TickyDict  -- ^ tickyticky profiling with dictionary call-sites.
             | TraceCalls -- ^ dynamic call-graph tracing
    deriving (Eq, Show, Ord, Enum)

rtsFlags :: Config -> String
rtsFlags cfg = unwords (map show $ defaultRtsFlags ++ concatMap flavourRtsFlags (cFlavour cfg))

defaultRtsFlags :: [String]
defaultRtsFlags = ["-N2", "-I0", "--disable-delayed-os-memory-return", "-A16m", "-qg", "-qb" ]

flavourRtsFlags :: Flavour -> [String]
flavourRtsFlags Vanilla    = []
flavourRtsFlags Perf       = []
flavourRtsFlags ProfLate   = ["-pj"]
flavourRtsFlags StgStacks  = ["-A1m", "--enable-gc-stack-dumps"] -- more GCs since we get a stack trace for each GC
flavourRtsFlags Ticky      = ["-lT" ]
flavourRtsFlags TickyDict  = ["-lT" ]
flavourRtsFlags TraceCalls = ["-A1m", "--enable-call-graph-tracing" ] -- reduce memory

-- the test profile
data Profile = Default
             | Solo
             | SoloXs
    deriving (Eq, Show, Ord, Enum)

configs :: [Config]
configs = [ Config Ghc8107 Orig [Vanilla]
          , Config Ghc8107 Opt  [Vanilla]
          , Config Ghc964  Orig [Vanilla]
          , Config Ghc964  Opt  [Vanilla]
          , Config Ghc964  Orig [Ticky, TickyDict]
          , Config Ghc964  Opt  [Ticky, TickyDict]
          , Config Ghc964  Orig [ProfLate]
          , Config Ghc964  Opt  [ProfLate]
          , Config Ghc964  Orig [TraceCalls]
          , Config Ghc964  Opt  [TraceCalls]
          , Config Ghc964  Orig [StgStacks]
          , Config Ghc964  Opt  [StgStacks]
          ]

checkPrepareDirectory :: IO ()
checkPrepareDirectory = do
    -- check that expected files exist
    requireExists' ["cardano-bench-utils.cabal", "configs", "configs/generated", "configs/include/default.project"]
        "please run this in the cardano-bench-utils source directory"
    requireExists "cardano-node"
        "please install a copy of the cardano-node source repository"
    forM_ [minBound..] $ \ver -> requireExists' [ghcLoc ver, ghcPkgLoc ver]
        "please symlink ghc and ghc-pkg for the tested GHC versions"
    requireExists' ["lib.opt", "lib.orig", "configs/generated/libs-opt.project", "configs/generated/libs-orig.project"]
        "please run `localize-dependencies' to initialize the library directories"

    -- make the config/generated/ghc-VERSION.project configuration files with absolute paths
    forM_ [minBound..] $ \ver -> writeIfNotExists ("configs/generated/ghc-" ++ ghcName ver ++ ".project") (\h -> do
        abs_ghc_loc <- makeAbsolute (ghcLoc ver)
        abs_ghc_pkg_loc <- makeAbsolute (ghcPkgLoc ver)
        hPutStrLn h ("with-compiler: " ++ abs_ghc_loc)
        hPutStrLn h ("with-hc-pkg: " ++ abs_ghc_pkg_loc)
      )
    eh <- getEventlog2html
    when (isNothing eh) (die "Make sure that `eventlog2html' is in the search path")

usage :: IO ()
usage = do
    hPutStrLn stderr "usage: run-bench <profile> <config|all>"
    hPutStrLn stderr ""
    hPutStrLn stderr "Available profiles:"
    forM_  (enumFrom Default) $ \p -> hPutStrLn stderr ("    " ++ profileName p)
    hPutStrLn stderr ""
    hPutStrLn stderr "Available configs:"
    forM_ configs $ \cfg -> hPutStrLn stderr ("    " ++ configName cfg)
    hPutStrLn stderr ""

libDir :: Libs -> FilePath
libDir Orig = "lib.orig"
libDir Opt  = "lib.opt"

profileMakeTarget :: Profile -> String
profileMakeTarget p =
    case profileName p of
        "default" -> "forge-stress-pre-autonix"
        pn        -> "forge-stress-pre-" ++ pn ++ "-autonix"

profileName :: Profile -> String
profileName Default = "default"
profileName Solo    = "solo"
profileName SoloXs  = "solo-xs"

configName :: Config -> String
configName cfg = L.intercalate "-" (ghcName (cGhc cfg) : libName (cLibs cfg) : map flavourName (cFlavour cfg))
  where
    libName Orig = "orig"
    libName Opt = "opt"
    flavourName Vanilla = "vanilla"
    flavourName Perf = "perf"
    flavourName StgStacks = "stgstacks"
    flavourName ProfLate = "prof-late"
    flavourName Ticky = "ticky"
    flavourName TickyDict = "ticky-dict"
    flavourName TraceCalls = "trace-calls"

data Config = Config { cGhc     :: GhcVer
                     , cLibs    :: Libs
                     , cFlavour :: [Flavour]
                     }
    deriving (Eq, Ord, Show)

main :: IO ()
main = do
    checkPrepareDirectory
    args <- getArgs
    case args of
        [profile, cfg]
          | Just p <- L.find (\p -> profileName p == profile) (enumFrom Default)
          , Just c <- L.find (\c -> configName c == cfg) configs
          -> printExceptions (runBench p c)
        [profile, "all"]
          | Just p <- L.find (\p -> profileName p == profile) (enumFrom Default) -> do
            putStrLn "running all bench configurations"
            printExceptions (forM_ configs (runBench p))
        _ -> usage >> exitWith (ExitFailure 1)

printExceptions :: IO a -> IO a
printExceptions x = x `E.catch` \(e::E.SomeException) ->
    putStrLn ("exception: " ++ show e) >> E.throwIO e

imp :: Handle -> String -> String -> IO ()
imp h from c = hPutStrLn h ("import: " ++ from ++ "/" ++ c ++ ".project")

writeCabalConfig :: FilePath -> Config -> IO ()
writeCabalConfig cfg_file cfg = withFile cfg_file WriteMode $ \h -> do
  hPutStrLn h "import: ../cardano-node/cabal.project"
  imp h "include" "default"
  imp h "generated" ("ghc-" ++ ghcName (cGhc cfg))
  case cLibs cfg of
    Orig    -> imp h "generated" "libs-orig"
    Opt     -> imp h "generated" "libs-opt"
  forM_ (cFlavour cfg) $ \case
      Vanilla    -> imp h "include" "fl-vanilla"
      Perf       -> imp h "include" "fl-perf"
      ProfLate   -> imp h "include" "fl-prof-late"
      StgStacks  -> imp h "include" "fl-stgstacks"
      Ticky      -> imp h "include" "fl-ticky"
      TickyDict  -> imp h "include" "fl-ticky-dict"
      TraceCalls -> imp h "include" "fl-trace-calls"

{-
    we need to adjust cardano-node-service.nix a little to:
      - run our own cardano-node instead of the one built by nix
      - pass the correct RTS flags for the type of run we're doing
 -}
patchServiceFile :: Config -> IO ()
patchServiceFile cfg = do
    let serviceFile = "nix/nixos/cardano-node-service.nix"
        serviceFile' = "cardano-node" </> serviceFile
    -- first restore it
    runIn' "cardano-node" "git" ["checkout", serviceFile]
    dir <- getCurrentDirectory
    let cnode = dir </> "cnode"
        updLine :: String -> String
        updLine x
            | "\"${cfg.executable} run\"" `L.isInfixOf` x
            = "       \"" ++ cnode ++ " run\"" -- XXX this probably breaks if the path has spaces
            | "--disable-delayed-os-memory-return" `L.isInfixOf` x
            = "        default = [ " ++ rtsFlags cfg ++ " ];"
            | otherwise
            = x
    serv <- lines <$> readFile' serviceFile'
    withFile serviceFile' WriteMode (\h -> forM_ serv (hPutStrLn h . updLine))

runBench :: Profile -> Config -> IO ()
runBench pr cfg = do
    putStrLn ("running bench: " ++ profileName pr ++ " " ++ configName cfg)

    -- make sure the config file exists
    let name = configName cfg
        configFile = cfgdir </> name <.> "project"
    configExists <- doesFileExist configFile
    when (not configExists) (writeCabalConfig configFile cfg)

    -- build the cardano-node executable
    let buildDir = "dist-" ++ name
        cabalLocs = [ "--project-file=" ++ "../" ++ configFile
                    , "--builddir=" ++ "../" ++ buildDir
                    ]
    runIn' "cardano-node" "cabal" (["build", "exe:cardano-node", "-j"] ++ cabalLocs)

    -- symlink 'cnode' to the executable we just built
    removeItemIfExists "cardano-node.location.txt"
    removeItemIfExists "cnode"
    runIn "cardano-node" "cabal" (Just "cardano-node.location.txt") (["list-bin", "cardano-node"] ++ cabalLocs)
    nodeloc <- trim <$> readFile "cardano-node.location.txt"
    nodeexists <- doesFileExist nodeloc
    when (not nodeexists) (die $ "cardano node executable not found at: " ++ nodeloc)
    createFileLink nodeloc "cnode"
    putStrLn $ "using node location: " ++ nodeloc

    -- make sure we run the now-symlinked node with the right rts flags
    patchServiceFile cfg

    -- actually run the benchmark
    -- _ <- pure pr
    runIn' "cardano-node" "make" [profileMakeTarget pr]

    -- generate profile files
    forM_ [0..9] $ \(i::Int) -> do
        let nodepath = "cardano-node/run/current/node-" ++ show i
        exists <- doesDirectoryExist nodepath
        when exists (postProcess cfg nodepath)

    -- XXX todo, publish processed profile files in some web hosted dir?

    -- resolve symlink, prefix the output
    putStrLn "renaming result directory"
    origName <- getSymbolicLinkTarget "cardano-node/run/current"
    renameDirectory ("cardano-node/run" </> origName)
                    ("cardano-node/run" </> origName ++ "_" ++ configName cfg)

    -- make sure no stray processes are left

    putStrLn ("finished bench: " ++ profileName pr ++ " " ++ configName cfg)


forceKill :: String -> IO ()
forceKill process_name = runIn' "." "killall" ["-9", process_name] `E.catch`
  \(_::ExitCode) -> pure () -- killall will return a non-zero exit status if there are no processes killed. Just ignore that

postProcess :: Config -> FilePath -> IO ()
postProcess cfg nodepath = do
  -- write what we did
  writeFile (nodepath </> "config.txt") (show cfg)

  -- write library diffs:
  when (cLibs cfg == Opt) $ do
    (runIn "." "diff" (Just (nodepath </> "libs.patch")) ["-ur", libDir Orig, libDir Opt ]) `E.catch`
      \(e::ExitCode) -> case e of
                          ExitSuccess   -> pure ()
                          ExitFailure 1 -> pure ()      -- diff exits with status 1 if there are differences, it's not an error condition
                          _             -> E.throwIO e
  -- if we have an eventlog file, filter it to get mempool snapshot events
  haveEventlog <- doesFileExist (nodepath </> "cnode.eventlog")
  when haveEventlog $ do
    e2h <- maybe (die "'eventlog2html' not found") pure =<< getEventlog2html
    runIn' "." "cabal" ["run", "filter-eventlog", nodepath </> "cnode.eventlog", nodepath </> "cnode.filtered.eventlog"]
    runIn' nodepath e2h ["cnode.eventlog"]
    runIn' nodepath e2h ["cnode.filtered.eventlog"]

  -- kill any remaining stray processes
  forceKill "cardano-tracer"
  -- XXX add more here if we see runs interrupted by different strays

  -- remove things that use unnecessary space
  cleanupNodeDir nodepath

cleanupNodeDir :: FilePath -> IO ()
cleanupNodeDir nodepath = do
    removePathForcibly (nodepath </> "db")
    renameIfExists (nodepath </> "stderr") (nodepath </> "stderr.txt")
    renameIfExists (nodepath </> "stdout") (nodepath </> "stdout.txt")
    removeIfExists (nodepath </> "node.socket")

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import System.Environment
import System.Exit
import Text.Read (readMaybe)

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy as TL

import Data.Text (Text)

import qualified Data.Map as M
import Data.Map (Map)

import System.IO
import Data.Maybe

main :: IO ()
main = do
    args <- getArgs
    case args of
        [min_count_str, in_file, out_file] | Just min_count <- readMaybe min_count_str -> processGraph min_count in_file out_file
        _ -> die "usage: filtergraph <min_count> <in_file> <out_file>"

{-

 -}
type Node = Text

data Stats = Stats { edges :: Map (Node, Node) Int
                   , inDegree :: Map Node Int
                   , outDegree :: Map Node Int
                   }

processGraph :: Int -> FilePath -> FilePath -> IO ()
processGraph min_count in_file out_file = do
    in_lines <- T.lines <$> T.readFile in_file
    let collected = collectLines in_lines
        stats = mkStats collected
    withFile out_file WriteMode $ \h -> do
        T.hPutStrLn h "digraph {"
        putCollected h stats min_count collected
        T.hPutStrLn h "}"
        pure ()
    putStrLn "done"

mkStats :: R -> Stats
mkStats m = Stats { edges = m
                  , inDegree  = M.fromListWith (+) (map (\((_, t), c) -> (t, c)) (M.toList m))
                  , outDegree = M.fromListWith (+) (map (\((f, _), c) -> (f, c)) (M.toList m))
                  }

outDegMin :: Int -> Text -> Stats -> Bool
outDegMin c node s = maybe False (>= c) (M.lookup node (outDegree s))

inDegMin :: Int -> Text -> Stats -> Bool
inDegMin c node s = maybe False (>= c) (M.lookup node (inDegree s))

putCollected :: Handle -> Stats -> Int -> R -> IO ()
putCollected h s min_count m = forM_ (M.toList m) putItem
  where
    putItem :: ((Text,Text), Int) -> IO ()
    putItem ((f,t),c)
      | c >= min_count || inDegMin min_count t s || outDegMin min_count f s || alwaysAccept s f t = 
        let ct = if c == 1 then ";"
                           else " [label=" <> TL.toStrict (TB.toLazyText (TB.decimal c)) <> "];"
        in T.hPutStrLn h ("  " <> f <> mkio f <> " -> " <> t <> mkio t <> ct)
      | otherwise = pure ()

    it x = TL.toStrict . TB.toLazyText . TB.decimal $ x
    mkio nd =
      let indeg  = fromMaybe 0 (M.lookup nd $ inDegree s)
          outdeg = fromMaybe 0 (M.lookup nd $ outDegree s)
       in "_#" <> it indeg <> "+" <> it outdeg 
    --   | otherwise = mempty
    -- fio = "_" <> it 

alwaysAccept :: Stats -> Text -> Text -> Bool
alwaysAccept s f t = False -- {- isPoly f || -} isPoly t
  where
    isPoly x = "polyzu" `T.isInfixOf` x

type R = Map (Text, Text) Int

collectLines :: [Text] -> R
collectLines ys = go ys mempty
  where
    go :: [Text] -> R -> R
    go [] m = m
    go (x:xs) m
      | [from, to] <- T.splitOn "->" x = go xs (addItem m (T.strip from) (T.strip to))
      | otherwise = go xs m

    addItem :: R -> Text -> Text -> R
    addItem m f t
      | [t', w] <- T.splitOn " [weight=" t
      , Right (wt, _) <- T.decimal w = addItemWeight m f t' wt
      | otherwise = addItemWeight m f (T.takeWhile (/=';') t) 1

    dropAddr :: Text -> Text
    dropAddr t = let (before, _after) = T.breakOn "_@0x" t in before

    addItemWeight :: R -> Text -> Text -> Int -> R
    addItemWeight m f t w
      | not (T.null f) && not (T.null t) = M.insertWith (+) (dropAddr f, dropAddr t) w m
      | otherwise = m

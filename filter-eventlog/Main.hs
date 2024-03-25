module Main where
import System.Environment (getArgs)
import System.Exit (die)

import GHC.RTS.Events
import qualified Data.Text as T
import Data.List (sortBy)
import Data.Function (on)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [event_name, event_log_in, event_log_out] ->
            processLog event_name event_log_in event_log_out
        _ -> putStrLn "Usage: filterlog <event_name> <event_log_in> <event_log_out>"

processLog :: String -> String -> String -> IO ()
processLog event_name event_log_in event_log_out = do
    putStrLn $ "Processing log for event " ++ event_name
    putStrLn $ "Log in: " ++ event_log_in
    putStrLn $ "Log out: " ++ event_log_out
    r <- readEventLogFromFile event_log_in
    case r of
        Right event_log -> do
            putStrLn "Event log read successfully"
            -- putStrLn $ "Event log: " ++ show event_log
            filtered_event_log <- filterLog event_name event_log
            writeEventLogToFile event_log_out filtered_event_log
        Left err -> die err

filterLog :: String -> EventLog -> IO EventLog
filterLog event_name event_log = do
    new_events <- go False (sortBy (compare `on` evTime) $ events (dat event_log))
    pure $ EventLog {
                    header = header event_log,
                    dat = Data new_events }
  where
    go _ [] = pure []
    go b (e:es)
      | UserMarker name <- evSpec e, name == start_marker = print e >> (e:) <$> go True es
      | UserMarker name <- evSpec e, name == end_marker = print e >> (e:) <$> go False es
      | TickyCounterSample {} <- evSpec e = if b then (e:) <$> go b es else go b es
      | otherwise = (e:) <$> go b es    
    start_marker = T.pack (event_name ++ "_start")
    end_marker = T.pack (event_name ++ "_end")

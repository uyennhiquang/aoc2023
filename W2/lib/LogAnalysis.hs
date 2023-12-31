module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = go (words s)
  where
  go :: [String] -> LogMessage
  go l = case l of
    ("I" : time : message)        -> if null message
                                      then Unknown (unwords l)
                                      else LogMessage Info (read time :: Int) (unwords message)
    ("W" : time : message)        -> if null message
                                      then Unknown (unwords l)
                                      else LogMessage Warning (read time :: Int) (unwords message)
    ("E" : sev : time : message)  -> if null message
                                      then Unknown (unwords l)
                                      else LogMessage (Error (read sev :: Int)) (read time :: Int) (unwords message)
    _                             -> Unknown (unwords l)

parse :: String -> [LogMessage]
parse s = go (lines s) []
  where
  go :: [String] -> [LogMessage] -> [LogMessage]
  go input output 
    | null input = output
    | otherwise = go (tail input) (output ++ [parseMessage (head input)])

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert (LogMessage msgType1 time1 msg1) (Node left (LogMessage msgType2 time2 msg2) right)
  | time1 < time2 = Node (insert (LogMessage msgType1 time1 msg1) left) (LogMessage msgType2 time2 msg2) right
  | time1 > time2 = Node left (LogMessage msgType2 time2 msg2) (insert (LogMessage msgType1 time1 msg1) right)
  | otherwise     = Node left (LogMessage msgType2 time2 msg2) right

build :: [LogMessage] -> MessageTree
build l = go l Leaf
  where 
    go :: [LogMessage] -> MessageTree -> MessageTree
    go [] tree = tree
    go l tree = go (tail l) (insert (head l) tree)

inOrder :: MessageTree -> [LogMessage]
inOrder tree = go tree []
  where
    go :: MessageTree -> [LogMessage] -> [LogMessage]
    go Leaf logs = logs
    go (Node left (LogMessage msgType time msg) right) logs
      = go left (LogMessage msgType time msg : go right logs) 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = go logs []
  where
    go :: [LogMessage] -> [String] -> [String]
    go [] messages = messages
    -- go (Unknown _ : theLogs) messages = go theLogs messages
    go (LogMessage msgType time msg : theLogs) messages = case msgType of
      Error n -> if n >= 50
                    then go theLogs (messages ++ [msg])
                    else go theLogs messages
      _       -> go theLogs messages
    go (_ : theLogs) messages = go theLogs messages

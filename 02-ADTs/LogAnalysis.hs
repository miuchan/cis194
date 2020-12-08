{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage message
  | head ws == "I" = LogMessage Info (read (ws!!1)::Int) (unwords (drop 2 ws))
  | head ws == "W" = LogMessage Warning (read (ws!!1)::Int) (unwords (drop 2 ws))
  | head ws == "E" = LogMessage (Error (read (ws!!1)::Int)) (read (ws!!2)::Int) 
    (unwords (drop 3 ws))
  | otherwise = Unknown message
  where ws = words message
  
parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert lmsg@LogMessage{} Leaf = Node Leaf lmsg Leaf
insert lmsg1@(LogMessage _ ts1 _) (Node left lmsg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left lmsg2 (insert lmsg1 right)
  | otherwise = Node (insert lmsg1 left) lmsg2 right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lmsg right) = inOrder left ++ [lmsg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong strings = filter (not . null) (map message strings)
  where message (LogMessage (Error errorLevel)  _ string)
          | errorLevel >= 50 = string
          | otherwise = ""
        message _ = ""
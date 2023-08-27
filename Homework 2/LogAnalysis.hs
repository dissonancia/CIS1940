{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage logMsg = case words logMsg of
  "I" : time : msg     -> LogMessage Info (read time) (unwords msg)
  "W" : time : msg     -> LogMessage Warning (read time) (unwords msg)
  "E" : n : time : msg -> LogMessage (Error (read n)) (read time) (unwords msg)
  _                    -> Unknown logMsg

parse :: String -> [LogMessage]
parse [] = []
parse xs = map parseMessage $ lines xs

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) sortedTree = sortedTree
insert logMsg Leaf            = Node Leaf logMsg Leaf
insert logM@(LogMessage _ time _)
       (Node left currentLog@(LogMessage _ currentTime _) right) =
         if time <= currentTime then
           Node (insert logM left) currentLog right
         else
           Node left currentLog (insert logM right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf logMsg right) = logMsg : inOrder right
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong xs =
                let filterErrorLog logMsg = case logMsg of
                      LogMessage (Error severity) _ _ -> severity >= 50
                      _                               -> False
                    mapErrorLog (LogMessage _ _ msg) = msg
                in map mapErrorLog $ inOrder $ build $ filter filterErrorLog xs

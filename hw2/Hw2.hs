module Hw2 where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Log


parseInfo :: [String] -> Maybe LogMessage
parseInfo (time:str)
    | all isDigit time
    = Just $ LogMessage Info (read time) (unwords str)
    | otherwise
    = Nothing

parseWarning :: [String] -> Maybe LogMessage
parseWarning (time:str)
    | all isDigit time
    = Just $ LogMessage Warning (read time) (unwords str)
    | otherwise
    = Nothing

parseError :: [String] -> Maybe LogMessage
parseError (int:time:str)
    | all isDigit int && all isDigit time
    = Just $ LogMessage (Error $ read int) (read time) (unwords str)
    | otherwise
    = Nothing

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
    "I":time:str     -> fromMaybe (Unknown msg) $ parseInfo (time:str)
    "W":time:str     -> fromMaybe (Unknown msg) $ parseWarning (time:str)
    "E":int:time:str -> fromMaybe (Unknown msg) $ parseError (int:time:str)
    _                -> Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert logMsg Leaf         = Node Leaf logMsg Leaf

insert logMsg (Node a b c)
    | inputTime < nodeTime   = Node (insert logMsg a) b c
    | otherwise              = Node a b (insert logMsg c)
  where
    LogMessage _ inputTime _ = logMsg
    LogMessage _ nodeTime _  = b

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node a b c) = inOrder a ++ [b] ++ inOrder c

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMsgs = [msg | LogMessage (Error int) _ msg <- logMsgs', int >= 50]
  where logMsgs'      = inOrder $ build logMsgs

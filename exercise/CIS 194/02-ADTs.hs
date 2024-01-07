{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage str
  | head strList == "I" =
      LogMessage
        Info
        (read (strList !! 1) :: TimeStamp)
        (unwords $ drop 2 strList)
  | head strList == "W" =
      LogMessage
        Warning
        (read (strList !! 1) :: TimeStamp)
        (unwords $ drop 2 strList)
  | head strList == "E" =
      LogMessage
        (Error (read (strList !! 1) :: Int))
        (read (strList !! 2) :: TimeStamp)
        (unwords $ drop 3 strList)
  | otherwise = Unknown str
  where
    strList = words str

parse :: String -> [LogMessage]
parse logStr = [parseMessage str | str <- lines logStr]

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree =
  msgTree
insert logMsg Leaf =
  Node Leaf logMsg Leaf
insert logMsg (Node leftChild nodelogMsg@(LogMessage _ nodeTimeStamp _) rightChild) =
  if timeStamp < nodeTimeStamp
    then Node (insert logMsg leftChild) nodelogMsg rightChild
    else Node leftChild nodelogMsg (insert logMsg rightChild)
  where
    (LogMessage _ timeStamp _) = logMsg
insert _ _ = error "func(insert) should not be here!"

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftChild nodelogMsg rightChild) = inOrder leftChild ++ [nodelogMsg] ++ inOrder rightChild

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logList =
  [ msgStr
    | (LogMessage (Error errNo) _ msgStr) <- (inOrder . build) logList,
      errNo > 50
  ]

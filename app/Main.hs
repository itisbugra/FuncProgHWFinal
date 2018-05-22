module Main where

import qualified CLI as CLI
import qualified CharTrie as CharTrie

import System.IO
import System.Environment

main :: IO ()
main = do
  agent

agent :: IO ()
agent = do
  CLI.printManifest
  args <- getArgs
  case length args of
    1 -> do
      words <- readWords $ head args
      loop $ CharTrie.insertList words
      return ()
    _ -> do
      return ()

loop :: CharTrie.Trie -> IO CharTrie.Trie
loop trie = do
  nextTrie <- CLI.promptAction trie
  loop nextTrie

readWords :: String -> IO [String]
readWords fileName = do
  contents <- readFile fileName
  return $ lines contents

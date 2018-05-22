module Main where

import qualified CLI as CLI
import qualified CharTrie as CharTrie

main :: IO ()
main = do
  agent

agent :: IO ()
agent = do
  CLI.printManifest
  loop CharTrie.empty
  return ()

loop :: CharTrie.Trie -> IO CharTrie.Trie
loop trie = do
  nextTrie <- CLI.promptAction trie
  loop nextTrie

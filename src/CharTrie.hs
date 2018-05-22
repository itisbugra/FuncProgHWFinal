module CharTrie (
  Trie,
  Word,
  empty,
  insert,
  insertList,
  search,
  getWords,
  prefix
) where
  import qualified Data.Map as M
  import Data.Maybe
  import System.Environment
  import Prelude hiding (Word)

  data Trie = Trie {end :: Bool, children :: M.Map Char Trie} 
    deriving (Eq, Show)
  type Word = String

  empty :: Trie
  empty = Trie {end = False, children = M.empty}

  insert :: Word -> Trie -> Trie
  insert ""   = error "unable to insert empty string"
  insert word = insert' word
    where
      insert' :: Word -> Trie -> Trie
      insert' [] trie =
        let contained = children trie
          in case M.null contained of
            True ->
              trie { end = True, children = M.empty }
            False ->
              trie { end = True, children = contained }
      insert' (h:t) trie =
        let contained = children trie 
          in case M.lookup h contained of
            Just element -> 
              trie { end = (end trie), children = M.insert h (insert' t $ element) contained }
            Nothing -> 
              trie { end = (end trie), children = M.insert h (insert' t $ empty) contained }
      

  insertList :: [Word] -> Trie
  insertList list =
    insertList' list empty
    where
      insertList' :: [Word] -> Trie -> Trie
      insertList' (h:t) trie = insertList' t (insert h trie)

  search :: Word -> Trie -> Bool
  search = undefined

  getWords :: Trie -> [Word]
  getWords = undefined

  prefix :: Word -> Trie -> Maybe [Word]
  prefix = undefined

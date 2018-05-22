module Main where
  import System.IO
  import System.Environment
  import System.Exit
  import qualified Data.Text as Text
  import Data.Char 
  import qualified Data.Map as M
  import Data.Maybe
  import Data.List hiding (insert)
  import Prelude hiding (Word)

  main :: IO ()
  main = do
    agent

  agent :: IO ()
  agent = do
    args <- getArgs
    case length args of
      1 -> do
        words <- readWords $ head args
        printManifest
        loop $ insertList words
        return ()
      _ -> do
        hPutStrLn stderr $ "== usage: ./main file_name"
        return ()

  loop :: Trie -> IO Trie
  loop trie = do
    nextTrie <- promptAction trie
    loop nextTrie

  readWords :: String -> IO [String]
  readWords fileName = do
    contents <- readFile fileName
    return $ lines contents

  data Action = Add | Search | PrefixQuery | Print | Exit
    deriving (Eq, Show)

  -- | The manifest string showing options
  manifest :: String
  manifest = "a) Add word \ns) Search word \nf) Find words with prefix \np) Print all words \ne) Exit"

  ask :: String
  ask = "  Enter the action:"

  convertAction :: Char
                -> Maybe Action
  convertAction 'a' = Just Add
  convertAction 's' = Just Search
  convertAction 'f' = Just PrefixQuery
  convertAction 'p' = Just Print
  convertAction 'e' = Just Exit
  convertAction _ = Nothing

  -- | Sanitizes the given string by trimming it, and converting it to a lower case equivalent.
  sanitize :: String -- ^ The input string to be sanitized.
           -> String -- ^ The sanitized string.
  sanitize = Text.unpack . Text.strip . Text.toLower . Text.pack

  -- | Enqueries the standard input stream to get a string input, then sanitizes it and returns.
  getInput :: IO String -- ^ The sanitized input.
  getInput = do
    input <- getLine
    return $ sanitize input

  doAction :: Action -> Trie -> IO Trie
  doAction Add trie = do
    wordOrPrefix <- promptWordOrPrefix
    let newTrie = insert wordOrPrefix trie
    putStrLn $ ">> Word \"" ++ wordOrPrefix ++ "\" is added to the glossary."
    return newTrie
  doAction Search trie = do
    word <- promptWord
    case search word trie of
      True -> do
        putStrLn $ ">> Word \"" ++ word ++ "\" found in glossary."
        return trie
      False -> do
        putStrLn $ ">> Word \"" ++ word ++ "\" not found in glossary."
        return trie
  doAction PrefixQuery trie = do
    prefixToSearch <- promptPrefix
    case prefix prefixToSearch trie of
      Just words -> do
        printStringList words
        putStrLn $ ">> " ++ (show (length words)) ++ " words found at total."
        return trie
      Nothing -> do
        hPutStrLn stderr $ "== error: no strings found with given prefix \"" ++ prefixToSearch ++ "\"."
        return trie
  doAction Print trie = do
    printStringList $ getWords trie
    putStrLn $ ">> " ++ (show $ length $ getWords trie) ++ " words found at total."
    return trie
  doAction Exit trie = do
    exitSuccess
    return trie
  
  askWordOrPrefix :: String
  askWordOrPrefix = "  Enter word/prefix:"

  askWord :: String
  askWord = "  Enter word:"

  askPrefix :: String
  askPrefix = "  Enter prefix:"

  promptWordOrPrefix :: IO String
  promptWordOrPrefix = prompt askWordOrPrefix

  promptWord :: IO String
  promptWord = prompt askWord

  promptPrefix :: IO String
  promptPrefix = prompt askPrefix

  prompt :: String -> IO String
  prompt str = do
    putStrLn str
    input <- getInput
    return input

  printStringList :: [String] -> IO ()
  printStringList list = printStringList' list 1
    where 
      printStringList' :: [String] -> Integer -> IO ()
      printStringList' (h:t) i = do
        putStrLn $ "  " ++ (show i) ++ ". \"" ++ h ++ "\""
        printStringList' t $ i + 1
      printStringList' [] i = do
        return ()

  printManifest :: IO ()
  printManifest = do
    putStrLn manifest

  promptAction :: Trie -> IO (Trie)
  promptAction trie = do
    putStrLn ask
    input <- getInput
    case convertAction (input !! 0) of
      Just action -> doAction action trie
      Nothing     -> do
        hPutStrLn stderr "== error: unexpected action literal"
        return trie

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
      insertList' [] trie = trie

  search :: Word -> Trie -> Bool
  search "" trie   = False
  search word trie = search' word trie
    where
      search' :: Word -> Trie -> Bool
      search' [] trie = end trie
      search' (h:t) trie =
        case M.lookup h $ children trie of
          Just element ->
            search' t $ element
          Nothing ->
            False

  getWords :: Trie -> [Word]
  getWords trie = scanDepth "" [] trie
    where
      scanDepth :: String -> [Word] -> Trie -> [Word]
      scanDepth passed words trie = foldl (\acc el -> acc ++ (fetch el) ++ (traverse el)) words (M.toList $ children trie)
        where
          fetch :: (Char, Trie) -> [Word]
          fetch (char, trie) =
            case end trie of
              True  ->
                [passed ++ [char]]
              False ->
                []

          traverse :: (Char, Trie) -> [Word]
          traverse (char, trie) =
            case trie == empty of
              True  ->
                []
              False ->
                scanDepth (passed ++ [char]) [] trie

  prefix :: Word -> Trie -> Maybe [Word]
  prefix word trie = 
    case prefix' word trie of
      []  -> Nothing
      any -> Just any
    where
      prefix' :: Word -> Trie -> [Word]
      prefix' word trie = filter (\el -> word `isPrefixOf` el) (getWords trie)

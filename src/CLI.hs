module CLI (
  printManifest,
  promptAction
) where
  import System.IO
  import System.Exit
  import qualified Data.Text as Text
  import Data.Char 

  import CharTrie as CharTrie

  data Action = Add | Search | PrefixQuery | Print | Exit
    deriving (Eq, Show)

  -- | The manifest string showing options
  manifest :: String
  manifest = "a) Add word \ns) Search word \nf) Find words with prefix \np) Print all words \ne) Exit"

  ask :: String
  ask = "Enter the action:"

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

  doAction :: Action -> CharTrie.Trie -> IO CharTrie.Trie
  doAction Add trie = do
    wordOrPrefix <- promptWordOrPrefix
    return $ CharTrie.insert wordOrPrefix trie
  doAction Search trie = do
    word <- promptWord
    let exists = CharTrie.search word trie
    return trie
  doAction PrefixQuery trie = do
    prefix <- promptPrefix
    let word = CharTrie.prefix prefix trie
    return trie
  doAction Print trie = do
    printStringList $ CharTrie.getWords trie
    return trie
  doAction Exit trie = do
    exitSuccess
    return trie
  
  askWordOrPrefix :: String
  askWordOrPrefix = "Enter word/prefix:"

  askWord :: String
  askWord = "Enter word:"

  askPrefix :: String
  askPrefix = "Enter prefix:"

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
  printStringList (h:t) = do
    putStrLn h
    printStringList t

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

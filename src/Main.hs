{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (IOException, catch)
import Control.Monad (when)

import Data.Monoid (mconcat)

import Options.Applicative

import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Process (readProcess)

-- Git stuff. Find the branch we're on and the .git directory.

git :: String -> IO [String]
git args = lines <$> readProcess "git" (words args) []
            `catch` \(_ :: IOException) -> do
                             putStrLn "Something went wrong, maybe you're not in a Git repository?"
                             exitFailure

getBranch, getGitDir :: IO String
getBranch = head <$> git "symbolic-ref --short HEAD"
getGitDir = head <$> git "rev-parse --show-cdup" >>= \move -> return $ move ++ ".git"

-- Data structures!

data ToDo = ToDo { description :: String
                 , complete    :: Bool
                 } deriving (Eq, Read, Show)

data Checklist = Checklist { branchRef :: String
                           , todos     :: [ToDo]
                           } deriving (Eq,Show)

-- Loading and saving data (by cheating).

putChecklist :: Checklist -> IO ()
putChecklist checklist = do
    root <- getGitDir
    let listdir = root </> "checklist"
        output  = todos checklist
    createDirectoryIfMissing False listdir
    if null output
        then removeFile (listdir </> branchRef checklist) `catch`
                (\(_ :: IOError) -> putStrLn "Error cleaning up checklist file in .git/checklist/")
        else writeFile (listdir </> branchRef checklist) (show output)
                `catch` \(_ :: IOException) -> putStrLn "Error writing checklist file."

getChecklist :: String -> IO Checklist
getChecklist branch = do
    root <- getGitDir
    let path = root </> "checklist" </> branch
    exists <- doesFileExist path
    if exists
        then Checklist branch . read <$> readFile path
        else return (Checklist branch [])

-- Operations: Adding, removing, marking done and not done.

change :: Act -> Checklist -> Checklist
change (Add desc) (Checklist b ts) = Checklist b (ToDo desc False:ts)
change (Remove n) (Checklist b ts) = let del = map snd . filter (\i -> n /= fst i)
                                     in Checklist b (del `withNumbered` ts)
change (Done   n) (Checklist b ts) = Checklist b (mark True n `withNumbered` ts)
change (Undo   n) (Checklist b ts) = Checklist b (mark False n `withNumbered` ts)

withNumbered :: ([(Int,a)] -> [a]) -> [a] -> [a]
f `withNumbered` as = f $ zip [1..] as

mark :: Bool -> Int -> [(Int,ToDo)] -> [ToDo]
mark v n its = map f its
    where f (i,t) | i == n    = t { complete = v }
                  | otherwise = t

-- Pretty printing! Not very pretty for >9 items.

view :: Observe -> Checklist -> String
view List  checklist = unlines $ prettyChecklist checklist
view Stats (Checklist br []) = br ++ ": No tasks defined yet"
view Stats (Checklist br ts) = br ++ ": " ++ tasksToDo ++ totalTasks
    where tasksToDo  = case length $ filter (not . complete) ts of
                            0 -> "Nothing to do! ("
                            1 -> "1 task left! ("
                            n -> show n ++ " tasks to do ("
          totalTasks = show (length ts) ++ " in total)"

prettyTodo :: ToDo -> String
prettyTodo t = xmark ++ description t
    where xmark = if complete t then "[x] " else "[ ] "

prettyChecklist :: Checklist -> [String]
prettyChecklist = zipWith f [1..] . map prettyTodo . todos
    where f i desc = show i ++ ": " ++ desc

-- Overall control actions

-- Sometimes the checklist is Left alone and
-- sometimes we Right on it...
withBranch :: Either Observe Act -> String -> IO ()
withBranch (Left act)  branch = getChecklist branch >>= putStr . view act
withBranch (Right act) branch = do
    oldlist <- getChecklist branch
    let newlist = change act oldlist
    when (newlist /= oldlist)
         (putChecklist newlist)
    putStr (view List newlist)

usingArgs :: Option -> IO ()
usingArgs (Option (Common Nothing)       act) = getBranch >>= withBranch act
usingArgs (Option (Common (Just branch)) act) = withBranch act branch

-- Define command line flags and options

data Option = Option { commonOpt :: Common
                     , actionOpt :: Either Observe Act
                     } deriving Show

data Common = Common { branchName :: (Maybe String) } deriving Show

data Observe = List | Stats deriving Show
data Act = Add String | Remove Int | Done Int | Undo Int deriving Show

cli :: Parser Option
cli = subparser $ mconcat
            [ command "show" (info (Option <$> common <*> showlist)
                                   (progDesc "Show current TODOs"))
            , command "add" (info (Option <$> common <*> add)
                                  (progDesc "Add a TODO"))
            , command "done" (info (Option <$> common <*> done)
                                   (progDesc "Mark a TODO as done."))
            , command "undo" (info (Option <$> common <*> undo)
                                   (progDesc "Item needs redone!"))
            , command "remove" (info (Option <$> common <*> remove)
                                     (progDesc "Remove a TODO (can't be undone)"))
            , command "stats" (info (Option <$> common <*> showstat)
                                    (progDesc "Summary statistics of checklist"))
            ]
    where add    = Right . Add . unwords <$> arguments str (metavar "DESCRIPTION")
          remove = Right . Remove <$> argument auto (metavar "N")
          done   = Right . Done <$> argument auto (metavar "N")
          undo   = Right . Undo <$> argument auto (metavar "N")

          showlist = pure $ Left List
          showstat = pure $ Left Stats

          common = Common <$> (optional $ strOption (long "branch" <> short 'b'
                                                     <> metavar "BRANCH"))

argParser :: ParserInfo Option
argParser = info (helper <*> (blank <|> cli))
                    (progDesc "Per-branch TODO list for Git repositories")
    where blank :: Parser Option -- user enters no arguments
          blank = nullOption (value (Option (Common Nothing) (Left List)) <> internal)

main :: IO ()
main = execParser argParser >>= usingArgs

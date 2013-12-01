module Main where

import Control.Applicative
import Control.Exception
import Control.Monad (when)

import Data.Monoid

import Options.Applicative

import System.Exit
import System.Environment
import System.FilePath ((</>))
import System.Directory
import System.Process (readProcess)

-- Git stuff. Find the branch we're on and the .git directory.

git :: String -> IO [String]
git args = lines <$> readProcess "git" (words args) []
            `catch` \e -> do let x = e :: IOException
                             putStrLn "Something went wrong, maybe you're not in a Git repository?"
                             exitFailure

getBranch = head <$> git "symbolic-ref --short HEAD"
getGitDir = head <$> git "rev-parse --show-cdup" >>= \move -> return $ move ++ ".git"

-- Data structures!

data ToDo = ToDo { description :: String
                 , complete    :: Bool
                 } deriving (Eq, Read, Show)

data Checklist = Checklist { branch :: String
                           , todos  :: [ToDo]
                           } deriving (Eq,Show)

-- Loading and saving data (by cheating).

putChecklist :: Checklist -> IO ()
putChecklist checklist = do
    root <- getGitDir
    let listdir = root </> "checklist"
    createDirectoryIfMissing False listdir
    writeFile (listdir </> branch checklist) $ show $ todos checklist

getChecklist :: String -> IO Checklist
getChecklist branch = do
    root <- getGitDir
    let path = root </> "checklist" </> branch
    exists <- doesFileExist path
    if exists
        then Checklist branch . read <$> readFile path
        else return (Checklist branch [])

-- Operations: Adding, removing, marking done and not done.

run :: Action -> Checklist -> Checklist
run (Add desc) (Checklist b ts) = Checklist b (ToDo desc False:ts)
run (Remove n) (Checklist b ts) = let del = map snd . filter (\i -> n /= fst i)
                                  in Checklist b (del `withNumbered` ts)
run (Done   n) (Checklist b ts) = Checklist b (mark True n `withNumbered` ts)
run (Undo   n) (Checklist b ts) = Checklist b (mark False n `withNumbered` ts)
run Show       c                = c

withNumbered :: ([(Int,a)] -> [a]) -> [a] -> [a]
f `withNumbered` as = f $ zip [1..] as

mark :: Bool -> Int -> [(Int,ToDo)] -> [ToDo]
mark v n its = map f its
    where f (i,t) | i == n    = t { complete = v }
                  | otherwise = t

-- Pretty printing! Not very pretty for >9 items.

prettyTodo :: ToDo -> String
prettyTodo t = xmark ++ description t
    where xmark = if complete t then "[x] " else "[ ] "

prettyChecklist :: Checklist -> [String]
prettyChecklist = zipWith f [1..] . map prettyTodo . todos
    where f i desc = show i ++ ": " ++ desc

printChecklist :: Checklist -> IO ()
printChecklist = putStr . unlines . prettyChecklist

-- 

modifyChecklist act branch = do
    checklist <- getChecklist branch
    let newlist = run act checklist
    when (newlist /= checklist) (putChecklist newlist)
    printChecklist newlist

usingArgs :: Option -> IO ()
usingArgs opts = do
    branch <- maybe getBranch return $ branchName $ commonOpt opts
    modifyChecklist (actionOpt opts) branch

data Option = Option { commonOpt :: Common
                     , actionOpt :: Action
                     } deriving Show

data Common = Common { branchName :: (Maybe String) } deriving Show
instance Monoid Common where
    mempty = Common Nothing
    (Common l) `mappend` (Common r) = Common $ getFirst (First l <> First r)

common :: Parser Common
common = Common <$> branchname
    where branchname = optional $ strOption (long "branch" <> short 'b' <> metavar "BRANCH")

data Action = Show | Add String | Remove Int | Done Int | Undo Int deriving Show

option :: Parser Option
option = subparser $ mconcat
            [ command "show" (info (Option <$> common <*> pure Show)
                                   (progDesc "Show current TODOs"))
            , command "add" (info (Option <$> common <*> add)
                                  (progDesc "Add a TODO"))
            , command "done" (info (Option <$> common <*> done)
                                   (progDesc "Mark a TODO as done."))
            , command "undo" (info (Option <$> common <*> undo)
                                   (progDesc "Item needs redone!"))
            , command "remove" (info (Option <$> common <*> remove)
                                     (progDesc "Remove a TODO (can't be undone)"))
            ]
    where add    = Add . unwords <$> arguments str (metavar "DESCRIPTION")
          remove = Remove <$> argument auto (metavar "N")
          done   = Done <$> argument auto (metavar "N")
          undo   = Undo <$> argument auto (metavar "N")

blank :: Parser Option
blank = nullOption (value (Option mempty Show))

argParser = info (helper <*> (blank <|> Main.option))
                    (progDesc "Per-branch TODO list for Git repositories")

main = execParser argParser >>= usingArgs

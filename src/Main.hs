{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (IOException, catch)
import Control.Monad (when)

import Data.List (intersperse, intercalate)
import Data.Monoid (mconcat)

import Options.Applicative

import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents, removeFile)
import System.Process (readProcess)

import Text.PrettyPrint hiding ((<>), empty)
import qualified Text.PrettyPrint as P

import qualified Paths_git_checklist as V
import Data.Version (showVersion)

-- Git stuff. Find the branch we're on and the .git directory.

git :: String -> IO [String]
git args = lines <$> readProcess "git" (words args) []
            `catch` \(_ :: IOException) -> do
                             putStrLn "Something went wrong, maybe you're not in a Git repository?"
                             exitFailure

getBranch, getGitDir :: IO String
getBranch = head <$> git "symbolic-ref --short HEAD"
getGitDir = head <$> git "rev-parse --show-cdup" >>= \move -> return $ move ++ ".git"

listBranches :: IO [String]
listBranches = do
    root <- getGitDir
    filter (`notElem` [".",".."]) <$> getDirectoryContents (root </> "checklist")
        `catch` \(_ :: IOException) -> return []

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
mark v n = map f
    where f (i,t) | i == n    = t { complete = v }
                  | otherwise = t

-- Pretty printing! Copes with up to 99 items :-)

view :: Observe -> Checklist -> Doc
view List  checklist = prettyChecklist checklist
view Stats (Checklist _ []) = text "No tasks defined yet"
view Stats (Checklist _ ts) = tasksToDo <+> parens (int (length ts) <+> text "in total")
    where tasksToDo  = case length $ filter (not . complete) ts of
                            0 -> text "Nothing to do!"
                            1 -> text "1 task left!"
                            n -> int n <+> text "tasks to do"

prettyTodo :: ToDo -> Doc
prettyTodo t = brackets xmark <+> text (description t)
    where xmark = if complete t then char 'x' else space

prettyChecklist :: Checklist -> Doc
prettyChecklist = vcat . zipWith rightAlign [1..] . map prettyTodo . todos
    where rightAlign i doc = nest (shiftInt i) $ hcat [int i, colon, space, doc]
          shiftInt n = if n < 10 then 1 else 0 -- Over 100 TODOs is silly.

-- Overall control actions

updateWith :: Maybe Act -> [Checklist] -> IO [Checklist]
updateWith (Just a) [c]= let c' = change a c
                         in when (c' /= c) (putChecklist c') >> return [c']
updateWith _        cs = return cs

viewWith :: Observe -> [Checklist] -> Doc
viewWith o []  = P.empty
viewWith o [c] = view o c
viewWith o cs  = vcat $ vsep $ map (\c -> text (branchRef c) $+$ view o c) cs
    where vsep = punctuate (char '\n') -- join list with blank lines

usingArgs :: Option -> IO ()
usingArgs (Option (Common loc) behaviour) = loc2checklist loc >>=
                                            updateWith actOpts >>=
                                            print . viewWith viewOpts
    where viewOpts = either id (const List) behaviour
          actOpts  = either (const Nothing) Just behaviour

          loc2checklist :: Location -> IO [Checklist]
          loc2checklist All       = listBranches >>= mapM getChecklist
          loc2checklist Head      = getBranch >>= getChecklist >>= \c -> return [c]
          loc2checklist (Named b) = mapM getChecklist [b]

-- Define command line flags and options

data Option = Option Common (Either Observe Act) deriving Show

data Common = Common Location deriving Show

data Location = Head | Named String | All deriving Show
data Observe = List | Stats deriving Show
data Act = Add String | Remove Int | Done Int | Undo Int deriving Show

cli :: Parser Option
cli = subparser $ mconcat
            [ command "show" (info (Option <$> common False <*> showlist)
                                   (progDesc "Show current TODOs"))
            , command "add" (info (Option <$> common True <*> add)
                                  (progDesc "Add a TODO"))
            , command "done" (info (Option <$> common True <*> done)
                                   (progDesc "Mark a TODO as done."))
            , command "undo" (info (Option <$> common True <*> undo)
                                   (progDesc "Item needs redone!"))
            , command "remove" (info (Option <$> common True <*> remove)
                                     (progDesc "Remove a TODO (can't be undone)"))
            , command "stats" (info (Option <$> common False <*> showstat)
                                    (progDesc "Summary statistics of checklist"))
            ]
    where add    = Right . Add . unwords <$> arguments str (metavar "DESCRIPTION")
          remove = Right . Remove <$> argument auto (metavar "N")
          done   = Right . Done <$> argument auto (metavar "N")
          undo   = Right . Undo <$> argument auto (metavar "N")

          showlist = pure $ Left List
          showstat = pure $ Left Stats

-- Some commands only valid on a single branch
common :: Bool -> Parser Common
common single = Common <$> if single
                              then onebranch <|> thisbranch
                              else onebranch <|> thisbranch <|> allbranches
    where allbranches = flag' All (long "all" <> short 'a')
          onebranch   = nullOption (reader (return . Named) <> long "branch"
                                        <> short 'b' <> metavar "BRANCH")
          thisbranch  = nullOption (value Head <> internal)

argParser :: ParserInfo Option
argParser = info (helper <*> version <*> (blank <|> cli))
                    (progDesc "Per-branch TODO list for Git repositories")
    where blank :: Parser Option -- user enters no arguments
          blank = nullOption (value (Option (Common Head) (Left List)) <> internal)
          version = flip abortOption (long "version" <> short 'v' <> help "Show version")
                        (InfoMsg $ "git-checklist v" ++ showVersion V.version)

main :: IO ()
main = execParser argParser >>= usingArgs

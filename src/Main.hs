{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (IOException, catch)
import Control.Monad (when)

import Data.List (intersperse, intercalate, sort)
import Data.Monoid (mconcat)

import Options.Applicative
import qualified Text.Parsec as P
import qualified Text.Parsec.String

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents, removeFile)
import System.Process (readProcess)

import Text.PrettyPrint hiding ((<>), empty)
import qualified Text.PrettyPrint as Pretty

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
    dirs <- getDirectoryContents (root </> "checklist")
                        `catch` \(_ :: IOException) -> return []
    return $ sort $ filter (`notElem` [".",".."]) dirs

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
        else writeFile (listdir </> branchRef checklist) (printChecklist checklist)
                `catch` \(_ :: IOException) -> putStrLn "Error writing checklist file."

getChecklist :: String -> IO Checklist
getChecklist branch = do
    root <- getGitDir
    let path = root </> "checklist" </> branch
    exists <- doesFileExist path
    if exists
        then do contents <- readFile path
                case P.parse parseChecklist path contents of
                    Left err -> error $ show err
                    Right todolist -> return $ Checklist branch todolist
        else return (Checklist branch [])


-- TODO
-- quickcheck property: roundtrip
--   parseChecklist . printChecklist (Checklist _ x) == x

parseChecklist :: P.Parsec String () [ToDo]
parseChecklist = parseTodo `P.sepEndBy` P.newline
    where checkmark = P.char '[' *> P.anyChar <* P.char ']' <* P.space
          remaining = P.many1 (P.noneOf "\n")
          parseTodo = do done <- ('x'==) <$> checkmark
                         what <- remaining
                         return $ ToDo { description = what, complete = done }

printChecklist :: Checklist -> String
printChecklist = renderStyle noBreaks . vcat . map prettyTodo . todos
    where noBreaks = style { mode = LeftMode }

upgradeFileFormat :: IO ()
upgradeFileFormat = do
    branchNames <- listBranches
    putStrLn $ "Processing " ++ (show (length branchNames)) ++ " files..."
    mapM_ upgradeOldFormat branchNames
  where
    upgradeOldFormat branch = do
        root <- getGitDir
        let path = root </> "checklist" </> branch
        exists <- doesFileExist path
        if exists
            then do contents <- readFile path
                    putChecklist $ Checklist branch $ read contents
            else putStrLn $ "Failed reading branch " ++ branch

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

updateWith :: Act -> [Checklist] -> IO [Checklist]
updateWith a [c] = let c' = change a c
                   in when (c' /= c) (putChecklist c') >> return [c']

viewWith :: Observe -> [Checklist] -> Doc
viewWith o []  = Pretty.empty
viewWith o [c] = view o c
viewWith o cs  = vcat $ vsep $ map (\c -> text (branchRef c) $+$ view o c) cs
    where vsep = punctuate (char '\n') -- join list with blank lines

usingArgs :: Option -> IO ()
usingArgs (Option loc behaviour) = loc2checklist loc >>= operation
    where
    operation :: [Checklist] -> IO ()
    operation = either (\o cs -> print (viewWith o cs))
                       (\a cs -> updateWith a cs >>= print . viewWith List)
                       behaviour

    loc2checklist :: Location -> IO [Checklist]
    loc2checklist All       = listBranches >>= mapM getChecklist
    loc2checklist Head      = getBranch >>= getChecklist >>= \c -> return [c]
    loc2checklist (Named b) = mapM getChecklist [b]

-- Define command line flags and options

data Option = Option Location (Either Observe Act) deriving Show

data Location = Head | Named String | All deriving Show
data Observe = List | Stats deriving Show
data Act = Add String | Remove Int | Done Int | Undo Int deriving Show

cli_observers :: Mod CommandFields Option
cli_observers =
  command "show"
    (info (Option <$> common <*> show_list)
          (progDesc "Show current TODOs"))
  <> command "stats"
    (info (Option <$> common <*> show_stats)
          (progDesc "Summary statistics of checklist"))
    where
        common = location True -- can have apply to many branches
        show_list = pure (Left List)
        show_stats = pure (Left Stats)

cli_actors :: Mod CommandFields Option
cli_actors = mconcat
            [ command "add" (info (Option <$> common <*> add)
                                  (progDesc "Add a TODO"))
            , command "done" (info (Option <$> common <*> done)
                                   (progDesc "Mark a TODO as done."))
            , command "undo" (info (Option <$> common <*> undo)
                                   (progDesc "Item needs redone!"))
            , command "remove" (info (Option <$> common <*> remove)
                                     (progDesc "Remove a TODO (can't be undone)"))
            ]
    where add    = Right . Add . unwords <$> arguments str (metavar "DESCRIPTION")
          remove = Right . Remove <$> argument auto (metavar "N")
          done   = Right . Done <$> argument auto (metavar "N")
          undo   = Right . Undo <$> argument auto (metavar "N")
          common = location False -- cannot apply to many branches

location :: Bool -> Parser Location
location many = nullOption (value Head <> internal)
              <|> Named <$> strOption (long "branch" <> short 'b' <> metavar "BRANCH")
              <|> if many then allbranches else empty
    where allbranches = flag' All (long "all" <> short 'a')

argParser :: ParserInfo Option
argParser = info (helper <*> version <*> cli)
                    (progDesc "Per-branch TODO list for Git repositories")
    where blank :: Parser Option -- user enters no arguments
          blank = nullOption (value (Option Head (Left List)) <> internal)
          cli = blank <|> hsubparser (cli_observers <> cli_actors)
          version = flip abortOption (long "version" <> short 'v' <> help "Show version")
                        (InfoMsg $ "git-checklist v" ++ showVersion V.version)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--upgrade"] -> upgradeFileFormat --secret option
        _             -> execParser argParser >>= usingArgs

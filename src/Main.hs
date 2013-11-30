module Main where

import Control.Applicative
import Control.Exception

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

putChecklist :: Checklist -> IO Checklist
putChecklist checklist = do
    root <- getGitDir
    let listdir = root </> "checklist"
    createDirectoryIfMissing False listdir
    writeFile (listdir </> branch checklist) $ show $ todos checklist
    return checklist

getChecklist :: String -> IO Checklist
getChecklist branch = do
    root <- getGitDir
    let path = root </> "checklist" </> branch
    exists <- doesFileExist path
    if exists
        then Checklist branch . read <$> readFile path
        else return (Checklist branch [])

-- Operations: Adding, removing, marking done and not done.

add :: String -> Checklist -> Checklist
add desc (Checklist b ts) = Checklist b (ToDo desc False:ts)

remove :: Int -> Checklist -> Checklist
remove n (Checklist b ts) = Checklist b (del n ts)

done n (Checklist b ts) = Checklist b (mark True n ts)
undo n (Checklist b ts) = Checklist b (mark False n ts)

del :: Int -> [ToDo] -> [ToDo]
del n = map snd . filter (\(i,_) -> i/=n) . zip [1..]

mark :: Bool -> Int -> [ToDo] -> [ToDo]
mark v n = zipWith f [1..]
    where f i t | i == n    = t { complete = v }
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

printKey checklist = if length (todos checklist) == 0
                        then putStrLn "key: add <description>"
                        else putStrLn "key: add <description> | done <n> | undo <n> | del <n>"

modifyChecklist f branch = do
    checklist <- getChecklist branch
    printKey checklist
    let newlist = f checklist
    if newlist == checklist
        then return newlist
        else putChecklist newlist

usingArgs :: ([String] -> Checklist -> Checklist) -> [String] -> IO ()
usingArgs _ []   = return ()
usingArgs f args = do
    let (altbranch,unused) = case args of
                            ("-b":b:r)       -> (Just b, r)
                            ("--branch":b:r) -> (Just b, r)
                            _                -> (Nothing,args)
    branch <- maybe getBranch return altbranch
    modifyChecklist (f unused) branch >>= printChecklist

main = do
    args <- getArgs
    case args of
        []            -> getBranch >>= getChecklist >>= \c -> printKey c >> printChecklist c
        ("show":rest) -> const id `usingArgs` rest
        ("add":rest)  -> (add . unwords) `usingArgs` rest
        ("del":rest)  -> (remove . read . head) `usingArgs` rest
        ("done":rest) -> (done . read . head) `usingArgs` rest
        ("undo":rest) -> (undo . read . head) `usingArgs` rest
        _             -> putStrLn "Not implemented yet."

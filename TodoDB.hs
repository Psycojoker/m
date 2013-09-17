module TodoDB
( Todo
, addTodo
, getTodos
, todosToString
, checkDBExist
) where

import Control.Monad(when)
import qualified Utils
import qualified Data.Yaml.Syck as Yaml
import qualified System.Directory as Directory

data Todo = Todo { description :: String
                 , done :: Bool
                 , todoId :: Int
                 } deriving (Show)

-- Yes, those path should be calculated
-- but this make the code soooooooo much more complicated :(
dbPath :: String
dbPath = "/home/psycojoker/.config/t/db.yaml"

dbPathSubPath :: String
dbPathSubPath = "/home/psycojoker/.config/t/"

getDictKeys :: Yaml.YamlElem -> [String]
getDictKeys = Utils.foldlEMap (\acc (key, value) -> Utils.elemToStr key:acc) []

printKeys :: Yaml.YamlNode -> IO ()
printKeys = putStr . unlines . getDictKeys . Yaml.n_elem

parseTodos :: Yaml.YamlElem -> [Todo]
parseTodos Yaml.ENil = []
parseTodos normalCase = (Utils.foldlESeq (\acc elem -> acc ++ [elemToTodo elem]) []) normalCase

elemToTodo :: Yaml.YamlElem -> Todo
elemToTodo elem = Todo {description = (getDescription elem), done = False, todoId = (getId elem)}

todosToString :: [Todo] -> [String]
todosToString = map (\todo -> (show $ todoId todo) ++ " - [" ++ (if done todo then "X" else " ") ++ "] " ++ description todo)

getDescription :: Yaml.YamlElem -> String
getDescription todo = case Utils.getValueFromToString todo "description" of Just a -> a
                                                                            Nothing -> ""

getId :: Yaml.YamlElem -> Int
getId todo = case Utils.getValueFromToString todo "id" of Just a -> read a :: Int
                                                          Nothing -> -1

getTodosDB :: IO Yaml.YamlNode
getTodosDB = Yaml.parseYamlFile dbPath

getTodos :: IO [Todo]
getTodos = (return . parseTodos . Yaml.n_elem) =<< getTodosDB

todosToYaml :: [Todo] -> Yaml.YamlNode
todosToYaml todos = Yaml.mkNode $ Yaml.ESeq $ map (Yaml.mkNode . todoToEmap) todos

todoToEmap :: Todo -> Yaml.YamlElem
todoToEmap todo = Yaml.EMap [convertDescription, convertDone]
    where convertDescription = (stringToYamlNode "description", stringToYamlNode $ description todo)
          convertDone = (stringToYamlNode "done", stringToYamlNode $ show $ done todo)
          stringToYamlNode = Yaml.mkNode . Yaml.EStr . Yaml.packBuf
          boolToYamlNode = Yaml.mkNode . Yaml.EStr . Yaml.packBuf

addTodoToCollection :: String -> [Todo] -> [Todo]
addTodoToCollection todoDescription todos = todos ++ [Todo {description=todoDescription, done=False, todoId=(getNextTodoId todos)}]

getNextTodoId :: [Todo] -> Int
getNextTodoId [] = 1
getNextTodoId todos = if lastId /= -1 then 1 + lastId else 1
    where lastId = maximum $ map todoId todos

addTodo :: String -> IO ()
addTodo description = getTodos >>= (Yaml.emitYamlFile dbPath . todosToYaml . addTodoToCollection description)

checkDBExist :: IO ()
checkDBExist = do
            subDirExist <- Directory.doesDirectoryExist dbPathSubPath
            when (not subDirExist) $ Directory.createDirectoryIfMissing True dbPathSubPath
            fileExist <- Directory.doesFileExist dbPath
            when (not fileExist) $ writeFile dbPath ""

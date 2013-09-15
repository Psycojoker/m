import qualified Utils
import qualified System.Environment as Environment
import qualified Data.Yaml.Syck as Yaml

data Todo = Todo { description :: String
                 , done :: Bool
                 } deriving (Show)

getDictKeys :: Yaml.YamlElem -> [String]
getDictKeys = Utils.foldlEMap (\acc (key, value) -> Utils.elemToStr key:acc) []

printKeys :: Yaml.YamlNode -> IO ()
printKeys = putStr . unlines . getDictKeys . Yaml.n_elem

parseTodos :: Yaml.YamlElem -> [Todo]
parseTodos = Utils.foldlESeq (\acc elem -> acc ++ [elemToTodo elem]) []

elemToTodo :: Yaml.YamlElem -> Todo
elemToTodo elem = Todo {description = (getDescription elem), done = False}

todosToString :: [Todo] -> [String]
todosToString todoList = map (\todo -> "[" ++ (if done todo then "X" else " ") ++ "] " ++ description todo) todoList

getDescription :: Yaml.YamlElem -> String
getDescription todo = Utils.elemToStr $ Utils.getValue todo testKey (Yaml.EStr $ Yaml.packBuf "")

testKey :: Yaml.YamlElem -> Bool
testKey key = (Utils.elemToStr key) == "description"

numberise :: [String] -> [String]
numberise s = map (\(number, string) -> (show number) ++ " - " ++ string) (zip [1..] s)

printDescriptions :: [Todo] -> IO ()
printDescriptions = putStr . unlines . numberise . todosToString

getTodosDB :: IO Yaml.YamlNode
getTodosDB = Yaml.parseYamlFile "pouet.yaml"

getTodos :: IO [Todo]
getTodos = (return . parseTodos . Yaml.n_elem) =<< getTodosDB

addTodoToCollection :: String -> [Todo] -> IO ()
addTodoToCollection _ _ = return ()

addTodo :: [String] -> IO ()
addTodo _ = getTodos >>= addTodoToCollection "" >> return ()

printTodos :: IO ()
printTodos = printDescriptions =<< getTodos

handleCLIArguments :: [String] -> IO ()
handleCLIArguments [] = printTodos
handleCLIArguments ("l":xs) = printTodos
handleCLIArguments ("list":xs) = printTodos
handleCLIArguments ("add":xs) = addTodo xs
handleCLIArguments _ = printTodos

main = Environment.getArgs >>= handleCLIArguments

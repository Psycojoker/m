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

todosToYaml :: [Todo] -> Yaml.YamlNode
todosToYaml todos = Yaml.mkNode $ Yaml.ESeq $ map (Yaml.mkNode . todoToEmap) todos

todoToEmap :: Todo -> Yaml.YamlElem
todoToEmap todo = Yaml.EMap [convertDescription, convertDone]
    where convertDescription = (stringToYamlNode "description", stringToYamlNode $ description todo)
          convertDone = (stringToYamlNode "done", stringToYamlNode $ show $ done todo)
          stringToYamlNode = Yaml.mkNode . Yaml.EStr . Yaml.packBuf
          boolToYamlNode = Yaml.mkNode . Yaml.EStr . Yaml.packBuf

addTodoToCollection :: String -> [Todo] -> [Todo]
addTodoToCollection todoDescription todos = todos ++ [Todo {description=todoDescription, done=False}]

addTodo :: String -> IO ()
addTodo description = getTodos >>= (Yaml.emitYamlFile "/tmp/caca.yaml" . todosToYaml . addTodoToCollection description)

printTodos :: IO ()
printTodos = printDescriptions =<< getTodos

handleCLIArguments :: [String] -> IO ()
handleCLIArguments [] = printTodos
handleCLIArguments ("l":xs) = printTodos
handleCLIArguments ("list":xs) = printTodos
handleCLIArguments ("add":xs) = addTodo $ unwords xs
handleCLIArguments _ = printTodos

main = Environment.getArgs >>= handleCLIArguments

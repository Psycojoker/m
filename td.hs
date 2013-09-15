import Utils(foldlEMap, elemToStr, foldlESeq, getValue)
import Data.Yaml.Syck(YamlElem, YamlNode, parseYamlFile, n_elem, packBuf, YamlElem(EStr))

data Todo = Todo { description :: String
                 , done :: Bool
                 } deriving (Show)

getDictKeys :: YamlElem -> [String]
getDictKeys = foldlEMap (\acc (key, value) -> elemToStr key:acc) []

printKeys :: YamlNode -> IO ()
printKeys = putStr . unlines . getDictKeys . n_elem

parseTodos :: YamlElem -> [Todo]
parseTodos = foldlESeq (\acc elem -> acc ++ [elemToTodo elem]) []

elemToTodo :: YamlElem -> Todo
elemToTodo elem = Todo {description = (getDescription elem), done = False}

todosToString :: [Todo] -> [String]
todosToString todoList = map (\todo -> "[" ++ (if done todo then "X" else " ") ++ "] " ++ description todo) todoList

getDescription :: YamlElem -> String
getDescription todo = elemToStr $ getValue  todo testKey (EStr $ packBuf "")

testKey :: YamlElem -> Bool
testKey key = (elemToStr key) == "description"

numberise :: [String] -> [String]
numberise s = map (\(number, string) -> (show number) ++ " - " ++ string) (zip [1..] s)

printDescriptions = putStr . unlines . numberise . todosToString . parseTodos . n_elem

main = do
    content <- parseYamlFile "pouet.yaml"
    printDescriptions content
    return ()

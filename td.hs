import Utils
import Data.Yaml.Syck

data Todo = Todo { description :: String
                 , done :: Bool
                 } deriving (Show)

getDictKeys :: YamlElem -> [String]
getDictKeys = foldlEMap (\acc (key, value) -> elemToStr key:acc) []

printKeys :: YamlNode -> IO ()
printKeys = putStr . unlines . getDictKeys . n_elem

parseTodos :: YamlElem -> [String]
parseTodos = foldlESeq (\acc elem -> acc ++ [getDescription elem]) []

getDescription :: YamlElem -> String
getDescription (EMap []) = ""
getDescription (EMap ((key, value):xs))
    | nodeStr key == "description" = nodeStr value
    | otherwise               = getDescription (EMap xs)

printDescriptions = putStr . unlines . parseTodos . n_elem

main = do
    content <- parseYamlFile "pouet.yaml"
    printDescriptions content
    return ()

import Utils
import Data.Yaml.Syck

data Todo = Todo { desciption :: String
                 , done :: Bool
                 } deriving (Show)

getDictKeys :: YamlElem -> [String]
getDictKeys (EMap []) = []
getDictKeys (EMap ((key, _):xs)) = (elemToStr $ n_elem key):(getDictKeys (EMap xs))

printKeys :: YamlNode -> IO ()
printKeys = putStr . unlines . getDictKeys . n_elem

parseTodos :: YamlElem -> [String]
parseTodos = foldlESeq (\acc elem -> acc ++ [getDescription elem]) []

getDescription :: YamlElem -> String
getDescription (EMap []) = ""
getDescription (EMap ((key, value):xs))
    | keyStr == "description" = valueStr
    | otherwise               = getDescription (EMap xs)
    where valueStr = (elemToStr $ n_elem value)
          keyStr = (elemToStr $ n_elem key)

printDescriptions = putStr . unlines . parseTodos . n_elem

main = do
    content <- parseYamlFile "pouet.yaml"
    printDescriptions content
    return ()

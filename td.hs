import Data.Yaml.Syck

data Todo = Todo { desciption :: String
                 , done :: Bool
                 } deriving (Show)

getDictKeys :: YamlElem -> [String]
getDictKeys (EMap []) = []
getDictKeys (EMap ((key, _):xs)) = (elemToStr $ n_elem key):(getDictKeys (EMap xs))

elemToStr :: YamlElem -> String
elemToStr (EStr string) = unpackBuf string

printKeys :: YamlNode -> IO ()
printKeys = putStr . unlines . getDictKeys . n_elem

main = do
    content <- parseYamlFile "pouet.yaml"
    printKeys content
    return ()

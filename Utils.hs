module Utils
( elemToStr
, foldlESeq
, foldlEMap
, nodeStr
, getValue
, numberise
) where

import Data.Yaml.Syck as Yaml

foldlESeq :: (a -> Yaml.YamlElem -> a) -> a -> Yaml.YamlElem -> a
foldlESeq function acc (Yaml.ESeq []) = acc
foldlESeq function acc (Yaml.ESeq (x:xs)) = foldlESeq function (function acc $ Yaml.n_elem x) (Yaml.ESeq xs)

getValue :: Yaml.YamlElem -> (Yaml.YamlElem -> Bool) -> Yaml.YamlElem -> Yaml.YamlElem
getValue (Yaml.EMap []) _ defaultValue = defaultValue
getValue (Yaml.EMap ((key, value):xs)) test defaultValue
    | test $ Yaml.n_elem key = Yaml.n_elem value
    | otherwise              = getValue (Yaml.EMap xs) test defaultValue

foldlEMap :: (a -> (Yaml.YamlElem, Yaml.YamlElem) -> a) -> a -> Yaml.YamlElem -> a
foldlEMap function acc (Yaml.EMap []) = acc
foldlEMap function acc (Yaml.EMap ((key, value):xs)) = foldlEMap function (function acc content) (Yaml.EMap xs)
    where content = (n_elem key, n_elem value)

elemToStr :: Yaml.YamlElem -> String
elemToStr (Yaml.EStr string) = Yaml.unpackBuf string

nodeStr :: Yaml.YamlNode -> String
nodeStr = elemToStr . Yaml.n_elem

numberise :: [String] -> [String]
numberise s = map (\(number, string) -> (show number) ++ " - " ++ string) (zip [1..] s)

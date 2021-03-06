module Utils
( elemToStr
, foldlESeq
, foldlEMap
, nodeStr
, getValue
, getValueFromToString
, numberise
) where

import Data.Yaml.Syck as Yaml

foldlESeq :: (a -> Yaml.YamlElem -> a) -> a -> Yaml.YamlElem -> a
foldlESeq function acc (Yaml.ESeq []) = acc
foldlESeq function acc (Yaml.ESeq (x:xs)) = foldlESeq function (function acc $ Yaml.n_elem x) (Yaml.ESeq xs)

getValueFromToString :: Yaml.YamlElem -> String -> Maybe String
getValueFromToString dict key = case getValue dict (\x -> (elemToStr x) == key) of Just a -> Just $ elemToStr a
                                                                                   Nothing -> Nothing

getValue :: Yaml.YamlElem -> (Yaml.YamlElem -> Bool) -> Maybe Yaml.YamlElem
getValue (Yaml.EMap []) _ = Nothing
getValue (Yaml.EMap ((key, value):xs)) test
    | test $ Yaml.n_elem key = Just $ Yaml.n_elem value
    | otherwise              = getValue (Yaml.EMap xs) test

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

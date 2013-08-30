module Utils
( elemToStr
, foldlESeq
, foldlEMap
, nodeStr
) where

import Data.Yaml.Syck

foldlESeq :: (a -> YamlElem -> a) -> a -> YamlElem -> a
foldlESeq function acc (ESeq []) = acc
foldlESeq function acc (ESeq (x:xs)) = foldlESeq function (function acc $ n_elem x) (ESeq xs)

foldlEMap :: (a -> (YamlElem, YamlElem) -> a) -> a -> YamlElem -> a
foldlEMap function acc (EMap []) = acc
foldlEMap function acc (EMap ((key, value):xs)) = foldlEMap function (function acc content) (EMap xs)
    where content = (n_elem key, n_elem value)

elemToStr :: YamlElem -> String
elemToStr (EStr string) = unpackBuf string

nodeStr :: YamlNode -> String
nodeStr = elemToStr . n_elem

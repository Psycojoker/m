module Utils
( elemToStr
, foldlESeq
, nodeStr
) where

import Data.Yaml.Syck

foldlESeq :: (a -> YamlElem -> a) -> a -> YamlElem -> a
foldlESeq function acc (ESeq []) = acc
foldlESeq function acc (ESeq (x:xs)) = foldlESeq function (function acc $ n_elem x) (ESeq xs)

elemToStr :: YamlElem -> String
elemToStr (EStr string) = unpackBuf string

nodeStr :: YamlNode -> String
nodeStr = elemToStr . n_elem

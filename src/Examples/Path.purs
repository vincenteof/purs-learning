module Examples.Path
  ( Path(..)
  , allFiles
  , allFiles'
  , filename
  , isDirectory
  , largestSmallest
  , ls
  , onlyFiles
  , root
  , size
  , whereIs
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array (filter, head, foldl, concatMap, (:))
import Data.Maybe (Maybe(..))

data Path
  = Directory String (Array Path)
  | File String Int

instance showPath :: Show Path where
  show = filename

root :: Path
root =
  Directory "/"
    [ Directory "/bin/"
        [ File "/bin/cp" 24800
        , File "/bin/ls" 34700
        , File "/bin/mv" 20200
        ]
    , Directory "/etc/"
        [ File "/etc/hosts" 300
        ]
    , Directory "/home/"
        [ Directory "/home/user/"
            [ File "/home/user/todo.txt" 1020
            , Directory "/home/user/code/"
                [ Directory "/home/user/code/js/"
                    [ File "/home/user/code/js/test.js" 40000
                    ]
                , Directory "/home/user/code/haskell/"
                    [ File "/home/user/code/haskell/test.hs" 5000
                    ]
                ]
            ]
        ]
    ]

filename :: Path -> String
filename (File name _) = name
filename (Directory name _) = name

isDirectory :: Path -> Boolean
isDirectory (Directory _ _) = true
isDirectory _ = false

ls :: Path -> Array Path
ls (Directory _ xs) = xs
ls _ = []

size :: Path -> Maybe Int
size (File _ bytes) = Just bytes
size _ = Nothing

-- concatMap 的形式
allFiles :: Path -> Array Path
allFiles file = file : concatMap allFiles (ls file)

-- do 形式
allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles path = filter (not isDirectory) (allFiles path)

whereIs :: Path -> String -> Maybe Path
whereIs path fileName = head $ do
  path' <- allFiles path
  child <- ls path'
  guard $ filename child == filename path' <> fileName
  pure path'

-- 父的 path + 命令名 = target 的 path
-- <> 是半群上的 append 操作，对于 String 而言就是拼接

-- Array (Maybe a) 如何转换成 Maybe (List a)，用 Applicative？

-- 如何定义只针对 data 中一种类型的函数？
largestSmallest :: Path -> Array Path
largestSmallest path = foldl iter [] (onlyFiles path)
  where
  compareSize :: Path -> Path -> Maybe Boolean
  compareSize path1 path2 = pure (>) <*> size path1 <*> size path2

  iter :: Array Path -> Path -> Array Path
  iter ret (Directory _ _) = ret
  iter [] curPath = [ curPath ]
  iter [ prevPath ] curPath = case (compareSize curPath prevPath) of
    Just bool -> if bool then [ prevPath, curPath ] else [ curPath, prevPath ]
    Nothing -> []
  iter prevResult@[ minPath, maxPath ] curPath = case ([ compareSize minPath curPath, compareSize curPath maxPath ]) of
    [ Nothing, Nothing ] -> []
    [ Just true, _ ] -> [ curPath, maxPath ]
    [ _, Just true ] -> [ minPath, curPath ]
    [ Just false, Just false ] -> prevResult
    _ -> []
  iter _ _ = []


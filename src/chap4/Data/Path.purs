module Data.Path (
    Path(..)
  , File(..)
  , Directory(..)
  , root
  , ls
  , filename
  , isDirectory
  , size) where

import Data.Maybe (Maybe(..))
import Prelude

import Data.Int (toNumber)

instance showFile :: Show File where
  show (File name size) = "File " <> name <> " " <> sizeString
    where
        sizeString = show size

data File = File String Number

data Directory = Directory String (Array Path)

data Path = DirectoryPath Directory | FilePath File

instance showPath :: Show Path where
  show = filename

root :: Path
root =
  DirectoryPath $ Directory "/"
    [ DirectoryPath $ Directory "/bin/"
        [ FilePath $ File "/bin/cp" $ toNumber 24800
        , FilePath $ File "/bin/ls" $ toNumber 34700
        , FilePath $ File "/bin/mv" $ toNumber 20200
        ]
    , DirectoryPath $ Directory "/etc/"
        [ FilePath $ File "/etc/hosts" $ toNumber 300
        ]
    , DirectoryPath $ Directory "/home/"
        [ DirectoryPath $ Directory "/home/user/"
            [ FilePath $ File "/home/user/todo.txt" $ toNumber 1020
            , DirectoryPath $ Directory "/home/user/code/"
                [ DirectoryPath $ Directory "/home/user/code/js/"
                    [ FilePath $ File "/home/user/code/js/test.js" $ toNumber 40000
                    ]
                , DirectoryPath $ Directory "/home/user/code/haskell/"
                    [ FilePath $ File "/home/user/code/haskell/test.hs" $ toNumber 5000
                    ]
                ]
            ]
        ]
    ]

filename :: Path -> String
filename (FilePath (File name _)) = name
filename (DirectoryPath (Directory name _)) = name

isDirectory :: Path -> Boolean
isDirectory (DirectoryPath _) = true
isDirectory _ = false

ls :: Path -> Array Path
ls (DirectoryPath (Directory _ xs)) = xs
ls _ = []

size :: Path -> Maybe Number
size (FilePath (File _ bytes)) = Just bytes
size _ = Nothing

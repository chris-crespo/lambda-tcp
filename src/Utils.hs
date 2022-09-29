module Utils (backticks, parenthesize) where

backticks :: String -> String
backticks = enclose "`" "`"

parenthesize :: String -> String
parenthesize = enclose "(" ")"

enclose :: String -> String -> String -> String
enclose left right = (left ++) . (++ right)

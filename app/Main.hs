module Main (main) where

import Table (showTable, default_option, test_table, makeTable)
import LLParser (Symbol(..), Produce(..), parser, getSLRparser)

tokenizer :: String -> [Symbol]
tokenizer []  = []
tokenizer (x:xs) = case x of
        'a' -> Symbol "a":tokenizer xs
        'b' -> Symbol "b":tokenizer xs
        '+' -> Symbol "+":tokenizer xs
        ' ' -> tokenizer xs
        '\n' -> tokenizer xs
        _ -> error "tokenize error"

gramma = [    Produce (Symbol "<Root>") [Symbol "sof", Symbol "<E>", Symbol "eof"]
            , Produce (Symbol "<E>") [Symbol "<E>", Symbol "+", Symbol "a"]
            , Produce (Symbol "<E>") [Symbol "a"]
           ]

main :: IO ()
main = do 
    let table = getSLRparser gramma
    (putStrLn . show) $ table
    let result = parser tokenizer "a + a + a" table
    print result

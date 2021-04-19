{-# LANGUAGE MultiWayIf #-}
module Tr
where

import Data.Maybe

type CharSet = String

tr :: CharSet -> Maybe CharSet -> String -> String
tr _inset _outset xs =
    if checkParams _inset xs
        then if workMode _outset
            then 
                let param2 = equalize _inset (fromMaybe "" _outset)
                    replacements = _inset `zip` param2
                in
                    test xs replacements
            else 
                "delete"
                -- testDel xs _inset
        else "Error: invalid params"


tr1 :: CharSet -> Maybe CharSet -> String -> String
tr1 _inset _outset xs =
    if checkParams _inset xs
        then if workMode _outset
            then 
                let param2 = equalize _inset (fromMaybe "" _outset)
                    replacements = _inset `zip` param2
                in
                    test xs replacements
            else 
                testDel xs _inset
        else "Error: invalid params"

testDel str repl =
    if length str == 1
        then 
            let result = test2Del (head str) repl
            in
                if | null result -> result
                   | length result /= length repl -> tail ['1']
                   | otherwise -> [head result] 
        else testDel [head str] repl ++ testDel (tail str) repl

test2Del char repl =
    if length repl == 1
        then
            if char == head repl
                then tail ['1']
                else [char]
        else test2Del char [head repl] ++ test2Del char (tail repl)

validDel p1 char =
    if null p1
        then char
        else head p1

test str repl =
    if length str == 1
        then [validReplace (test2 (head str) repl) (head str)]
        else test [head str] repl ++ test (tail str) repl

test2 char repm =
    if length repm == 1
        then
            if checkReplace char (head repm) /= char
                then [snd (head repm)]
                else tail ['1']
        else test2 char [head repm] ++ test2 char (tail repm)

validReplace p1 char =
    if null p1
        then char
        else head p1

checkReplace :: Char -> (Char, Char) -> Char
checkReplace p1 p2 =
    if p1 == fst p2
        then snd p2
        else p1

checkParams :: CharSet -> CharSet -> Bool
checkParams p1 p2 = not (null p1) && not (null p2)

-- True = translate; False = delete
workMode :: Maybe CharSet -> Bool
workMode prm = not (null (fromMaybe "" prm))

equalize :: CharSet -> CharSet -> CharSet
equalize _inset _outset =
    let n1 = length _inset
        n2 = length _outset
        in
            if | n1 > n2 ->
                    _outset ++ replicate (n1 - n2) (last _outset)
                | n2 > n1 ->
                    take n1 _outset
                | otherwise -> _outset

-- equalize _inset _outset =
--     let n1 = length _inset
--         n2 = length (fromMaybe "" _outset)
--         param2 = fromMaybe "" _outset :: CharSet
--         in
--             if | n1 > n2 ->
--                     param2 ++ replicate (n1 - n2) (last param2)
--                 | n2 > n1 ->
--                     take n1 param2
--                 | otherwise -> "default"

-- test str repm helper =
--     if length str == 1
--         then [head str]
--         else test [head str] repm helper ++ test (tail str) repm helper

-- test2 char repm =
--     if length repm == 1
--         then
--             if checkReplace char (head repm) /= char
--                 then [snd (head repm)]
--                 else tail ['1']

--         else test2 char [head repm] ++ test2 char (tail repm)

-- test str repl index =
--     if length str == 1
--         then [checkReplace (head str) (repl !! index)]
--         else test [head str] repl index ++ test (tail str) repl (index + 1)

-- test2 char repm =
--     if length repm == 1
--         then
--             if checkReplace char (head repm) /= char
--                 then [snd (head repm)]
--                 else tail ['1']
--         else
--             test2 char [head repm] ++ test2 char (tail repm)
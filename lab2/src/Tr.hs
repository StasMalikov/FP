{-# LANGUAGE MultiWayIf #-}
module Tr
where

import Data.Maybe

type CharSet = String


testM :: String -> String
testM n = 
    if | True -> ""
       | False -> ""
       | otherwise -> n

tr :: CharSet -> Maybe CharSet -> String -> String
tr _inset _outset xs =
    if | null xs -> ""
       | null _inset -> xs
       | otherwise ->
            let outsetValue = fromMaybe "" _outset
            in
                if not (null outsetValue)
                    then 
                        let outsetValueEqual = equalize _inset outsetValue
                            replacements = _inset `zip` outsetValueEqual
                        in
                            translateMode xs replacements
                    else 
                        deleteMode xs _inset

deleteMode str repl =
    if length str == 1
        then 
            let result = delModeSupport (head str) repl
            in
                if | null result -> result
                   | length result /= length repl -> tail ['1']
                   | otherwise -> [head result] 
        else deleteMode [head str] repl ++ deleteMode (tail str) repl

delModeSupport char repl =
    if length repl == 1
        then
            if char == head repl
                then tail ['1']
                else [char]
        else delModeSupport char [head repl] ++ delModeSupport char (tail repl)

validDel p1 char =
    if null p1
        then char
        else head p1

translateMode str repl =
    if length str == 1
        then [validReplace (translateModeReplace (head str) repl) (head str)]
        else translateMode [head str] repl ++ translateMode (tail str) repl

translateModeReplace char repm =
    if length repm == 1
        then
            if checkReplace char (head repm) /= char
                then [snd (head repm)]
                else tail ['1']
        else translateModeReplace char [head repm] ++ translateModeReplace char (tail repm)

validReplace p1 char =
    if null p1
        then char
        else head p1

checkReplace :: Char -> (Char, Char) -> Char
checkReplace p1 p2 =
    if p1 == fst p2
        then snd p2
        else p1

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
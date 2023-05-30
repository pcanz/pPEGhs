module TestPeg where

import Peg
import Data.List
import Data.Char

-- test cases are read from a file with this simple format,
-- use a manual implementation since pPEG may not be working!

test_format = "\
\    File   = (block / _line)*            \                       
\    block  = ' '*4 tag ' '? Body         \ 
\    tag    = 'use:'/'match:'/'not:'      \    
\    Body   = line (' '*8 line)* _nl      \    
\    line   = ~[\n\r]*                    \
\    _line  = ~[\n\r]* _nl                \    
\    _nl    = '\n' / '\r' '\n'? / _eof    \
\    _eof   = !(~[])                      "

testFile fn = do
    txt <- readFile fn
    let results = runTests txt in
        if all ok results
        then putStrLn $ passed (length results)
        else mapM_ print results
        where
            ok (True, _) = True
            ok _ = False
            passed n = "Passed all "++(show n)++" test cases."
    
runTests txt = testCases (readTats txt) [] []

type Tat = (String, String) -- (tag, text) test items

testCases :: [Tat] -> [Parser] -> [(Bool,Tat)] -> [(Bool,Tat)]
testCases [] _ results = reverse results

testCases (tat@(tag,txt):tats) [] results
    | tag == "use:" = testCases tats p (result:results)
    | otherwise = testCases tats [] ((False,tat):results)
      where
        (p, result) = case Peg.compile txt of
            Right parser -> ([parser], (True, tat))
            Left err -> ([], (False, tat)) -- trace (error $ show err) []

testCases tats@(tat@(tag,txt):tts) [parser] results
    | tag == "use:"   = testCases tats [] results
    | tag == "match:" = testCases tts [parser] ((result,tat):results)
    | tag == "not:"   = testCases tts [parser] (((not result),tat):results)
    | otherwise       = testCases tts [parser] ((False,tat):results)
    where
      result = case parser txt of
        Right _ -> True
        Left _  -> False

readTats txt = tats (lines txt) []

tats :: [String] -> [Tat] -> [Tat]
tats [] acc = acc
tats (ln:lns) acc
  | (isTag ln) = tats rest (acc ++ [tat])
  | otherwise = tats lns acc
    where
      (blk, rest) = block ln lns
      tag = takeWhile (not . isSpace) (drop 4 ln) -- "    tag:"
      bod = drop (4+(length tag)) blk
      txt = if isPrefixOf " " bod then drop 1 bod else bod
      tat = (tag, txt)

tags = ["use:", "match:", "not:"]

isTag ln = any (\tag -> isPrefixOf ("    "++tag) ln) tags

block :: String -> [String] -> (String, [String])
block ln [] = (ln, [])
block ln (nxt:lns)
    | (isPrefixOf "        " nxt) = block (ln++"\n"++(drop 8 nxt)) lns
    | otherwise = (ln, nxt:lns)

test24 = testFile "verify.txt"
test25 = testFile "std_test.txt"

std_test = testFile "std_test.txt"


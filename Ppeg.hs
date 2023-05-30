module Ppeg where

import Data.List
import Data.Char
import Data.Bits

type Parser = String -> Either Perror Ptree

type Parse = (Status, Cursor, Results, Log)

data Status = Ok | Fail | Fault String
    deriving Show

type Cursor = (Int, String)  -- (pos, input)

type Results = ([Ptree], Int, [String]) -- ([Ptree], depth, [rules])

type Log = (Int, Op, Results)  -- (pos, expected, Results)

data Ptree = Val String String | Elem String [Ptree]

instance Show Ptree where
  show ptree = treeView ptree 0 0 -- ""

type Code = [(String, Op)]

data Op = Alt [Op] | Seq [Op]
        | Rep Op Int Int
        | Not Op | Nan Op | Amp Op
        | Call Op String | CapCall Op String | Do String
        | Str String | Chs String | Stri String
        | Ext String
        deriving Eq -- (Show, Eq)

instance Show Op where
  show op = showOp op

data Perror = Perr String Parse

instance Show Perror where
  show err = showError err

-- API ----- compile grammar -> parser ----------------------------------------------------------

compile :: String -> Either Perror Parser
compile txt = case runCode peg_code txt of
    (Ok, (_, ""), ([(Elem "Peg" rules)], _, _), _) -> 
        Right $ runParse $ runCode $ map ruleCode rules  -- TODO may be errors in Code -> parse
    fail -> Left $ Perr txt fail

  -- curry runParse with single arg runner ==> Parser -----------------------

runParse :: (String -> Parse) -> String -> Either Perror Ptree
runParse runner input = case runner input of
    (Ok, (_, ""), ([tree], _, _), _) -> Right tree
    parse -> Left $ Perr input parse

-- Parser machine ------------------------------------------------------------------

runCode :: Code -> String -> Parse
runCode code input = run (startRule code) (initParse input)
  where
    run :: Op -> Parse -> Parse

    run (Do name) parse =  -- Do is used to call named rule in literal Code
      case lookup name code of
        Just op -> case cap name of
            '_' -> run op parse
            'A' -> run (CapCall op name) parse
            'a' -> run (Call op name) parse
        Nothing -> (Fault ("undefined rule: "++name), c, t, l)
            where (_, c, t, l) = parse
        
    run (CapCall op name) (Ok, c1, r1@(t1, d, rs), l1) =   -- cap Name rule
        if d > 100 then loopingFault c1 r1 l1 else
        case (run op (Ok, c1, (t1, d+1, (name:rs)), l1)) of
        (Ok, c2, (t2, _, _), l2)  -> -- (Ok, c2, ([Elem name t2], d, rs), l2)
            (Ok, c2, (take k1 t2 ++ [Elem name (drop k1 t2)], d, rs), l2)
            where
                k1 = length t1
                k2 = length t2
        (fail, _, _, l2)          -> (fail, c1, r1, l2)

    run (Call op name) (Ok, c1@(n1, s1), r1@(t1, d, rs), l1) =   -- call name rule
        if d > 100 then loopingFault c1 r1 l1 else
        case (run op (Ok, c1, (t1, d+1, (name:rs)), l1)) of
        (Ok, c2@(n2, _), (t2, _, _), l2) ->  
            if k1 == k2 then (Ok, c2, (t2++[Val name (take (n2-n1) s1)], d, rs), l2)
            else if k1+1 == k2 then (Ok, c2, (t2, d, rs), l2)
            else (Ok, c2, (take k1 t2 ++ [Elem name (drop k1 t2)], d, rs), l2)
            where
                k1 = length t1
                k2 = length t2
        (fail, _, _, l2) -> (fail, c1, r1, l2)

    run (Alt []) parse = failure parse

    run (Alt (e:es)) parse = alt (run e parse) es
        where
            alt ok@(Ok, _, _, _) _ = ok
            alt fail []            = fail
            alt _ (x:xs)           = alt (run x parse) xs

    run (Seq (e:es)) parse@(Ok, c1, t1, _) = seq (run e parse) es
        where
            seq p@(Ok, _, _, _) []     = p
            seq p@(Ok, _, _, _) (x:xs) = seq (run x p) xs
            seq (Fail, (n,_), t2, l2@(m, _, _)) ys  
                | n > m       = (Fail, c1, t1, (n, op, t2))
                | otherwise   = (Fail, c1, t1, l2)
                where op = (e:es) !! (length es - length ys)
            seq fault _ = fault

    run (Rep op min max) p1@(Ok, c1, t1, _) =
      rep (run op p1) c1 1 where
        rep p2@(Ok, c2@(n2, _), _, _) (n1, _) i
            | i == max    = p2
            | n2 == n1    = p2  -- no further progress
            | otherwise   = rep (run op p2) c2 (i+1)
        rep (Fail, c2, t2, l2) _ i
            | i <= min  = (Fail, c1, t1, l2) -- reset fallback
            | otherwise = (Ok, c2, t2, l2) -- assumes state is preserved on failure

    run (Str str) (Ok, c1@(n, input), t1, l1) =
      if (isPrefixOf str input) 
        then (Ok, (n+len, drop len input), t1, l1) 
        else (Fail, c1, t1, l1)
      where len = length str

    run (Stri str) (Ok, c1@(n, input), t1, l1) =
      if str == (map toUpper $ take len input) 
        then (Ok, (n+len, drop len input), t1, l1) 
        else (Fail, c1, t1, l1)
      where len = length str

    run (Chs (x:'-':z:xs)) p1@(Ok, (n, (c:cs)), t1, l1) =
      if (x <= c && z >= c)
          then (Ok, (n+1, cs), t1, l1)
          else run (Chs xs) p1

    run (Chs (x:xs)) p1@(Ok, (n, (c:cs)), t1, l1)
        | x == c    = (Ok, (n+1, cs), t1, l1)
        | otherwise = run (Chs xs) p1

    run (Chs _) parse = failure parse

    run (Nan op) parse  =
      if (isOk $ run op parse) then failure parse else advance parse

    run (Not op) parse =
      if (isOk $ run op parse) then failure parse else parse

    run (Amp op) parse =
      if (isOk $ run op parse) then parse else failure parse

    run (Ext str) parse@(Ok, c, t, l) =
        let key = takeWhile (not . isSpace) str in
        case lookup key extensions of
            Just fn -> fn parse
            Nothing -> (Fault ("undefined extension: "++key), c, t, l)
    
initParse input = (Ok, (0, input), initResults, initLog) 

initResults = ([], 0, [])  -- ([Ptree], depth, [rules])

initLog = (0, Str "", ([], 0, []))   -- (max-pos, expected, Results)

startRule :: Code -> Op
startRule ((name, _):_) = (Do name)
startRule code = error $ "Bad code: " ++ show code

isOk :: Parse -> Bool
isOk (Ok, _, _, _) = True
isOk _             = False

failure :: Parse -> Parse
failure (Ok, c, t, l) = (Fail, c, t, l)
failure fault         = fault

advance :: Parse -> Parse
advance (Ok, (n, (c:cs)), t, l) = (Ok, (n+1, cs), t, l)
advance (Ok, c, t, l)           = (Fail, c, t, l)

cap (x:_) | x == '_'    = '_'
          | (isUpper x) = 'A'
          | otherwise   = 'a'

loopingFault c t l = (Fault "looping....", c, t, l)

-- std-peg grammar --------------------------------

peg_peg = "\
\    Peg   = _ (rule _)+                        \                        
\    rule  = id _ '=' _ alt                     \
\                                               \
\    alt   = seq ('/' _ seq)*                   \
\    seq   = rep+                               \
\    rep   = pre sfx? _                         \
\    pre   = pfx? term                          \
\    term  = call / quote / chars / group / extn\
\    group = '('_ alt ')'                       \
\                                               \
\    call  = id _ !'='                          \
\    id    = [a-zA-Z_] [a-zA-Z0-9_-]*           \
\                                               \
\    pfx   = [~!&]                              \
\    sfx   = [+?] / '*' range?                  \
\    range = num (dots num?)?                   \
\    num   = [0-9]+                             \
\    dots  = '..'                               \
\                                               \
\    quote = ['] str ['] case?                  \
\    str   = ~[']*                              \
\    case  = 'i'                                \
\    chars = '[' chs ']'                        \
\    chs   =  ~']'*                             \
\    extn  = '<' ext '>'                        \
\    ext   = ~'>'*                              \
\    _     = ([ \t\n\r]+ / '#' ~[\n\r]*)*       "

peg_code = [
    ("Peg", Seq [Do "_", Rep (Seq [Do "rule", Do "_"]) 1 0]),
    ("rule", Seq [Do "id", Do "_", Str "=", Do "_", Do "alt"]),
    ("alt", Seq [Do "seq", Rep (Seq [Str "/", Do "_", Do "seq"]) 0 0]),
    ("seq", Rep (Do "rep") 1 0),
    ("rep", Seq [Do "pre", Rep (Do "sfx") 0 1, Do "_"]),
    ("pre", Seq [Rep (Do "pfx") 0 1, Do "term"]),
    ("term", Alt [Do "call", Do "quote", Do "chars", Do "group", Do "extn"]),
    ("group", Seq [Str "(", Do "_", Do "alt", Str ")"]),
    ("call", Seq [Do "id", Do "_", Not (Str "=")]),
    ("id", Seq [Chs "a-zA-Z_", Rep (Chs "a-zA-Z0-9_-") 0 0]),
    ("pfx", Chs "~!&"),
    ("sfx", Alt [Chs "+?", Seq [Str "*", Rep (Do "range") 0 1]]),
    ("range", Seq [Do "num", Rep (Seq [Do "dots", Rep (Do "num") 0 1]) 0 1]),
    ("num", Rep (Chs "0-9") 1 0),
    ("dots", Str ".."), 
    ("quote", Seq [Chs "'", Do "str", Chs "'", Rep (Do "case") 0 1]),
    ("str", Rep (Nan (Chs "'")) 0 0),
    ("case", Str "i"),
    ("chars", Seq [Chs "[", Do "chs", Chs "]"]),
    ("chs", Rep (Nan (Chs "]")) 0 0),
    ("extn", Seq [Str "<", Do "ext", Str ">"]),
    ("ext", Rep (Nan (Chs ">")) 0 0),
    ("_", Rep (Alt [Rep (Chs " \t\n\r") 1 0, Seq [Str "#", Rep (Nan (Chs "\n\r")) 0 0]]) 0 0)
    ]


-- Report errors -------------------------------------------

showError parse = case parse of
    Perr input (Fail, (n, _), t, log) ->
        "*** parse failed at:\n" ++
        showErrorLine input n t log 
    Perr input (Ok, (n, _), t, log) ->
        "*** parse fell short at:\n" ++
        showErrorLine input n t log
    Perr _ (Fault msg, _, _, _) -> "Fault: " ++ msg

showErrorLine input n (_, _, calls) (m, op, (ts, _, rules)) =
    matched ts ++
    cursorPos (max m n) input ++
    inrule calls rules -- "\nIn: " ++ rule ++ expected op
    where
        matched [] = ""
        matched (t:ts) = "   " ++ show t ++ matched ts
        inrule _ (rule:_) = "\nIn: " ++ rule ++ expected op
        inrule (rule:_) _ = "\nIn: " ++ rule
        inrule _ _ = ""
        expected op = if m == 0 then "" else ", expected: " ++ (show op)

cursorPos pos input =
    let pre = take pos input
        pres = trim $ show pre
        before = if (length pres) > 30 then
            " ... " ++ drop ((length pres)-25) pres else pres
        cursor = replicate (length before) ' ' ++ "^ "
        post = trim $ show (drop pos input)
        after = if (length post) > 30 then
            take 25 post ++ " ... " else post
        ln = length $ filter (== '\n') pre
        in
        before ++ after ++ "\n" ++ cursor ++ "line: " ++ show (ln+1) 
        where
            trim s = init (drop 1 s)

-- Show a Parse tree result -------------------------------------------------------------

parseView :: Parse -> String
parseView (Ok, (_, ""), ([tree], _, _), _) = treeView tree 0 0 -- ""
parseView parse = show parse

-- treeView :: Ptree -> String -> String
-- treeView (Val name value) inset = inset++name++" "++(show value)++"\n"
-- treeView (Elem name kids) inset  = inset++name++"\n"++(listView kids (inset++"| ")) where
--     listView [] _         = ""
--     listView (t:ts) inset = (treeView t inset) ++ (listView ts inset)

treeView :: Ptree -> Int -> Int -> String
treeView (Val name value) inset last = (draw inset inset last)++name++" "++(show value)++"\n"
treeView (Elem name kids) inset last = (draw inset inset last)++name++"\n"++
                                        (listView kids (inset+1) last) where
    listView [] _  _           = ""
    listView ts@(t:[]) inset last = (treeView t inset (setBit last (inset-1)))
    listView (t:ts) inset last = (treeView t inset last) ++ (listView ts inset last)

draw :: Int -> Int -> Int -> String
draw 0 _ _ = ""    
draw 1 inset last 
    | testBit last (inset-1) = "\x2514\x2500"   -- "`-"  
    | otherwise              = "\x251C\x2500"   -- |-           
draw n inset last
    | testBit last (inset-n) = "  " ++ draw (n-1) inset last -- "  "
    | otherwise              = "\x2502 " ++ draw (n-1) inset last -- "| "

-- parseCode used for internal testing use, use compile API to get failure info...

parseCode :: Parse -> Code
parseCode (Ok, (_, ""), ([(Elem "Peg" rules)], _, _), _) = map ruleCode rules
parseCode _ = []

-- Compile a peg grammar into a Parser ------------------------------------------------
 
ruleCode :: Ptree -> (String, Op)
ruleCode (Elem "rule" ((Val id name):[expr])) = (name, opCode expr)

opCode :: Ptree -> Op
opCode (Elem "alt" expr) = Alt (map opCode expr)
opCode (Elem "seq" expr) = Seq (map opCode expr)

opCode (Elem "rep" [op, Val "sfx" r])
    | r == "*" = Rep (opCode op) 0 0
    | r == "+" = Rep (opCode op) 1 0
    | r == "?" = Rep (opCode op) 0 1
opCode (Elem "rep" [op, Val "num" n]) = 
    Rep (opCode op) k k where k = read n
opCode (Elem "rep" [op, Elem "range" [Val "num" n, dots]]) =
    Rep (opCode op) (read n) 0
opCode (Elem "rep" [op, Elem "range" [Val "num" n, dots, Val "num" m]]) = 
    Rep (opCode op) (read n) (read m)

opCode (Elem "pre" [Val "pfx" p, op])
    | p == "~" = Nan opx
    | p == "!" = Not opx
    | p == "&" = Amp opx
    where opx = opCode op

opCode (Val "id" s)  = Do s
opCode (Val "str" s) = Str $ esc s
opCode (Val "chs" s) = Chs $ esc s
opCode (Val "ext" s) = Ext $ esc s

opCode (Elem "quote" [Val "str" s, i]) = Stri $ map toUpper s

-- only needed for core grammar....  TODO update core grammar
opCode (Val "quote" s) = Str $ esc $ dropWhileEnd (== '\'') (drop 1 s)
opCode (Val "chars" s) = Chs $ esc $ dropWhileEnd (== ']') (drop 1 s)

-- Panic exit on undefined Peg grammar elements.......
opCode (Elem name _) = error ("undefined Elem "++name)
opCode (Val name xs)  = error ("undefined Val "++name++(show xs))

esc :: String -> String
esc [] = []
esc ('\\':'t':cs) = '\t':(esc cs)
esc ('\\':'n':cs) = '\n':(esc cs)
esc ('\\':'r':cs) = '\r':(esc cs)
esc ('\\':'u':'{':cs) = hexChr (takeWhile (/= '}') cs):(esc (drop 1 (dropWhile (/= '}') cs)))
esc ('\\':'u':cs) = hexChr (take 4 cs):(esc (drop 4 cs))
esc ('\\':'U':cs) = hexChr (take 8 cs):(esc (drop 8 cs))
esc (c:cs)       = c:(esc cs)

hexChr hex = chr $ read $ "0x"++hex

-- show op codes as literal source strings ----------------------------

-- data Op = Alt [Op] | Seq [Op]
--         | Rep Op Int Int
--         | Not Op | Nan Op | Amp Op
--         | Call Op String | CapCall Op String | Do String
--         | Str String | Chs String | Stri String

showOp :: Op -> String
showOp (Do rule) = rule
showOp (Str str) = "'"++(showEsc str)++"'"
showOp (Chs str) = "["++(showEsc str)++"]"
showOp (Seq ops) = "("++(unwords (map showOp ops))++")"
showOp (Alt ops) = "("++(intercalate " / " (map showOp ops))++")"
showOp (Rep op 0 0) = showOp op ++ "*"
showOp (Rep op 1 0) = showOp op ++ "+"
showOp (Rep op 0 1) = showOp op ++ "?"
showOp (Rep op n 0) = showOp op ++ "*" ++ (show n) ++ ".."
showOp (Rep op n m) = showOp op ++ "*" ++ (show n) ++ ".." ++ (show m)
showOp (Call _ rule) = rule
showOp (CapCall _ rule) = rule
showOp (Stri str) = "'"++(showEsc str)++"'i"
showOp (Not op) = "!" ++ showOp op
showOp (Nan op) = "~" ++ showOp op
showOp (Amp op) = "&" ++ showOp op

showEsc str = init $ drop 1 (show str)


-- extensions --------------------------------------

type Extensions = [(String, Pex)]

type Pex = Parse -> Parse

extensions = [] -- [("trace", exTrace)]

-- exTrace (Ok, (n, rest), (trees, d, calls), log) =



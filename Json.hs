module Json where

import Peg

json_grammar1 = "\
\    json   = _ value _                                  \n\
\    value  =  Str / Arr / Obj / num / lit               \n\  
\    Obj    = '{'_ (memb (_','_ memb)*)? _'}'            \n\  
\    memb   = Str _':'_ value                            \n\
\    Arr    = '['_ (value (_','_ value)*)? _']'          \n\
\    Str    = '\"' chars* '\"'                           \n\
\    chars  = ~([\\u0000-\\u001F]/'\\'/'\"')+ / '\\' esc \n\
\    esc    = [\"\\/bfnrt] / 'u' [0-9a-fA-F]*4           \n\
\    num    = _int _frac? _exp?                          \n\
\    _int   = '-'? ([1-9] [0-9]* / '0')                  \n\
\    _frac  = '.' [0-9]+                                 \n\
\    _exp   = [eE] [+-]? [0-9]+                          \n\
\    lit    = 'true' / 'false' / 'null'                  \n\
\    _      = [ \t\n\r]*                                 \n"

json txt = case Peg.compile json_grammar of
    Left fault -> print fault
    Right parser -> case parser txt of
        Right ptree -> print ptree
        Left parse -> print parse

json1 = json "{\"a\": 1, \"b\": 2 }"

json_grammar = "\
\    json   = _ value _                                  \n\
\    value  =  Str / Arr / Obj / num / lit               \n\  
\    Obj    = '{'_ (memb (_','_ memb)*)? _'}'            \n\  
\    memb   = Str _':'_ value                            \n\
\    Arr    = '['_ (value (_','_ value)*)? _']'          \n\
\    Str    = '\"' chars* '\"'                           \n\
\    chars  = ~([\0-\x1F]/'\\'/'\"')+ / '\\' esc         \n\
\    esc    = [\"\\/bfnrt] / 'u' [0-9a-fA-F]*4           \n\
\    num    = _int _frac? _exp?                          \n\
\    _int   = '-'? ([1-9] [0-9]* / '0')                  \n\
\    _frac  = '.' [0-9]+                                 \n\
\    _exp   = [eE] [+-]? [0-9]+                          \n\
\    lit    = 'true' / 'false' / 'null'                  \n\
\    _      = [ \t\n\r]*                                 \n"


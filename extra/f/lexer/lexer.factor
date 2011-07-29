! Copyright (C) 2010 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors ascii classes combinators
combinators.short-circuit constructors continuations
destructors f.dictionary fry grouping io io.encodings.utf8
io.files io.streams.document io.streams.string
kernel lexer make math namespaces nested-comments sequences
splitting strings words arrays locals ;
QUALIFIED-WITH: io.streams.document io
IN: f.lexer

: loop>sequence ( quot exemplar -- seq )
    [ '[ [ @ [ [ , ] when* ] keep ] loop ] ] dip make ; inline

: loop>array ( quot -- seq )
    { } loop>sequence ; inline

TUPLE: lexer stream comment-nesting-level ;

: new-lexer ( lexer -- lexer )
    new
        0 >>comment-nesting-level ; inline

: <lexer> ( stream -- lexer )
    lexer new-lexer
        swap >>stream ; inline

ERROR: lexer-error error ;

: with-lexer ( lexer quot -- )
    [ drop \ lexer ] 2keep
    '[
        _ [ <document-reader> ] change-stream
        [
            _
            [ input-stream get stream>> dispose ]
            [ ] cleanup
        ] with-input-stream
    ] with-variable ; inline
        
TUPLE: string-lexer < lexer ;

: <string-lexer> ( string -- lexer )
    string-lexer new-lexer
        swap <string-reader> >>stream ; inline

: with-string-lexer ( string quot -- )
    [ <string-lexer> ] dip with-lexer ; inline

TUPLE: file-lexer < lexer path ;

: <file-lexer> ( path -- lexer )
    file-lexer new-lexer
        swap utf8 <file-reader> >>stream ; inline

: with-file-lexer ( path quot -- )
    [ <file-lexer> ] dip with-lexer ; inline

TUPLE: lexed tokens ;

: new-lexed ( tokens class -- parsed )
    new
        swap >>tokens ; inline

TUPLE: long-string < lexed text ;

: <long-string> ( text tokens -- string )
    long-string new-lexed
        swap >>text ; inline

TUPLE: lexed-string < lexed name text ;

: <lexed-string> ( tokens -- lexed-string )
    [ lexed-string new-lexed ] keep
        [ first >>name ]
        [ third >>text ] bi ; inline

TUPLE: line-comment < lexed ;

: <line-comment> ( sequence -- line-comment )
    line-comment new-lexed ; inline
    
TUPLE: lua-string < lexed name text ;

: <lua-string> ( name text tokens -- lua-string )
    lua-string new-lexed
        swap >>text
        swap >>name ; inline

TUPLE: lua-comment < lexed start text stop ;

: <lua-comment> ( tokens text -- lua-comment )
    lua-comment new-lexed
        swap >>text ; inline

UNION: comment line-comment lua-comment ;


GENERIC: first-token ( obj -- token/f )
GENERIC: last-token ( obj -- token/f )

M: io:token first-token ;
M: io:token last-token ;

M: string first-token ;
M: string last-token ;

M: sequence first-token [ f ] [ first first-token ] if-empty ;
M: sequence last-token [ f ] [ last last-token ] if-empty ;

M: lexed first-token tokens>> [ f ] [ first first-token ] if-empty ;
M: lexed last-token tokens>> [ f ] [ last last-token ] if-empty ;

: text ( token/f -- string/f ) dup token? [ text>> ] when ;

: lex-blanks ( -- )
    [ peek1 text blank? [ read1 ] [ f ] if ] loop>array drop ;

: lex-til-eol ( -- comment )
    "\r\n" read-until drop ;

ERROR: bad-string ;

ERROR: stream-read-until-string-error needle string stream ;

:: stream-read-until-string ( needle stream -- string' )
    [
        0 :> i!
        needle length :> len
        [
            stream stream-read1 :> ch
            ch [ needle building get >string stream stream-read-until-string-error ] unless
            i needle nth ch = [ i 1 + i! ] [ 0 i! ] if
            ch ,
            len i = not
        ] loop
    ] "" make ;
    
: read-until-string ( needle -- string' )
    input-stream get stream-read-until-string ;

ERROR: bad-separator string ;    
ERROR: lua-string-error name string ;
: lex-lua-string ( start first -- string )
    [ 1string ] change-text
    " [\r\n" read-until text>> CHAR: [ = [
        text>> dup [ CHAR: = = ] all? [ bad-separator ] unless
        [ '[ _ "[" 3append ] change-text ]
        [ length CHAR: = <string> "]" "]" surround ] bi
        
        [ input-stream get stream>> stream-read-until-string ] keep length cut*
        4array [ first text>> ] [ third ] [ ] tri
        <lua-string>
     ] [
        [ text>> ] bi@ append lua-string-error
    ] if ;

ERROR: lua-comment-error string ;
: lex-lua-comment ( -- string )
    1 read
    " (\r\n" read-until text>> CHAR: ( = [
        text>> dup [ CHAR: * = ] all? [ bad-separator ] unless
        [ '[ _ "(" 3append ] change-text ]
        [ length CHAR: * <string> ")" ")" surround ] bi
        
        [ input-stream get stream>> stream-read-until-string ] keep length cut*
        3array [ second ] keep
        <lua-comment>
     ] [
        [ text>> ] bi@ append lua-comment-error
    ] if ;

: read-short-string ( -- string end )
    tell-input
    [
        [
            peek1 text CHAR: " = [
                1 read ,
                f
            ] [
                peek1 text {
                    { CHAR: \ [ 2 read text % ] }
                    [ drop read1 dup [ bad-string ] unless text , ]
                } case
                t
            ] if
        ] loop
    ] { } make unclip-last [ >string tell/string>token ] dip ;

: read-string ( string delimiter -- lexed-string )
    [ 1string ] change-text
    read-short-string 4array <lexed-string> ;

: read-long-string ( -- long-string )
    3 read "\"\"\"" input-stream get stream>> stream-read-until-string
    3 cut* 3array [ second ] keep <long-string> ;
    
: read-identifier ( token1 token2 -- token )
    " \r\n" read-until drop
    [ text>> ] bi@ [ 1string ] dip '[ _ _ 3append ] change-text ;

: lex-string/token ( -- string/token/f )
    " \n\r\"[" read-until [
        dup text>> {
            { CHAR: " [ read-string ] }
            { CHAR: [ [ peek1 text>> CHAR: = = [ lex-lua-string ] [ read-identifier ] if ] }
            [ 2drop ]
        } case
    ] [
        drop f
    ] if* ;
    
: lex-token ( -- token/string/comment/f )
    lex-blanks
    3 peek
    text {
        { [ dup "!" head? ] [ drop 1 read lex-til-eol 2array <line-comment> ] }
        { [ dup "#!" head? ] [ drop 2 read lex-til-eol 2array <line-comment> ] }
        { [ dup "(*" head? ] [ drop lex-lua-comment ] }
        { [ dup f = ] [ drop f ] }
        { [ dup "\"\"\"" head? ] [ drop read-long-string ] }
        [ drop lex-string/token ]
    } cond ;

: lex-chunk ( -- token/f )
    " \n\r" input-stream get stream>> stream-read-until [
        drop f
    ] unless ;

M: lexer dispose stream>> dispose ;

M: lexer stream-read1
    stream>> [
        lex-token
    ] with-input-stream* ;

M: lexer stream-read
    stream>> [
        [ lex-token ] replicate
    ] with-input-stream* sift f like ;

:: lexer-stream-read-until ( seps -- sep/f )
    lex-token [
        dup text>> seps member? [
            , seps lexer-stream-read-until
        ] unless
    ] [
        f
    ] if* ;

M: lexer stream-read-until
    stream>> swap '[
        [ _ lexer-stream-read-until ] { } make f like swap
    ] with-input-stream* ;

M: lexer stream-peek1
    stream>> [
        [ lex-token ] with-input-rewind
    ] with-input-stream* ;

M: lexer stream-peek
    stream>> [
        '[
            [ lex-token ] replicate
        ] with-input-rewind
    ] with-input-stream* sift f like ;

M: lexer stream-seek
    stream>> stream-seek ;

M: lexer stream-tell
    stream>> stream-tell ;

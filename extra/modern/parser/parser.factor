! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: assocs combinators constructors formatting io
io.encodings.utf8 io.files kernel make math.parser namespaces
sequences strings sequences.extras fry arrays ;
IN: modern.parser

SYMBOL: parsers
parsers [ H{ } clone ] initialize

SYMBOL: comments
: save-comment ( comment -- )
    [ comments get push ] when* ;

TUPLE: mnumber n ;
CONSTRUCTOR: mnumber ( n -- mnumber ) ;

ERROR: string-expected got separator ;
TUPLE: mstring class string ;
CONSTRUCTOR: mstring ( class string -- mstring ) ;

: parse-string' ( -- )
    "\\\"" read-until {
        { CHAR: " [ % ] }
        { CHAR: \ [ % read1 , parse-string' ] }
        { f [ f string-expected ] }
        [ string-expected ]
    } case ;

: parse-string ( class -- mstring )
    [ parse-string' ] "" make <mstring> ;

TUPLE: comment text ;
CONSTRUCTOR: comment ( text -- comment ) ;
: parse-comment ( -- comment ) readln <comment> ;

: execute-parser ( word -- object/f )
    \ parsers get ?at [ execute( -- parsed ) ] when ;

: parse-action ( string -- object/f )
    dup string? [
        [ f ] [ execute-parser ] if-empty
    ] when ;

: token' ( -- string/f )
    "\n\s\"#" read-until {
        { [ dup "\n\s" member? ] [ drop [ token' ] when-empty ] }
        { [ dup CHAR: " = ] [ drop [ f ] when-empty parse-string ] }
        { [ dup CHAR: # = ] [
            drop parse-comment save-comment [ token' ] when-empty ] }
        [ drop ]
    } cond ;

: token ( -- object )
    token' dup string? [
        dup string>number [ <mnumber> ] when
    ] when ;

: get-string ( -- string/f )
    "\n\s#" read-until {
        { [ dup "\n\s" member? ] [ drop [ get-string ] when-empty ] }
        { [ dup CHAR: # = ] [
            drop parse-comment save-comment [ get-string ] when-empty ] }
        [ drop ]
    } cond ;

: strings-until ( string -- strings )
    '[
        _ get-string 2dup = [ 2drop f ] [ nip ] if
    ] loop>array ;

ERROR: no-more-tokens ;
: parse ( -- object/f )
    token parse-action ;

: parse-input ( -- seq comments )
    V{ } clone comments [
        [ parse ] loop>array
        comments get
    ] with-variable ;

: parse-file ( path -- seq comments )
    utf8 [ parse-input ] with-file-reader ; inline

: parse-stream ( stream -- seq comments )
    [ parse-input ] with-input-stream ; inline

ERROR: token-expected token ;
: parse-until ( string -- strings/f )
    '[
        _ token [ token-expected ] unless*
        2dup = [ 2drop f ] [ nip parse-action ] if
    ] loop>array ;

: string-until-eol ( -- string )
    "\n" read-until drop ;

ERROR: expected expected got ;
: expect ( string -- )
    token 2dup = [ 2drop ] [ expected ] if ;

: expect-one ( strings -- )
    token 2dup swap member? [ 2drop ] [ expected ] if ;

: body ( -- strings ) ";" parse-until ;

TUPLE: signature in out ;
CONSTRUCTOR: signature ( in out -- signature ) ;

: parse-signature--) ( -- signature )
    "--" strings-until ")" strings-until <signature> ;

: parse-signature(--) ( -- signature )
    "(" expect parse-signature--) ;

TUPLE: function name signature body ;
CONSTRUCTOR: function ( name signature body -- function ) ;
: parse-function ( -- function )
    token
    parse-signature(--)
    body <function> ;

TUPLE: use strings ;
CONSTRUCTOR: use ( strings -- use ) ;
: parse-use ( -- use ) ";" strings-until <use> ;

TUPLE: author name ;
CONSTRUCTOR: author ( name -- author ) ;
: parse-author ( -- author )
    string-until-eol [ " " member? ] trim <author> ;

TUPLE: block body ;
CONSTRUCTOR: block ( body -- block ) ;
: parse-block ( -- block )
    "]" parse-until <block> ;

TUPLE: marray elements ;
CONSTRUCTOR: marray ( elements -- block ) ;
: parse-marray ( -- block )
    "}" parse-until <marray> ;

TUPLE: mvector elements ;
CONSTRUCTOR: mvector ( elements -- block ) ;
: parse-mvector ( -- block )
    "}" parse-until <mvector> ;

TUPLE: mhashtable elements ;
CONSTRUCTOR: mhashtable ( elements -- block ) ;
: parse-mhashtable ( -- block )
    "}" parse-until <mhashtable> ;

TUPLE: char n ;
CONSTRUCTOR: char ( n -- char ) ;
: parse-char ( -- char )
    token <char> ;

TUPLE: in name ;
CONSTRUCTOR: in ( name -- in ) ;
: parse-in ( -- in )
    token <in> ;

TUPLE: main name ;
CONSTRUCTOR: main ( name -- main ) ;
: parse-main ( -- main )
    token <main> ;

TUPLE: escaped name ;
CONSTRUCTOR: escaped ( name -- escaped ) ;
: parse-escaped ( -- escaped )
    token <escaped> ;

TUPLE: execute( signature ;
CONSTRUCTOR: execute( ( signature -- execute ) ;
: parse-execute( ( -- execute( )
    parse-signature--) <execute(> ;

TUPLE: call( signature ;
CONSTRUCTOR: call( ( signature -- call ) ;
: parse-call( ( -- call( )
    parse-signature--) <call(> ;

TUPLE: mgeneric name signature ;
CONSTRUCTOR: mgeneric ( name signature -- generic ) ;
: parse-mgeneric ( -- generic )
    token parse-signature(--) <mgeneric> ;

TUPLE: mmethod class name body ;
CONSTRUCTOR: mmethod ( class name body -- method ) ;
: parse-mmethod ( -- method )
    token token body <mmethod> ;

! TUPLE: private body ;
! CONSTRUCTOR: private ( body -- private ) ;
! : parse-private ( -- private )
    ! "PRIVATE>" parse-until <private> ;

TUPLE: from module functions ;
CONSTRUCTOR: from ( module functions -- from ) ;
: parse-from ( -- from )
    token ";" strings-until <from> ;

TUPLE: constant name object ;
CONSTRUCTOR: constant ( name object -- constant ) ;
: parse-constant ( -- constant )
    token parse <constant> ;

TUPLE: mtuple name body ;
CONSTRUCTOR: mtuple ( name body -- tuple ) ;
: parse-mtuple ( -- mtuple )
    token body <mtuple> ;

TUPLE: merror name body ;
CONSTRUCTOR: merror ( name body -- error ) ;
: parse-merror ( -- merror )
    token body <merror> ;

TUPLE: mparser name start slots body ;
CONSTRUCTOR: mparser ( name start slots body -- package ) ;
: parse-mparser ( -- parser )
    get-string parse parse body <mparser> ;

TUPLE: package name ;
CONSTRUCTOR: package ( name -- package ) ;
: parse-package ( -- package )
    get-string <package> ;

TUPLE: import name ;
CONSTRUCTOR: import ( name -- package ) ;
: parse-import ( -- import )
    get-string <import> ;

TUPLE: imports names ;
CONSTRUCTOR: imports ( names -- package ) ;
: parse-imports ( -- import )
    ";" strings-until <imports> ;

\ parse-mparser "PARSER:" parsers get set-at
\ parse-package "PACKAGE:" parsers get set-at
\ parse-import "IMPORT:" parsers get set-at
\ parse-imports "IMPORTS:" parsers get set-at
\ parse-author "AUTHOR:" parsers get set-at
\ parse-from "FROM:" parsers get set-at
\ parse-use "USE:" parsers get set-at
\ parse-in "IN:" parsers get set-at
\ parse-main "MAIN:" parsers get set-at

\ parse-char "CHAR:" parsers get set-at
\ parse-escaped "\\" parsers get set-at
\ parse-execute( "execute(" parsers get set-at
\ parse-call( "call(" parsers get set-at
! \ parse-private "<PRIVATE" parsers get set-at

\ parse-constant "CONSTANT:" parsers get set-at
\ parse-mtuple "TUPLE:" parsers get set-at
\ parse-merror "ERROR:" parsers get set-at

\ parse-block "[" parsers get set-at
\ parse-marray "{" parsers get set-at
\ parse-mvector "V{" parsers get set-at
\ parse-mhashtable "H{" parsers get set-at

\ parse-mgeneric "GENERIC:" parsers get set-at
\ parse-mmethod "M:" parsers get set-at
\ parse-function ":" parsers get set-at


! FUNCTOR: define-box ( T -- )

! ALIAS: B ${T}-box
! ALIAS: <B> <${T}>

! TUPLE: B { value T } ;
! C: <B> B ( T -- B )

! ;

! \ float define-box
 
! FUNCTOR: define-box ( T -- )
! H{ { T float } }

! ALIAS: B ${T}-box
! H{ { T float } { B float-box } }
! ALIAS: <B> <${T}>
! H{ { T float } { B float-box } { <B> <float-box>}

! TUPLE: B { value T } ;
! ! TUPLE: float-box { value float } ;
! C: <B> B ( T -- B )
! ! C: <float-box> float-box ( float -- float-box )

! ;

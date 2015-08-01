! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays ascii combinators fry io.encodings.utf8
io.files kernel locals make math modern.paths multiline
sequences sequences.extras strings ;
IN: modern.quick-parser

/*
USE: modern.quick-parser
"math" quick-parse-vocab
[ >string . ] each
*/

TUPLE:  parsed-atom { slice slice } ;
TUPLE:  parsed-compound atoms { slice slice } ;

! Include the separator, which is not whitespace
:: take-until-separator ( n string tokens -- n' string slice/f ch/f )
    n string '[ tokens member? ] find-from [ dup [ 1 + ] when ] dip  :> ( n' ch )
    n' string
    n n' [ string length ] unless* string <slice> ch ; inline

! Don't include the whitespace
:: take-until-whitespace ( n string -- n' string slice/f ch/f )
    n string '[ "\s\r\n" member? ] find-from :> ( n' ch )
    n' string
    n n' [ string length ] unless* string <slice> ch ; inline

! If it's whitespace, don't include it
:: take-until-either ( n string tokens -- n' string slice/f ch/f )
    n string '[ tokens member? ] find-from dup "\s\r\n" member? [
        :> ( n' ch )
        n' string
        n n' [ string length ] unless* string <slice> ch
    ] [
        [ dup [ 1 + ] when ] dip  :> ( n' ch )
        n' string
        n n' [ string length ] unless* string <slice> ch
    ] if ; inline

:: take-until-multi ( n string multi -- n' string inside end/f )
    multi string n start* :> n'
    n' multi length + string
    n n' string <slice>
    n' dup multi length + string <slice> ;

: skip-blank ( n string -- n' string )
    [ [ blank? not ] find-from drop ] keep ; inline

: skip-til-eol ( n string -- n' string )
    [ [ "\r\n" member? ] find-from drop ] keep ; inline


: prepend-slice ( end begin -- slice )
    [ nip from>> ]
    [ drop [ to>> ] [ seq>> ] bi <slice> ] 2bi ; inline

: complete-token ( n string seq -- n' seq' )
    [ take-until-whitespace drop nip ] dip prepend-slice ;

: parse-action ( n string -- n obj/f )
    ;

DEFER: parse
DEFER: parse-until

ERROR: closing-paren-expected n string last ;
: read-paren ( n string seq -- n' seq )
    2over ?nth [ closing-paren-expected ] unless* blank? [
        [ ")" parse-until ] dip prefix
    ] [
        complete-token
    ] if ;

: extend-slice ( slice n -- slice' )
    [ [ from>> ] [ to>> ] [ seq>> ] tri ] dip
    swap [ + ] dip <slice> ;

! Ugly
:: read-long-bracket ( n string tok ch -- n seq )
    ch {
        { CHAR: = [
            n string "[" take-until-separator CHAR: [ = [ "omg error" throw ] unless :> ( n' string' tok2 )
            tok2 length 1 - CHAR: = <string> "]" "]" surround :> needle

            n' string' needle take-until-multi :> ( n'' string'' inside end )
            n''
            tok tok2 length extend-slice
            inside
            end 3array
        ] }
        { CHAR: [ [
            n 1 + string "]]" take-until-multi :> ( n' string' inside end )
            n'
            tok 1 extend-slice
            inside
            end 3array
        ] }
    } case ;

:: read-long-brace ( n string tok ch -- n seq )
    ch {
        { CHAR: = [
            n string "{" take-until-separator CHAR: { = [ "omg error" throw ] unless :> ( n' string' tok2 )
            tok2 length 1 - CHAR: = <string> "}" "}" surround :> needle

            n' string' needle take-until-multi :> ( n'' string'' inside end )
            n''
            tok tok2 length extend-slice
            inside
            end 3array
        ] }
        { CHAR: { [
            n 1 + string "}}" take-until-multi :> ( n' string' inside end )
            n'
            tok 1 extend-slice
            inside
            end 3array
        ] }
    } case ;


ERROR: closing-bracket-expected n string last ;
: read-bracket ( n string seq -- n' seq )
    2over ?nth [ closing-bracket-expected ] unless* {
        { [ dup "=[" member? ] [ read-long-bracket ] } ! double bracket, read [==[foo]==]
        { [ dup blank? ] [ drop [ "]" parse-until ] dip prefix ] } ! regular[ word
        [ drop complete-token ] ! something like [foo]
    } cond ;

ERROR: closing-brace-expected n string last ;
: read-brace ( n string seq -- n' seq )
    2over ?nth [ closing-brace-expected ] unless* {
        { [ dup "={" member? ] [ read-long-brace ] } ! double brace read {=={foo}==}
        { [ dup blank? ] [ drop [ "}" parse-until ] dip prefix ] } ! regular{ word
        [ drop complete-token ] ! something like {foo}
    } cond ;

: token ( n/f string -- n'/f slice/f )
    over [
        skip-blank over
        [
            "!([{\s\r\n" take-until-either {
                { f [ nip ] }
                { CHAR: ! [ drop skip-til-eol token ] }
                { CHAR: ( [ read-paren ] }
                { CHAR: [ [ read-bracket ] }
                { CHAR: { [ read-brace ] }
                [ drop nip ] ! "\s\r\n" found
            } case
            ! ensure-token [ drop token ] [ nip ] if
        ] [ 2drop f f ] if
    ] [
        2drop f f
    ] if ; inline recursive

: parse ( n/f string -- n'/f object/f )
    over [ token dup [ parse-action ] when ] [ 2drop f f ] if ; inline

: parse-until ( n/f string token -- n/f object/f )
    '[
        [ _ parse _ over sequence= [ , f ] [ , t ] if ] loop
    ] { } make ;

: quick-parse-string ( string -- sequence )
    [ 0 ] dip '[ _ parse ] loop>array nip ;

: quick-parse-path ( path -- sequence )
    utf8 file-contents quick-parse-string ;

: quick-parse-vocab ( path -- sequence )
    modern-source-path quick-parse-path ;

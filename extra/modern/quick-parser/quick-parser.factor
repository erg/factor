! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays ascii combinators fry io.encodings.utf8
io.files kernel make math modern.paths multiline sequences
sequences.extras ;
IN: modern.quick-parser

/*
USE: modern.quick-parser
"math" quick-parse-vocab
[ >string . ] each
*/

TUPLE:  parsed-atom { slice slice } ;
TUPLE:  parsed-compound atoms { slice slice } ;

: take-from-until ( n/f string tokens -- n'/f string slice/f ch/f )
    ! If token is single character like ! or (, then width is zero
    ! Add one for slice, and then take the to>> of the slice at the end
    [ dup ] 2dip
    [ '[ _ member? ] find-from ] 2keep drop
    ! n n' ch string
    {
        [ nip over [ nip [ length ] keep ] unless
            ! [ 2dup = [ 1 + ] when ] dip
            <slice>
        ]
        [ drop 2nip ]
    } 4cleave
    [ [ to>> ] [ seq>> ] [ ] tri ] dip ; inline

: skip-blank ( n string -- n' string )
    [ [ blank? not ] find-from drop ] keep ; inline

: skip-til-eol ( n string -- n' string )
    [ [ "\r\n" member? ] find-from drop ] keep ; inline

: prepend-slice ( end begin -- slice )
    [ nip from>> ]
    [ drop [ to>> ] [ seq>> ] bi <slice> ] 2bi ; inline

: complete-token ( n string seq -- n' string seq' )
    [ "\s\r\n" take-from-until drop ] dip prepend-slice ; inline

: parse-action ( n string -- n obj/f )
    ;

DEFER: parse
DEFER: parse-until
: read-brace ( n string seq -- n string seq )
    2over nth blank? [ complete-token ] unless ; inline

: read-paren ( n string seq -- n string seq )
    [ 1 + ] 2dip
    2over ?nth blank? [
        [ drop nip ] ! string
        [ drop ")" parse-until ]
        [ 2nip ] 3tri prefix swapd
    ] [
        complete-token
    ] if ; inline

: read-bracket ( n string seq -- n string seq )
    2over nth blank? [ complete-token ] unless ; inline

: ensure-token ( n string seq/f ch/f -- n/f string/f seq/f loop? )
    {
        { f [ f ] }
        { CHAR: ! [ drop skip-til-eol f t ] }
        { CHAR: { [ read-brace f ] }
        { CHAR: ( [ read-paren f ] }
        { CHAR: [ [ read-bracket f ] }
        [ drop f ] ! whitespace
    } case ; inline

: token ( n/f string -- n'/f slice/f )
    over [
        skip-blank over
        [
            "\s\r\n{([!" take-from-until ensure-token [
                drop token
            ] [
                nip
            ] if
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

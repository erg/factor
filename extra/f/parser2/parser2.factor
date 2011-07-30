! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs checksums checksums.crc32
combinators f.lexer f.manifests fry io io.files io.pathnames
kernel math namespaces nested-comments prettyprint sequences
sets splitting strings vectors vocabs vocabs.loader
vocabs.refresh.monitor ;
QUALIFIED: f.words
QUALIFIED: sets
IN: f.parser2

: with-output-variable ( obj symbol quot -- obj )
    over [ get ] curry compose with-variable ; inline

: (peek-token) ( -- token/string/f )
    read1 [
        dup comment? [ drop (peek-token) ] when
    ] [
        f
    ] if* ;

: peek-token ( -- token/string/f )
    [ (peek-token) text ] with-input-rewind ;

GENERIC: resolve ( object -- object' )

: parse-stack ( -- obj )
    manifest get parsed>> ;

: current-parse-vector ( -- obj )
    parse-stack dup empty? [
        dup last vector? [ last ] when
    ] unless ;

: push-parsed ( obj -- )
    current-parse-vector push ;

: push-all-parsed ( obj -- )
    current-parse-vector push-all ;

: new-parse ( -- )
    V{ } clone parse-stack push ;

: last-parsed ( -- obj )
    parse-stack last
    dup sequence? [ last ] when ;

: pop-parsed ( -- obj )
    parse-stack pop ;

: pop-all-parsed ( -- obj )
    parse-stack
    V{ } clone manifest get parsed<< ;

: push-comment ( comment -- )
    push-parsed ;

: do-parsing-word ( word -- )
    definition>> call( -- obj ) push-parsed ;

: maybe-call-parsing-word ( string -- )
    dup text manifest get search-syntax [
        dup f.words:parsing-word? [
            [
                [ manifest get parsing-word-stack>> push ]
                [ 1vector parse-stack push ] bi
            ]
            [ do-parsing-word ] bi*
            manifest get parsing-word-stack>> pop drop
        ] [
            drop push-parsed
        ] if
    ] [
        push-parsed
    ] if* ;

: just-parsed? ( -- ? )
    manifest get just-parsed>> ;

: just-parsed-on ( -- ) t manifest get just-parsed<< ;

: just-parsed-off ( -- ) f manifest get just-parsed<< ;

: read-token ( -- token/f )
    just-parsed-off
    read1 [
        dup comment? [
            push-comment read-token
        ] when
    ] [
        f
    ] if* ;

: parse ( -- obj/f )
    read-token [
        maybe-call-parsing-word
        last-parsed
    ] [
        f
    ] if* ;

TUPLE: parsing-error-tuple manifest words word-names line# column# error ;

: parsing-error ( error -- * )
    parsing-error-tuple new
        swap >>error
        manifest get >>manifest
        manifest get parsing-word-stack>>
            {
                [ >>words ]
                [ [ text ] map >>word-names ]
                [ first line#>> >>line# ]
                [ first column#>> >>column# ]
            } cleave
        throw ; inline

ERROR: token-expected expected ;

: parse-until ( string -- obj )
    new-parse
    dup '[
        parse [
            just-parsed? [
                just-parsed-off
                drop t
            ] [
                last-token text _ = [
                    [ just-parsed-on ] when
                ] keep not
            ] if
        ] [
            ! _ token-expected
            _ \ token-expected boa parsing-error
        ] if*
    ] loop
    pop-parsed [ push-all-parsed ] keep but-last
    [ comment? not ] filter ;

ERROR: premature-eof ;

: token ( -- string/f )
    read-token [
        [ push-parsed ] [ text ] bi
    ] [
        premature-eof
    ] if* ;

: tokens-until ( string -- sequence )
    new-parse
    dup '[
        read-token [ dup push-parsed text _ = not ] [ _ token-expected ] if*
    ] loop
    pop-parsed [ push-all-parsed ] keep
    but-last [ text ] map
    [ comment? not ] filter ;

: chunk ( -- token/f )
    lex-chunk [ premature-eof ] unless*
    dup push-parsed text ;

: chunks-until ( string -- seq )
    new-parse
    dup '[
        chunk [ _ = not ] [ _ token-expected ] if*
    ] loop
    pop-parsed [ push-all-parsed ] keep
    but-last [ text ] map ;
    
: add-parse-tree ( -- )
    pop-all-parsed manifest get objects>> push-all ;

: output-manifest ( quot -- manifest )
    [ manifest ] dip '[ @ ] with-output-variable ; inline

: parse-factor-stream ( manifest -- manifest )
    [
        [
            parse drop
            parse-stack empty? [ f ] [ add-parse-tree t ] if
        ] loop
    ] output-manifest ;

: should-parse? ( path -- ? )
    [ crc32 checksum-file ]
    [ path>vocab get-manifest checksum>> ] bi = not ;

: path>manifest ( path -- manifest/f )
    [
        dup exists? [
            <pathname>
            "Parsing " write dup .
            dup dup crc32 checksum-file <manifest>
            '[ _ parse-factor-stream ] with-file-lexer
        ] [
            drop f
        ] if
    ] [
        f
    ] if* ;
    
: parse-vocab ( string -- manifest/f )
    vocab-source-path [ path>manifest ] [ f ] if* ;

: parse-syntax ( string -- manifest )
    vocab-syntax-path [ path>manifest ] [ f ] if* ;
    
: parse-factor ( string -- manifest )
    f dup crc32 checksum-bytes <manifest>
    '[ _ parse-factor-stream ] with-string-lexer ;

ERROR: expected expected got ;

: expect ( string -- )
    token 2dup = [ 2drop ] [ \ expected boa parsing-error ] if ;

: add-search-vocabulary ( token manifest -- )
    used>> sets:adjoin ;

: parse-use ( -- string )
    token [ manifest get add-search-vocabulary ] [ ] bi ;

: parse-unuse ( -- string )
    token ;
    
: body ( -- seq )
    ";" parse-until ;

"f.cheat" require

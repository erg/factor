! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators f.lexer f.manifests
f.parser2 io.streams.document kernel namespaces sequences sets
splitting f.namespaces ;
QUALIFIED: f.words
QUALIFIED: sets
IN: f.identifiers

: maybe-create-vocabulary ( string hashtable -- )
    2dup key? [
        2drop
    ] [
        [ H{ } clone ] 2dip set-at
    ] if ;

: set-in ( string -- )
    text
    manifest get
    [ identifiers>> maybe-create-vocabulary ]
    [ in<< ] 2bi ;

: parse-in ( -- string )
    @token dup set-in ;

: current-vocabulary ( -- vocabulary )
    manifest get [ in>> ] [ identifiers>> ] bi at ;

: nest-at ( key hashtable -- value-hashtable )
    2dup ?at [
        2nip
    ] [
        drop [ H{ } clone dup ] 2dip set-at
    ] if ;

: get-identifiers ( string -- hashtable )
    manifest get identifiers>> nest-at ;
    
: current-vocabulary-name ( -- string )
    manifest get in>> ;

ERROR: identifier-redefined word vocabulary ;

: check-identifier-exists ( string -- string )
    dup
    text current-vocabulary key? [
        current-vocabulary-name identifier-redefined
    ] when ;

ERROR: no-IN:-form ;

: check-in-exists ( -- )
    manifest get in>> [ no-IN:-form ] unless ;

: remove-identifier ( string -- )
    check-in-exists
    current-vocabulary delete-at ;

: add-non-unique-identifier ( string -- )
    check-in-exists
    dup current-vocabulary set-at ;
    
: add-identifier ( string -- )
    check-identifier-exists
    add-non-unique-identifier ;

: lookup-identifier ( identifier vocabulary-name -- obj/f )
    manifest get identifiers>> ?at [
        at
    ] [
        2drop f
    ] if ;
    
: add-unique-identifier-to ( identifier vocabulary-name -- )
    2dup lookup-identifier [
        identifier-redefined
    ] [
        get-identifiers conjoin
    ] if ;

: add-non-unique-identifier-to ( identifier vocabulary-name -- )
    get-identifiers conjoin ;
    
: forget-identifier ( -- string )
    token dup remove-identifier ;

: identifier ( -- string )
    token
    dup add-identifier ;

: define-slot-identifier ( string -- )
    {
        [ "accessors" add-non-unique-identifier-to ]
        [ ">>" prepend "accessors" add-non-unique-identifier-to ]
        [ ">>" append "accessors" add-non-unique-identifier-to ]
        [ "change-" prepend "accessors" add-non-unique-identifier-to ]
    } cleave ;
    
: identifiers-until ( string -- seq )
    tokens-until
    dup [ add-identifier ] each ;

: method-identifier ( -- pair )
    token token 2array dup add-identifier ;

: ensure-in-manifest ( -- ) manifest get ensure-in drop ;

: trim-private ( string -- string )
    ".private" ?tail drop ;
    
: append-private ( string -- string' )
    trim-private ".private" append ;

: private-on ( -- )
    ensure-in-manifest
    manifest get in>>
    append-private set-in ;

: private-off ( -- )
    ensure-in-manifest
    manifest get in>> trim-private set-in ;

! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs combinators f.dictionary f.parser2
f.words io.files kernel namespaces sequences strings vocabs
vocabs.loader ;
IN: f.vocabularies

SYMBOL: vocabularies
vocabularies [ H{ } clone ] initialize

TUPLE: vocabulary
    name
    syntax
    source
    docs
    tests ;

: <vocabulary> ( name -- vocabulary )
    vocabulary new
        swap >>name ; inline

: lookup-vocabulary ( name -- vocabulary )
    dup vocabularies get ?at [
        nip
    ] [
        <vocabulary> [ swap vocabularies get-global set-at ] keep
    ] if ;
    
: load-syntax ( name -- )
    [ vocab-syntax-path path>manifest ]
    [ lookup-vocabulary ] bi syntax<< ;

: load-source ( name -- )
    [ vocab-source-path path>manifest ]
    [ lookup-vocabulary ] bi source<< ;
    
: load-docs ( name -- )
    [ vocab-docs-path path>manifest ]
    [ lookup-vocabulary ] bi docs<< ;
    
: load-tests ( name -- )
    [ vocab-tests-path path>manifest ]
    [ lookup-vocabulary ] bi tests<< ;
  
: load-vocabulary ( name -- )
    {
        [ load-syntax ] [ load-source ] [ load-docs ] [ load-tests ]
    } cleave ;    

: load-all-syntax ( -- )
    vocabs [ load-syntax ] each ;

: load-all-source ( -- )
    vocabs [ load-source ] each ;

: load-all-docs ( -- )
    vocabs [ load-docs ] each ;

: load-all-tests ( -- )
    vocabs [ load-tests ] each ;

: load-all ( -- )
    vocabs [ load-vocabulary ] each ;

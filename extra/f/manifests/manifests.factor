! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs assocs.private checksums
checksums.crc32 combinators kernel math namespaces sequences ;
IN: f.manifests

GENERIC: preload-syntax-namespaces ( manifest -- manifest )

TUPLE: manifest
    path
    checksum
    objects
    using
    used
    parsed
    parsing-word-stack
    just-parsed
    syntax-namespaces ;
    
: <manifest> ( path checksum -- obj )
    manifest new
        swap >>checksum
        swap >>path
        V{ } clone >>objects
        V{ } clone >>using
        V{ } clone >>used
        V{ } clone >>parsed
        V{ } clone >>parsing-word-stack
        V{ } clone >>syntax-namespaces
    preload-syntax-namespaces ; inline
    
SYMBOL: manifests
manifests [ H{ } clone ] initialize

: get-manifest ( string -- manifest/f )
    manifests get-global at ;

: set-manifest ( manifest vocab -- )
    manifests get-global set-at ;

: manifest-uptodate? ( manifest -- ? )
    [ path>> crc32 checksum-file ] [ checksum>> ] bi = ;
    
: add-namespace-to-syntax ( vocabulary manifest -- )
    syntax-namespaces>> push ;
    
ERROR: ambiguous-word words ;

: check-ambiguities ( sequence -- word/f )
    dup length {
        { 0 [ drop f ] }
        { 1 [ first ] }
        [ ambiguous-word ]
    } case ;

: search-namespaces ( string namespaces -- words )
    [ words>> at ] with map sift check-ambiguities ;

: search-syntax ( string manifest -- word/f )
    syntax-namespaces>> search-namespaces ;

: use-namespace ( string -- )
    manifest get
    [ using>> push ]
    [ used>> push ] 2bi ;

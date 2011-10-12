! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs assocs.private f.lexer f.words kernel
math nested-comments sequences strings ;
IN: f.namespaces

ERROR: key-exists value key assoc ;

: set-at-unique ( value key assoc -- )
   2dup key? [ key-exists ] [ set-at ] if ;

: assoc-union-unique! ( assoc1 assoc2 -- assoc1 )
   over [ set-at-unique ] with-assoc assoc-each ;

: assoc-union-unique ( assoc1 assoc2 -- union )
   [ [ [ assoc-size ] bi@ + ] [ drop ] 2bi new-assoc ] 2keep
   [ assoc-union-unique! ] bi@ ;

GENERIC: add-word-to-namespace ( word namespace -- )

SLOT: in

ERROR: no-in object ;

: ensure-in ( object -- object )
    dup in>> [ no-in ] unless ;
    

TUPLE: #namespace < identity-tuple { name string } words ;

: <namespace> ( name -- namespace )
    #namespace new
        swap >>name
        H{ } clone >>words ; inline

ERROR: symbol-redefined string namespace ;

: ensure-unique ( string namespace -- string namespace )
    2dup words>> key? [ symbol-redefined ] when ;

M: #namespace add-word-to-namespace ( word namespace -- )
    2dup [ name>> ] dip ensure-unique 2drop
    [ [ ] [ name>> ] bi ] [ words>> ] bi* set-at ;

: add-parsing-word ( namespace name quot -- )
    <#parsing-word> dup namespace>> add-word-to-namespace ;

TUPLE: #compound-namespace < identity-tuple in namespaces ;

: <compound-namespace> ( -- compound-namespace )
    #compound-namespace new
        H{ } clone >>namespaces ; inline

: lookup-namespace ( string compound-namespace -- namespace )
    2dup namespaces>> ?at [
        2nip
    ] [
        drop
        [ [ <namespace> ] keep ] dip namespaces>> [ set-at ] 3keep 2drop
    ] if ;

: init-symbol ( object string namespace -- )
    lookup-namespace ensure-unique add-word-to-namespace ;
    
M: #compound-namespace add-word-to-namespace
    ensure-in
    [ in>> ] keep lookup-namespace add-word-to-namespace ;
    
GENERIC: flatten-namespaces ( object -- namespace )

M: #namespace flatten-namespaces
    words>> ;
    
M: #compound-namespace flatten-namespaces
    namespaces>> flatten-namespaces ;
    
M: sequence flatten-namespaces
    H{ } clone [ flatten-namespaces assoc-union-unique! ] reduce ;

! Copyright (C) 2009 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays continuations db.connections db.errors
db.result-sets db.utils destructors fry kernel sequences math ;
IN: db.statements

TUPLE: statement handle sql in out after
retries errors retry-quotation ;
! reconstructor

: normalize-statement ( statement -- statement )
    [ object>vector ] change-in
    [ object>vector ] change-out ; inline

: initialize-statement ( statement -- statement )
    V{ } clone >>in
    V{ } clone >>out
    V{ } clone >>errors ; inline
 
: <sql> ( string -- statement )
    statement new
        swap >>sql
        initialize-statement ; inline

: <statement> ( -- statement )
    statement new
        initialize-statement ; inline

HOOK: next-bind-index db-connection ( -- string )
HOOK: init-bind-index db-connection ( -- )

: add-sql ( statement sql -- statement )
    '[ _ "" append-as ] change-sql ;

: add-in ( statement binder -- statement ) over in>> push ;
: add-out ( statement binder -- statement ) over out>> push ;

HOOK: statement>result-set db-connection ( statement -- result-set )
HOOK: prepare-statement* db-connection ( statement -- statement' )
HOOK: dispose-statement db-connection ( statement -- )
HOOK: bind-sequence db-connection ( statement -- )
HOOK: reset-statement db-connection ( statement -- statement' )

ERROR: no-database-in-scope ;

M: statement dispose dispose-statement ;
M: f dispose-statement no-database-in-scope ;
M: object reset-statement ;

: with-sql-error-handler ( quot -- )
    [ dup sql-error? [ parse-sql-error ] when rethrow ] recover ; inline

: prepare-statement ( statement -- statement )
    [ dup handle>> [ prepare-statement* ] unless ] with-sql-error-handler ;

: result-set-each ( statement quot: ( statement -- ) -- )
    over more-rows?
    [ [ call ] 2keep over advance-row result-set-each ]
    [ 2drop ] if ; inline recursive

: result-set-map ( statement quot -- sequence )
    collector [ result-set-each ] dip { } like ; inline

: statement>result-sequence ( statement -- sequence )
    statement>result-set
    [ [ sql-row ] result-set-map ] with-disposal ;

: (run-after-setters) ( tuple statement -- )
    after>> [
        [ value>> ] [ setter>> ] bi
        call( obj val -- obj ) drop
    ] with each ;

: run-after-setters ( tuple statement -- )
    dup sequence? [
        [ (run-after-setters) ] with each
    ] [
        (run-after-setters)
    ] if ;

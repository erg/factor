! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs classes f.cheat f.lexer f.namespaces
fry kernel prettyprint sequences f.identifiers ;
QUALIFIED-WITH: f.cheat f
QUALIFIED-WITH: kernel k
QUALIFIED-WITH: io.streams.document io
IN: f.process

TUPLE: processing using in namespaces other last-defined top-level ;

: <processing> ( -- processing )
    processing new
        V{ "syntax" } clone >>using
        H{ } clone >>namespaces
        V{ } clone >>other
        V{ } clone >>top-level ;
        
ERROR: no-in object processing ;

: ensure-in ( object processing -- object processing )
    dup in>> [ no-in ] unless ;
    
: get-namespace ( string processing -- namespace )
    namespaces>> 2dup at [
        2nip
    ] [
        [ [ <namespace> ] [ ] bi ] dip [ set-at ] 3keep 2drop
    ] if* ;

: current-namespace ( processing -- namespace )
    [ in>> ] [ ] bi get-namespace ;
    
: define-symbol ( object processing -- )
    ensure-in
    [ [ [ ] [ name>> ] bi ] dip [ in>> ] [ ] bi get-namespace init-symbol ]
    [ [ name>> ] dip last-defined<< ] 2bi ;
    
: define-identifier ( object string processing -- )
    [ current-namespace init-symbol ]
    [ last-defined<< drop ] 3bi ;
    
: define-other ( object processing -- )
    other>> push ;
    
: mark-last-defined ( object processing -- )
    2drop ;
    
: do-begin-private ( processing -- )
    [ append-private ] change-in drop ;

: do-end-private ( processing -- )
    [ trim-private ] change-in drop ;
    
: top-level ( object processing -- )
    top-level>> push ;
    
GENERIC# process 1 ( object processing -- )

M: @defer process 2drop ;
M: @line-comment process 2drop ;
M: @lua-comment process 2drop ;
M: @using process [ vocabularies>> ] [ using>> ] bi* push-all ;
M: @in process [ vocabulary>> ] dip in<< ;

! TODO
M: @from process 2drop ;
M: @qualified-with process 2drop ;
M: @qualified process 2drop ;
M: @rename process 2drop ;
M: @exclude process 2drop ;
M: @slot process define-symbol ;
M: @functor-syntax process 2drop ;

M: @generic process define-symbol ;
M: @generic# process define-symbol ;
M: @word process define-symbol ;
M: @math process define-symbol ;
M: @union process define-symbol ;
M: @error process define-symbol ;
M: @tuple process define-symbol ;
M: @mixin process define-symbol ;
M: @constructor process define-symbol ;
M: @predicate process define-symbol ;
M: @function process define-symbol ;
M: @function-alias process [ [ ] [ alias>> ] bi ] dip define-identifier ;
M: @constant process define-symbol ;
M: @hook process define-symbol ;
M: @macro process define-symbol ;
M: @local-macro process define-symbol ;
M: @local-word process define-symbol ;
M: @gl-function process define-symbol ;
M: @local-memo process define-symbol ;
M: @memo process define-symbol ;
M: @struct process define-symbol ;
M: @ebnf process define-symbol ;
M: @functor process define-symbol ;
M: @peg process define-symbol ;
M: @syntax process define-symbol ;
M: @library process define-symbol ;
M: @ctype process define-symbol ;
M: @com-interface process define-symbol ;
M: @article process define-symbol ;
M: @typed process define-symbol ;
M: @about process define-symbol ;

M: @alias process [ [ ] [ new>> ] bi ] dip define-identifier ;
M: @typedef process [ [ ] [ new>> ] bi ] dip define-identifier ;
M: io:token process top-level ;
M: @quotation process top-level ;
M: @lexed-string process top-level ;
M: @array process top-level ;
M: @main process top-level ;
M: @local-method process top-level ;
M: @hashtable process top-level ;
M: @literal process top-level ;
M: @parse-time process top-level ;
M: @hex process top-level ;
M: @assoc-tuple process top-level ;
M: @vector process top-level ;
M: @lua-string process top-level ;

M: @symbols process [ sequence>> ] dip '[ dup _ define-identifier ] each ;
M: @singletons process [ sequence>> ] dip '[ dup _ define-identifier ] each ;
M: @instance process define-other ;
M: @method process define-other ;

M: @inline process mark-last-defined ;
M: @foldable process mark-last-defined ;
M: @recursive process mark-last-defined ;
M: @flushable process mark-last-defined ;

M: @begin-private process nip do-begin-private ;
M: @end-private process nip do-end-private ;
M: object process over class . top-level>> push ;


: process-manifest ( manifest -- processing )
    objects>> <processing> [ '[ _ process ] each ] keep ;
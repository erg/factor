! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs classes f.cheat f.identifiers f.lexer
f.namespaces fry io.streams.document kernel prettyprint
sequences ;
QUALIFIED-WITH: kernel k
IN: f.process

TUPLE: processing using in dependents last-defined top-level ;

: <processing> ( -- processing )
    processing new
        V{ "syntax" } clone >>using
        V{ } clone >>dependents
        V{ } clone >>top-level ;
    
: define-sym ( object processing -- )
    ensure-in
    <compound-namespace>
    over in>> >>in
    [ nip add-word-to-namespace ]
    [ drop [ name>> ] dip last-defined<< ]
    [ nip >>namespace drop ] 3tri ;
    
: define-dependent ( object processing -- )
    dependents>> push ;
    
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
M: @slot process define-sym ;
M: @functor-syntax process 2drop ;

M: @generic process define-sym ;
M: @generic# process define-sym ;
M: @word process define-sym ;
M: @math process define-sym ;
M: @union process define-sym ;
M: @error process define-sym ;
M: @tuple process define-sym ;
M: @mixin process define-sym ;
M: @constructor process define-sym ;
M: @predicate process define-sym ;
M: @function process define-sym ;
M: @function-alias process define-sym ;
M: @constant process define-sym ;
M: @hook process define-sym ;
M: @macro process define-sym ;
M: @local-macro process define-sym ;
M: @local-word process define-sym ;
M: @gl-function process define-sym ;
M: @local-memo process define-sym ;
M: @memo process define-sym ;
M: @struct process define-sym ;
M: @ebnf process define-sym ;
M: @functor process define-sym ;
M: @peg process define-sym ;
M: @syntax process define-sym ;
M: @library process define-sym ;
M: @ctype process define-sym ;
M: @com-interface process define-sym ;
M: @article process define-sym ;
M: @typed process define-sym ;
M: @about process define-sym ;

M: @alias process define-sym ;
M: @typedef process define-sym ;
M: @token process top-level ;
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

M: @symbols process [ sequence>> ] dip '[ _ define-sym ] each ;
M: @singletons process [ sequence>> ] dip '[ _ define-sym ] each ;

M: @instance process define-dependent ;
M: @method process define-dependent ;

M: @inline process mark-last-defined ;
M: @foldable process mark-last-defined ;
M: @recursive process mark-last-defined ;
M: @flushable process mark-last-defined ;

M: @begin-private process nip do-begin-private ;
M: @end-private process nip do-end-private ;
M: object process over class . top-level>> push ;

: process-manifest ( manifest -- processing )
    objects>> <processing> [ '[ _ process ] each ] keep ;
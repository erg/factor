! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators
combinators.short-circuit fry io io.files io.streams.document
io.streams.string kernel math modern.parser modern.parser.factor
modern.paths multiline sequences sequences.deep sequences.extras
unicode.categories ;
FROM: sequences => change-nth ;
IN: modern.refactor

GENERIC: parsed-objects ( obj -- obj )
M: object parsed-objects ;
M: psequence parsed-objects
    [ object>> [ parsed-objects ] map ] [ prefix ] bi ;
M: sequence parsed-objects
    [ parsed-objects ] map flatten ;

GENERIC# refactor' 1 ( obj quot: ( obj -- obj' ) -- )
M: object refactor' call( obj -- obj ) drop ;
M: psequence refactor'
    [ call( obj -- obj ) drop ]
    [ [ object>> ] dip '[ _ refactor' ] each ] 2bi ;
M: sequence refactor' '[ _ refactor' ] each ;

: refactor ( seq pred quot -- seq' )
    [
        [ parsed-objects ] 2dip
        [ filter ] dip
        '[ @ drop ] each
    ] 3keep 2drop ; inline

: refactor-path ( path pred quot -- )
    '[ parse-modern-file _ _ refactor ] keep write-modern-file ; inline

: refactor-vocab ( path pred quot -- )
    [ modern-source-path ] 2dip refactor-path ; inline

: refactor-codebase ( pred quot -- )
    [ all-factor-files [ ".modern" tail? ] reject ] 2dip '[ _ _ refactor-path ] each ; inline


: refactor-macro-out ( obj -- obj' )
    object>> third object>> fourth
    dup poutputs? [
        [
            [ "quot" <spaced-reldoc> 1array ] when-empty
        ] change-object
    ] when ;

: refactor-codebase-macro-out ( -- )
    [ { [ pmacro? ] [ pmacro-locals? ] } 1|| ]
    [ refactor-macro-out ] refactor-codebase ;



: rename-comment ( obj comment -- obj' )
    [ object>> first ] dip '[ drop _ ] change-object ;

: rename-comment-codebase ( str -- )
    [ { [ pcomment? ] [ pshell-comment? ] } 1|| ] swap
    '[ _ rename-comment ] refactor-codebase ;

: rename-c-comment ( -- ) "//" rename-comment-codebase ;


: trim-trailing-comment-whitespace ( obj -- obj' )
    object>> second [
        [ blank? ] trim-tail
    ] change-object ;

: refactor-comment-whitespace ( -- )
    [ { [ pcomment? ] [ pshell-comment? ] } 1|| ]
    [ trim-trailing-comment-whitespace ] refactor-codebase ;


: boa-tuple-literal? ( obj -- ? )
    {
        [ ptuple-literal? ]
        [ object>> length 3 > ]
        [ object>> third ?first [ object>> "f" = ] [ f ] if* ]
    } 1&& ;

: rename-boa-tuple ( obj -- obj' )
    [
        {
            [ first [ drop "T[" ] change-object drop ]
            [ third first [ drop f ] change-object drop ]
            [ last [ drop "]" ] change-object drop ]
            [ ]
        } cleave
    ] change-object ;

: rename-boa-tuple-literal-codebase ( -- )
    [ boa-tuple-literal? ] [ rename-boa-tuple ] refactor-codebase ;


: c-function-declaration? ( obj -- ? )
    {
        [ pc-function? ]
        [ pfunction-alias? ]
        [ px-function? ]
        [ pgl-function? ]
        [ pc-callback? ]
        [ psubroutine? ]
    } 1|| ;

: rename-function-no-semi ( obj -- obj )
    object>> last [ drop f ] change-object ;

: rename-function-no-semi-codebase ( -- )
    [ c-function-declaration? ] [ rename-function-no-semi ] refactor-codebase ;

/*
! Renames "[" "]" to "{" "}"
: block>array ( block -- array )
    dup pblock? [
        dup object>>
        [ first "{" >>object drop ]
        [ last "}" >>object drop ] bi
    ] when ;

: warn-rename-unit-test-quots ( obj -- obj )
    {
        { [ dup { [ ptoken? ] [ name>> "1array" = ] } 1&& ] [ "1array pattern detected, try using ${ } instead" print ] }
        [ ]
    } cond ;

: rename-unit-test-quots ( vocab -- )
    modern-tests-path
    dup exists? [
        dup print flush
        parse-modern-file [
            second
            dup [ unit-test? ] find-all keys
            ! Make sure [ ] unit-test
            [ 1 - ] map
            over '[ _ nth pblock? ] filter
            [ 1 - ] map
            ! Make sure looks like [ ] [ ] unit-test
            over '[ _ nth pblock? ] filter
            over
            '[
                _ [
                    warn-rename-unit-test-quots
                    block>array
                ] change-nth
            ] each
        ] [
            first write-modern-file
        ] bi
    ] [
        drop
    ] if ;

: rewrite-all-tests ( -- ) all-vocabs [ rename-unit-test-quots ] each ;

! Rewrite FUNCTION: to not have a trailing ;

: c-function-remove-semi ( obj -- obj )
    dup object>> dup [
        last object>> ";" = [ [ but-last ] change-object ] when
    ] [
        drop
    ] if ;

: c-function-add-semi ( obj -- obj )
    dup object>> dup [
        last object>> ";" = [
            [
                dup last " ;" doc-after suffix
            ] change-object
        ] unless
    ] [
        drop
    ] if ;

! Needs a new syntax definition word at same time
: rename-functions-no-semi ( names -- )
    [
        [
            parse-modern-file
            [ dup pc-function? [ c-function-remove-semi ] when ] map
        ] keep write-modern-file
    ] each ;

: rename-functions-add-semi ( names -- )
    [
        [
            parse-modern-file
            [ dup pc-function? [ c-function-add-semi ] when ] map
        ] keep write-modern-file
    ] each ;

! simple text replacement
: rename-texts ( object assoc -- object )
    '[
        [ [ _ ?at drop ] change-object ] map
    ] change-object ;

: rename-by-name ( paths assoc -- )
    '[
        [
            parse-modern-file [ _ rename-texts ] map
        ] keep write-modern-file
    ] each ;

: rename-all-by-name ( assoc -- )
    [ all-factor-files ] dip rename-by-name ;

: rewrite-sbufs ( path -- )
    [
        parse-modern-file
        [
            dup pblock? [
                dup body>> [
                    dup { [ pstring? ] [ class>> "SBUF" = ] } 1&&
                    [
                        [
                            rest
                        ] change-string
                    ] when drop
                ] each
            ] when
        ] map
    ] keep write-modern-file ;

: rewrite-string ( string assoc -- string' )
    [ parse-modern-string ] dip '[ _ rename-texts ] map documents>string ;



! move word from vocab to vocab
! - should maybe not recompile
! rename word
! generate text from words
! - generate ast? generate quotation?
*/

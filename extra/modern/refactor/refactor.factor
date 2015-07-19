! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators
combinators.short-circuit fry io io.files io.streams.document
io.streams.string kernel math modern.lookup modern.parser
modern.parser.factor modern.paths multiline sequences
sequences.extras sequences.deep unicode.categories ;
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

: rename-comment ( obj -- obj' )
    object>> first [
        drop "//"
    ] change-object ;

: trim-trailing-comment-whitespace ( obj -- obj' )
    object>> second [
        [ blank? ] trim-tail
    ] change-object ;

: refactor-comment-whitespace ( -- )
    [ { [ pcomment? ] [ pshell-comment? ] } 1|| ]
    [ trim-trailing-comment-whitespace ] refactor-codebase ;

/*

"MACRO: nover ( n -- quot )
    dup 1 + '[ _ npick ] n*quot ;" parse-modern-string
[ "hi" print . ] doit


"MACRO: nover ( n -- quot )
    dup 1 + '[ _ npick ] n*quot ;" parse-modern-string
dup
first object>> [ psignature? ] filter
first object>> [ poutputs? ] filter
first [ dup length 1 = [ "quot" <spaced-reldoc> prefix ] unless ] change-object drop
write-modern-string print
*/


! : refactor-macro-out ( seq -- ) ;


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

/*
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

: change-macro-out ( macro -- macro )
    B ;

: rewrite-macro-out ( -- )
    all-factor-files [
        [
            parse-modern-file [
                dup pmacro? [
                    change-macro-out
                ] when
            ] map
        ] keep write-modern-file
    ] each ;

! move word from vocab to vocab
! - should maybe not recompile
! rename word
! generate text from words
! - generate ast? generate quotation?
*/

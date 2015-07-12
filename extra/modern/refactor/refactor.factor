! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs combinators combinators.short-circuit
fry io io.files io.streams.document io.streams.string kernel
math modern.lookup modern.parser modern.parser.factor sequences
sequences.extras ;
FROM: sequences => change-nth ;
IN: modern.refactor

! move word from vocab to vocab
! - should maybe not recompile
! rename word
! generate text from words
! - generate ast? generate quotation?

: documents>string ( documents -- string )
    [
        output>document-stream
        [ texts>> [ write ] each ] each nl
    ] with-string-writer ;

! Renames "[" "]" to "{" "}"
: block>array ( block -- array )
    dup block? [
        dup texts>>
        [ first "{" >>object drop ]
        [ last "}" >>object drop ] bi
    ] when ;

: warn-rename-unit-test-quots ( obj -- obj )
    {
        { [ dup { [ mtoken? ] [ name>> "1array" = ] } 1&& ] [ "1array pattern detected, try using ${ } instead" print ] }
        [ ]
    } cond ;

: rename-unit-test-quots ( vocab -- )
    modern-tests-path
    dup exists? [
        dup print flush
        parse-modern-file [
            second
            dup [ munit-test? ] find-all keys
            ! Make sure [ ] unit-test
            [ 1 - ] map
            over '[ _ nth block? ] filter
            [ 1 - ] map
            ! Make sure looks like [ ] [ ] unit-test
            over '[ _ nth block? ] filter
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
    dup texts>> dup [
        last object>> ";" = [ [ but-last ] change-texts ] when
    ] [
        drop
    ] if ;

: c-function-add-semi ( obj -- obj )
    dup texts>> dup [
        last object>> ";" = [
            [
                dup last " ;" document-object-after suffix
            ] change-texts
        ] unless
    ] [
        drop
    ] if ;

! Needs a new syntax definition word at same time
: rename-functions-no-semi ( names -- )
    [
        [
            parse-modern-file
            [ dup c-function? [ c-function-remove-semi ] when ] map
        ] keep write-modern-file
    ] each ;

: rename-functions-add-semi ( names -- )
    [
        [
            parse-modern-file
            [ dup c-function? [ c-function-add-semi ] when ] map
        ] keep write-modern-file
    ] each ;

! simple text replacement
: rename-texts ( object assoc -- object )
    '[
        [ [ _ ?at drop ] change-object ] map
    ] change-texts ;

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
            dup block? [
                dup body>> [
                    dup { [ mstring? ] [ class>> "SBUF" = ] } 1&&
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
                dup mmacro? [
                    change-macro-out
                ] when
            ] map
        ] keep write-modern-file
    ] each ;

! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs combinators combinators.smart fry
generalizations kernel lexer locals nested-comments parser
sequences sequences.generalizations slots.private vocabs.parser
words arrays ;
IN: accs

ERROR: unknown-slot word name ;

ERROR: invalid-getter-dispatch object expected-class ;

ERROR: sequence-expected got ;
ERROR: invalid-sequence-length setter sequence expected-length ;

:: slot# ( name slots -- n )
    slots [ name>> name = ] find nip
    [ offset>> ] [ word name unknown-slot ] if* ;

: getter{-word-name ( string -- string' )
    "{-getter-word" append ;

: getter[-word-name ( string -- string' )
    "[-getter-word" append ;

: setter{-word-name ( string -- string' )
    "{-setter-word" append ;

: setter[-word-name ( string -- string' )
    "[-setter-word" append ;

:: accessor-class-guard ( class -- quot )
    class "predicate" word-prop
    '[ dup @ [ class invalid-getter-dispatch ] unless ] ;

:: accessor-sequence-guard ( n -- quot )
    '[
        dup sequence? [
            dup length n = [
                [ word ] dip n invalid-sequence-length
            ] unless
        ] [
            sequence-expected
        ] if
    ] ;

: lookup-execute ( tree seq name -- tree )
    current-vocab name>> lookup execute( seq -- quot ) append! ;

:: define-tuple-getter-word ( name class -- )
    name "-impl:" append create-in
    [
        [ class accessor-class-guard ] dip
        '[ @ _ slot ]
    ] (( string -- quot )) define-declared ;

:: define-tuple-getter-syntax ( name class -- )
    name "get-" ":" surround create-in
    class "slots" word-prop :> slots
    [
        scan slots slot#
        name "-impl:" append lookup-execute
    ] define-syntax ;


:: define-tuple-setter-word ( name class -- )
    name "set-" "-impl:" surround create-in
    [
        [ class accessor-class-guard ] dip
        '[ over @ _ set-slot ]
    ] (( string -- quot )) define-declared ;

:: define-tuple-setter-syntax ( name class -- )
    name "set-" ":" surround create-in
    class "slots" word-prop :> slots
    [
        scan slots slot#
        name "set-" "-impl:" surround lookup-execute
    ] define-syntax ;


:: define-tuple-changer-word ( name class -- )
    name "change-" "-impl:" surround create-in
    [
        [ class accessor-class-guard ] dip
        dup '[ over @ [ [ _ slot ] dip call ] dip swap over _ set-slot ]
    ] (( string -- quot )) define-declared ;

:: define-tuple-changer-syntax ( name class -- )
    name "change-" ":" surround create-in
    class "slots" word-prop :> slots
    [
        scan slots slot#
        name "change-" "-impl:" surround lookup-execute
    ] define-syntax ;


:: define-tuple-getter-word{ ( name class -- )
    name getter{-word-name create-in
    [
        [ class accessor-class-guard ] dip
        [ '[ _ slot ] ] map
        '[ @ [ _ cleave ] output>array ]
    ] (( seq -- quot )) define-declared ;

:: define-tuple-getter-syntax{ ( name class -- )
    name "get-" "{" surround create-in
    class "slots" word-prop :> slots
    [
        "}" [ slots slot# ] map-tokens
        name getter{-word-name lookup-execute
    ] define-syntax ;


:: define-tuple-getter-word[ ( name class -- )
    name getter[-word-name create-in
    [
        [ class accessor-class-guard ] dip
        [ '[ _ slot ] ] map
        '[ @ _ cleave ]
    ] (( seq -- quot )) define-declared ;

:: define-tuple-getter-syntax[ ( name class -- )
    name "get-" "[" surround create-in
    class "slots" word-prop :> slots
    [
        "]" [ slots slot# ] map-tokens
        name getter[-word-name lookup-execute
    ] define-syntax ;


:: define-tuple-setter-word{ ( name class -- )
    name setter{-word-name create-in
    [
        [ class accessor-class-guard ] dip
        [ length [ accessor-sequence-guard ] [ ] bi ]
        [ [ '[ over _ set-slot ] ] map ] bi
        '[ [ @ ] [ @ ] bi* _ firstn _ spread ]
    ] (( seq -- quot )) define-declared ;

:: define-tuple-setter-syntax{ ( name class -- )
    name "set-" "{" surround create-in
    class "slots" word-prop :> slots
    [
        "}" [ slots slot# ] map-tokens
        name setter{-word-name
        lookup-execute
    ] define-syntax ;


:: define-tuple-setter-word[ ( name class -- )
    name setter[-word-name create-in
    [
        [ class accessor-class-guard ] dip
        dup length :> n
        [ '[ over _ set-slot ] ] map
        '[ [ @ ] n ndip _ spread ]
    ] (( seq -- quot )) define-declared ;

:: define-tuple-setter-syntax[ ( name class -- )
    name "set-" "[" surround create-in
    class "slots" word-prop :> slots
    [
        "]" [ slots slot# ] map-tokens
        name setter[-word-name lookup-execute
    ] define-syntax ;


:: define-tuple-class-accessors ( class -- )
    class name>> :> name
    class "slots" word-prop :> slots

    name class define-tuple-getter-word
    name class define-tuple-getter-syntax

    name class define-tuple-setter-word
    name class define-tuple-setter-syntax

    name class define-tuple-changer-word
    name class define-tuple-changer-syntax

    name class define-tuple-getter-word{
    name class define-tuple-getter-syntax{

    name class define-tuple-getter-word[
    name class define-tuple-getter-syntax[

    name class define-tuple-setter-word{
    name class define-tuple-setter-syntax{

    name class define-tuple-setter-word[
    name class define-tuple-setter-syntax[ ;


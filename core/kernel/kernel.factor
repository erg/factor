! Copyright (C) 2004, 2009 Slava Pestov.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel.private slots.private math.private ;
IN: kernel

DEFER: dip
DEFER: 2dip
DEFER: 3dip

! Stack stuff
: 2over ( x y z -- x y z x y ) pick pick ; inline

: clear ( -- ) { } set-datastack ;

! Combinators
GENERIC: call ( callable -- )

GENERIC: execute ( word -- )

GENERIC: ?execute ( word -- value )

M: object ?execute ;

DEFER: if

: ? ( ? true false -- true/false )
    #! 'if' and '?' can be defined in terms of each other
    #! because the JIT special-cases an 'if' preceeded by
    #! two literal quotations.
    rot [ drop ] [ nip ] if ; inline

: if ( ..a ? true: ( ..a -- ..b ) false: ( ..a -- ..b ) -- ..b ) ? call ;

! Single branch
: unless ( ..a ? false: ( ..a -- ..a ) -- ..a )
    swap [ drop ] [ call ] if ; inline

: when ( ..a ? true: ( ..a -- ..a ) -- ..a )
    swap [ call ] [ drop ] if ; inline

! Anaphoric
: if* ( ..a ? true: ( ..a ? -- ..b ) false: ( ..a -- ..b ) -- ..b )
    pick [ drop call ] [ 2nip call ] if ; inline

: when* ( ..a ? true: ( ..a ? -- ..a ) -- ..a )
    over [ call ] [ 2drop ] if ; inline

: unless* ( ..a ? false: ( ..a -- ..a x ) -- ..a x )
    over [ drop ] [ nip call ] if ; inline

! Default
: ?if ( ..a default cond true: ( ..a cond -- ..b ) false: ( ..a default -- ..b ) -- ..b )
    pick [ drop [ drop ] 2dip call ] [ 2nip call ] if ; inline

! Dippers.
! Not declared inline because the compiler special-cases them

: dip ( x quot -- x ) swap [ call ] dip ;

: 2dip ( x y quot -- x y ) swap [ dip ] dip ;

: 3dip ( x y z quot -- x y z ) swap [ 2dip ] dip ;

: 4dip ( w x y z quot -- w x y z ) swap [ 3dip ] dip ; inline

! Keepers
: keep ( ..a x quot: ( ..a x -- ..b ) -- ..b x )
    over [ call ] dip ; inline

: 2keep ( ..a x y quot: ( ..a x y -- ..b ) -- ..b x y )
    [ 2dup ] dip 2dip ; inline

: 3keep ( ..a x y z quot: ( ..a x y z -- ..b ) -- ..b x y z )
    [ 3dup ] dip 3dip ; inline

! Cleavers
: bi ( x p q -- )
    [ keep ] dip call ; inline

: tri ( x p q r -- )
    [ [ keep ] dip keep ] dip call ; inline

! Double cleavers
: 2bi ( x y p q -- )
    [ 2keep ] dip call ; inline

: 2tri ( x y p q r -- )
    [ [ 2keep ] dip 2keep ] dip call ; inline

! Triple cleavers
: 3bi ( x y z p q -- )
    [ 3keep ] dip call ; inline

: 3tri ( x y z p q r -- )
    [ [ 3keep ] dip 3keep ] dip call ; inline

! Spreaders
: bi* ( x y p q -- )
    [ dip ] dip call ; inline

: tri* ( x y z p q r -- )
    [ [ 2dip ] dip dip ] dip call ; inline

! Double spreaders
: 2bi* ( w x y z p q -- )
    [ 2dip ] dip call ; inline

: 2tri* ( u v w x y z p q r -- )
    [ 4dip ] 2dip 2bi* ; inline

! Appliers
: bi@ ( x y quot -- )
    dup bi* ; inline

: tri@ ( x y z quot -- )
    dup dup tri* ; inline

! Double appliers
: 2bi@ ( w x y z quot -- )
    dup 2bi* ; inline

: 2tri@ ( u v w x y z quot -- )
    dup dup 2tri* ; inline

! Quotation building
: 2curry ( obj1 obj2 quot -- curry )
    curry curry ; inline

: 3curry ( obj1 obj2 obj3 quot -- curry )
    curry curry curry ; inline

: with ( param obj quot -- obj curry )
    swapd [ swapd call ] 2curry ; inline

: prepose ( quot1 quot2 -- compose )
    swap compose ; inline

! Curried cleavers
<PRIVATE

: [curry] ( quot -- quot' ) [ curry ] curry ; inline

PRIVATE>

: bi-curry ( x p q -- p' q' ) [ [curry] ] bi@ bi ; inline

: tri-curry ( x p q r -- p' q' r' ) [ [curry] ] tri@ tri ; inline

: bi-curry* ( x y p q -- p' q' ) [ [curry] ] bi@ bi* ; inline

: tri-curry* ( x y z p q r -- p' q' r' ) [ [curry] ] tri@ tri* ; inline

: bi-curry@ ( x y q -- p' q' ) [curry] bi@ ; inline

: tri-curry@ ( x y z q -- p' q' r' ) [curry] tri@ ; inline

! Booleans
UNION: boolean POSTPONE: t POSTPONE: f ;

: >boolean ( obj -- ? ) [ t ] [ f ] if ; inline

: not ( obj -- ? ) [ f ] [ t ] if ; inline

: and ( obj1 obj2 -- ? ) over ? ; inline

: or ( obj1 obj2 -- ? ) dupd ? ; inline

: xor ( obj1 obj2 -- ? ) [ f swap ? ] when* ; inline

: both? ( x y quot -- ? ) bi@ and ; inline

: either? ( x y quot -- ? ) bi@ or ; inline

: most ( x y quot -- z ) 2keep ? ; inline

! Loops
: loop ( ... pred: ( ... -- ... ? ) -- ... )
    [ call ] keep [ loop ] curry when ; inline recursive

: do ( pred body -- pred body )
    dup 2dip ; inline

: while ( ..a pred: ( ..a -- ..b ? ) body: ( ..b -- ..a ) -- ..b )
    swap do compose [ loop ] curry when ; inline

: until ( ..a pred: ( ..a -- ..b ? ) body: ( ..b -- ..a ) -- ..b )
    [ [ not ] compose ] dip while ; inline

! Object protocol
GENERIC: hashcode* ( depth obj -- code )

M: object hashcode* 2drop 0 ; inline

M: f hashcode* 2drop 31337 ; inline

: hashcode ( obj -- code ) 3 swap hashcode* ; inline

: identity-hashcode ( obj -- code )
    dup tag 0 eq? [
        dup tag 1 eq? [ drop 0 ] [
            dup (identity-hashcode) dup 0 eq? [
                drop dup compute-identity-hashcode
                (identity-hashcode)
            ] [ nip ] if
        ] if
    ] unless ; inline

GENERIC: equal? ( obj1 obj2 -- ? )

M: object equal? 2drop f ; inline

TUPLE: identity-tuple ;

M: identity-tuple equal? 2drop f ; inline

M: identity-tuple hashcode* nip identity-hashcode ; inline

: = ( obj1 obj2 -- ? )
    2dup eq? [ 2drop t ] [
        2dup both-fixnums? [ 2drop f ] [ equal? ] if
    ] if ; inline

GENERIC: clone ( obj -- cloned )

M: object clone ; inline

M: callstack clone (clone) ; inline

! Tuple construction
GENERIC: new ( class -- tuple )

GENERIC: boa ( slots... class -- tuple )

! Error handling -- defined early so that other files can
! throw errors before continuations are loaded
GENERIC: throw ( error -- * )

ERROR: assert got expect ;

: assert= ( a b -- ) 2dup = [ 2drop ] [ assert ] if ;

<PRIVATE

: declare ( spec -- ) drop ;

: do-primitive ( number -- ) "Improper primitive call" throw ;

PRIVATE>

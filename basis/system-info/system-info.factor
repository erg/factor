! Copyright (C) 2008 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: combinators io kernel math math.parser system
vocabs.loader ;
IN: system-info

HOOK: os-version os ( -- version )    
HOOK: cpus os ( -- n )
HOOK: cpu-mhz os ( -- n )
HOOK: memory-load os ( -- n )
HOOK: physical-mem os ( -- n )
HOOK: available-mem os ( -- n )
HOOK: total-page-file os ( -- n )
HOOK: available-page-file os ( -- n )
HOOK: total-virtual-mem os ( -- n )
HOOK: available-virtual-mem os ( -- n )
HOOK: available-virtual-extended-mem os ( -- n )

: write-unit ( x n str -- )
    [ 2^ /f number>string write bl ] [ write ] bi* ;

: kb ( x -- ) 10 "kB" write-unit ;
: megs ( x -- ) 20 "MB" write-unit ;
: gigs ( x -- ) 30 "GB" write-unit ;
: ghz ( x -- ) 1000000000 /f number>string write bl "GHz" write ;

: system-report. ( -- )
    "CPUs: " write cpus number>string write nl
    "CPU Speed: " write cpu-mhz ghz nl
    "Physical RAM: " write physical-mem megs nl ;

: win7? ( -- ? )
    os windows?
    os-version { 6 1 } = and ;
    
: winxp? ( -- ? )
    os windows?
    os-version { 5 1 } = and ;
        
<< {
    { [ os windows? ] [ "system-info.windows" ] }
    { [ os linux? ] [ "system-info.linux" ] }
    { [ os macosx? ] [ "system-info.macosx" ] }
} cond [ require ] when* >>

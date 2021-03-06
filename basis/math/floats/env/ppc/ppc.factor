USING: accessors alien alien.c-types alien.syntax arrays assocs
biassocs classes.struct combinators kernel literals math
math.bitwise math.floats.env math.floats.env.private system
cpu.ppc.assembler ;
IN: math.floats.env.ppc

STRUCT: ppc-fpu-env
    { padding uint }
    { fpscr uint } ;

STRUCT: ppc-vmx-env
    { vscr uint } ;

: get_ppc_fpu_env ( env -- )
    void { void* } cdecl [
        0 MFFS
        0 3 0 STFD
    ] alien-assembly ;

: set_ppc_fpu_env ( env -- )
    void { void* } cdecl [
        0 3 0 LFD
        HEX: ff 0 0 0 MTFSF
    ] alien-assembly ;

: get_ppc_vmx_env ( env -- )
    void { void* } cdecl [
        0 MFVSCR
        4 1 16 SUBI
        5 HEX: f LI
        4 4 5 ANDC
        0 0 4 STVXL
        5 HEX: c LI
        6 5 4 LWZX
        6 3 0 STW
    ] alien-assembly ;

: set_ppc_vmx_env ( env -- )
    void { void* } cdecl [
        3 1 16 SUBI
        5 HEX: f LI
        4 4 5 ANDC
        5 HEX: c LI
        6 3 0 LWZ
        6 5 4 STWX
        0 0 4 LVXL
        0 MTVSCR
    ] alien-assembly ;

: <ppc-fpu-env> ( -- ppc-fpu-env )
    ppc-fpu-env (struct)
    [ get_ppc_fpu_env ] keep ;

: <ppc-vmx-env> ( -- ppc-fpu-env )
    ppc-vmx-env (struct)
    [ get_ppc_vmx_env ] keep ;

M: ppc-fpu-env (set-fp-env-register)
    set_ppc_fpu_env ;

M: ppc-vmx-env (set-fp-env-register)
    set_ppc_vmx_env ;

M: ppc (fp-env-registers)
    <ppc-fpu-env> 1array ;

CONSTANT: ppc-exception-flag-bits HEX: fff8,0700
CONSTANT: ppc-exception-flag>bit
    H{
        { +fp-invalid-operation+ HEX: 2000,0000 }
        { +fp-overflow+          HEX: 1000,0000 }
        { +fp-underflow+         HEX: 0800,0000 }
        { +fp-zero-divide+       HEX: 0400,0000 }
        { +fp-inexact+           HEX: 0200,0000 }
    }

CONSTANT: ppc-fp-traps-bits HEX: f8
CONSTANT: ppc-fp-traps>bit
    H{
        { +fp-invalid-operation+ HEX: 80 }
        { +fp-overflow+          HEX: 40 }
        { +fp-underflow+         HEX: 20 }
        { +fp-zero-divide+       HEX: 10 }
        { +fp-inexact+           HEX: 08 }
    }

CONSTANT: ppc-rounding-mode-bits HEX: 3
CONSTANT: ppc-rounding-mode>bit
    $[ H{
        { +round-nearest+ HEX: 0 }
        { +round-zero+    HEX: 1 }
        { +round-up+      HEX: 2 }
        { +round-down+    HEX: 3 }
    } >biassoc ]

CONSTANT: ppc-denormal-mode-bits HEX: 4

M: ppc-fpu-env (get-exception-flags) ( register -- exceptions )
    fpscr>> ppc-exception-flag>bit mask> ; inline
M: ppc-fpu-env (set-exception-flags) ( register exceptions -- register' )
    [ ppc-exception-flag>bit >mask ppc-exception-flag-bits remask ] curry change-fpscr ; inline

M: ppc-fpu-env (get-fp-traps) ( register -- exceptions )
    fpscr>> ppc-fp-traps>bit mask> ; inline
M: ppc-fpu-env (set-fp-traps) ( register exceptions -- register' )
    [ ppc-fp-traps>bit >mask ppc-fp-traps-bits remask ] curry change-fpscr ; inline

M: ppc-fpu-env (get-rounding-mode) ( register -- mode )
    fpscr>> ppc-rounding-mode-bits mask ppc-rounding-mode>bit value-at ; inline
M: ppc-fpu-env (set-rounding-mode) ( register mode -- register' )
    [ ppc-rounding-mode>bit at ppc-rounding-mode-bits remask ] curry change-fpscr ; inline

M: ppc-fpu-env (get-denormal-mode) ( register -- mode )
    fpscr>> ppc-denormal-mode-bits mask zero? +denormal-keep+ +denormal-flush+ ? ; inline
M: ppc-fpu-env (set-denormal-mode) ( register mode -- register' )
    [
        {
            { +denormal-keep+  [ ppc-denormal-mode-bits unmask ] }
            { +denormal-flush+ [ ppc-denormal-mode-bits bitor  ] }
        } case
    ] curry change-fpscr ; inline

CONSTANT: vmx-denormal-mode-bits HEX: 10000

M: ppc-vmx-env (get-exception-flags) ( register -- exceptions )
    drop { } ; inline
M: ppc-vmx-env (set-exception-flags) ( register exceptions -- register' )
    drop ;

M: ppc-vmx-env (get-fp-traps) ( register -- exceptions )
    drop { } ; inline
M: ppc-vmx-env (set-fp-traps) ( register exceptions -- register' )
    drop ;

M: ppc-vmx-env (get-rounding-mode) ( register -- mode )
    drop +round-nearest+ ;
M: ppc-vmx-env (set-rounding-mode) ( register mode -- register' )
    drop ;

M: ppc-vmx-env (get-denormal-mode) ( register -- mode )
    vscr>> vmx-denormal-mode-bits mask zero? +denormal-keep+ +denormal-flush+ ? ; inline
M: ppc-vmx-env (set-denormal-mode) ( register mode -- register )
    [
        {
            { +denormal-keep+  [ vmx-denormal-mode-bits unmask ] }
            { +denormal-flush+ [ vmx-denormal-mode-bits bitor  ] }
        } case
    ] curry change-vscr ; inline


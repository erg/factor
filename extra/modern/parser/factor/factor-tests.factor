! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: ascii io.encodings.utf8 io.files io.streams.document
kernel modern.parser modern.parser.factor multiline sequences
sets tools.test vocabs.hierarchy vocabs.loader ;
IN: modern.parser.factor.tests

{
    {
        T{ marray
            { texts
                V{
                    T{ document-object
                        { object "{" }
                        { start T{ document-position { column 0 } } }
                        { finish T{ document-position { column 1 } } }
                    }
                    T{ document-object
                        { object "1" }
                        { start T{ document-position { column 2 } } }
                        { finish T{ document-position { column 3 } } }
                    }
                    T{ document-object
                        { object "2" }
                        { start T{ document-position { column 4 } } }
                        { finish T{ document-position { column 5 } } }
                    }
                    T{ document-object
                        { object "3" }
                        { start T{ document-position { column 6 } } }
                        { finish T{ document-position { column 7 } } }
                    }
                    T{ document-object
                        { object "}" }
                        { start T{ document-position { column 8 } } }
                        { finish T{ document-position { column 9 } } }
                    }
                }
            }
            { elements
                {
                    T{ mnumber { n "1" } }
                    T{ mnumber { n "2" } }
                    T{ mnumber { n "3" } }
                }
            }
        }
    }
} [ "{ 1 2 3 }" parse-modern-string ] unit-test


: check-parsed-file ( path -- ? )
    [ utf8 file-contents [ blank? ] trim-tail ]
    [ parse-modern-file second write-parsed-string ] bi sequence= ;

: replace-parsed-file ( path -- )
    [ parse-modern-file second ] keep
    write-modern-file ;

{ t } [ "resource:core/alien/alien.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/alien/strings/strings.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/arrays/arrays.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/assocs/assocs.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/byte-arrays/byte-arrays.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/combinators/combinators.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/compiler/units/units.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/kernel/kernel.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/layouts/layouts.factor" check-parsed-file ] unit-test

{ t } [ "resource:core/lexer/lexer.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/make/make.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/math/math.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/parser/parser.factor" check-parsed-file ] unit-test
{ t } [ "resource:core/parser/notes/notes.factor" check-parsed-file ] unit-test

{ t } [ "resource:core/sequences/sequences.factor" check-parsed-file ] unit-test


{ t } [
    "resource:core" disk-vocabs-in-root
    ! [ vocab? ] filter
    [ vocab-source-path ] map sift
    {
        "resource:core/vocabs/loader/test/a/a.factor"
        "resource:core/vocabs/loader/test/b/b.factor"
        "resource:core/vocabs/loader/test/c/c.factor"
    } diff
    ! [ parse-modern-file ] map
    [ check-parsed-file ] all?
] unit-test

! Test multiline string experimental syntax
CONSTANT: tools-scaffold-string """m"[${example-indent}"Example:"
${example-indent}{ $example "USING: ${example-using} ;"
${example-indent}    ""
${example-indent}    ""
${example-indent}}]""""

{ t } [ tools-scaffold-string parse-modern-string length 1 = ] unit-test
{ t } [ tools-scaffold-string parse-modern-string ?last mstring? ] unit-test


: check-parser-exact ( string -- ? )
    [ parse-modern-string write-parsed-string ] keep = ;

: check-parsed-exact ( string -- ? )
    parse-modern-string [ write-parsed-string parse-modern-string ] keep = ;

/*
{ } [
    "resource:basis" vocabs-in-root
    [ vocab? ] filter
    [ vocab-source-path ] map sift
    {

    } diff
    [ dup . flush parse-modern-file ] map
] unit-test


! check if files are exact on disk
    "resource:core" disk-vocabs-in-root
    [ modern-source-path ] map sift
    [ "loader/test" swap subseq? ] reject
    [ replace-parsed-file ] each


    "resource:basis" disk-vocabs-in-root
    [ modern-source-path ] map sift
    {
    } diff [ dup  check-parsed-file ] { } map>assoc
    [ second not ] filter keys [ . ] each



    "resource:extra" disk-vocabs-in-root
    ! [ vocab? ] filter
    [ vocab-source-path ] map sift
    {
"resource:extra/irc/messages/messages.factor"
"resource:extra/yaml/conversion/conversion.factor"

    } diff [ dup flush check-parsed-file ] { } map>assoc
    [ second not ] filter keys [ . ] each
"resource:extra/asn1/asn1.factor"
"resource:extra/backtrack/backtrack.factor"
"resource:extra/balloon-bomber/balloon-bomber.factor"
"resource:extra/boids/boids.factor"
"resource:basis/calendar/calendar.factor"
"resource:extra/codebook/codebook.factor"
"resource:extra/constructors/constructors.factor"
"resource:extra/coroutines/coroutines.factor"
"resource:extra/couchdb/couchdb.factor"
"resource:extra/cursors/cursors.factor"
"resource:extra/decimals/decimals.factor"
"resource:extra/dns/dns.factor"
"resource:extra/dwarf/dwarf.factor"
"resource:extra/ecdsa/ecdsa.factor"
"resource:extra/fastcgi/fastcgi.factor"
"resource:extra/fluids/fluids.factor"
"resource:extra/fullscreen/fullscreen.factor"
"resource:extra/geobytes/geobytes.factor"
"resource:extra/hashcash/hashcash.factor"
"resource:core/hashtables/hashtables.factor"
"resource:basis/help/help.factor"
"resource:extra/id3/id3.factor"
"resource:extra/jamshred/jamshred.factor"
"resource:extra/jvm-summit-talk/jvm-summit-talk.factor"
"resource:extra/libudev/libudev.factor"
"resource:extra/libusb/libusb.factor"
"resource:extra/log-viewer/log-viewer.factor"
"resource:extra/lua/lua.factor"
"resource:extra/lunar-rescue/lunar-rescue.factor"
"resource:extra/macho/macho.factor"
"resource:extra/minneapolis-talk/minneapolis-talk.factor"
"resource:extra/model-viewer/model-viewer.factor"
"resource:extra/morse/morse.factor"
"resource:extra/native-thread-test/native-thread-test.factor"
"resource:extra/nested-comments/nested-comments.factor"
"resource:extra/noise/noise.factor"
"resource:extra/ogg/ogg.factor"
"resource:extra/openal/openal.factor"
"resource:extra/opencl/opencl.factor"
"resource:extra/pair-methods/pair-methods.factor"
"resource:extra/persistency/persistency.factor"
"resource:extra/pong/pong.factor"
"resource:extra/quadtrees/quadtrees.factor"
"resource:extra/robots/robots.factor"
"resource:extra/s3/s3.factor"
"resource:extra/site-watcher/site-watcher.factor"
"resource:core/slots/slots.factor"
"resource:extra/space-invaders/space-invaders.factor"
"resource:extra/spheres/spheres.factor"
"resource:extra/svg/svg.factor"
"resource:extra/tc-lisp-talk/tc-lisp-talk.factor"
"resource:extra/terrain/terrain.factor"
"resource:extra/tetris/tetris.factor"
"resource:extra/trails/trails.factor"
"resource:extra/trees/trees.factor"
"resource:extra/twitter/twitter.factor"
"resource:extra/variables/variables.factor"
"resource:extra/wordtimer/wordtimer.factor"
"resource:extra/zoneinfo/zoneinfo.factor"
"resource:extra/alien/fortran/fortran.factor"
"resource:extra/alien/data/map/map.factor"
"resource:extra/asn1/ldap/ldap.factor"
"resource:extra/audio/aiff/aiff.factor"
"resource:extra/audio/engine/engine.factor"
"resource:extra/benchmark/dispatch5/dispatch5.factor"
"resource:extra/benchmark/fib6/fib6.factor"
"resource:extra/benchmark/nbody/nbody.factor"
"resource:extra/benchmark/nbody-simd/nbody-simd.factor"
"resource:extra/benchmark/nsieve-bits/nsieve-bits.factor"
"resource:extra/benchmark/ring/ring.factor"
"resource:extra/benchmark/spectral-norm-simd/spectral-norm-sim..."
"resource:extra/benchmark/mandel/params/params.factor"
"resource:extra/bitcoin/client/client.factor"
"resource:extra/bson/reader/reader.factor"
"resource:extra/bson/writer/writer.factor"
"resource:extra/bunny/outlined/outlined.factor"
"resource:extra/c/preprocessor/preprocessor.factor"
"resource:extra/calendar/holidays/holidays.factor"
"resource:extra/calendar/holidays/us/us.factor"
"resource:extra/chipmunk/demo/demo.factor"
"resource:extra/combinators/tuple/tuple.factor"
"resource:extra/compiler/graphviz/graphviz.factor"
"resource:extra/compiler/cfg/gvn/expressions/expressions.factor"
"resource:extra/compiler/cfg/gvn/simd/simd.factor"
"resource:extra/cpu/8080/8080.factor"
"resource:extra/cpu/8080/emulator/emulator.factor"
"resource:extra/cpu/8080/test/test.factor"
"resource:extra/crypto/aes/aes.factor"
"resource:extra/crypto/passwd-md5/passwd-md5.factor"
"resource:extra/crypto/rsa/rsa.factor"
"resource:extra/ctags/etags/etags.factor"
"resource:extra/cuda/ffi/ffi.factor"
"resource:extra/cuda/gl/gl.factor"
"resource:extra/cuda/libraries/libraries.factor"
"resource:extra/cuda/ptx/ptx.factor"
"resource:extra/cuda/types/types.factor"
"resource:extra/elf/nm/nm.factor"
"resource:extra/euler/b-rep/b-rep.factor"
"resource:extra/euler/modeling/modeling.factor"
"resource:extra/euler/operators/operators.factor"
"resource:extra/euler/b-rep/subdivision/subdivision.factor"
"resource:extra/euler/b-rep/io/obj/obj.factor"
"resource:extra/game/debug/debug.factor"
"resource:extra/game/debug/tests/tests.factor"
"resource:extra/game/input/demos/key-caps/key-caps.factor"
"resource:extra/game/models/half-edge/half-edge.factor"
"resource:extra/gml/modeling/modeling.factor"
"resource:extra/gml/runtime/runtime.factor"
"resource:extra/gml/ui/ui.factor"
"resource:extra/gml/viewer/viewer.factor"
"resource:extra/gpu/buffers/buffers.factor"
"resource:extra/gpu/framebuffers/framebuffers.factor"
"resource:extra/gpu/render/render.factor"
"resource:extra/gpu/shaders/shaders.factor"
"resource:extra/gpu/state/state.factor"
"resource:extra/gpu/textures/textures.factor"
"resource:extra/gpu/util/util.factor"
"resource:extra/gpu/demos/bunny/bunny.factor"
"resource:extra/gpu/demos/raytrace/raytrace.factor"
"resource:extra/gpu/effects/blur/blur.factor"
"resource:extra/gpu/util/wasd/wasd.factor"
"resource:extra/gtk-samples/hello-world/hello-world.factor"
"resource:extra/gtk-samples/opengl/opengl.factor"
"resource:extra/images/atlas/atlas.factor"
"resource:extra/images/gif/gif.factor"
"resource:extra/images/viewer/viewer.factor"
"resource:extra/io/serial/serial.factor"
"resource:extra/io/encodings/detect/detect.factor"
"resource:extra/jamshred/game/game.factor"
"resource:extra/jamshred/player/player.factor"
"resource:extra/llvm/core/core.factor"
"resource:extra/llvm/types/types.factor"
"resource:extra/math/affine-transforms/affine-transforms.factor"
"resource:extra/math/binpack/binpack.factor"
"resource:extra/math/derivatives/derivatives.factor"
"resource:extra/math/dual/dual.factor"
"resource:extra/math/numerical-integration/numerical-integrati..."
"resource:extra/math/splines/splines.factor"
"resource:basis/math/vectors/vectors.factor"
"resource:extra/math/blas/ffi/ffi.factor"
"resource:extra/math/blas/matrices/matrices.factor"
"resource:extra/math/blas/vectors/vectors.factor"
"resource:extra/math/derivatives/syntax/syntax.factor"
"resource:extra/math/matrices/simd/simd.factor"
"resource:extra/math/splines/viewer/viewer.factor"
"resource:extra/math/vectors/homogeneous/homogeneous.factor"
"resource:extra/models/conditional/conditional.factor"
"resource:extra/models/history/history.factor"
"resource:extra/modern/loader/loader.factor"
"resource:extra/mongodb/benchmark/benchmark.factor"
"resource:extra/mongodb/cmd/cmd.factor"
"resource:extra/mongodb/connection/connection.factor"
"resource:extra/mongodb/driver/driver.factor"
"resource:extra/mongodb/msg/msg.factor"
"resource:extra/mongodb/operations/operations.factor"
"resource:extra/mongodb/tuple/tuple.factor"
"resource:extra/mongodb/tuple/collection/collection.factor"
"resource:extra/mongodb/tuple/persistent/persistent.factor"
"resource:extra/nehe/5/5.factor"
"resource:extra/ogg/theora/theora.factor"
"resource:extra/ogg/vorbis/vorbis.factor"
"resource:extra/openal/example/example.factor"
"resource:extra/opencl/ffi/ffi.factor"
"resource:extra/opengl/demo-support/demo-support.factor"
"resource:extra/opengl/glu/glu.factor"
"resource:extra/parser-combinators/simple/simple.factor"
"resource:extra/peg/expr/expr.factor"
"resource:extra/peg/javascript/parser/parser.factor"
"resource:extra/peg/javascript/tokenizer/tokenizer.factor"
"resource:extra/pop3/server/server.factor"
"resource:extra/project-euler/051/051.factor"
"resource:extra/project-euler/062/062.factor"
"resource:extra/project-euler/081/081.factor"
"resource:extra/project-euler/102/102.factor"
"resource:extra/random/lagged-fibonacci/lagged-fibonacci.factor"
"resource:extra/reports/noise/noise.factor"
"resource:extra/rosetta-code/animate-pendulum/animate-pendulum..."
"resource:extra/rosetta-code/animation/animation.factor"
"resource:extra/rosetta-code/bitmap/bitmap.factor"
"resource:extra/rosetta-code/bitmap-bezier/bitmap-bezier.factor"
"resource:extra/rosetta-code/bitmap-line/bitmap-line.factor"
"resource:extra/rosetta-code/bulls-and-cows/bulls-and-cows.factor"
"resource:extra/rosetta-code/gray-code/gray-code.factor"
"resource:extra/rosetta-code/image-noise/image-noise.factor"
"resource:extra/rosetta-code/knapsack/knapsack.factor"
"resource:extra/rosetta-code/opengl/opengl.factor"
"resource:extra/rosetta-code/pythagorean-triples/pythagorean-t..."
"resource:extra/rosetta-code/top-rank/top-rank.factor"
"resource:extra/rosetta-code/tree-traversal/tree-traversal.factor"
"resource:extra/sequences/n-based/n-based.factor"
"resource:extra/site-watcher/db/db.factor"
"resource:extra/site-watcher/email/email.factor"
"resource:extra/slots/macros/macros.factor"
"resource:extra/slots/syntax/syntax.factor"
"resource:extra/smalltalk/compiler/compiler.factor"
"resource:extra/smalltalk/parser/parser.factor"
"resource:extra/taxes/usa/federal/federal.factor"
"resource:extra/terrain/generation/generation.factor"
"resource:extra/terrain/shaders/shaders.factor"
"resource:extra/tetris/board/board.factor"
"resource:extra/tetris/game/game.factor"
"resource:extra/tetris/tetromino/tetromino.factor"
"resource:extra/time/windows/windows.factor"
"resource:extra/tools/dns/public/public.factor"
"resource:extra/trees/avl/avl.factor"
"resource:extra/trees/splay/splay.factor"
"resource:extra/twitter/prettyprint/prettyprint.factor"
"resource:basis/ui/gadgets/worlds/worlds.factor"
"resource:extra/ui/render/test/test.factor"
"resource:extra/units/constants/constants.factor"
"resource:extra/units/imperial/imperial.factor"
"resource:extra/update/backup/backup.factor"
"resource:extra/vocabs/git/git.factor"
"resource:extra/webapps/fjsc/fjsc.factor"
"resource:extra/webapps/imagebin/imagebin.factor"
"resource:extra/webapps/irc-log/irc-log.factor"
"resource:extra/webapps/todo/todo.factor"
"resource:extra/webapps/wee-url/wee-url.factor"
"resource:extra/webapps/wiki/wiki.factor"
"resource:extra/yaml/config/config.factor"
*/

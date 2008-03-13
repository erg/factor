IN: tools.deploy.tests
USING: tools.test system io.files kernel tools.deploy.config
tools.deploy.backend math sequences ;

: shake-and-bake
    "." resource-path [
        vm
        "test.image" temp-file
        rot dup deploy-config make-deploy-image
    ] with-directory ;

[ ] [ "hello-world" shake-and-bake ] unit-test

[ t ] [
    "hello.image" temp-file file-length 500000 <=
] unit-test

[ ] [ "sudoku" shake-and-bake ] unit-test

[ t ] [
    "hello.image" temp-file file-length 1500000 <=
] unit-test

[ ] [ "hello-ui" shake-and-bake ] unit-test

[ t ] [
    "hello.image" temp-file file-length 2000000 <=
] unit-test

[ ] [ "bunny" shake-and-bake ] unit-test

[ t ] [
    "hello.image" temp-file file-length 3000000 <=
] unit-test

[ ] [
    "tools.deploy.test.1" shake-and-bake
    vm "-i=" "test.image" temp-file append try-process
] unit-test

[ ] [
    "tools.deploy.test.2" shake-and-bake
    vm "-i=" "test.image" temp-file append try-process
] unit-test

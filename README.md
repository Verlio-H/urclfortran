# urcl fortran
A work in progress modern fortran compiler written entirely in modern fortran.

Currently this compiler targets 16 bit URCL, but more backends will likely be added in the future.

# Installing
This compiler is intended to be installed using fpm (https://fpm.fortran-lang.org/install/index.html#install). From the root of the project, run `fpm install` to install the compiler. The compiler can then be executed by running `urclfortran`.

The compile.sh shell script is provided for developers and those who prefer a manual installation. This script is intended primarily for debugging purposes (to enable testing with ifort).

# Support
Currently, this compiler supports a very very limited set of functionality. The code contained in input.f90 is effectively the extent of support.
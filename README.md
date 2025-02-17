# urcl fortran
A work in progress modern fortran compiler written entirely in modern fortran.

Currently this compiler targets 16 bit URCL, but more backends will likely be added in the future.

# Installing
This compiler is intended to be installed using fpm (https://fpm.fortran-lang.org/install/index.html#install). From the root of the project, run `fpm install` to install the compiler. The compiler can then be executed by running `urclfortran`.

# Support
Currently, this compiler supports a very very limited set of functionality. The code contained in input.f90 is effectively the extent of support.
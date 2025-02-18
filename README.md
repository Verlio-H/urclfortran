# urcl fortran
A work in progress modern fortran compiler written entirely in modern fortran.

Currently this compiler targets 16 bit URCL, but more backends will likely be added in the future.

# Installing
This compiler is intended to be installed using fpm (https://fpm.fortran-lang.org/install/index.html#install). From the root of the project, run `fpm install --profile release` to install the compiler. The compiler can then be executed by running `urclfortran`.

The compile.sh shell script is provided for developers and those who prefer a manual installation. This script is intended primarily for debugging purposes (to enable testing with ifort).

# Usage
urclfortran follows a rather typical syntax for command line arguments. Use `-o (filename)` to specify output file name. Use `-c` to disable linking.

urclfortran relies on urcl-ld for linking urcl fles (https://github.com/Verlio-H/urcl-ld.git). The environment variable `LIBFORT_PATH` must be set to the global path of the libfortran directory for linking to work.

urclfortran currently performs no preprocessing. In other words, the file extension is ignored and .F90 is equivalent to .f90.

# Support
Currently, this compiler supports a very very limited set of functionality. Don't expect most fortran code to be functional.
# urclfortran
A limited fortran 77 compiler for URCL, for more information regarding URCL, see the urcl discord (https://discord.gg/Nv8jzWg5j8) or the urcl documentation (https://github.com/ModPunchtree/URCL).

# Installing and running
This program requires gfortran to be installed and expects a bash (or similar) terminal, if you are on Linux or MacOS you should have little to no issues running this software, on Windows something like WSL or Cygwin will most likely be needed. In order to run the compiler do  
`sh compile.sh (sourcefile).f -o (outputfile).urcl`  
If you recieve an error relating to sed on MacOS, install gnu-sed (using brew), add it to your path, then try again.

# Supported Syntax
Currently the compiler supports integer and character variables, assignments involving those two, and goto statements.

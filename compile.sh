case $2 in
    "ifort") COMPILER="ifort";;
    *) COMPILER="gfortran";;
esac
case $1 in
    "debug") 
        case $2 in
            "ifort") ARGS="-O0 -g -gen-interface -warn all -traceback -check all -check bounds -check uninit -check -ftrapuv -debug all";;
            *) ARGS="-std=f2018 -O0 -Wimplicit-interface -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-99999999";;
        esac
    ;;
    "release")
        ARGS="-Ofast"
    ;;
    *)
        echo "Expected build mod"
        exit -1
    ;;
esac

mkdir -p bin

echo "Compiling include.f90"
if ! $COMPILER src/include.f90 -c -o bin/include.o $ARGS; then
    exit -1
fi

echo "Compiling lexer.f90"
if ! $COMPILER src/lexer.f90 -c -o bin/lexer.o $ARGS; then
    exit -1
fi

echo "Compiling astgen.f90"
if ! $COMPILER src/astgen.f90 -c -o bin/astgen.o $ARGS; then
    exit -1
fi

echo "Compiling parsing.f90"
if ! $COMPILER src/parsing.f90 -c -o bin/parsing.o $ARGS; then
    exit -1
fi

echo "Compiling consts.f90"

if ! $COMPILER src/consts.f90 -c -o bin/consts.o $ARGS; then
    exit -1
fi

echo "Compiling semantic.f90"
if ! $COMPILER src/semantic.f90 -c -o bin/semantic.o $ARGS; then
    exit -1
fi

echo "Compiling compile.f90"
if ! $COMPILER src/compile.f90 -c -o bin/compile.o $ARGS; then
    exit -1
fi

echo "Compiling main.f90"
if ! $COMPILER src/main.f90 -c -o bin/main.o $ARGS; then
    exit -1
fi

echo "Linking"
$COMPILER bin/*.o -o compiler

#gfortran src/*.f90 -o compiler -std=f2018 -O0 -Wimplicit-interface -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-99999999
#ifort src/*.f90 -o compiler -O0 -g -gen-interface -warn all -traceback -check all -check bounds -check uninit -check -ftrapuv -debug all


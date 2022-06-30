#!/bin/bash
getopts o: flag
case "${flag}" in
    o) output=${OPTARG};;
    ?) output="OUTPUT.URCL";;
esac

if [[ $1 == "-o" ]]
then
    input=$3
else
    input=$1
fi

if [[ $input = "" ]]
then
    input='SOURCE.F'
fi

mv $input SOURCE.F
gfortran -o urclcompile compiler.f
./urclcompile
for i in {1..3}
do
    cat OUTPUT.URCL | tr '\n' '\f' | sed -e 's/PSH \(\w\w*\)\fPOP \(\w\w*\)/MOV \2 \1/g' -e 's/MOV \(\w\w*\) \1\f//' -e 's/PSH \(\w\w*\)\fMOV \(\w\w*\) \(\w\w*\)\fPOP \(\w\w*\)/MOV \2 \3\fMOV \4 \1/' -e 's/MOV \(\w\w*\) \(\w\w*\)\f\(\w\w*\) \(\w\w*\) \1 \(\w\w*\)/\3 \4 \2 \5/' -e 's/MOV \(\w\w*\) \(\w\w*\)\f\(\w\w*\) \(\w\w*\) \(\w\w*\) \1/\3 \4 \5 \2/' -e 's/MOV \(\w\w*\) \(\w\w*\)\f\(\w\w*\) \(\w\w*\) \1/\3 \4 \2/' -e 's/MLT R1 \(\w\w*\) 1/MOV R1 \1/' -e 's/MLT R1 1 \(\w\w*\)/MOV R1 \1/' -e 's/MLT R1 \w* 0/MOV R1 0/' -e 's/MLT R1 0 \w*/MOV R1 0/' -e 's/ADD R1 \(\w\w*\) 0/MOV R1 \1/' -e 's/SUB R1 \(\w\w*\) 0/MOV R1 \1/' -e 's/ADD R1 0 \(\w\w*\)/MOV R1 \1/' | tr '\f' '\n' > TEMP.URCL && mv TEMP.URCL OUTPUT.URCL
done
cat OUTPUT.URCL | tr '\n' '\f' | sed -e 's/LLOD R2 SP \(\w\w*\)/MOV R1 \1\fLLOD R2 SP R1/' -e 's/MOV \(\w\w*\) \1\f//' -e 's/MOV \(\w\w*\) \([0-9][0-9]*\)/IMM \1 \2/' | tr '\f' '\n' > TEMP.URCL && mv TEMP.URCL OUTPUT.URCL
mv OUTPUT.URCL $output
mv SOURCE.F $input

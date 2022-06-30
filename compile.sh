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
    cat OUTPUT.URCL | tr '\n' '\f' | sed -e 's/PSH \(\S\S*\)\fPOP \(\S\S*\)/MOV \2 \1/g' -e 's/MOV \(\S\S*\) \1\f//g' -e 's/PSH \(\S\S*\)\fMOV \(\S\S*\) \(\S\S*\)\fPOP \(\S\S*\)/MOV \2 \3\fMOV \4 \1/g' -e 's/MOV \(\S\S*\) \(\S\S*\)\f\(\S\S*\) \(\S\S*\) \1 \(\S\S*\)/\3 \4 \2 \5/g' -e 's/MOV \(\S\S*\) \(\S\S*\)\f\(\S\S*\) \(\S\S*\) \(\S\S*\) \1/\3 \4 \5 \2/g' -e 's/MOV \(\S\S*\) \(\S\S*\)\f\(\S\S*\) \(\S\S*\) \1/\3 \4 \2/g' -e 's/MLT R1 \(\S\S*\) 1/MOV R1 \1/g' -e 's/MLT R1 1 \(\S\S*\)/MOV R1 \1/g' -e 's/MLT R1 \S* 0/MOV R1 0/g' -e 's/MLT R1 0 \S*/MOV R1 0/g' -e 's/ADD R1 \(\S\S*\) 0/MOV R1 \1/g' -e 's/SUB R1 \(\S\S*\) 0/MOV R1 \1/g' -e 's/ADD R1 0 \(\S\S*\)/MOV R1 \1/g' -e 's/STR \(\S\S*\) \(\S\S*\)\fLOD \(\S\S*\) \1/STR \1 \2\fMOV \3 \2/g' | tr '\f' '\n' > TEMP.URCL && mv TEMP.URCL OUTPUT.URCL
done
cat OUTPUT.URCL | tr '\n' '\f' | sed -e 's/MOV \(\S\S*\) \([0-9][0-9]*\)/IMM \1 \2/g' -e 's/\/\/\f//g' -e 's/ 0/ R0/g'| tr '\f' '\n' > TEMP.URCL && mv TEMP.URCL OUTPUT.URCL
mv OUTPUT.URCL $output
mv SOURCE.F $input

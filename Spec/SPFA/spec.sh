#!/bin/sh

DIR=`dirname $0`
SRC=$DIR/../../Graph/SPFA.ml
TEST=$DIR/SPFA.ml
TEMP=$DIR/../../Template.ml

SPEC=`echo $TEST | sed -e s/".ml"/"Spec.ml"/g`

cat $TEMP $SRC $TEST > $SPEC

ocamlopt $SPEC -o a.out


CASE=0
CORRECT=0

for i in `ls $DIR/testcase/*-in*.txt`
do
    j=`echo $i | sed -e s/"-in"/"-out"/g`
    n=`echo $i | sed -e s/.*-in//g | sed -e s/".txt"//g`
    ./a.out < $i > out.txt
    diff out.txt $j > diff.txt
    if [ -s diff.txt ]; then
        echo "Case #$n: Wrong"
    else
        echo "Case #$n: Correct"
        CORRECT=`expr $CORRECT + 1`
    fi
    CASE=`expr $CASE + 1`
    rm out.txt diff.txt
done

echo "$CORRECT/$CASE cases passed."

rm a.out $DIR/*.cmi $DIR/*.cmx $DIR/*.o $SPEC

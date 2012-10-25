#!/bin/bash

#TMPFILE=`mktemp`
TMPFILE="./test"

function run {
    echo "$1" > $TMPFILE
    echo "Input: $1"
    out=`./prefixer $TMPFILE`
    echo "Output: $out"
    outR=`./prefixer "-r" $TMPFILE`
    echo "Output Reduced: $outR"
    echo ""
}

##### MAIN #####

if [ ! -e "./prefixer" ]; then
    echo "Be sure to run \"make\" to compile prefixer"
    exit 1
fi

run "3 + 4 * 5"
run "3"
run "( 1 + 1 )"
run "1 + 1"
run "2 * 5 + 1"
run "2 * ( 5 + 1 )"
run "3 * x + ( 9 + y ) / 4"
run "( x + y ) / ( x * y )"
run "( x + y ) * ( x - y )"
run "( x + 3 ) * ( x + 3 ) * ( x + 3 )"
run "( ( x + 3 ) * ( x + 3 ) * ( x + 3 ) - 27 ) / x"
run "46 / ( 3 + 4 * 5 )"
run "1 / ( 1 + x )"
run "1 / ( ( 1 + x ) * ( 1 - x ) )"
run "( 1 + x ) / ( 1 + x )"
run "( 1 + x ) * ( x + 1 ) / ( ( 1 + x ) * ( 1 + x )"
run "( 3 + x ) * ( x + 1 ) / ( ( 1 + x ) * ( 1 - x )"
run "( 1 + x ) * ( 1 - x ) / ( ( 3 + x ) * ( x + 1 ) )"
run "0 / 1"
run "1 / 0"
run "5 - 5 + 8 - 4 - 4"
run "1 / ( x * x ) + 2 / ( x * x )"

echo ""
echo "Test cases that either fail terribly when reducing or do not reduce completely"
echo ""
echo "Adding fractions that have more than one term in the denominator that need to be added together after the denominator has been reduced"
run "1 / ( x + 3 ) + 1 / ( x + 5 )"
echo "Fractions that require factoring of polynomials with mutiple terms"
run "( x * x - 1 ) / ( ( x - 1 ) * ( x + 3 ) )"

rm -f $TMPFILE

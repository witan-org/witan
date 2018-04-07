#!/bin/dash

#File without check-sat are not acceptable (popop print sat in this case...)
grep -q "^(check-sat)$" $1 || exit 1

#We want well typed problems
CVC4=$(cvc4 --lang=smt2 $1 2>&1)
(echo $CVC4 | grep -q "CVC4 Error:") && exit 1

if [ "$SEED" != "" ]; then
    SEED="--seed $SEED"
fi

if [ "$STEP" != "" ]; then
    STEP="--step $STEP"
fi

if [ "$INVSTATUS" != "" ]; then
    OK=0
    BAD=1
else
    #for deltasmt
    OK=1
    BAD=0
fi


if test "$CVC4" = "$(_build/default/src/bin/witan.exe $SEED $STEP --input=smt2 $1 2>&1)"; then
  exit $OK
else
  exit $BAD
fi

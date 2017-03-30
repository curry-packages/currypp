#!/bin/sh
# script to test the current set of test examples

CURRYBIN="../../../../bin"

ALLTESTS="BreakWhere BubbleSort DutchFlag FloatString Guards Lookup Rev2 WorldCup"

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

# clean up before
$CURRYBIN/cleancurry

# execute all tests:
# The option --nosource is necessary since currypp introduces calls
# to Prelude.=:<= in the source program when transforming sequential rules
CCOPTS=--nosource
LOGFILE=xxx$$
if [ $VERBOSE = yes ] ; then
  $CURRYBIN/curry check $CCOPTS $ALLTESTS
  if [ $? -gt 0 ] ; then
    exit 1
  fi
else
  $CURRYBIN/curry check $CCOPTS $ALLTESTS > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR in curry check:"
    cat $LOGFILE
    exit 1
  fi
fi

# clean:
/bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry

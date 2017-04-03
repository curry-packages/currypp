#!/bin/sh
# script to test the examples for the SQL preprocessor:

# Root location of the Curry System specified by variable CURRYROOT
CURRYROOT=`curry :set v0 :set -time :add Distribution :eval "putStrLn installDir" :quit`

CURRYBIN=$CURRYROOT/bin

if [ -x "$CURRYBIN/pakcs" ] ; then
    CURRYEXEC=pakcs
elif [ -x "$CURRYBIN/kics2" ] ; then
    CURRYEXEC=kics2
else
    echo "ERROR: Unknown Curry system!"
    exit 1
fi

ERD2CURRY=$HOME/.cpm/bin/erd2curry
if [ ! -x "$ERD2CURRY" ] ; then
  ERD2CURRY=$CURRYBIN/$CURRYEXEC-erd2curry
  if [ ! -x "$ERD2CURRY" ] ; then
    echo "No executable 'erd2curry' found!"
    exit 1
  fi
fi

ALLTESTS="test*.curry"

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

cleandir () {
  $CURRYBIN/cleancurry
  /bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
  /bin/rm -f Uni.erdterm Uni_ERDT.term Uni_SQLCode.info Uni_CDBI.curry Uni.db
  $CURRYBIN/cleancurry
}

# compile and execute all tests:
exectests() {
  cleandir
  # compile model:
  "$ERD2CURRY" --db `pwd`/Uni.db --cdbi UniERD.curry
  # fill database:
  $CURRYBIN/curry $REPL_OPTS :l CreateData :eval createTestData :q
  # run query tests:
  $CURRYBIN/curry check SelectExamples
}

LOGFILE=xxx$$
if [ $VERBOSE = yes ] ; then
  exectests
  if [ $? -gt 0 ] ; then
    exit 1
  fi
else
  exectests > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR during testing occurred:"
    cat $LOGFILE
    exit 1
  fi
fi
cleandir

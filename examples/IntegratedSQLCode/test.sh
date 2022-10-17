#!/bin/sh
# script to test the examples for the SQL preprocessor:

# Compute bin directory of the Curry System:
CURRYBINDIR=$(dirname $(realpath $CURRYBIN))

ERD2CURRY=`which erd2curry`
if [ ! -x "$ERD2CURRY" ] ; then
  echo "SQL integration not tested: no executable 'erd2curry' found!"
  echo "To run the SQL integration test, install 'erd2curry' by:"
  echo "> cypm install ertools"
  exit
fi

ALLTESTS="test*.curry"

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBINDIR:$PATH
export PATH

cleandir () {
  $CURRYBINDIR/cypm clean
  $CURRYBINDIR/cleancurry
  /bin/rm -f $LOGFILE *_PUBLIC.curry TEST*.curry
  /bin/rm -f Uni.erdterm Uni_ERDT.term Uni_SQLCode.info Uni.curry Uni.db
  $CURRYBINDIR/cleancurry
}

# compile and execute all tests:
exectests() {
  echo "Clean test directory..."
  cleandir
  echo "Install dependencies..."
  $CURRYBINDIR/cypm install
  echo "Compile ER model with 'erd2curry'..."
  "$ERD2CURRY" --db `pwd`/Uni.db --cdbi UniERD.curry
  echo "Fill the test database..."
  $CURRYBINDIR/curry $REPL_OPTS :l CreateData :eval main :q
  echo "Run SQL query tests..."
  curry-check SelectExamples
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

This directory contains various examples and tests for
the SQL integration into Curry via currypp.

Before running the following tests, one has to install the
required dependencies via

    > cypm install

Compile the model:

    > erd2curry --cdbi --db `pwd`/Uni.db UniERD.curry

Fill the database:

    > curry :l CreateData :eval main :quit

Show the database with sqlite3 command line interface:

    > sqlite3 Uni.db 
    ...> .dump
    ...> select * from Student;

or the SQLite browser:

    > sqlitebrowser Uni.db

Testing some queries:

    > curry :l SelectExamples
    ...> queryS1


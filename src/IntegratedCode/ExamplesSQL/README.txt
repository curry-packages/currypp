This directory contains various examples and tests for
the SQL integration into Curry via currypp.

Compile the model:

    > erd2curry --cdbi --db `pwd`/Uni.db UniERD.curry

Fill the database:

    > curry :l CreateData :eval createTestData :q

Show the database with sqlite3:

    > sqlite3 Uni.db 
    ...> .dump
    ...> select * from Student;


Testing some queries:

    > curry :l SelectExamples
    ...> queryS1


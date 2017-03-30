This directory contains various examples and test for
the SQL integration into Curry via currypp.

Compile the model:

> erd2cdbi Uni_ERD.term `pwd`/Uni.db

Fill the database:

> curry :l CreateData :eval createTestData :q

Show the database with sqlite3:

> sqlite3 Uni.db 
...> .dump
...> select * from Student;


Testing some queries:

> curry :l SelectExamples
...> queryS1


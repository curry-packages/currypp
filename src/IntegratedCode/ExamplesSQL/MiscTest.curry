{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

--- test module for integration of SQL
--- using currypp

import Database.CDBI.ER 
import Time
import Uni_CDBI

-- Selecting key Values
testKey1 :: IO( SQLResult [(StudentID,String)])
testKey1 = ``sql Select s.Key, s.Name from Student as s where s.Key = 1;''
      
testKey2 :: IO( SQLResult [(StudentID,String)])
testKey2 = ``sql Select s.Key, s.Name from Student as s where s.Key in (1,4,7);''

testKey3 :: IO( SQLResult [(StudentID,String)])
testKey3 = ``sql Select s.Key, s.Name from Student as s where s.Key between 2 and 5;''

testKey4 :: IO( SQLResult [(StudentID,String)])
testKey4 = ``sql Select s.Key, s.Name from Student as s group by s.Name having s.Key != 3;''

testKey5 :: IO( SQLResult [(StudentID,String)])
testKey5 = ``sql Select s.Key, s.Name from Student as s where s.Key not null;''

testKey6 :: IO( SQLResult [(StudentID,String)])
testKey6 = ``sql Select s.Key, s.Name from Student as s where s.Key = {(StudentID 2)};''

testFK :: IO( SQLResult ())
testFK = ``sql Update Result Set StudentTakingKey = 11 Where Key = 9;''

-- Testing case expressions
testCase1 :: IO( SQLResult [(String, String)])
testCase1 = ``sql Select s.Name,
                   Case When s.Age < 20 Then "Young" Else "Old" End 
                   From Student as s;''

testCase2 :: IO( SQLResult [(String, String)])
testCase2 = ``sql Select s.Name,
                   Case When s.Key < 4 Then "First" Else "Second" End 
                   From Student as s;''

-- queries with date-type  
--attention - wrong format -> warning                 
testDate :: IO(SQLResult ())
testDate = ``sql Insert into Time Values (Null, 24:09:2015:17:25:00);''

--wrong format -> warning
testDate1 :: IO(SQLResult ())
testDate1 = ``sql Insert into Time Values (Null, 15:09:24);''

testDate2 :: IO(SQLResult ())
testDate2 = ``sql Update Time set Time = 2015:08:26 where key=1;''

testDate3 :: IO( SQLResult[Time])
testDate3 = ``sql Select * from Time where Time = 2015:09:23;''

checkDates :: IO( SQLResult [Time])
checkDates = ``sql Select * from Time;''

-- complex having conditions

checkHaving :: IO(SQLResult [(Float, Float)])
checkHaving = ``sql Select Avg(r.Grade), Avg(r.Points) 
                    From Result as r Inner Join Student as s On Satisfies r belongs_to s 
                    Group By s.Name having not Avg(r.Grade) < 3.0 And Avg(r.Points) > 80.0;''
                
checkHaving1 ::IO(SQLResult [(Float, Float)])
checkHaving1 = ``sql Select Avg(r.Grade), Avg(r.Points) 
                     From Result as r Inner Join Student as s On Satisfies r belongs_to s 
                     Group By s.Name having (not (Avg(r.Grade) < 3.0 And Avg(r.Points) > 80.0));''

-- testing warnings for notation difference

testAlias :: IO (SQLResult [String])
testAlias = ``sql Select S.name from Student as s;''

testAlias1 :: IO (SQLResult [String])
testAlias1 = ``sql Select stud.name from Student as STUD;''

testNames :: IO (SQLResult [String])
testNames = ``sql Select name from sTUdent;''

testRelName :: IO (SQLResult [(String, (Maybe Int))])
testRelName = ``sql Select s.Name, r.Points from Student as s inner join Result as r on satisfies s HAS_A r;''

testRelation :: IO (SQLResult [(Student, Result)])
testRelation = ``sql Select * from Student inner join Result on Satisfies Student has_a Result;''

testAlias2 :: IO (SQLResult [(String, String)])
testAlias2 = ``sql Select s.Name, Student.Name from Student as s, Student;''

testNames1 :: IO (SQLResult [(String, (Maybe Int))])
testNames1 = ``sql Select Student.Name, Result.Points from Student, Result;''

testAlias3 :: IO (SQLResult [(String, (Maybe Int))])
testAlias3 = ``sql Select Name, Points from Student, Result;''

-- test insert and update with single embedded expressions
testI2 :: Int -> IO (SQLResult ())
testI2 age = ``sql Insert Into Student Values 
                    ( "Julia", "Kunst", 5959, "juk@mail.de", {age});''
                    
testI3 :: IO (SQLResult ())
testI3 = ``sql Insert into Result (Attempt, StudentTakingKey, ExamResultingKey) Values (1,10,3);''
                    
testU4 :: Int -> IO (SQLResult ())
testU4 x = ``sql Update Student Set Age = {x} Where Age < {x};'' 

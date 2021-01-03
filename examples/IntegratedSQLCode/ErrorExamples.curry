{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=foreigncode #-}

--- Test module for integration of SQL:
--- Checking whether the preprocessor detects errors

import Database.CDBI.ER as C
import Data.Time
import Uni

-- "Scanner" errors
-- unsupported character '&'
testE0 :: IO(SQLResult  [Student])
testE0 =``sql Select * from Student where Age > 20 & Age < 30;''

-- Parsing errors
--missing semicolon
testE1 :: IO(SQLResult  [Student])
testE1 =``sql Select * From Student''

-- missing table pseudony in Satisfies and missing parenthesis in first condition
testE2 :: IO(SQLResult  [(String, String)])
testE2 =``sql Select s.Firstname, s.Email
               From Student As s Inner Join Result as r On Satisfies  has_a r
               Where (r.Attempt = 1 And r.Grade <= 3.0 Or
                     (r.Attempt = 2  And r.Grade = 1.0);''

-- missing and in between operator
testE3 :: IO( SQLResult [(String, (Maybe Int))])
testE3 =``sql Select Name Age From Student Where Age between 20 30
                                              OrderBy Name Asc, Age Desc;''

-- missing Select in second query
testE4 :: IO( SQLResult  [String])
testE4 =``sql Select Name From Student Union Name From Lecturer;''

-- missing Set keyword
testE5 :: String -> IO( SQLResult  ())
testE5 mail =``sql Update Student Email = {mail}, Age = 20;''

-- missing from in delete and set in update
testE5' :: IO( SQLResult  ())
testE5' = ``sql Delete Student where Age is Null;Update Student Email = {mail}, Age = 20;''

-- missing Values keyword
testE6 :: Student ->  IO( SQLResult  ())
testE6 student =``sql Insert Into Student {student};''

-- too much parenthesis surrounding values
testE7 :: IO( SQLResult  ())
testE7 =``sql Insert Into Student  Values
                   ((10, 2608, "Wulf", "Malte", "mwu@mail.de", 34),
                    (11, 4736 ,"Krause", "Johanna", "jek@mail.de", 25));''

-- missing into keyword
testE8 :: IO( SQLResult  ())
testE8 =``sql Insert Student Values
                    (8, 6828, "Julia", "Krone", "julia@mail.de", 26);''

-- using alias in delete query
testE81 :: String -> IO( SQLResult  ())
testE81 studName =``sql Delete From Student s Where s.Name = {studName};''

-- missing semicolon
testE82 :: IO( SQLResult  ())
testE82 =``sql Insert Into Student Values
                    (8, 6828, "Julia", "Krone", "julia@mail.de", 26)''

-- SQL/consistency errors
--same alias but differnet notations
testAliasE :: IO (SQLResult [(String, Int)])
testAliasE = ``Select Alias.name, ALIAS.Points from Student as Alias inner join result as ALIAS on Satisfies Alias has_a ALIAS;''

-- same column name in different tables
testColumns :: IO (SQLResult [(String, String)])
testColumns = ``sql Select Name, Name from Student, Lecturer;''

--missing alias - ambiguous columns
testColumns1 :: IO (SQLResult [(String, String)])
testColumns1 = ``sql Select Student.Name, Student.Name from Student, Student;''

-- Point-column has typo and is withour pseudonym and result can not be used in Satisfies
testE9 :: IO( SQLResult  [(String, (Maybe Int))])
testE9 = ``sql Select s.Name, r.Points From Student
                           as s, Result as r Where Point > 45 and Satisfies s has_a result;''

-- Alias s defined twice
testE91 :: IO(SQLResult  [(String, String)])
testE91 = ``sql Select s.Name, s. Name from Student as s, Lecturer as s;''

-- Alias as is not defined and alias r is used outside its scope
testE92 :: IO(SQLResult  [(String)])
testE92 = ``sql Select s.Name from Student where exists (Select * from Result as r)
                order by r.Points;''

-- Alias as is not defined and alias r is used outside its scope
testE93 :: IO(SQLResult  [(String)])
testE93 = ``sql Select s.Name from Student where exists (Select * from Result as r)
                group by r.Points;''

-- Point has a typo, undefined relation used
testE94 :: IO( SQLResult  [(String, (Maybe Int))])
testE94 = ``sql Select s.Name, r.Points From Student
                           as s, Result as r Where r.Point > 45 and Satisfies s belongs_to r;''

-- alias defined but not used for age
testSE11 :: IO( SQLResult  [(String, (Maybe Int))])
testSE11 ``sql Select s.FirstName, s.Age from Student as s Where Age not null;''

-- typo in table name
testE12 :: IO( SQLResult  [Student])
testE12 = ``sql Select * From Students;''

-- constraint not allowed
testSE13 :: IO( SQLResult  [(String, (Maybe Int))])
testSE13 = ``sql Select s.Firstname, s.Age from Student as s Where s.Age = Null;''

-- this one at least results in an type error
-- missing values for e-mail which con not be null
testE14 :: IO( SQLResult  ())
testE15 = ``sql Insert Into Student Values
                    (8, 6828, "Julia", "Krone", 26);''

-- missing value for column matnum
testE15 :: IO( SQLResult  ())
testE15 = ``sql Insert Into Student
                    (Key, Name, Firstname, Email, Age)
                      Values (9,"Susan", "Krone", "susan@mail.de", 17);''

-- non existent column given
testE16 :: IO( SQLResult  ())
testE16 = ``sql Insert Into Student
                    (Key, MatNum, Name, Firstname, Career, Age)
                      Values (9, 2001 ,"Susan", "Krone", "Informatik", 17);''

-- keys are referencing different tables
testE9' :: IO(SQLResult  [(Student, Result)])
testE9' =``sql Select Distinct * From Student As s , Result as r
                    Where  r.StudentTakingKey = r.Key
                     Limit 5;''

-- type errors
-- keys compared to values
testE10 :: IO(SQLResult  [Student])
testE10 = ``sql Select Distinct * From Student As s
                Where Exists
                 ( Select * From Participation As p
                    Where s.Key = p.StudentParticipationKey And p.LectureParticipationKey
                       In (1,2,3)) Limit 5;''

-- diffenrent types in both queries
testE17 :: IO( SQLResult  [String])
testE17 = ``sql Select MatNum From Student Union Select Name From Lecturer;''

-- diffenrent types in both queries
testE17' :: IO( SQLResult  [String])
testE17' =``sql Select Name, MatNum From Student Union Select Name From Lecturer;''

-- Points is of type Int
testE18 :: IO( SQLResult  [(String, (Maybe Int))])
testE18 = ``sql Select s.Name, r.Points From Student
                           as s, Result as r Where r.Points > 45.5 and Satisfies s has_a r;''

-- StudentID compared with column of type Int
testE19 :: IO( SQLResult  [String])
testE19 = ``sql Select s.Name From Result As r, Student As s
                          Where s.Key = r.Points Group By s.Name Having r.Grade < 2.0;''

-- Column of type String inside avg function
testE20 :: IO(SQLResult  [String])
testE20 = ``sql Select s.Name From Result As r, Student As s
                Where s.Key = r.StudentTakingKey And s.Age < 20
                Group By s.Name Having Avg(s.Name) < 0.0;''

-- different types in case branches
testE21 :: IO( SQLResult  [(String, String)])
testE21 = ``sql Select s.Name,
                Case When s.Age < 20 Then "Young" Else 20 End
                From Student as s;''

-- key- and null-value im Case
testE22 :: IO( SQLResult  [(String, String)])
testE22 = ``sql Select s.Name,
                Case When s.Age < 20 Then s.Key Else Null End
                From Student as s;''

-- null-value used in case
testE22' :: IO( SQLResult  [(String, String)])
testE22' =``sql Select s.Name,
                Case When s.Age < 20 Then 19 Else Null End
                From Student as s;''

-- comparison of two embedded expressions
testE23 :: Int -> Int ->  IO( SQLResult  [(String, (Maybe Int))])
testE23 v1 v2 = ``sql Select s.Firstname, s.Age from Student as s Where {v1} < {v2};''

-- set value to null in update
testE24 ::  IO( SQLResult  () )
testE24 = ``sql Update Student Set Age = Null Where Age Not Null;''

-- set key value in update
testE25 ::  IO( SQLResult  ())
testE25  = ``sql Update Student Set Key = 1  Where  Not Age < 20;''

-- String column set to int value
testE26 ::  IO( SQLResult  ())
testE26 = ``sql Update Student Set Email = 42  Where  Not Age < 20;''

-- comparison of int-value to key
testE27 :: IO( SQLResult  [String])
testE27 = ``sql Select s.Name From Result As r, Student As s
                          Where s.Key = 1 Group By s.Name Having r.Grade < 2.0;''

-- set int-Column to String value
testE28 :: IO( SQLResult  ())
testE28 = ``sql Insert Into Student Values
                    (8, "6828", "Julia", "Krone", "julia@mail.de", 26);''

-- null value for not nullable column
testE28' :: IO( SQLResult  ())
testE28' = ``sql Insert Into Student Values
                    (8, 6828, "Julia", "Krone", Null, 26);''

-- missing value for e-mail
testE28'' :: IO( SQLResult  ())
testE28'' = ``sql Insert Into Student (Key, MatNum, Name, Firstname, Email) Values
                    (8, 6828, "Julia", "Krone", Null);''

-- two embedded expressions in case
testKeyfail1 :: IO( SQLResult  [(StudentID,String)])
testKeyfail1 = ``sql Select s.Name,
                     Case When s.Age < 20 Then {(StudentID 2)} Else {(StudentID 3)} End
                     From Student as s;''

-- key-value and int together
testKeyfail2 :: IO( SQLResult  [(StudentID,String)])
testKeyfail2 = ``sql Select s.Name,
                     Case When s.Age < 20 Then 1 Else s.Key End
                     From Student as s;''


-- CDBI constraints
--more than three tables
testE29 :: IO( SQLResult  [(Student, UniModel.Result, Exam, Time)])
testE29 =``sql Select * from Student, Result, Exam, Time;''

-- more than five columns
testE30 :: IO( SQLResult  [(Int,String,String, String, (Maybe Int), (Maybe Float))])
testE30 =``sql Select s.MatNum, s.Firstname, s.Name, s.Email, r.Points, r.Grade From Student as s, Result as r;''

-- combination of * and set operation
testE31 :: IO( SQLResult  [(String, String)]
testE31 =``sql Select * from Lecture Union Select * from Lecturer;''

-- group by in exists
testE32 :: IO(SQLResult  [Student])
testE32 =``sql Select Distinct * From Student As s
                Where Exists
                 ( Select * From Participation As p
                    Where s.Key = p.StudentParticipationKey Group By s.Name )
                     Limit 5;''

-- order by and limit in exists
testE33 :: IO(SQLResult  [Student])
testE33 =``sql Select Distinct * From Student As s
                Where Exists
                 ( Select * From Participation As p
                    Where s.Key = p.StudentParticipationKey Order By s.Name
                     Limit 5);''

-- more than one table in exists
testE34 :: IO(SQLResult  [Student])
testE34 =``sql Select Distinct * From Student As s
                Where Exists
                 ( Select * From Participation As p, Result as r)
                     Limit 5;''

-- order by in exists
testE35 :: IO(SQLResult  [Student])
testE35 =``sql Select Distinct * From Student As s
                Where Exists
                 ( Select * From Participation As p
                    Where s.Key = p.StudentParticipationKey Order By s.Name )
                     Limit 5;''

-- limit in exists
testE36 :: IO(SQLResult  [Student])
testE36 =``sql Select Distinct * From Student As s
                Where Exists
                 ( Select * From Participation As p
                    Where s.Key = p.StudentParticipationKey Limit 5);''

-- set op in exists
testE37 :: IO(SQLResult  [Result])
testE37 = ``sql Select Distinct * From Result As r
                Where Exists
                 ( Select s.Name From Student as s
                   intersect
                   Select l.Name from Lecturer as l);''

-- key-value in case expression
testKeyfail :: IO( SQLResult  [(StudentID,String)])
testKeyfail = ``sql Select s.Name,
                     Case When s.Age < 20 Then {(StudentID 2)} Else s.Key End
                     From Student as s;''

--Tests to demonstrate error management
-- three times used : instead of .
testE38 :: IO(SQLResult [(String, String)])
testE38 = ``sql Select s:Name from Student s
                 Union
                 Select l:Name from Lecturer as l order by l:Name;''

-- from keyword and , instead of .
testE39 :: IO(SQLResult  [(String, (Maybe Int)])
testE39 = ``sql Select s.Name, s.Age fro Student as order by s,Name;''

-- twice unsupported operation <>
testE40 :: IO(SQLResult  [Student])
testE40 = ``sql Select * from Student as s where s.Age <> 20 and s.Age <> 30;''

-- twice In instead of into and missing (
testE41 :: IO (SQLResult  ())
testE41 =``sql In Transaction Insert In Lecture Values (6, "Logik", Null, 3);
                              Insert In Lecture Values (7, "Kryptographie", Null, 1);) ;''

-- unsupported operator !=, invalid assignment and wrong usage of Null
testE42 :: IO(SQLResult  ())
testE42 = ``sql Update Student Set Name =! "Falsch",  Age = Age-1 where Age Null;''

--alias s not defined but st and typo in table name
testE43 :: IO(SQLResult  [String])
testE43 = ``sql Select st.Name from Students as st, Results as r;''

-- type error in case in condition and twice in having
testE44 :: IO(SQLResult  [(String, String)])
testE44 =  ``sql Select s.Name,
                 Case When s.Age < 20 Then "Young" Else 20 End
                 From Student as s Where s.MatNum > 100.0
                 Group By s.Name Having Avg(s.Name) < 1 ;''

-- too much columns and group by im exists -> da im Translator wird nach erster abgebrochen - okay
testE45 :: IO(SQLResult [Student])
testE45 =``sql Select s.Key, s.MatNum, s.Firstname, s.Name, s.Email, s.Age From Student As s
                Where Exists
                 ( Select * From Participation As p
                    Where s.Key = p.StudentParticipationKey Group By s.Name )
                     Limit 5;''

-- missing semicolon behind first request
testE46 :: IO(SQLResult ())
testE46 = ``sql Insert In Lecture Values (6, "Logik", Null, 3)
                Insert In Lecture Values (7, "Kryptographie", Null, 1);''

--typo in column names
testE47 :: IO(SQLResult  [Student])
testE47 = ``sql Select s.FirstName from Student as s, Result as r where s.Key = r.studenttakingkey;''

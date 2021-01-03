{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=foreigncode #-}

--- Test module for integration of SQL:
--- Testing also insert/update/delete operations

import Database.CDBI.ER
import Uni

-- control functions
showStudents :: IO (SQLResult [Student])
showStudents = ``sql Select * From Student;'' 

showResult :: IO (SQLResult [Result])
showResult = ``sql Select * From Result;''  

-- update tests 
testU1 :: Student -> IO ( SQLResult ())
testU1 student = ``sql Update Student Set {student};''

testU2 :: IO (SQLResult ())
testU2 = ``sql Update Student Set {(Student (StudentID 5) "Mond" "Thorben" 55 "thorbenMond@mail.com" Nothing)};''

testU3 :: String -> Int -> IO (SQLResult ())
testU3 mail age = ``sql Update Student Set Email = {mail} , Age = {age} 
                        Where ( Name like "M%" And Firstname like "A%");''
                        
testU4 :: Int -> Int -> IO (SQLResult ())
testU4 x y  = ``sql Update Student Set Age = 20 Where Age < {1+x+6*y};'' 

testU5 :: IO (SQLResult())
testU5 =``sql Update Result Set {(Result (ResultID 8) 1 (Just 4.0) (Just 51) (StudentID 4) (ExamID 3))};''

--delete functions
testD1 :: String -> IO (SQLResult ())
testD1 studName = ``sql Delete From Student Where name = {studName};''

testD2 :: String -> IO (SQLResult ())
testD2 studName = ``sql Delete From Student Where (name = {studName} or age > 30);''

-- insert functions

testI1 :: Student ->  IO (SQLResult ())
testI1 student = ``sql Insert Into Student Values {student};''

testI11 :: Student -> Student -> IO (SQLResult ())
testI11 student1 student2 = ``sql Insert Into Student Values {student1}, {student2};''

testI12 :: Student -> IO (SQLResult ())
testI12 student1 = ``sql Insert Into Student Values {student1},
                         (8, "Julia", "Krone", 6828, "julia@mail.de", 26);''

testI13 :: Student -> IO (SQLResult ())
testI13 student1 = ``sql Insert Into Student Values (8, "Julia", "Krone", 6828, "julia@mail.de", 26), 
                         {student1};''

testI2 :: IO (SQLResult ())
testI2 = ``sql Insert Into Student Values 
                    ( "Julia", "Kunst", 5959, "juk@mail.de", 25);''

testI3 :: IO (SQLResult ())
testI3 = ``sql Insert Into Student  Values 
                   (10, "Wulf", "Malte", 2608, "mwu@mail.de", 34),
                   (11, "Krause", "Johanna", 4736 ,"jek@mail.de", 25);''

testI4 :: IO (SQLResult ())
testI4 = ``sql Insert Into Student 
                    (Key, Name, Firstname, MatNum, Email, Age)
                      Values (9, "Susan", "Krone", 2001 ,"susan@mail.de", 17);''

testI5 :: IO (SQLResult())
testI5 = ``sql Insert Into Student 
                   (Key, Name, Firstname, MatNum, Email) Values 
                     (9, "Krone", "Chuck", 2010 , "chuck@mail.de");''
                     
testI6 :: IO (SQLResult())
testI6 = ``sql Insert Into Result Values (4, 1, 1.3, 80, 1, 2);''

-- combined requests

testC1 ::IO(SQLResult [(String, String)])
testC1 = ``sql Select l.Title, e.GradeAverage from Lecture as l, exam as e;
                 Select l.Firstname, l.Name from Lecturer as l;''

testC2 :: IO (SQLResult ())
testC2 = ``sql Begin;
               Insert Into Result Values (5, 2, 3.0, 50, 4, 2);
               Commit;''
               
testC3 :: IO (SQLResult [Lecture])
testC3 =``sql In Transaction (Insert Into Lecture Values (4, "Compilerbau", Null, 2);
                              Select * from Lecture;) ;''  
                              
testC4 :: IO (SQLResult ())
testC4 =``sql Begin;
              Insert Into Lecture Values (5, "Deklarative Sprachen", Null, 3);
              Select * from Lecture ;
              Commit;''        
              
testC5 :: IO (SQLResult [Lecture])
testC5 =``sql In Transaction (Insert Into Lecture Values (6, "Logik", Null, 3);
                              Insert Into Lecture Values (7, "Kryptographie", Null, 1);) ;
              Select * from Lecture;''      
              
testC6 :: IO(SQLResult ())
testC6 = ``sql In Transaction (Update Student Set Name = "Krone", firstname = "Susan" Where Name = "Susan" and  firstname = "Krone";
                               Update Student Set Age = 35 where Name = "Wulf";);''

testC7 :: IO(SQLResult ())
testC7 = ``sql Delete From Student Where Name = "Ziege";
               Delete From Student where Firstname = "Thorben";''

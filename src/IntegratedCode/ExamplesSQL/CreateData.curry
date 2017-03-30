import Uni_CDBI
import Database.CDBI.ER 
import Database.CDBI.Connection
import Time

createTestData :: IO ()
createTestData = do 
  conn   <- connectSQLite sqliteDBFile
  result <- ((insertEntries studentList student_CDBI_Description) >+
             (insertEntries lectureList lecture_CDBI_Description) >+
             (insertEntries lecturerList lecturer_CDBI_Description) >+
             (insertEntries placeList place_CDBI_Description) >+
             (insertEntries timeList time_CDBI_Description) >+
             (insertEntries examList exam_CDBI_Description) >+
             (insertEntries resultList result_CDBI_Description) >+
             (insertEntries participList participation_CDBI_Description) >+
             (insertEntryCombined sse1 sseDescription)
            ) conn
  disconnect conn
  case result of
    Left (DBError kind str) -> putStrLn ((show kind) ++ " " ++ str)
    Right _ -> putStrLn "Ok"

-- Students
studentList = [student1, student2, student3, student4]
student1 = Student (StudentID 1) "Muster" "Max" 23 "max@email.de" Nothing
student2 = Student (StudentID 2) "Arndt" "Annika" 66 "annika@email.de" (Just 25)
student3 = Student (StudentID 3) "Ziege" "Stefan" 44 "stefan@email.de" (Just 20)
student4 = Student (StudentID 4) "Sonne" "Ruben" 100 "ruben@email.de" (Just 28)

-- Lecturers
lecturerList = [lecturer1, lecturer2, lecturer3]
lecturer1 = Lecturer (LecturerID 1) "Mueller" "Hans"
lecturer2 = Lecturer (LecturerID 2) "Klein" "Frauke"
lecturer3 = Lecturer (LecturerID 3) "Hansen" "Frank"

-- Lectures
lectureList = [lecture1, lecture2, lecture3]
lecture1 = Lecture (LectureID 1) "Advanced Programming"
                   "Advanced techniques for functional and logic programming."
                   (LecturerID 1)
lecture2 = Lecture (LectureID 2)
                   "Theoretical Computer Science" "" (LecturerID 2)
lecture3 = Lecture (LectureID 3) "Databases"
                   "This lecture provides an introduction to databases and SQL."
                   (LecturerID 3)

-- Places 
placeList = [place1, place2]
place1 = Place (PlaceID 1) "Main street" 3 24
place2 = Place (PlaceID 2) "Main street" 3 25

-- Times
timeList = [time1, time2, time3, time4]
time1 = Time (TimeID 1) (toClockTime (CalendarTime 2012 7 12 8 15 0 0))
time2 = Time (TimeID 2) (toClockTime (CalendarTime 2012 10 5 8 15 0 0))
time3 = Time (TimeID 3) (toClockTime (CalendarTime 2012 7 15 12 15 0 0))
time4 = Time (TimeID 4) (toClockTime (CalendarTime 2012 7 17 15 30 0 0))

-- Exams
examList = [exam1, exam2, exam3, exam4]
exam1 = Exam (ExamID 1) (Just 2.54) (LectureID 1) (TimeID 1) (PlaceID 1)
exam2 = Exam (ExamID 2) (Just 3.3) (LectureID 1) (TimeID 2) (PlaceID 2)
exam3 = Exam (ExamID 3) (Just 1.7) (LectureID 2) (TimeID 3) (PlaceID 1)
exam4 = Exam (ExamID 4) Nothing (LectureID 3) (TimeID 4) (PlaceID 2)

-- Results
resultList = [result1, result2, result3, result4, result5, result6, result7, result8]
result1 = Result (ResultID 1) 1 (Just 5.0) (Just 30) (StudentID 1) (ExamID 1)
result2 = Result (ResultID 2) 2 (Just 3.7) (Just 58) (StudentID 1) (ExamID 2)  
result3 = Result (ResultID 3) 1 (Just 1.0) (Just 97) (StudentID 2) (ExamID 1)  
result4 = Result (ResultID 4) 1 (Just 1.3) (Just 90) (StudentID 3) (ExamID 1)
result5 = Result (ResultID 5) 1 (Just 1.3) (Just 91) (StudentID 1) (ExamID 3)
result6 = Result (ResultID 6) 1 (Just 5.0) (Just 20) (StudentID 4) (ExamID 1)
result7 = Result (ResultID 7) 2 (Just 5.0) (Just 39) (StudentID 4) (ExamID 2)
result8 = Result (ResultID 8) 1 (Just 5.0) (Just 49) (StudentID 4) (ExamID 3)

cd = combineDescriptions student_CDBI_Description 0 exam_CDBI_Description 0
                          (\st ex -> (StudentStudentExam _ st ex))
                          (\(StudentStudentExam _ st ex) -> (st, ex))
                          
data StudentStudentExam = StudentStudentExam Student Student Exam
sseDescription = 
  addDescription student_CDBI_Description 1 
  (\st1 (StudentStudentExam _ st2 ex) -> (StudentStudentExam st1 st2 ex))
  (\(StudentStudentExam st _ _) -> st) cd
  
sse1 :: StudentStudentExam  
sse1 = StudentStudentExam (Student (StudentID 5) "Mond" "Thorben" 55 "thorben@email.de" (Just 18))
                          (Student (StudentID 6) "Stern" "Susanne" 77 "susanne@email.de" (Just 18))
                          (Exam (ExamID 5) (Just 2.2) (LectureID 3) (TimeID 2) (PlaceID 1))
                          
participList::[Participation] 
participList = [particip1, particip2, particip3, particip4, particip5, particip6, particip7, particip8, particip9]
particip1 :: Participation
particip1 = Participation (StudentID 1) (LectureID 1) 
particip2 :: Participation
particip2 = Participation (StudentID 2) (LectureID 1) 
particip3 :: Participation
particip3 = Participation (StudentID 1) (LectureID 2) 
particip4 :: Participation
particip4 = Participation (StudentID 3) (LectureID 2) 
particip5 :: Participation
particip5 = Participation (StudentID 4) (LectureID 2) 
particip6 :: Participation
particip6 = Participation (StudentID 1) (LectureID 3) 
particip7:: Participation
particip7 = Participation (StudentID 2) (LectureID 3) 
particip8 :: Participation
particip8 = Participation (StudentID 5) (LectureID 3) 
particip9 :: Participation
particip9 = Participation (StudentID 6) (LectureID 3) 
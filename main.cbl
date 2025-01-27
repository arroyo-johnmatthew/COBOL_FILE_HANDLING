       IDENTIFICATION DIVISION.
       PROGRAM-ID. Online-Registration.

      *SETTING UP THE FILE AND ITS LOCATION
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *ASSIGNING THE FILE TO THE LOCATION
           SELECT Regi-Form ASSIGN TO "C:\Users\Ellen\Desktop\Form.txt"
      *SETTING UP THE FILE ORGANIZATION
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.       
      *SETTING UP THE FILE STRUCTURE
       FD Regi-Form.
      *DECLARING THE VARIABLES TO BE MANIPULATED BY THE USER
       01 Regi-Info.
           05 Regi-Item PIC X(50).

      *DECLARING THE VARIABLES TO BE USED IN THE PROGRAM
       WORKING-STORAGE SECTION.
       01 User-Choice PIC X.
       01 User-Grade PIC 9(3).
       01 EOF-Indicator PIC X VALUE 'N'.

      *THIS IS WHERE THE PROGRAM STARTS AND PROCESS THE USER'S REQUEST
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *OPENING THE FILE FOR WRITING
           OPEN OUTPUT Regi-Form
           PERFORM WITH TEST AFTER UNTIL User-Choice = User-Choice
      *CLEARS THE SCREEN 
               DISPLAY X"1B" & "[2J"
      *RESET THE CURSOR POSITION 
               DISPLAY X"1B" & "[H"
      *SET THE COLOR TO GREEN
               DISPLAY X"1B" & "[32m" "***ONLINE REGISTRATION SYSTEM***"
      *SET THE COLOR TO RED
               DISPLAY X"1B" & "[31m ---choose a program to enroll---"
      *SET THE COLOR TO WHITE
               DISPLAY X"1B" & "[0m" "1. BSIT"
               DISPLAY "2. BSECE"
               DISPLAY "3. BSA"
               DISPLAY "4. BSBA"
               DISPLAY "5. BSED"
               DISPLAY "6. BSOA"
               DISPLAY X"1B" & "[31m" "-----for diploma courses------"
               DISPLAY X"1B" & "[0m" "7. DIT"
               DISPLAY "8. DOMT"
               DISPLAY X"1B" & "[32m" "********************************"
               DISPLAY X"1B" & "[0m" "Enter your choice: "
               ACCEPT User-Choice
      *CONDITIONS TO CHECK THE USER'S COURSE CHOICE     
           IF User-Choice = 1   
      *DISPLAYS IT IN THE ACTUAL .TXT FILE          
               MOVE "---- Course: BSIT ----" TO Regi-Item
      *SAVES IT IN THE .TXT FILE
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: BSIT"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J" 
               DISPLAY X"1B" & "[H"   
           ELSE IF User-Choice = 2
               MOVE "---- Course: BSECE ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: BSECE"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J" 
               DISPLAY X"1B" & "[H"   
           ELSE IF User-Choice = 3
               MOVE "---- Course: BSA ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: BSA"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J" 
               DISPLAY X"1B" & "[H"   
           ELSE IF User-Choice = 4
               MOVE "---- Course: BSBA ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: BSBA"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J" 
               DISPLAY X"1B" & "[H"   
           ELSE IF User-Choice = 5
               MOVE "---- Course: BSED ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: BSED"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J" 
               DISPLAY X"1B" & "[H"   
           ELSE IF User-Choice = 6
               MOVE "---- Course: BSOA ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: BSOA"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J" 
               DISPLAY X"1B" & "[H"   
           ELSE IF User-Choice = 7
               MOVE "---- Course: DIT ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: DIT"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J" 
               DISPLAY X"1B" & "[H"   
           ELSE IF User-Choice = 8
               MOVE "---- Course: DOMT ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: DOMT"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J" 
               DISPLAY X"1B" & "[H"
           ELSE
               DISPLAY " "
               DISPLAY X"1B" & "[31m" "Invalid choice"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J" 
               DISPLAY X"1B" & "[H"   
           END-IF
           END-PERFORM

           MOVE SPACES TO User-Choice
      *ANOTHER MENU THAT DISPLAY ALL THE SECTIONS OF THE COURSE
           PERFORM WITH TEST AFTER UNTIL User-Choice = User-Choice
               DISPLAY X"1B" & "[32m" "***ONLINE REGISTRATION SYSTEM***"
               DISPLAY X"1B" & "[31m ---choose year and section---"
               DISPLAY " "
               DISPLAY X"1B" & "[31m" "----------SECTION 1----------"
               DISPLAY X"1B" & "[0m" "A. 1-1"
               DISPLAY "B. 2-1"
               DISPLAY "C. 3-1"
               DISPLAY "D. 4-1"
               DISPLAY X"1B" & "[31m" "----------SECTION 2----------"
               DISPLAY X"1B" & "[0m" "E. 1-2"
               DISPLAY "F. 2-2"
               DISPLAY "G. 3-2"
               DISPLAY "H. 4-2"
               DISPLAY X"1B" & "[31m" "-------for ladderized--------"
               DISPLAY X"1B" & "[0m" "I. 4-1"
               DISPLAY "J. 4-2"
               DISPLAY "(press X to go to main menu)"
               DISPLAY X"1B" & "[32m" "********************************"
               DISPLAY X"1B" & "[0m" "Enter your choice: "
               ACCEPT User-Choice
      *CONDITIONS TO CHECK THE USER'S CHOICE OF YEAR AND SECTION
           IF User-Choice = "A" OR User-Choice = "a"
               MOVE "---- Year and Section: 1-1 ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: 1-1"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = "B" OR User-Choice = "b"
               MOVE "---- Year and Section: 2-1 ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: 2-1"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = "C" OR User-Choice = "c"
               MOVE "---- Year and Section: 3-1 ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: 3-1"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = "D" OR User-Choice = "d"
               MOVE "---- Year and Section: 4-1 ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: 4-1"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = "E" OR User-Choice = "e"
               MOVE "---- Year and Section: 1-2 ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: 1-2"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = "F" OR User-Choice = "f"
               MOVE "---- Year and Section: 2-2 ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: 2-2"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = "G" OR User-Choice = "g"
               MOVE "---- Year and Section: 3-2 ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: 3-2"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = "H" OR User-Choice = "h"
               MOVE "---- Year and Section: 4-2 ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: 4-2"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = "I" OR User-Choice = "i"
               MOVE "---- Year and Section: 4-1 ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: 4-1"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = "J" OR User-Choice = "j"
               MOVE "---- Year and Section: 4-2 ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: 4-2"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = "X" or User-Choice = "x"
             DISPLAY X"1B" & "[2J" 
             DISPLAY X"1B" & "[H"
             CLOSE Regi-Form   
             PERFORM MAIN-PROCEDURE
           ELSE
               DISPLAY " "
               DISPLAY X"1B" & "[31m" "Invalid choice"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           END-IF
           END-PERFORM

           MOVE SPACES TO User-Choice
      *ANOTHER MENU THAT DISPLAY THE WHEN TO START THE SEMESTER
           PERFORM WITH TEST AFTER UNTIL User-Choice = User-Choice
               DISPLAY X"1B" & "[32m" "***ONLINE REGISTRATION SYSTEM***"
               DISPLAY X"1B" & "[31m ---choose when to start---"
               DISPLAY " "
               DISPLAY X"1B" & "[0m" "1. First Semester"
               DISPLAY "2. Second Semester"
               DISPLAY "3. Third Semester"
               DISPLAY "(press X to go to main menu)"
               DISPLAY X"1B" & "[32m" "********************************"
               DISPLAY X"1B" & "[0m" "Enter your choice: "
               ACCEPT User-Choice
      *CONDITIONS TO CHECK THE USER'S CHOICE OF SEMESTER
           IF User-Choice = 1
               MOVE "---- Start in: First Semester ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: First Semester"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = 2
               MOVE "---- Start in: Second Semester ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: Second Semester"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = 3
               MOVE "---- Start in: Third Semester ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You enrolled in: Third Semester"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE IF User-Choice = "X" or User-Choice = "x"
               DISPLAY X"1B" & "[2J" 
               DISPLAY X"1B" & "[H"
               CLOSE Regi-Form   
               PERFORM MAIN-PROCEDURE
           ELSE
               DISPLAY " "
               DISPLAY X"1B" & "[31m" "Invalid choice"
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"

           END-IF
           END-PERFORM

           MOVE SPACES TO User-Choice 
      *ANOTHER MENU THAT INPUT THE USER'S PREVIOUS GRADE
           PERFORM WITH TEST AFTER UNTIL User-Choice = User-Choice
               DISPLAY X"1B" & "[32m" "***ONLINE REGISTRATION SYSTEM***"
               DISPLAY X"1B" & "[31m ---input your previous grade---"
               DISPLAY X"1B" & "[0m"
               ACCEPT User-Grade
               DISPLAY X"1B" & "[32m" "********************************"
               DISPLAY X"1B" & "[0m"
      *CONDITIONS TO CHECK THE USER'S GRADE IF IT IS QUALIFIED OR NOT
           IF User-Grade >= 75
               MOVE "---- Qualified to enroll ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[32m" "You are qualified to enroll."
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               DISPLAY "(or choose X to go to main menu)  "
               ACCEPT User-Choice
               IF User-Choice = "X" or User-Choice = "x"
                   DISPLAY X"1B" & "[2J" 
                   DISPLAY X"1B" & "[H"
                   CLOSE Regi-Form   
                   PERFORM MAIN-PROCEDURE
               END-IF
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           ELSE
               MOVE "---- Not qualified to enroll ----" TO Regi-Item
               WRITE Regi-Info
               DISPLAY " "
               DISPLAY X"1B" & "[31m" "You are not qualified to enroll."
               DISPLAY "(or choose X to go to main menu)  "
               DISPLAY X"1B" & "[0m" "Press Enter to continue..."
               ACCEPT User-Choice
               IF User-Choice = "X" or User-Choice = "x"
                   DISPLAY X"1B" & "[2J" 
                   DISPLAY X"1B" & "[H"
                   CLOSE Regi-Form   
                   PERFORM MAIN-PROCEDURE
               END-IF
               DISPLAY X"1B" & "[2J"
               DISPLAY X"1B" & "[H"
           END-IF
           END-PERFORM
      *CLOSES THE FILE
           CLOSE Regi-Form
      *DISPLAYS THE INFORMATION SAVED IN THE .TXT FILE IN THE TERMINAL
           DISPLAY X"1B" & "[0m" "Information: "
      *OPENS THE FILE FOR READING PURPOSES ONLY
           OPEN INPUT Regi-Form
      *READS THE FILE UNTIL THE END-OF-FILE (EOF). Y IS THE END
           PERFORM UNTIL EOF-Indicator = 'Y'
      *READS THE FILES FROM THE REGI-INFO
               READ Regi-Form INTO Regi-Info
                   AT END
                       MOVE 'Y' TO EOF-Indicator
                   NOT AT END
                       DISPLAY Regi-Item
               END-READ
           END-PERFORM
      *CLOSES THE FILE AGAIN
           CLOSE Regi-Form
      *LETS THE USER KNOW THAT THE INFORMATION IS SAVED IN THE .TXT FILE
           DISPLAY " "
           DISPLAY "*Information saved at Form.txt*"
           STOP RUN.



           
             
                   
           







       IDENTIFICATION DIVISION.
       PROGRAM-ID. MILLARD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PERSONNEL-REC.
          05 NAME        PIC X(20).
          05 ADDR        PIC X(40).
          05 DATE-WS     PIC X(30).
          05 RATE        PIC 9(3)V99.
          05 BONUS-RATE  PIC V99.
          05 HOURS       PIC 9(3).
          05 GROSS-PAY   PIC 9(6)V99.
          05 JOB         PIC X(12).

       77 VP             PIC X(20).

       PROCEDURE DIVISION.
           PERFORM ASSIGNMENT-PARAGRAPH.
           PERFORM CONDITIONAL-SELECTION.
           PERFORM DISPLY-DATA-PARAGRAPH.
       ASSIGNMENT-PARAGRAPH.
      ****** COBOL MOVE statements - Literals assigned to variables
           MOVE "Millard Fillmore" TO NAME.
           MOVE "61 Brigham Tavern Lane, Duxbury MA" TO ADDR.
           MOVE "Week of: February 24th, 2020" TO DATE-WS.
           MOVE 19 TO HOURS.
           MOVE 23.50 TO RATE.
           MOVE "PRESIDENT" TO JOB.
           MOVE "Abigail Fillmore" TO VP.
       CONDITIONAL-SELECTION.
      ****** Conditional expressions
           IF RATE > 18
              MOVE .25 TO BONUS-RATE
           ELSE
              MOVE ZERO TO BONUS-RATE.
           IF JOB = "PRESIDENT"
              MOVE .33 TO BONUS-RATE.
           MOVE "Week of: February 24th, 2020" TO DATE-WS.
       DISPLY-DATA-PARAGRAPH.
      ****** COBOL DISPLAY statements - Literals assigned to variables
           COMPUTE GROSS-PAY =(HOURS * RATE) *(1 + BONUS-RATE).
           DISPLAY "Name: " NAME.
           DISPLAY "Job: " JOB.
           DISPLAY "Address: " ADDR.
           DISPLAY "Today's Date: " DATE-WS.
           DISPLAY "Hours Worked: " HOURS.
           DISPLAY "Hourly Rate: " RATE.
           DISPLAY "Bonus-Rate: " BONUS-RATE.
           DISPLAY "Gross Pay: " GROSS-PAY.
           DISPLAY NAME " " ADDR.
           DISPLAY "Vice President: " VP.
           GOBACK.
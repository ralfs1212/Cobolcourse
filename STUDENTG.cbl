       IDENTIFICATION DIVISION.
       PROGRAM-ID.   STUDENT.
      *
      * ***************************************************
      * *** student.cbl
      * ***
      * ***     The program produces a report that shows
      * ***     the number of courses and credits that
      * ***     a student has taken.  The input file has
      * ***     two types of records, a student record showing
      * ***     information about a student, followed by
      * ***     a number of course records for that student.
      * ***     Each course record shows the information
      * ***     for a course taken by the student.
      * ***
      * ***     Program repaired:
      * ***     Control break processing added for
      * ***     summing up students' course counts
      * ***     and credit totals correctly - report writing is
      * ***     now done in control break postprocessing.
      * ***     Student Records without course records will be
      * ***     processed - a course record without preceding
      * ***     student record (first record ist type 2 )
      * ***     causes an end of processing.
      * ***     Report output and data validation left unchanged!
      * ***     Ralf Straube 07/21/2020
      * ***
      * ***************************************************
      * **************************************************
       INSTALLATION.  IBM.
       DATE-WRITTEN.  01-01-2009.
       DATE-COMPILED. 07-21-2020.
       SECURITY.   NONE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   IBM.
       OBJECT-COMPUTER.   IBM.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE   ASSIGN TO UT-S-STDNTFL
                  ORGANIZATION IS SEQUENTIAL.
           SELECT CREDITS-REPORT ASSIGN TO UT-S-PRTLINE
                  ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-FILE
           LABEL RECORDS ARE STANDARD.
       01 STUDENT-RECORD.
          05 SR-NAME            PIC X(19).
          05 FILLER             PIC X(5).
          05 SR-ADDRESS         PIC X(20).
          05 FILLER             PIC XXXXX.
          05 SR-PHONE           PIC X(7).
          05 FILLER             PIC XXX.
          05 SR-BIRTH-DATE      PIC X(6).
          05 FILLER             PIC XXXX.
          05 SR-RECORD-TYPE     PIC X.
          05 FILLER             PIC X(10).
       01 COURSE-RECORD.
          05 CR-NAME            PIC X(19).
          05 FILLER             PIC X(5).
          05 CR-COURSE-NUMBER   PIC X(5).
          05 FILLER             PIC X(5).
          05 CR-CREDITS         PIC 9.
          05 FILLER             PIC X(34).
          05 FILLER             PIC X(11).
       FD  CREDITS-REPORT
           LABEL RECORDS ARE STANDARD.
       01 REPORT-LINE-OUT       PIC X(60).
       WORKING-STORAGE SECTION.
       01 SWITCHES-IN-PROGRAM.
          05 SW-END-OF-DATA     PIC X     VALUE 'N'.
             88 END-OF-DATA               VALUE 'Y'.

      * Control Break indicator
       77 CTRLBRK-STATUS        PIC X(01) VALUE SPACE.
       88 CTRLBRK-IND                     VALUE 'Y'.

       01 ACCUMS-AND-COUNTERS.
          05 ACCUM-CREDITS      PIC 999   VALUE 0.
          05 CTR-COURSES        PIC 999   VALUE 0.
          05 CTR-STUDENTS       PIC 9(5)  VALUE 0.
          05 CTR-LINES          PIC 99    VALUE 0.
       01 SAVE-AREAS.
          05 SAVE-NAME          PIC X(19).
       01 GRAND-TOTAL-LINE.
          05 FILLER             PIC X(30)
                                          VALUE
                ' TOTAL STUDENTS PROCESSED IS: '.
          05 GTL-STUDENT-COUNT  PIC ZZZZZ.
       01 DETAIL-LINE.
          05 FILLER             PIC X(5)  VALUE SPACE.
          05 DL-NAME            PIC X(19).
          05 FILLER             PIC X(8)  VALUE SPACE.
          05 DL-COURSES         PIC ZZ9.
          05 FILLER             PIC X(10) VALUE SPACE.
          05 DL-CREDITS         PIC ZZZ9.
       01 HEADING-1.
          05 FILLER             PIC X(10) VALUE SPACE.
          05 FILLER             PIC X(80) VALUE
                'S T U D E N T   C R E D I T S   R E P O R T'.
       01 HEADING-2.
          05 FILLER             PIC X(5)  VALUE SPACE.
          05 FILLER             PIC X(25) VALUE 'STUDENT NAME'.
          05 FILLER             PIC X(15) VALUE 'COURSES'.
          05 FILLER             PIC X(7)  VALUE 'CREDITS'.
      *
       PROCEDURE DIVISION.
       000-TOP-LEVEL.
           PERFORM 100-INITIALIZATION.
           PERFORM 200-PROCESS-RECORDS UNTIL END-OF-DATA.
           PERFORM 300-WRAP-UP.
           GOBACK.
       100-INITIALIZATION.
           OPEN INPUT STUDENT-FILE.
           OPEN OUTPUT CREDITS-REPORT.
           PERFORM 230-READ-A-RECORD.
      * First Record always has to be a Type 1 (student) record
           IF SR-RECORD-TYPE IS NOT EQUAL TO '1'
              THEN
              DISPLAY STUDENT-RECORD
              DISPLAY "** Bad Data - Processing stopped **"
              MOVE 'Y' TO SW-END-OF-DATA
           END-IF.
           MOVE 31 TO CTR-LINES.   *> Force Pagebreak at Start
       200-PROCESS-RECORDS.
           DISPLAY "200-PROCESS_RECORDS" SW-END-OF-DATA.
           PERFORM 210-PREPROC-GRP1.
      * Group Processing
           PERFORM 250-GRP1-PROCESSING UNTIL CTRLBRK-IND OR
              END-OF-DATA.
           PERFORM 280-POSTPROC-GRP1.
       210-PREPROC-GRP1.
           DISPLAY '210-PREPROC-GRP1'.
      * Group control only with type '1' records
           IF SR-RECORD-TYPE IS EQUAL TO '1'
              THEN
              MOVE SR-NAME TO SAVE-NAME
              MOVE ZERO TO CTR-COURSES ACCUM-CREDITS
           END-IF.
       250-GRP1-PROCESSING.
           DISPLAY '250-GRP1-PROCESSING'.
           IF SR-RECORD-TYPE IS EQUAL TO '1'
              THEN
              PERFORM 210-PROCESS-1-RECORDS                             L
           ELSE
              PERFORM 220-PROCESS-2-RECORDS
           END-IF.
      * Read next record
           PERFORM 230-READ-A-RECORD.
      * Test for control break (last step in group processing)
           IF SAVE-NAME NOT EQUAL TO SR-NAME AND SR-RECORD-TYPE
              IS EQUAL TO '1'
              THEN
              PERFORM 240-CONTROL-BREAK
           END-IF.
       210-PROCESS-1-RECORDS.
           DISPLAY "210-PROCESS_1-RECORDS".
           DISPLAY STUDENT-RECORD.
           ADD 1 TO CTR-STUDENTS.
       211-PAGE-CHANGE-RTN.
           WRITE REPORT-LINE-OUT FROM HEADING-1
              AFTER ADVANCING PAGE.
           WRITE REPORT-LINE-OUT FROM HEADING-2
              AFTER ADVANCING 2.
           MOVE ZERO TO CTR-LINES.
       212-BUILD-DETAIL-LINE.
           MOVE SAVE-NAME TO DL-NAME.
           MOVE CTR-COURSES TO DL-COURSES.
           MOVE ACCUM-CREDITS TO DL-CREDITS.
       220-PROCESS-2-RECORDS.
           DISPLAY "220-PROCESS_2-RECORDS".
           DISPLAY COURSE-RECORD.
           ADD CR-CREDITS TO ACCUM-CREDITS.
           ADD 1 TO CTR-COURSES.
       230-READ-A-RECORD.
           DISPLAY "230-READ-A-RECORD".
           READ STUDENT-FILE
           AT END
              MOVE 'Y' TO SW-END-OF-DATA.
       240-CONTROL-BREAK.
           DISPLAY "240-CONTROL-BREAK".
           DISPLAY SAVE-NAME.
           DISPLAY SR-NAME.
           MOVE "Y" TO CTRLBRK-STATUS.
       280-POSTPROC-GRP1.
           DISPLAY '280-POSTPROC-GRP1'.
           PERFORM 212-BUILD-DETAIL-LINE.
           IF CTR-LINES IS GREATER THAN 30
              THEN
              PERFORM 211-PAGE-CHANGE-RTN
           END-IF
           WRITE REPORT-LINE-OUT FROM DETAIL-LINE
              AFTER ADVANCING 1.
           ADD 2 TO CTR-LINES.
           MOVE "N" TO CTRLBRK-STATUS.
       300-WRAP-UP.
           MOVE CTR-STUDENTS TO GTL-STUDENT-COUNT.
           WRITE REPORT-LINE-OUT FROM GRAND-TOTAL-LINE
              AFTER ADVANCING 2.
           CLOSE CREDITS-REPORT STUDENT-FILE.


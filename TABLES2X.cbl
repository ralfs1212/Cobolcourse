       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLES2X.
      ****************************************************************
      * TABLES2X, Table Processing Programm                          *
      * This programm loads a 2 Dim Table from Input file, then writes
      * report from Table Elements, 1. Dim: Students , 2. Dim:       *
      * 1 to 6 Courses per Student                                   *
      *                                                              *
      * RALF STRAUBE                                                 *
      *                                                              *
      * Initial Version, based on reusable PGM TEMPLAT2,             *
      * added code from                                              *
      * DDS0001.TABLES02         08/12/2020                          *
      ****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE   ASSIGN TO STDNTCRS
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS STUDENT-FILE-ST.
           SELECT CREDITS-REPORT ASSIGN TO STCRSRPT
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS OUTPUT-FILE-ST.
           SELECT ERRORFILE ASSIGN TO ERRFILE
                  ORGANIZATION IS SEQUENTIAL
                  FILE STATUS IS ERROR-FILE-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.
       01 STUDENT-RECORD.
          05 STUDENT-NAME               PIC X(20).
          05 STUDENT-COURSES.
             10 STUDENT-COURSE-TAB OCCURS 6 TIMES.
                15 COURSE-NBR           PIC X(7).
                15 COURSE-GRADE         PIC X(1).
                   88 A-GRADE                          VALUE "A".
                   88 B-GRADE                          VALUE "B".
                   88 C-GRADE                          VALUE "C".
                   88 D-GRADE                          VALUE "D".
                   88 F-GRADE                          VALUE "F".
          05 FILLER                     PIC X(12).
       FD  CREDITS-REPORT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD.
       01 REPORT-LINE-OUT               PIC X(80).
       FD ERRORFILE
           RECORDING MODE IS F.
       01 ERR-RECORD.
          05 STUDENT-OUT                PIC X(20).
          05 COURSE-OUT                 PIC X(7).
          05 GRADE-OUT                  PIC X(1).

       WORKING-STORAGE SECTION.
       01 PROGRAM-SWITCHES.
          05 STUDENT-FILE-ST            PIC X(2).
             88 STUDENT-FILE-OK                        VALUE '00'.
          05 OUTPUT-FILE-ST             PIC X(2).
             88 CREDITS-REPORT-OK                      VALUE '00'.
          05 ERROR-FILE-ST              PIC X(2).
             88 ERROR-FILE-OK                          VALUE '00'.
          05 SW-MUS-FOUND               PIC X          VALUE 'N'.
             88 MUS-FOUND                              VALUE 'Y'.
          05 SW-STUDENT-FOUND           PIC X          VALUE 'N'.
             88 STUDENT-FOUND                          VALUE 'Y'.
      * EOF indicator
          05 STUDENT-FILE-STATUS        PIC X(01)      VALUE SPACE.
             88 STUDENT-FILE-EOF                       VALUE 'Y'.
      * Valid Data  indicators
          05 VALID-STUDENT-STATUS       PIC X(01)      VALUE SPACE.
             88 VALID-STUDENT-IND                      VALUE 'Y'.
          05 VALID-COURSE-STATUS        PIC X(01)      VALUE SPACE.
             88 VALID-COURSE-IND                       VALUE 'Y'.



       01 SUBSCRIPTS-AND-COUNTERS.
          05 CTR-STUDENTS               PIC 99         VALUE 0.
          05 STUDENTN-QPA-HI            PIC X(20). *> Name w/ Hi QPA
          05 STUDENTN-QPA-LO            PIC X(20). *> Name w/ Lo QPA
          05 STUDENTS-QPA-AVG           PIC 99V9       VALUE 0.
          05 STUDENTS-QPA-MAX           PIC 99V9       VALUE 0.
          05 STUDENTS-QPA-MIN           PIC 99V9       VALUE 0.

          05 STUDENT-SUB                PIC 99         VALUE 0 COMP.
      * Max. 5 Student records can be processed,
      * to enable program for more then that, the STUDENT-TABLE would
      * have to be defined OCCURS DEPENDING ON
      * R.S.
          05 STUDENT-SUB-MAX            PIC 99         VALUE 5 COMP.
          05 GRADE-ACCUM-ST             PIC 99         VALUE 0 COMP.
          05 GRADE-ACCUM-ALL            PIC 999        VALUE 0 COMP.
          05 QPA-ACCUM-ST               PIC 99V9       VALUE 0 COMP.
          05 CTR-COURSES-ALL            PIC 999        VALUE 0 COMP.
          05 COURSES-SUB                PIC 99         VALUE 0 COMP.
      * 6 (not 5) Course Segments found in Input File
      * R.S.
          05 COURSES-SUB-MAX            PIC 99         VALUE 6 COMP.
       01 WS-STUDENT-RECORD.
          02 WS-STUDENT-TABLE OCCURS 5 TIMES.
             05 WS-STUDENT-NAME         PIC X(20).
             05 WS-STUDENT-COURSES.
                10 WS-STUDENT-COURSE-TAB OCCURS 6 TIMES.
                   15 WS-COURSE-NBR     PIC X(7).
                   15 WS-COURSE-GRADE   PIC X(1).
                      88 A-GRADE-T                     VALUE "A".
                      88 B-GRADE-T                     VALUE "B".
                      88 C-GRADE-T                     VALUE "C".
                      88 D-GRADE-T                     VALUE "D".
                      88 F-GRADE-T                     VALUE "F".



       01 ACCUMS-AND-COUNTERS.
          05 WS-ACCUM-FLD-NUMERIC       PIC 9(7)       VALUE 0.
          05 WS-REC-KTR                 PIC 9(7)       VALUE 0.
          05 WS-REC-KTR-VALID           PIC 9(7)       VALUE 0.
          05 CTR-LINES                  PIC 99         VALUE 0.
          05 CTR-PAGES                  PIC 999        VALUE 0.
          05 CTR-LINES-MAX              PIC 99         VALUE 10.


      * Print lines                           *


       01 HEADING-1.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(6)       VALUE "Date: ".
          05 WS-DATE-H                  PIC 9999/99/99.
          05 FILLER                     PIC X(10)      VALUE SPACES.
          05 FILLER                     PIC X(25)      VALUE
                "Student Courses Breakout".
          05 FILLER                     PIC X(10)      VALUE SPACES.
          05 FILLER                     PIC X(6)       VALUE "Page: ".
          05 PAGE-NUM                   PIC ZZ9        VALUE ZERO.
          05 FILLER                     PIC X(58)      VALUE SPACES.


       01 DETAIL-LINE-1.
          05 FILLER                     PIC X(6)       VALUE SPACES.
          05 FILLER                     PIC X(14)      VALUE
                "Student Name: ".
          05 STUDENT-NAME-O             PIC X(20).
          05 FILLER                     PIC X(91).

       01 DETAIL-LINE-2.
          05 FILLER                     PIC X(12)      VALUE SPACES.
          05 FILLER                     PIC X(8)       VALUE
                "Course: ".
          05 WS-COURSE-NBR-O            PIC X(7).
          05 FILLER                     PIC X(3)       VALUE SPACES.
          05 FILLER                     PIC X(7)       VALUE "Grade: ".
          05 WS-COURSE-GRADE-O          PIC X(1).
          05 FILLER                     PIC X(94).


       01 WS-TOTALS-REC-1.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(30)      VALUE ALL "-".
          05 FILLER                     PIC X(100)     VALUE SPACES.


       01 WS-TOTALS-REC-2.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(26)      VALUE
                "Total Number of Students: ".
          05 CTR-STUDENTS-O             PIC Z9.
          05 FILLER                     PIC X(102)     VALUE SPACES.

       01 WS-TOTALS-REC-3.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(26)      VALUE
                "Student with higehst QPA: ".
          05 STUDENTN-QPA-HI-O          PIC X(20).
          05 FILLER                     PIC X(84)      VALUE SPACES.

       01 WS-TOTALS-REC-4.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(25)      VALUE
                "Student with lowest QPA: ".
          05 STUDENTN-QPA-LO-O          PIC X(20).
          05 FILLER                     PIC X(85)      VALUE SPACES.

       01 WS-TOTALS-REC-5.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(30)      VALUE
                "Average QPA for all Students: ".
          05 STUDENTS-QPA-AVG-O         PIC Z9.9.
          05 FILLER                     PIC X(98)      VALUE SPACES.



       PROCEDURE DIVISION.
       MAIN.
           PERFORM 000-HOUSEKEEPING.
      * Phase 1: Load Table from Input File
           PERFORM 100-PHASE-ONE.

      * Phase 2: Write Report from Table Entries
           PERFORM 110-PHASE-TWO.

           PERFORM 600-ACCUMULATE-COURSES.
           PERFORM 700-WRITE-SUMMARY-REPORT.
           PERFORM 800-CLOSE-FILES.
           GOBACK.
       000-HOUSEKEEPING.
      ************************************************************
      * Inititialization Routine                                 *
      ************************************************************
           DISPLAY '000-HOUSEKEEPING'.
           PERFORM 300-OPEN-FILES.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE-H.
           MOVE 31 TO CTR-LINES. *> Force Pagebreak at Start
      * Priming Read:
           PERFORM 400-READ-STUDENT-FILE.
       100-PHASE-ONE.
      ************************************************************
      * Main Process Phase One: Table Load                       *
      ************************************************************
           DISPLAY '100-PHASE-ONE'.

           PERFORM 200-PROCESS-RECORDS VARYING STUDENT-SUB
              FROM 1 BY 1 UNTIL STUDENT-FILE-EOF
              OR STUDENT-SUB > STUDENT-SUB-MAX.


           DISPLAY "Student Table loaded from Input file:".
           DISPLAY STUDENT-SUB.
           DISPLAY WS-STUDENT-TABLE(1).
           DISPLAY WS-STUDENT-TABLE(2).
           DISPLAY WS-STUDENT-TABLE(3).
           DISPLAY WS-STUDENT-TABLE(4).
           DISPLAY WS-STUDENT-TABLE(5).


       110-PHASE-TWO.
      ************************************************************
      * Main Process Phase Two: Writing Report from Table        *
      ************************************************************
           DISPLAY '110-PHASE-TWO'.


           PERFORM VARYING STUDENT-SUB
              FROM 1 BY 1 UNTIL STUDENT-SUB > STUDENT-SUB-MAX
                   DISPLAY WS-STUDENT-NAME(STUDENT-SUB)
                   PERFORM 500-PROCESS-REPORT-STUDENT
                   PERFORM VARYING COURSES-SUB
                      FROM 1 BY 1 UNTIL COURSES-SUB > COURSES-SUB-MAX
                           PERFORM 550-PROCESS-REPORT-COURSES
                   END-PERFORM
           END-PERFORM.

       200-PROCESS-RECORDS.
      ************************************************************
      * Process Input File                                       *
      ************************************************************
           DISPLAY '200-PROCESS-RECORDS'.
           DISPLAY "INPUT-REC: " STUDENT-RECORD
           PERFORM 220-VALIDATE-DATA.
           ADD 1 TO WS-REC-KTR CTR-STUDENTS.
      * Process valid Students
           IF VALID-STUDENT-IND THEN
              ADD 1 TO WS-REC-KTR-VALID *> count valid recors
      * Process invalid course segments
              IF NOT VALID-COURSE-IND
                 THEN
                 MOVE STUDENT-NAME TO STUDENT-OUT
                 MOVE COURSE-NBR(COURSES-SUB) TO COURSE-OUT
                 MOVE COURSE-GRADE(COURSES-SUB) TO GRADE-OUT
                 WRITE ERR-RECORD
      * Set Marker for to exclude from further processing:
      * Report, computing Average by moving SPACE to grade field
                 MOVE SPACE TO COURSE-GRADE(COURSES-SUB)
              END-IF
              MOVE STUDENT-RECORD TO WS-STUDENT-TABLE(STUDENT-SUB)
              PERFORM VARYING COURSES-SUB FROM 1 BY 1
                 UNTIL COURSES-SUB > COURSES-SUB-MAX

                      EVALUATE TRUE
                      WHEN A-GRADE-T(STUDENT-SUB, COURSES-SUB)
                           MOVE '4' TO
                              WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                      WHEN B-GRADE-T(STUDENT-SUB, COURSES-SUB)
                           MOVE '3' TO
                              WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                      WHEN C-GRADE-T(STUDENT-SUB, COURSES-SUB)
                           MOVE '2' TO
                              WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                      WHEN D-GRADE-T(STUDENT-SUB, COURSES-SUB)
                           MOVE '1' TO
                              WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                      WHEN F-GRADE-T(STUDENT-SUB, COURSES-SUB)
                           MOVE '0' TO
                              WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)

                      END-EVALUATE

              END-PERFORM
              PERFORM 400-READ-STUDENT-FILE
      * Process invalid data: write error record
           ELSE
              MOVE STUDENT-NAME TO STUDENT-OUT
              MOVE "ALL" TO COURSE-OUT
              WRITE ERR-RECORD
              PERFORM 400-READ-STUDENT-FILE *> next Record, no Report
           END-IF.

       220-VALIDATE-DATA.
      ************************************************************
      * Data validation for every input field, saving            *
      * indexes and named conditions for further processing      *
      ************************************************************
           DISPLAY '220-VALIDATE-DATA'.

      * Validate Course Grade, testing each course segment        *
           PERFORM VARYING COURSES-SUB FROM 1 BY 1 UNTIL
              COURSES-SUB > COURSES-SUB-MAX
                   IF NOT A-GRADE(COURSES-SUB) AND
                      NOT B-GRADE(COURSES-SUB) AND
                      NOT C-GRADE(COURSES-SUB) AND
                      NOT D-GRADE(COURSES-SUB) AND
                      NOT F-GRADE(COURSES-SUB)
                      THEN
                      DISPLAY "Grade not valid: "
                              COURSE-GRADE(COURSES-SUB)
                      DISPLAY COURSES-SUB
                      DISPLAY STUDENT-NAME
                      MOVE "N" TO VALID-COURSE-STATUS
                      EXIT PARAGRAPH
                   END-IF
           END-PERFORM.

      * If this STMT is reached, no Errors have been found         *
           MOVE "Y" TO VALID-STUDENT-STATUS VALID-COURSE-STATUS.


       300-OPEN-FILES.
      *************************************************************
      * Open files                                                *
      *************************************************************
           DISPLAY '300-OPEN-FILES'.
           OPEN INPUT STUDENT-FILE.
           IF NOT STUDENT-FILE-OK
              THEN
              DISPLAY 'Input File Problem: ' STUDENT-FILE-ST
              GOBACK
           END-IF.

           OPEN OUTPUT CREDITS-REPORT.
           IF NOT CREDITS-REPORT-OK
              THEN
              DISPLAY 'Output File Problem: ' OUTPUT-FILE-ST
              GOBACK
           END-IF.

           OPEN OUTPUT ERRORFILE.
           IF NOT ERROR-FILE-OK
              THEN
              DISPLAY 'Error File Problem: ' ERROR-FILE-ST
              GOBACK
           END-IF.

       400-READ-STUDENT-FILE.
      ************************************************************
      * Read Input record                                        *
      ************************************************************
           DISPLAY '400-READ-STUDENT-FILE'.
           READ STUDENT-FILE
           AT END
              MOVE "Y" TO STUDENT-FILE-STATUS
           END-READ.
       500-PROCESS-REPORT-STUDENT.
      ************************************************************
      * Filling detail line, writing report file                 *
      ************************************************************
           DISPLAY '500-PROCESS-REPORT-STUDENT'.
           IF CTR-LINES IS GREATER THAN CTR-LINES-MAX
              THEN
              PERFORM 510-PAGE-CHANGE-RTN
           END-IF

           MOVE WS-STUDENT-NAME(STUDENT-SUB) TO STUDENT-NAME-O.

           WRITE REPORT-LINE-OUT FROM DETAIL-LINE-1
              AFTER ADVANCING 5 LINES.

           ADD 1 TO CTR-LINES.
       550-PROCESS-REPORT-COURSES.
      ************************************************************
      * Filling detail line, writing report file                 *
      ************************************************************
           DISPLAY '550-PROCESS-REPORT-COURSES'.
           IF CTR-LINES IS GREATER THAN CTR-LINES-MAX
              THEN
              PERFORM 510-PAGE-CHANGE-RTN
      * Repeat Student Detail line after Pagebreak
              PERFORM 500-PROCESS-REPORT-STUDENT
           END-IF.

      * Courses with invalid grades won't be reported
      * (marked with a SPACE in 220-VALIDATE-DATA)

           IF WS-COURSE-GRADE(STUDENT-SUB
              COURSES-SUB) NOT EQUAL TO SPACE
              THEN
              MOVE WS-COURSE-NBR(STUDENT-SUB COURSES-SUB
                 ) TO WS-COURSE-NBR-O
              MOVE WS-COURSE-GRADE(STUDENT-SUB
                 COURSES-SUB) TO WS-COURSE-GRADE-O

              WRITE REPORT-LINE-OUT FROM DETAIL-LINE-2
                 AFTER ADVANCING 2 LINES

              ADD 1 TO CTR-LINES
           END-IF.

       510-PAGE-CHANGE-RTN.
      ************************************************************
      * Page Break Routine                                       *
      ************************************************************
           DISPLAY '510-PAGE-CHANGE-RTN'.

      * if PageNo. at bottom of page counter->  increment here  *
      * (else increment after printing)                         *

           ADD 1 TO CTR-PAGES.
           MOVE CTR-PAGES TO PAGE-NUM.

           WRITE REPORT-LINE-OUT FROM HEADING-1
              AFTER ADVANCING PAGE.

           MOVE ZERO TO CTR-LINES.
       600-ACCUMULATE-COURSES.
      ***********************************************************
      * Accumulate / Aggregate Course grades                    *
      * with PERFORM VARYING                                    *
      ***********************************************************
           DISPLAY '600-ACCUMULATE-COURSES'.

      * Initialize every time when this para is performed

           MOVE 0 TO GRADE-ACCUM-ST QPA-ACCUM-ST.

      * Initialize Hi and Low for saving first comparison
      * values

           MOVE 0 TO STUDENTS-QPA-MAX.
           MOVE 99 TO STUDENTS-QPA-MIN.


           PERFORM VARYING STUDENT-SUB FROM 1 BY 1 UNTIL STUDENT-SUB
              > STUDENT-SUB-MAX

                   DISPLAY WS-STUDENT-NAME(STUDENT-SUB)

      * Calculating AVG by summming up course grades and dividing by
      * number of courses.

                   PERFORM VARYING COURSES-SUB FROM 1 BY 1
                      UNTIL COURSES-SUB
                      > COURSES-SUB-MAX

      * Courses with invalid grades won't be counted
      * (marked with a SPACE in 220-VALIDATE-DATA)

                           IF WS-COURSE-GRADE(STUDENT-SUB
                              COURSES-SUB) NOT EQUAL TO SPACE
                              THEN

                              ADD +1 TO CTR-COURSES-ALL

                              COMPUTE GRADE-ACCUM-ST = GRADE-ACCUM-ST +
                                 FUNCTION NUMVAL(
                                 WS-COURSE-GRADE(STUDENT-SUB COURSES-SUB
                                 ))
                              COMPUTE QPA-ACCUM-ST ROUNDED =
                                 GRADE-ACCUM-ST /
                                 COURSES-SUB

                              DISPLAY
                                 WS-COURSE-GRADE(STUDENT-SUB COURSES-SUB
                                 )

                              DISPLAY GRADE-ACCUM-ST
                              DISPLAY QPA-ACCUM-ST
                              DISPLAY "End Course"

                           ELSE
                              DISPLAY "Course not counted"
                           END-IF
                   END-PERFORM

      * Comparison for new HI and Low Values

                   IF QPA-ACCUM-ST > STUDENTS-QPA-MAX
                      THEN
                      MOVE QPA-ACCUM-ST TO STUDENTS-QPA-MAX
                      MOVE WS-STUDENT-NAME(STUDENT-SUB) TO
                         STUDENTN-QPA-HI

                      DISPLAY STUDENTS-QPA-MAX
                      DISPLAY STUDENTN-QPA-HI
                   END-IF

                   IF QPA-ACCUM-ST < STUDENTS-QPA-MIN
                      THEN
                      MOVE QPA-ACCUM-ST TO STUDENTS-QPA-MIN
                      MOVE WS-STUDENT-NAME(STUDENT-SUB) TO
                         STUDENTN-QPA-LO

                      DISPLAY STUDENTS-QPA-MIN
                      DISPLAY STUDENTN-QPA-LO
                   END-IF

      * Saving accumulated COURSE-GRADES for computing over-all average

                   ADD GRADE-ACCUM-ST TO GRADE-ACCUM-ALL

      * Initializing accumulators for next student

                   MOVE 0 TO GRADE-ACCUM-ST QPA-ACCUM-ST

                   DISPLAY "End Student"
           END-PERFORM.

.     * computing over-all average grade

           COMPUTE STUDENTS-QPA-AVG ROUNDED = GRADE-ACCUM-ALL /
              CTR-COURSES-ALL.

           DISPLAY CTR-COURSES-ALL "/" GRADE-ACCUM-ALL.
           DISPLAY "AVG: " STUDENTS-QPA-AVG.

           DISPLAY STUDENTN-QPA-HI "/" STUDENTS-QPA-MAX.

           DISPLAY STUDENTN-QPA-LO "/" STUDENTS-QPA-MIN.



           DISPLAY "End Accumulating".



       700-WRITE-SUMMARY-REPORT.
      ***********************************************************
      * Write summary at end of report                          *
      ***********************************************************
           DISPLAY '700-WRITE-SUMMARY'.


           WRITE REPORT-LINE-OUT FROM WS-TOTALS-REC-1
              AFTER ADVANCING 5 LINES.

           MOVE CTR-STUDENTS TO CTR-STUDENTS-O.

           WRITE REPORT-LINE-OUT FROM WS-TOTALS-REC-2
              AFTER ADVANCING 2 LINES.

           MOVE STUDENTN-QPA-HI TO STUDENTN-QPA-HI-O.

           WRITE REPORT-LINE-OUT FROM WS-TOTALS-REC-3
              AFTER ADVANCING 2 LINES.

           MOVE STUDENTN-QPA-LO TO STUDENTN-QPA-LO-O.

           WRITE REPORT-LINE-OUT FROM WS-TOTALS-REC-4
              AFTER ADVANCING 2 LINES.

           MOVE STUDENTS-QPA-AVG TO STUDENTS-QPA-AVG-O.

           WRITE REPORT-LINE-OUT FROM WS-TOTALS-REC-5
              AFTER ADVANCING 2 LINES.


      * Statistics
           DISPLAY "Records read / valid  / Total:".

           DISPLAY WS-REC-KTR " / " WS-REC-KTR-VALID " / "
      -    WS-ACCUM-FLD-NUMERIC.


      ***********************************************************
      * Close all Files                                         *
      ***********************************************************
       800-CLOSE-FILES.
           DISPLAY '800-CLOSE-FILES'.
           CLOSE STUDENT-FILE.
           IF NOT STUDENT-FILE-OK
              THEN
              DISPLAY 'Input File Problem: ' STUDENT-FILE-ST
              GOBACK
           END-IF.
           CLOSE CREDITS-REPORT.
           IF NOT CREDITS-REPORT-OK
              THEN
              DISPLAY 'Output File Problem: ' OUTPUT-FILE-ST
              GOBACK
           END-IF.








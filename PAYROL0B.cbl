       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROL0B.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-SPACE                      PIC X(1)       VALUE SPACE.
       01 PAYSTUB-V20.
          05 REPORT-DATE                PIC XX/XX/XXXX.
          05 FULLNAME.
      *                                         changed R.S.
             10 WS-FNAME                PIC X(20).
             10 WS-LNAME                PIC X(20).
      *                                         ************
          05 WS-ADDRESS.
             10 WS-CITY                 PIC X(20).
             10 WS-STREET.
                15 WS-STREET-NBR        PIC 9(06).
                15 WS-STREET-NAME       PIC X(40).
             10 WS-STATE                PIC X(02).
             10 ZIP.
                15 WS-ZIP-FIRST-5       PIC X(05).
                15 FILLER               PIC X(01)      VALUE '-'.
                15 WS-ZIP-PLUS-4        PIC X(04).
          05 WS-PAYROLL.
             10 WS-EMP-TYPE             PIC X(01).
                88 FULL-TIME                           VALUE 'F'.
      *                                       changed R.S.
                88 PART-TIME                           VALUE 'P'.
      *                                       ************
             10 WS-FULL-TIME.
      *                                               changed R.S.
                15 WS-FULL-TIME-SALARY  PIC 9(7)V99    VALUE 0.
                15 WS-FULL-TIME-BONUS   PIC 99V99      VALUE 0.
             10 WS-HOURLY.
                15 WS-HOURS-WORKED      PIC 9(02)      VALUE 0.
                15 WS-RATE              PIC 9(3)       VALUE 0.
                15 WS-OT-COMP           PIC V99        VALUE 0.
                15 WS-40-DIFF           PIC 9(2)       VALUE 0.
                15 WS-PART-TIME-SALARY  PIC 9(6)V99    VALUE 0.
      *                                 **************************
      *   inserted R.S.
       77 SALARY-OUT-FLD                PIC $ZZZ,ZZ9.99.
      *   ******************************************************

       PROCEDURE DIVISION.
           PERFORM ASSIGNMENT-PARAGRAPH.
      *            inserted R.S.
           PERFORM CONDITIONAL-SELECTION.
      *            ************
           PERFORM DISPLAY-DATA-PARAGRAPH.
           GOBACK.
       ASSIGNMENT-PARAGRAPH.
           MOVE 'P' TO WS-EMP-TYPE.
           MOVE "Millard Fillmore" TO FULLNAME.
           MOVE 61 TO WS-STREET-NBR.
           MOVE 'BRIGHAM TAVERN LANE' TO WS-STREET-NAME.
           MOVE FUNCTION CURRENT-DATE TO REPORT-DATE.
           MOVE 'NC' TO WS-STATE.
           MOVE '90210' TO WS-ZIP-FIRST-5.
           MOVE '1111' TO WS-ZIP-PLUS-4.
      *     Deleted R.S. , really bad!
      *     PERFORM CONDITIONAL-SELECTION.
      *     **********************************
       CONDITIONAL-SELECTION.
           IF FULL-TIME
              PERFORM FULL-TIME-PARA
           ELSE
              IF PART-TIME
                 PERFORM PART-TIME-PARA
              ELSE
                 DISPLAY 'BAD DATA'
              END-IF.
       FULL-TIME-PARA.
           MOVE 500000 TO WS-FULL-TIME-SALARY.
           MOVE .10 TO WS-FULL-TIME-BONUS.
           COMPUTE WS-FULL-TIME-SALARY =
      *                             changed R.S.
              WS-FULL-TIME-SALARY *(1 + WS-FULL-TIME-BONUS).
      *                             *************
      * inserted R.S.
           MOVE WS-FULL-TIME-SALARY TO SALARY-OUT-FLD.

       PART-TIME-PARA.
           MOVE 45 TO WS-HOURS-WORKED.
           MOVE 15 TO WS-RATE.
           MOVE .2 TO WS-OT-COMP.


      * calculating overtime  (5 h)
           COMPUTE WS-40-DIFF = WS-HOURS-WORKED - 40.
      * calculating regular salary (45 * 15 = 675)
           COMPUTE WS-PART-TIME-SALARY =
              (WS-HOURS-WORKED * WS-RATE).
      * inserted R.S.
           IF WS-40-DIFF > 0

      * ************   675 + 6 (5h * 0.2)  = 681 ****
              COMPUTE WS-PART-TIME-SALARY = WS-PART-TIME-SALARY +
      *                                changed R.S.
                 WS-40-DIFF * (1 + WS-OT-COMP).
      *                                ***************


           MOVE WS-PART-TIME-SALARY TO SALARY-OUT-FLD.
      **************
       DISPLAY-DATA-PARAGRAPH.
           DISPLAY "FULL-NAME:" FULLNAME.
           DISPLAY "ADDRESS: " WS-ADDRESS.
           DISPLAY "PAY-STUB:" WS-PAYROLL.
      * inserted R.S.
           DISPLAY "Salary: " SALARY-OUT-FLD.
      **************

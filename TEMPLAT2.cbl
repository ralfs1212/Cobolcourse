       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEMPLAT2.
      ****************************************************************
      * TEMPLAT2, Reusable Standard Programm                         *
      * This programm reads input file until eof, performs data      *
      * validation and writes a detail line for every                *
      * input record. Page Break is done in the Report Processing    *
      *                                                              *
      * Inital Version   07/28/2020                                  *
      * Page Break Added 07/29/2020                                  *
      * minor reworks    08/02/2020                                  *
      *                                                              *
      * RALF STRAUBE                                                 *
      *                                                              *
      * future additions: report header written                      *
      * although input file is empty                                 *
      *                                                              *
      ****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO STDINP
           FILE STATUS IS INPUT-FILE-ST.
           SELECT OUTPUT-FILE  ASSIGN TO PRTLINE
           FILE STATUS IS OUTPUT-FILE-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE      *> DCB instream File is FB 80
           RECORDING MODE IS F.


      * Example input file structure with 3 fields 80 Bytes     *
      * e.g. JCL instream file                                  *
      *                                                         *
       01 INPUT-REC.
          05 FLD-KEY               PIC X(10).
          05 FLD-NUMERIC           PIC 99999.
          05 FLD-ALPHA             PIC X(10).
          05 FILLER                PIC X(55).
       FD  OUTPUT-FILE
           RECORDING MODE IS F.
       01 OUTPUT-LINE              PIC X(132).


       WORKING-STORAGE SECTION.
       01 PROGRAM-SWITCHES.
          05 INPUT-FILE-ST         PIC X(2).
             88 INPUT-FILE-OK                  VALUE '00'.
          05 OUTPUT-FILE-ST        PIC X(2).
             88 OUTPUT-FILE-OK                 VALUE '00'.
      * EOF indicator
          05 INPUT-FILE-STATUS     PIC X(01)   VALUE SPACE.
             88 INPUT-FILE-EOF                 VALUE 'Y'.
      * Valid Data  indicator
          05 VALID-DATA-STATUS     PIC X(01)   VALUE SPACE.
             88 VALID-DATA-IND                 VALUE 'Y'.

       01 ACCUMS-AND-COUNTERS.
          05 WS-ACCUM-FLD-NUMERIC  PIC 9(7)    VALUE 0.
          05 WS-REC-KTR            PIC 9(7)    VALUE 0.
          05 WS-REC-KTR-VALID      PIC 9(7)    VALUE 0.
          05 CTR-LINES             PIC 99      VALUE 0.
          05 CTR-PAGES             PIC 999     VALUE 0.
          05 CTR-LINES-MAX         PIC 99      VALUE 3.


      * Print lines (Example Report)                            *


       01 HEADING-1.
          05 FILLER                PIC X(10)   VALUE SPACE.
          05 FILLER                PIC X(80)   VALUE
                '            T E S T    R E P O R T         '.

       01 HEADING-2.
          05 FILLER                PIC X(10)   VALUE 'KEY FIELD'.
          05 FILLER                PIC X(10)   VALUE SPACES.
          05 FILLER                PIC X(13)   VALUE 'NUMERIC FIELD'.
          05 FILLER                PIC X(10)   VALUE SPACES.
          05 FILLER                PIC X(11)   VALUE 'ALPHA FIELD'.
       01 DETAIL-LINE.
          05 FLD-KEY-O             PIC X(10).
          05 FILLER                PIC X(10)   VALUE SPACES.
          05 FLD-NUMERIC-O         PIC 99,999-.
          05 FILLER                PIC X(16)   VALUE SPACES.
          05 FLD-ALPHA-O           PIC X(10).
          05 FILLER                PIC X(79).

       01 FOOT-LINE.
          05 FILLER                PIC X(40)   VALUE SPACES.
          05 FILLER                PIC X(12)   VALUE 'PAGE NO. '.
          05 PAGE-NUM              PIC ZZ9     VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-MAIN UNTIL INPUT-FILE-EOF.
           PERFORM 700-WRITE-SUMMARY-REPORT.
           PERFORM 800-CLOSE-FILES.
           GOBACK.
       000-HOUSEKEEPING.
      ************************************************************
      * Inititialization Routine                                 *
      ************************************************************
           DISPLAY '000-HOUSEKEEPING'.
           PERFORM 300-OPEN-FILES.
           MOVE 31 TO CTR-LINES. *> Force Pagebreak at Start
      * Priming Read:
           PERFORM 400-READ-INPUT-FILE.
       100-MAIN.
      ************************************************************
      * Main Routine                                             *
      ************************************************************
           DISPLAY '100-MAIN'.

           PERFORM 200-PROCESS-DATA.

       200-PROCESS-DATA.
      ************************************************************
      * Main Process                                             *
      ************************************************************
           DISPLAY '200-PROCESS-DATA'.
           DISPLAY "INPUT-REC: " INPUT-REC.
           PERFORM 220-VALIDATE-DATA.
           ADD 1 TO WS-REC-KTR.
      * Process valid Data
           IF VALID-DATA-IND THEN
              ADD 1 TO WS-REC-KTR-VALID *> count valid recors
              COMPUTE WS-ACCUM-FLD-NUMERIC = WS-ACCUM-FLD-NUMERIC +
                 FLD-NUMERIC OF INPUT-REC *> accumulate input
              PERFORM 500-WRITE-REPORT
      * Read next record
              PERFORM 400-READ-INPUT-FILE
      * Process invalid data
           ELSE
              PERFORM 400-READ-INPUT-FILE *> next Record, no Output
           END-IF.

       220-VALIDATE-DATA.
      ************************************************************
      * Data validation for every input field, saving            *
      * indexes and named conditions for further processing      *
      ************************************************************
           DISPLAY '220-VALIDATE-DATA'.

      * Validate first field                                     *
           IF FLD-KEY OF INPUT-REC EQUAL TO SPACES
              THEN
              DISPLAY "** Key Field Error! **"

              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.

      * Validate second  field                                   *
           IF FLD-ALPHA OF INPUT-REC EQUAL TO SPACES
              THEN
              DISPLAY "** Data Field Error: blank Field **"
              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.
      * Validate third  field                                    *
           IF FLD-NUMERIC OF INPUT-REC EQUAL TO ZERO
              THEN
              DISPLAY "** Data Field Error: non numeric Field **"
              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.

      * at this point all tests have been successfuly passed     *
           MOVE "Y" TO VALID-DATA-STATUS.

       300-OPEN-FILES.
      *************************************************************
      * Open files                                                *
      *************************************************************
           DISPLAY '300-OPEN-FILES'.
           OPEN INPUT INPUT-FILE.
           IF NOT INPUT-FILE-OK
              THEN
              DISPLAY 'Input File Problem: ' INPUT-FILE-ST
              GOBACK
           END-IF.

           OPEN OUTPUT OUTPUT-FILE.
           IF NOT OUTPUT-FILE-OK
              THEN
              DISPLAY 'Output File Problem: ' OUTPUT-FILE-ST
              GOBACK
           END-IF.
       400-READ-INPUT-FILE.
      ************************************************************
      * Read Input record                                        *
      ************************************************************
           DISPLAY '400-READ-INPUT-FILE'.
           READ INPUT-FILE
           AT END
              MOVE "Y" TO INPUT-FILE-STATUS
           END-READ.
       500-WRITE-REPORT.
      ************************************************************
      * Filling detail line, writing report file                 *
      ************************************************************
           DISPLAY '500-WRITE-REPORT'.
           IF CTR-LINES IS GREATER THAN CTR-LINES-MAX
              THEN
              PERFORM 510-PAGE-CHANGE-RTN
           END-IF

           MOVE FLD-KEY TO FLD-KEY-O
           MOVE FLD-NUMERIC TO FLD-NUMERIC-O
           MOVE FLD-ALPHA TO FLD-ALPHA-O

           WRITE OUTPUT-LINE FROM DETAIL-LINE.
           ADD 1 TO CTR-LINES. *> only detail lines are counted
       510-PAGE-CHANGE-RTN.
      ************************************************************
      * Page Break Routine                                       *
      ************************************************************
           DISPLAY '510-PAGE-CHANGE-RTN'.
           IF CTR-PAGES GREATER ZERO *> not before 1st heading printed
              THEN
              PERFORM 520-PAGE-FOOTING
           END-IF.
           WRITE OUTPUT-LINE FROM HEADING-1
              AFTER ADVANCING PAGE.
           WRITE OUTPUT-LINE FROM HEADING-2
              AFTER ADVANCING 2.
           MOVE ZERO TO CTR-LINES.

      * if PageNo. at bottom of page counter->  increment here  *
      * (else increment before printing)                        *

           ADD 1 TO CTR-PAGES.

       520-PAGE-FOOTING.
      ***********************************************************
      * Page Footing                                            *
      ***********************************************************
           DISPLAY '520-PAGE-FOOTING'.
           MOVE CTR-PAGES TO PAGE-NUM.
           WRITE OUTPUT-LINE FROM FOOT-LINE
              AFTER ADVANCING 5.
       700-WRITE-SUMMARY-REPORT.
      ***********************************************************
      * Write summary at end of report                          *
      ***********************************************************
           DISPLAY '700-WRITE-SUMMARY'.
           DISPLAY "Records read / valid  / Total:".

           DISPLAY WS-REC-KTR " / " WS-REC-KTR-VALID " / "
      -    WS-ACCUM-FLD-NUMERIC.

           PERFORM 520-PAGE-FOOTING.
      ***********************************************************
      * Close all Files                                         *
      ***********************************************************
       800-CLOSE-FILES.
           DISPLAY '800-CLOSE-FILES'.
           CLOSE INPUT-FILE.
           IF NOT INPUT-FILE-OK
              THEN
              DISPLAY 'Input File Problem: ' INPUT-FILE-ST
              GOBACK
           END-IF.
           CLOSE OUTPUT-FILE.
           IF NOT OUTPUT-FILE-OK
              THEN
              DISPLAY 'Output File Problem: ' OUTPUT-FILE-ST
              GOBACK
           END-IF.








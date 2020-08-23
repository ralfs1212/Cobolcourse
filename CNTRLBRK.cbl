      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CNTRLBRK.
       AUTHOR.        SAYLES.
      *-----------------------------------------------------------------
      *Program CNTRLBRK reads Input file until EOF. Sort Key is US-State.
      *Predidents, their home states, terms of office and salaries are
      *listed in a report
      *
      * Workshop 18.2. Ralf Straube
      * added Detail Field Accumulated Salaries and Statistics:
      * President with highest and lowest salary, avg salary - this
      * is done by table funktions with Index ALL.
      *
      * Changes applied 08/17/2020 Ralf Straube
      *-----------------------------------------------------------------

       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTSORT.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01 PRINT-REC.
          05 FILLER                     PIC X(03)      VALUE SPACE.
          05 USA-STATE-O                PIC X(18).
          05 FIRST-NAME-O               PIC X(15).
          05 LAST-NAME-O                PIC X(20).
          05 ELECTED-O                  PIC X(6).
          05 LAST-YEAR-O                PIC X(6).
          05 ACCT-LIMIT-O               PIC $$,$$$,$$9.99.
          05 FILLER                     PIC X(03)      VALUE SPACES.
          05 ACCT-BALANCE-O             PIC $$,$$$,$$9.99.
          05 FILLER                     PIC X(5)       VALUE SPACES.
      * R.S. Output field detail line added
          05 SALARY-ACCUM-OUT           PIC $$,$$$,$$$,$$$.99.
          05 FILLER                     PIC X(2)       VALUE SPACE.
      *
       FD  ACCT-REC RECORDING MODE F.
       01 ACCT-FIELDS.
          05 ACCT-NO                    PIC X(8).
          05 ACCT-LIMIT                 PIC S9(7)V99 COMP-3.
          05 ACCT-BALANCE               PIC S9(7)V99 COMP-3.
          05 LAST-NAME                  PIC X(20).
          05 FIRST-NAME                 PIC X(15).
          05 CLIENT-ADDR.
             10 STREET-ADDR             PIC X(25).
             10 CITY-COUNTY             PIC X(20).
             10 USA-STATE               PIC X(15).
                                              *> Input Sort Key
          05 RESERVED                   PIC X(7).
          05 COMMENTS                   PIC X(50).
      *
       WORKING-STORAGE SECTION.
       01 PROGRAM-INDICATOR-SWITCHES.
          05 WS-EOF-INPUT-SW            PIC X(1)       VALUE 'N'.
             88 EOF-INPUT                              VALUE 'Y'.

       01 WS-BREAK-CONTROLS.
          05 WS-CONTROL-KEY             PIC X(15). *> Hold/Control Key

      *************************************************************
      ****** Report headings begin here ******
      *************************************************************
       01 WS-BLANK-LINE                 PIC X(133)     VALUE SPACES.

       01 WS-HEADER-1.
          05 FILLER                     PIC X(1)       VALUE SPACES.
          05 FILLER                     PIC X(12)      VALUE
                'Report: A124'.
          05 DATE-O                     PIC X(10)      VALUE SPACE.
          05 FILLER                     PIC X(13)      VALUE SPACES.
          05 FILLER                     PIC X(47)
                                                       VALUE
                'Presidents Broken Out By State of Birth'.
          05 RPT-DATE                   PIC XXXX/XX/XX.
          05 FILLER                     PIC X(10)      VALUE SPACES.
          05 FILLER                     PIC X(5)       VALUE 'PAGE '.
          05 RPT-PAGE-NO                PIC ZZ.
          05 FILLER                     PIC X(12)      VALUE SPACES.

       01 WS-HEADER-2.
          05 FILLER                     PIC X(3)       VALUE SPACES.
          05 FILLER                     PIC X(18)      VALUE 'STATE'.
          05 FILLER                     PIC X(9)       VALUE 'PRESIDENT'
           .
          05 FILLER                     PIC X(24)      VALUE SPACES.
          05 FILLER                     PIC X(7)       VALUE 'ELECTED'.
          05 FILLER                     PIC X(1)       VALUE SPACES.
          05 FILLER                     PIC X(8)       VALUE 'THRU'.
          05 FILLER                     PIC X(14)      VALUE 'SALARY'.

          05 FILLER                     PIC X(13)      VALUE
                '   NET WORTH'
           .
          05 FILLER                     PIC X(10)      VALUE SPACES.
          05 FILLER                     PIC X(12)      VALUE
                'SALARY-ACCUM'.

       01 WS-HEADER-3.
          05 FILLER                     PIC X(3)       VALUE SPACES.
          05 FILLER                     PIC X(17)      VALUE ALL '='.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(32)      VALUE ALL '='.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(7)       VALUE '======='.
          05 FILLER                     PIC X(1)       VALUE SPACES.
          05 FILLER                     PIC X(7)       VALUE '====='.
          05 FILLER                     PIC X(01)      VALUE SPACES.
          05 FILLER                     PIC X(12)      VALUE ALL '='.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(13)      VALUE
                '============='.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(20)      VALUE ALL '='.

      *************************************************************
      ****** Control Break Subtotal Line ******
      *************************************************************
       01 WS-TRLR-LINE-1.
          05 FILLER                     PIC X(03)      VALUE SPACES.
          05 FILLER                     PIC X(12)      VALUE
                'Sub Totals:'.
          05 STATE-TRLR-LINE            PIC X(15).
          05 FILLER                     PIC X(16)      VALUE SPACE.
          05 FILLER                     PIC X(21)
                                                       VALUE
                'Salary | Net Worth: '
                JUST
                RIGHT.
          05 SALARY-SUB-TOT-OUT         PIC $$$,$$$,$$$.99.
          05 FILLER                     PIC X(02)      VALUE SPACES.
          05 NET-WORTH-SUB-TOT-OUT      PIC $$$,$$$,$$$.99.
          05 FILLER                     PIC X(01)      VALUE SPACE.
      * R.S. Subotal added for salary accumulated
          05 SALARY-ACCUM-SUB-TOT-OUT   PIC $$,$$$,$$$,$$$,$$$.99.
          05 FILLER                     PIC X(02)      VALUE SPACE.

       01 WS-TOTALS-REC-1.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(60)      VALUE ALL "-".
          05 FILLER                     PIC X(100)     VALUE SPACES.
      * R.S. Output total lines added
       01 WS-TOTALS-REC-2.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(37)      VALUE
                "Grand Total Salary all Presidents:   ".
          05 WS-SALARY-GRAND-TOTAL-O    PIC $$$,$$$,$$$,$$$,$$$.99
                                                       VALUE ZEROS.
          05 FILLER                     PIC X(84)      VALUE SPACES.


       01 WS-TOTALS-REC-3.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(37)      VALUE
                "President with highest Salary      : ".
          05 WS-PRESIDENTN-SALARY-HI-O  PIC X(35).
          05 FILLER                     PIC X(84)      VALUE SPACES.

       01 WS-TOTALS-REC-4.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(37)      VALUE
                "President with lowest Salary       : ".
          05 WS-PRESIDENTN-SALARY-LO-O  PIC X(35).
          05 FILLER                     PIC X(85)      VALUE SPACES.

       01 WS-TOTALS-REC-5.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 FILLER                     PIC X(37)      VALUE
                "Average Salary (Acc) all Presidents: ".
          05 WS-SALARY-AVG-O            PIC $$$,$$$,$$$,$$$,$$$.99.





       01 WS-COUNTERS-AND-ACCUMULATORS.
          05 WS-CONTROL-BREAK-TOTAL     PIC S9(7)V99 COMP-3.
          05 WS-STATE-CTR               PIC  9(2) COMP.

       01 WS-FLAGS.
          05 WS-LASTREC                 PIC X          VALUE SPACE.
          05 WS-LINE-KTR                PIC 9(4) COMP  VALUE 0.
          05 WS-SALARY-SUB-TOT          PIC 9(09)V99   VALUE 0.
          05 WS-NET-WORTH-SUB-TOT       PIC 9(09)V99   VALUE 0.
      * R.S. added Fields for Workshop 18.2
          05 WS-SALARY-ACCUM            PIC 9(13)V99   VALUE 0.
          05 WS-SALARY-ACCUM-SUB-TOT    PIC 9(16)V99   VALUE 0.
          05 WS-SALARY-GRAND-TOTAL      PIC 9(16)V99   VALUE 0.
          05 WS-PRESIDENTN-SALARY-HI    PIC X(35)      VALUE SPACES.
          05 WS-PRESIDENTN-SALARY-LO    PIC X(35)      VALUE SPACES.
          05 WS-SALARY-AVG              PIC 9(11)V99   VALUE 0.
          05 WS-INPUT-REC-CTR           PIC 9(3) COMP  VALUE 0.


       01 PRESIDENT-TABLE.
          05 PRESIDENT-ITEM OCCURS 100 TIMES
                DEPENDING ON WS-INPUT-REC-CTR INDEXED BY TAB-IDX.
             10 TAB-PRESIDENT-NAME      PIC X(35).
             10 TAB-PRESIDENT-SALARY    PIC 9(13)V99.
       01 ALT-IDX                       PIC 9(3) COMP.
                                 *> alternate index


      *------------------
       PROCEDURE DIVISION.
      *------------------
           PERFORM 100-INIT-RTN *> Housekeeping, Initial Report Headings
           PERFORM 300-PROCESS-RECORDS UNTIL EOF-INPUT
           PERFORM 500-CONTROL-BREAK *> Final Control Break paragraphs
           PERFORM 900-WRAP-UP
           GOBACK
           .
       100-INIT-RTN.
           MOVE FUNCTION CURRENT-DATE TO RPT-DATE.
           PERFORM 200-OPEN-FILES
           MOVE SPACES TO PRINT-REC
           PERFORM 700-READ-RECORD
           PERFORM 500-CONTROL-BREAK *> Initial Control creates Rpt Headings
           .
       150-INIT-WS-FIELDS.
           INITIALIZE WS-COUNTERS-AND-ACCUMULATORS
           .
       200-OPEN-FILES.
           OPEN INPUT ACCT-REC
           OPEN OUTPUT PRINT-LINE
           .
       300-PROCESS-RECORDS.
           IF NOT EOF-INPUT   *> No duplicating last record
              IF WS-CONTROL-KEY = USA-STATE  *> Control Break Conditional
                 PERFORM 400-MOVE-DATA
                 PERFORM 600-WRITE-DATA
                 PERFORM 700-READ-RECORD
              ELSE
                 PERFORM 500-CONTROL-BREAK
              END-IF
           END-IF
           .
       400-MOVE-DATA.
           DISPLAY "400-MOVE-DATA"
           MOVE SPACES TO PRINT-REC
           ADD +1 TO WS-STATE-CTR
           IF WS-STATE-CTR > 1 *> Logic to create outline view in State column
              MOVE SPACES TO USA-STATE-O
           ELSE
              MOVE USA-STATE TO USA-STATE-O,    *> MOVE IN-STATE -> HOLD-KEY
                                STATE-TRLR-LINE
           END-IF

           ADD ACCT-LIMIT TO WS-SALARY-SUB-TOT
           ADD ACCT-BALANCE TO WS-NET-WORTH-SUB-TOT


      *** The ACCT file is actually a repurposed file for the presidents
      *** The first four bytes is their inaugural yr => last year in office
           MOVE ACCT-NO(1:4) TO ELECTED-O
           MOVE ACCT-NO(5:4) TO LAST-YEAR-O
           MOVE ACCT-LIMIT TO ACCT-LIMIT-O
           MOVE ACCT-BALANCE TO ACCT-BALANCE-O
           MOVE LAST-NAME TO LAST-NAME-O
           MOVE FIRST-NAME TO FIRST-NAME-O

      * R.S. ACCUMULATING SALARY for each President
      * Salary * number of years in office, must be at least one year

           COMPUTE WS-SALARY-ACCUM ROUNDED = ACCT-LIMIT *
              FUNCTION MAX(+1,

              (FUNCTION NUMVAL(ACCT-NO(5:4)) - FUNCTION NUMVAL
              (ACCT-NO(1:4))))

           MOVE WS-SALARY-ACCUM TO SALARY-ACCUM-OUT
           ADD WS-SALARY-ACCUM TO WS-SALARY-ACCUM-SUB-TOT
      * R.S. loading table for overall statistics

           ADD 1 TO WS-INPUT-REC-CTR

           STRING FIRST-NAME DELIMITED BY SIZE
                  LAST-NAME DELIMITED BY SIZE

              INTO TAB-PRESIDENT-NAME(WS-INPUT-REC-CTR)
           END-STRING

           MOVE WS-SALARY-ACCUM TO
              TAB-PRESIDENT-SALARY(WS-INPUT-REC-CTR)

           .
       500-CONTROL-BREAK.
           IF WS-LINE-KTR > 0  *> Check for first time (beginning of program)
              MOVE WS-SALARY-SUB-TOT TO SALARY-SUB-TOT-OUT
              MOVE WS-NET-WORTH-SUB-TOT TO NET-WORTH-SUB-TOT-OUT

      * R.S.
              MOVE WS-SALARY-ACCUM-SUB-TOT TO SALARY-ACCUM-SUB-TOT-OUT

              WRITE PRINT-REC FROM WS-BLANK-LINE
              WRITE PRINT-REC FROM WS-TRLR-LINE-1
              WRITE PRINT-REC FROM WS-BLANK-LINE
              WRITE PRINT-REC FROM WS-BLANK-LINE
           END-IF
           IF NOT EOF-INPUT
              ADD +1 TO WS-LINE-KTR
              MOVE ZERO TO WS-SALARY-SUB-TOT, WS-NET-WORTH-SUB-TOT
      * R.S. initializing  WS-SALARY-ACCUM
              MOVE ZERO TO WS-SALARY-ACCUM-SUB-TOT

              MOVE WS-LINE-KTR TO RPT-PAGE-NO
              MOVE USA-STATE TO WS-CONTROL-KEY   *> SET NEW CONTROL KEY
              WRITE PRINT-REC FROM WS-BLANK-LINE
              WRITE PRINT-REC FROM WS-HEADER-1
              WRITE PRINT-REC FROM WS-BLANK-LINE
              WRITE PRINT-REC FROM WS-HEADER-2
              WRITE PRINT-REC FROM WS-HEADER-3
              PERFORM 150-INIT-WS-FIELDS
           END-IF
           .
       600-WRITE-DATA.
           WRITE PRINT-REC
           .
       700-READ-RECORD.
           DISPLAY "700-READ-RECORD"
           DISPLAY ACCT-FIELDS
           READ ACCT-REC
           AT END
              MOVE 'Y' TO WS-EOF-INPUT-SW
           END-READ



           .
       900-WRAP-UP.
           DISPLAY "900-WRAP-UP".
           DISPLAY "REC-CTR: " WS-INPUT-REC-CTR.
           PERFORM VARYING TAB-IDX FROM 1 BY 1
              UNTIL TAB-IDX > WS-INPUT-REC-CTR
                   DISPLAY TAB-PRESIDENT-NAME(TAB-IDX)
                   DISPLAY TAB-PRESIDENT-SALARY(TAB-IDX)
           END-PERFORM.

      * R.S. WS 18.2 write totals at end of report using functions
      * SUM(ALL), ORD-MIN(ALL), ORD-MAX(ALL), MEAN(ALL)

           WRITE PRINT-REC FROM WS-TOTALS-REC-1
              AFTER ADVANCING 4 LINES

           COMPUTE WS-SALARY-GRAND-TOTAL =

              FUNCTION SUM(TAB-PRESIDENT-SALARY(ALL))

           MOVE WS-SALARY-GRAND-TOTAL TO WS-SALARY-GRAND-TOTAL-O

           WRITE PRINT-REC FROM WS-TOTALS-REC-2
              AFTER ADVANCING 2 LINES

           COMPUTE ALT-IDX =

              FUNCTION ORD-MAX(TAB-PRESIDENT-SALARY(ALL))

           DISPLAY "Index Ord-Max: " ALT-IDX
           DISPLAY TAB-PRESIDENT-NAME(ALT-IDX)
           DISPLAY TAB-PRESIDENT-SALARY(ALT-IDX)

           MOVE TAB-PRESIDENT-NAME(ALT-IDX) TO WS-PRESIDENTN-SALARY-HI
           MOVE WS-PRESIDENTN-SALARY-HI TO WS-PRESIDENTN-SALARY-HI-O

           WRITE PRINT-REC FROM WS-TOTALS-REC-3
              AFTER ADVANCING 2 LINES


           COMPUTE ALT-IDX =

              FUNCTION ORD-MIN(TAB-PRESIDENT-SALARY(ALL))

           DISPLAY "Index Ord-Min: " ALT-IDX
           DISPLAY TAB-PRESIDENT-NAME(ALT-IDX)
           DISPLAY TAB-PRESIDENT-SALARY(ALT-IDX)

           MOVE TAB-PRESIDENT-NAME(ALT-IDX) TO WS-PRESIDENTN-SALARY-LO
           MOVE WS-PRESIDENTN-SALARY-LO TO WS-PRESIDENTN-SALARY-LO-O

           WRITE PRINT-REC FROM WS-TOTALS-REC-4
              AFTER ADVANCING 2 LINES

           COMPUTE WS-SALARY-AVG =

              FUNCTION MEAN(TAB-PRESIDENT-SALARY(ALL))


           MOVE WS-SALARY-AVG TO WS-SALARY-AVG-O

           WRITE PRINT-REC FROM WS-TOTALS-REC-5
              AFTER ADVANCING 2 LINES



           CLOSE ACCT-REC
           CLOSE PRINT-LINE
           .

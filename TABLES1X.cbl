       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLES01.
      *****************************************************************
      * Workshop 15.2: Program loads table from input file,           *
      * performs different table scans for selecting subsets,         *
      * calculating subtotals (Para's 200 - 400) and a grand total    *
      * with intrinsic function SUM .... (ALL) in Para 500            *
      * Added also input data validation for one field in             *
      * Para 000 because of special data in the given input file.     *
      *                                                               *
      * All results are printed to SYSOUT                             *
      * Filled Para's 200 to 500 with code, added little changes      *
      * to existing code and variable declarations                    *
      *                                                               *
      * Initial Version                                               *
      * copied from DDS0001                08/05/2020 Ralf Straube    *
      * Workshop 15.2                      08/06/2020 Ralf Straube      *
      * Rewrite Search (Workshop 16.1)     08/06/2020 Ralf Straube    *
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO EMPROJ.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01 EMP-PROJECT-TABLE-I.
          05 EMP-PROJECT-I              PIC X(4).
          05 EMP-NAME-I                 PIC X(15).
          05 EMP-STATE-OFFICE-I         PIC X(02).
          05 EMP-PROJECT-POSITION-I     PIC X(20).
          05 EMP-NBR-DAYS-ON-PROJ-I     PIC 9(03).
          05 EMP-NBR-OT-HOURS-I         PIC 9(03).
          05 EMP-PER-DAY-BILLING-RATE-I PIC 9(03)V99.
      * added decimal implied decimal place to PIC clause  R.S.  *
          05 EMP-PER-HOUR-OT-RATE-I     PIC 9(03)V99.
          05 EMP-LANGUAGE-CERT-I        PIC X(20).
          05 EMP-ON-CALL-I              PIC X(01).
          05 FILLER                     PIC X(02).
       WORKING-STORAGE SECTION.
       77 PROJECT-INDEX                 PIC S9(4) COMP.
       77 TABLE-MAX                     PIC S9(4) COMP
                                                       VALUE 20.
       77 SW-END-OF-FILE                PIC X(01)      VALUE SPACES.
       88 END-OF-FILE                                  VALUE 'Y'.
       01 EMP-PROJECT-TABLE.
          05 EMP-PROJECT-ITEM OCCURS 20 TIMES
                ASCENDING KEY IS EMP-PROJECT
                INDEXED BY PROJ-IDX.
             10 EMP-PROJECT             PIC X(4).
             10 EMP-NAME                PIC X(15).
             10 EMP-STATE-OFFICE        PIC X(02).
             10 EMP-PROJECT-POSITION    PIC X(20).
             10 EMP-NBR-DAYS-ON-PROJ    PIC 9(03).
             10 EMP-NBR-OT-HOURS        PIC 9(03).
             10 EMP-PER-DAY-BILLING-RATE
                                        PIC 9(03)V99.
      * added decimal implied decimal place to PIC clause  R.S.  *
             10 EMP-PER-HOUR-OT-RATE    PIC 9(03)V99.
             10 EMP-LANGUAGE-CERT       PIC X(20).
             10 EMP-ON-CALL             PIC X(01).
             10 FILLER                  PIC X(02).
       01 ACCUMULATORS.
      * added  2 decimals to SUM1  for precision reasons   R.S.  *
          05 SUM-1                      PIC 9(16)V99   VALUE 0.
          05 MAX-OUT                    PIC 9(4).
      * added additional accumulator fields                R.S.  *
          05 A-111-TOT                  PIC 9(16)V99   VALUE 0.
          05 SUM-NBR-DAYS-ON-PROJ       PIC 9(7)       VALUE 0.
          05 SUM-NBR-OT-HOURS           PIC 9(7)       VALUE 0.
          05 SUM-PER-DAY-BILLING-RATE   PIC 9(7)V99    VALUE 0.
          05 SUM-PER-HOUR-OT-RATE       PIC 9(7)V99    VALUE 0.
          05 COST-CALC                  PIC 9(15)V99   VALUE 0.
          05 COST-CALC-O                PIC $,$$$,$$$,$$$,$$9.99
                                                       VALUE ZEROES.

          05 NUM-EDIT-FLD-O             PIC 9(7).99    VALUE ZERO.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-PROCESS-TABLE-DATA.
           PERFORM 900-WRAP-UP
           GOBACK.
       000-HOUSEKEEPING.
      ****************************************************************
      ***  Loading table from Input file                             *
      ****************************************************************
           DISPLAY "000-HOUSEKEEPING".

           INITIALIZE EMP-PROJECT-TABLE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
           AT END
              MOVE 'Y' TO SW-END-OF-FILE.
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
      * changed "=" to ">"  TABLE-MAX                                *

              UNTIL PROJECT-INDEX > TABLE-MAX *> Load Table
              OR END-OF-FILE
                   MOVE EMP-PROJECT-I TO
                      EMP-PROJECT(PROJECT-INDEX)
                   MOVE EMP-NAME-I TO
                      EMP-NAME(PROJECT-INDEX)
                   MOVE EMP-STATE-OFFICE-I TO
                      EMP-STATE-OFFICE(PROJECT-INDEX)
                   MOVE EMP-PROJECT-POSITION-I TO
                      EMP-PROJECT-POSITION(PROJECT-INDEX)
                   MOVE EMP-NBR-DAYS-ON-PROJ-I TO
                      EMP-NBR-DAYS-ON-PROJ(PROJECT-INDEX)
                   MOVE EMP-NBR-OT-HOURS-I TO
                      EMP-NBR-OT-HOURS(PROJECT-INDEX)
                   MOVE EMP-PER-DAY-BILLING-RATE-I TO
                      EMP-PER-DAY-BILLING-RATE(PROJECT-INDEX)
                   MOVE EMP-PER-HOUR-OT-RATE-I TO
                      EMP-PER-HOUR-OT-RATE(PROJECT-INDEX)

      * Special test coding added,   R.S.:
      *
      * The field PER-HOUR-OT-RATE of 13th input record of given file
      * DDS0001.LEARN.EMP.PROJ is not numeric ->
      * the last two digits are "S" (x'E2') and "Q" (x'D8') which
      * are the first characters of 'SQL'. It seems to be an
      * accidently happened overlay of the next field in sequence,
      * EMP-LANGUAGE-CERT when creating the test data.
      * Surprisingly these two bytes are treated
      * as zoned decimals and not as characters
      *
      * Assumed reason:
      * because the two dubious bytes have valid zone and digit portions
      * for an UNSIGNED zoned decimal field, the compiler accepts them
      * as they are. This would not work
      * with a signed field, because this would demand standard zone
      * portions (x'F' instead of x'E')


                   IF NOT EMP-PER-HOUR-OT-RATE(PROJECT-INDEX) NUMERIC
                      THEN
                      DISPLAY "HOUR-OT-RATE not numeric !!!! "
                      DISPLAY EMP-NAME(PROJECT-INDEX)
                      DISPLAY EMP-PER-HOUR-OT-RATE(PROJECT-INDEX)
                      MOVE EMP-PER-HOUR-OT-RATE(PROJECT-INDEX) TO
                         NUM-EDIT-FLD-O
                      DISPLAY NUM-EDIT-FLD-O
                   END-IF

                   MOVE EMP-LANGUAGE-CERT-I TO
                      EMP-LANGUAGE-CERT(PROJECT-INDEX)
                   MOVE EMP-ON-CALL-I TO
                      EMP-ON-CALL(PROJECT-INDEX)
                   READ INPUT-FILE
                   AT END
                      MOVE 'Y' TO SW-END-OF-FILE
                   END-READ
                   DISPLAY EMP-PROJECT-ITEM(PROJECT-INDEX)
                           PROJECT-INDEX
           END-PERFORM.
       100-PROCESS-TABLE-DATA.
           PERFORM 200-FIND-PROJECT.
           PERFORM 300-FIND-NC-OT-SKILL.
           PERFORM 400-TOTAL-PROJ-EXPENSE.
           PERFORM 500-TOTAL-ALL-PROJECTS-EXPENSE.

       200-FIND-PROJECT.
      ****************************************************************
      ***  Display all of the Employee names working on project 'A111'
      ****************************************************************
           DISPLAY "200-FIND-PROJECT".

           DISPLAY "only First Match (Sequential SEARCH)".

           SET PROJ-IDX TO 1.
           SEARCH EMP-PROJECT-ITEM
           AT END
              DISPLAY "End of Table EMP-PROJECT"
           WHEN EMP-PROJECT(PROJ-IDX) = 'A111'
                DISPLAY "Projekt A111 - Employee: " EMP-NAME(PROJ-IDX)
           END-SEARCH.

           DISPLAY "All Matches (BINARY SEARCH ALL)".


           SEARCH ALL EMP-PROJECT-ITEM
           AT END
              DISPLAY "End of Table EMP-PROJECT"
           WHEN EMP-PROJECT(PROJ-IDX) = 'A111'
                DISPLAY "Projekt A111 - Employee: " EMP-NAME(PROJ-IDX)
           END-SEARCH.

           DISPLAY "All Matches (PERFORM VARYING)"

           PERFORM VARYING PROJ-IDX FROM 1 BY 1
              UNTIL PROJ-IDX > TABLE-MAX
                   IF EMP-PROJECT(PROJ-IDX) = 'A111'
                      THEN
                      DISPLAY "Projekt A111 - Employee: "
                              EMP-NAME
                         (PROJ-IDX)
                   END-IF
           END-PERFORM.
.
       300-FIND-NC-OT-SKILL.
      ****************************************************************
      ***  Display all of the Employee names of Programmers in NC
      ***     who are allowed to bill for On-Call work
      ****************************************************************
           DISPLAY "300-FIND-NC-OT-SKILL".



           DISPLAY "All Matches (PERFORM VARYING)"

           PERFORM VARYING PROJ-IDX FROM 1 BY 1
              UNTIL PROJ-IDX > TABLE-MAX
                   IF EMP-ON-CALL(PROJ-IDX) = 'Y' AND
                      EMP-STATE-OFFICE(PROJ-IDX) = 'NC'
                      THEN
                      DISPLAY "Empl. from NC bills for On-Call work: "
                              EMP-NAME
                         (PROJ-IDX)
                   END-IF
           END-PERFORM.
       400-TOTAL-PROJ-EXPENSE.
      ****************************************************************
      ***  Calculate and Display the total cost for the 'A111' project
      ****************************************************************
           DISPLAY "400-TOTAL-PROJ-EXPENSE".

           PERFORM VARYING PROJ-IDX FROM 1 BY 1
              UNTIL PROJ-IDX > TABLE-MAX
                   IF EMP-PROJECT(PROJ-IDX) = 'A111'
                      THEN
                      COMPUTE SUM-1 ROUNDED =
                         (EMP-NBR-DAYS-ON-PROJ(PROJ-IDX) *
                         EMP-PER-DAY-BILLING-RATE(PROJ-IDX))
                         +(EMP-NBR-OT-HOURS(PROJ-IDX) *
                         EMP-PER-HOUR-OT-RATE(PROJ-IDX))
                      DISPLAY EMP-NAME(PROJ-IDX) " : " SUM-1
                      ADD SUM-1 TO A-111-TOT
                   END-IF
           END-PERFORM.

           DISPLAY "Projekt A111 - Total Cost: "
                   A-111-TOT.

       500-TOTAL-ALL-PROJECTS-EXPENSE.
      ****************************************************************
      ***  Calculate & Display the total cost for all of the projects
      **   Google the COBOL Intrinsic FUNCTION SUM(<field>(ALL))
      ****************************************************************
           DISPLAY "500-TOTAL-ALL-PROJECTS-EXPENSE".
           COMPUTE SUM-NBR-DAYS-ON-PROJ =
              FUNCTION SUM(EMP-NBR-DAYS-ON-PROJ(ALL)).

           DISPLAY "SUM Nbr of days: " SUM-NBR-DAYS-ON-PROJ.

           COMPUTE SUM-NBR-OT-HOURS =
              FUNCTION SUM(EMP-NBR-OT-HOURS(ALL)).

           DISPLAY "SUM Nbr of hours OT: " SUM-NBR-OT-HOURS.

           COMPUTE SUM-PER-DAY-BILLING-RATE =
              FUNCTION SUM(EMP-PER-DAY-BILLING-RATE(ALL)).

           DISPLAY "SUM per day billing Rate " SUM-PER-DAY-BILLING-RATE.

           COMPUTE SUM-PER-HOUR-OT-RATE =
              FUNCTION SUM(EMP-PER-HOUR-OT-RATE(ALL)).

           DISPLAY "SUM per hour OT  Rate " SUM-PER-HOUR-OT-RATE.

           COMPUTE COST-CALC ROUNDED =
              (SUM-NBR-DAYS-ON-PROJ *
              SUM-PER-DAY-BILLING-RATE) +
              (SUM-NBR-OT-HOURS *
              SUM-PER-HOUR-OT-RATE).

           DISPLAY " *******************************************".
           MOVE COST-CALC TO COST-CALC-O
           DISPLAY "Grand Total all projects: "
                   COST-CALC-O.


       900-WRAP-UP.
           CLOSE INPUT-FILE.
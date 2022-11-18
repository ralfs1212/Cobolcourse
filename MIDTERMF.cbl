       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIDTERM.
      ******************************************************************
      * Midterm Workshop 12: Processing of Health Insurance Claims   ***
      *
      * Program reads Input File with weekly client claims until EOF.
      * Every Input Field ist tested for valid data.
      * The amount to pay to the client for each claim is calculated
      * based on:
      *
      * policy amount, company's and individual deductible percentage,
      * already paid coinsurance amount and companies max. payment.
      * Together with the claim details the calculated
      * deduction and claim to pay amounts are written
      * into the report.
      *
      * A summary total will be written at end of report
      *
      * Non-valid claims won't be written.
      * Error- (and debug) messages are written to SYSOUT.
      *
      * Program based on INSCLAIM and reusable TEMPLAT2 (Batch
      * without Control Group Processing)
      *
      * Inital Version   07/29/2020
      * Final  Version   08/02/2020
      *
      * Ralf Straube                                               ****
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMFILE
             ASSIGN TO CLAIM
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS CLAIMFILE-ST.
           SELECT PRINTFILE
             ASSIGN TO CLAIMRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS PRINTFILE-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CLAIMFILE
           RECORDING MODE IS F.
       01 CLAIM-RECORD                  PIC X(90).

       FD  PRINTFILE
           RECORDING MODE IS F.
       01 PRINT-LINE                    PIC X(132).

       WORKING-STORAGE SECTION.
       77 ALLOWED-AMT                   PIC S9(7)V99   VALUE 9999999.99.
       77 DEDUCTIBLE-PERC               PIC V999       VALUE .002.

           COPY CLAIMREC.

       01 PROGRAM-SWITCHES.
          05 REINSURANCE                PIC XX         VALUE SPACES.
          05 CLAIMFILE-STATUS           PIC X(1)       VALUE 'N'.
             88 CLAIMFILE-EOF                          VALUE 'Y'.
          05 CLAIMFILE-ST               PIC X(2).
             88 CLAIMFILE-OK                           VALUE '00'.
          05 PRINTFILE-ST               PIC X(2).
             88 PRINTFILE-OK                           VALUE '00'.
          05 POLICY-DEDUCTIBLE-MET-WS   PIC X(1).
             88 DEDUCTIBLE-MET                         VALUE 'Y'.
          05 CLAIMFILE-ST-WS            PIC X(2).
          05 VALID-DATA-INDICATOR.
             10 VALID-DATA-STATUS       PIC X(01)      VALUE SPACE.      .
                88 VALID-DATA-IND                      VALUE 'Y'.


       01 COUNTERS-AND-ACCUMULATORS-WS.
          05 DEDUCTIBLE-WS              PIC S9(7)V99.
          05 DEDUCTIBLE-POLICY-WS       PIC S9(7)V99.
          05 CLAIM-PAID-WS              PIC S9(7)V99.

       01 WS-CURRENT-DATE-DATA.
          05 WS-CURRENT-DATE.
             10 WS-CURRENT-YEAR         PIC 9(04).
             10 WS-CURRENT-MONTH        PIC 9(02).
             10 WS-CURRENT-DAY          PIC 9(02).
          05 WS-CURRENT-DT-STRING REDEFINES WS-CURRENT-DATE
                                        PIC X(8).
          05 WS-CURRENT-TIME.
             10 WS-CURRENT-HOURS        PIC 9(02).
             10 WS-CURRENT-MINUTE       PIC 9(02).
             10 WS-CURRENT-SECOND       PIC 9(02).
             10 WS-CURRENT-MILLISECONDS PIC 9(02).
       01 CURRENT-WEEK-DAY              PIC 9(1).
       01 WEEKDAYS-TABLE.
          05                            PIC X(9)       VALUE "Monday".
          05                            PIC X(9)       VALUE "Tuesday".
          05                            PIC X(9)       VALUE "Wednesday"
           .
          05                            PIC X(9)       VALUE "Thursday".
          05                            PIC X(9)       VALUE "Friday".
          05                            PIC X(9)       VALUE "Saturday".
          05                            PIC X(9)       VALUE "Sunday".
       01 WEEKDAYS-VALUES REDEFINES WEEKDAYS-TABLE.
          05 DT-OF-WK OCCURS 7 TIMES    PIC X(9).

       01 REPORT-FIELDS.
      * force pagebreak at start
          05 LINE-COUNT                 PIC S9(2)      VALUE +6.
          05 PAGE-COUNT                 PIC S9(2)      VALUE ZEROS.
          05 LINES-PER-PAGE             PIC S9(2)      VALUE +5.

       01 TOT-BILL-INFORMATION.
          05 TOT-DEDUCTIBLE-PAID        PIC S9(9)V99   VALUE 0.
          05 TOT-CLAIM-AMOUNT-PAID      PIC S9(9)V99   VALUE 0.
          05 TOT-CLAIM-AMOUNT           PIC S9(9)V99   VALUE 0.

       01 HEADING-LINE-ONE.
          05 HDG-DAY                    PIC X(9).
          05 FILLER                     PIC X(2)       VALUE ', '.
          05 HDG-DATE                   PIC XXXX/XX/XX.
          05 FILLER                     PIC X(18)      VALUE SPACES.
          05 FILLER                     PIC X(25)
                                                       VALUE
                'Group Claims Daily Totals'.
          05 FILLER                     PIC X(22)      VALUE SPACES.
          05 FILLER                     PIC X(3)       VALUE ' '.
          05 FILLER                     PIC X(31)      VALUE SPACES.
          05 FILLER                     PIC X(5)       VALUE 'Page '.
          05 HDG-PAGE-NUMBER            PIC Z9.
          05 FILLER                     PIC X(5)       VALUE SPACES.

       01 HEADING-LINE-TWO.
          05 FILLER                     PIC X(24)      VALUE 'POLICY'.
          05 FILLER                     PIC X(11)      VALUE 'POLICY'.
          05 FILLER                     PIC X(15)      VALUE 'FIRST'.
          05 FILLER                     PIC X(13)      VALUE 'LAST'.
          05 FILLER                     PIC X(14)      VALUE 'RENEW'.
          05 FILLER                     PIC X(8)       VALUE 'COPAY'.
          05 FILLER                     PIC X(9)       VALUE 'COPAY'.
          05 FILLER                     PIC X(14)      VALUE 'DEDUC'.
          05 FILLER                     PIC X(17)      VALUE 'CLAIM'.
          05 FILLER                     PIC X(7)       VALUE 'CLAIM'.

       01 HEADING-LINE-THREE.
          05 FILLER                     PIC X(24)      VALUE 'TYPE'.
          05 FILLER                     PIC X(11)      VALUE 'NUMBER'.
          05 FILLER                     PIC X(15)      VALUE 'NAME'.
          05 FILLER                     PIC X(13)      VALUE 'NAME'.
          05 FILLER                     PIC X(15)      VALUE 'DATE'.
          05 FILLER                     PIC X(6)       VALUE 'MET'.
          05 FILLER                     PIC X(10)      VALUE 'PERCENT'.
          05 FILLER                     PIC X(14)      VALUE 'AMOUNT'.
          05 FILLER                     PIC X(17)      VALUE 'AMOUNT'.
          05 FILLER                     PIC X(7)       VALUE 'PAID'.

       01 HEADING-LINE-FOUR.
          05 FILLER                     PIC X(23)      VALUE ALL '-'.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(10)      VALUE ALL '-'.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(14)      VALUE ALL '-'.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(12)      VALUE ALL '-'.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(13)      VALUE ALL '-'.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(5)       VALUE ALL '-'.
          05 FILLER                     PIC X(03)      VALUE SPACE.
          05 FILLER                     PIC X(7)       VALUE ALL '-'.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(07)      VALUE ALL '-'.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(15)      VALUE ALL '-'.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(14)      VALUE ALL '-'.

       01 DETAIL-LINE.
          05 DET-POLICY-TYPE            PIC X(20)      VALUE SPACES.
          05 FILLER                     PIC X(4)       VALUE SPACES.
          05 DET-POLICY-NO              PIC 9B999B99.
          05 FILLER                     PIC X(3)       VALUE SPACES.
          05 DET-NAME.
             10 DET-FIRST-NAME          PIC X(15).
             10 DET-LAST-NAME           PIC X(10).
          05 FILLER                     PIC X(3)       VALUE SPACES.
          05 DET-RENEW-DATE             PIC XXXX/XX/XX.
          05 FILLER                     PIC X(6)       VALUE SPACES.
          05 DET-DEDUCTIBLE-MET         PIC X.
          05 FILLER                     PIC X(5)       VALUE SPACES.
          05 DET-DEDUCTIBLE-PERC        PIC .999.
          05 FILLER                     PIC X(2)       VALUE SPACES.
      * Field ist too small for printing big input numbers
          05 DET-COINSURANCE            PIC $,$$9.99.
          05 FILLER                     PIC X(3)       VALUE SPACES.
          05 DET-CLAIM-AMOUNT           PIC $$,$$$,$$9.99.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 DET-CLAIM-PAID             PIC $$,$$$,$$9.99.
          05 FILLER                     PIC X(1)       VALUE SPACES.
      *
       01 TOTAL-DASH-LINE.
          05 FILLER                     PIC X(91)      VALUE SPACE.
          05 FILLER                     PIC X(09)      VALUE ALL '='.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(15)      VALUE ALL '='.
          05 FILLER                     PIC X(01)      VALUE SPACE.
          05 FILLER                     PIC X(14)      VALUE ALL '='.
       01 TOTAL-LINE-OUT.
          05 FILLER                     PIC X(86)      VALUE SPACES.
          05 TOT-DEDUCTIBLE-OUT         PIC $$,$$$,$$9.99.
          05 FILLER                     PIC X(2)       VALUE SPACES.
          05 TOT-CLAIM-AMOUNT-OUT       PIC $$$,$$$,$$9.99.
          05 FILLER                     PIC X          VALUE SPACES.
          05 TOT-CLAIM-AMOUNT-PAID-OUT  PIC $$$,$$$,$$9.99.

       01 ACCUMS-AND-COUNTERS.
          05 WS-REC-KTR                 PIC 9(7)       VALUE 0.
          05 WS-REC-KTR-VALID           PIC 9(7)       VALUE 0.



       PROCEDURE DIVISION.
       MAIN.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-MAIN UNTIL CLAIMFILE-EOF.
           PERFORM 700-WRITE-SUMMARY-REPORT.
           PERFORM 800-CLOSE-FILES.
           GOBACK.
       000-HOUSEKEEPING.
           DISPLAY '000-HOUSEKEEPING'.
           PERFORM 300-OPEN-FILES.
           PERFORM 050-SET-DATE.
      * Priming Read:
           PERFORM 400-READ-INPUT-FILE.
       050-SET-DATE.
      ************************************************************
      * determine current date and weekday for report header     *
      ************************************************************
           DISPLAY '050-SET-DATE'.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           ACCEPT CURRENT-WEEK-DAY FROM DAY-OF-WEEK.

           MOVE DT-OF-WK(CURRENT-WEEK-DAY) TO HDG-DAY.
           MOVE WS-CURRENT-DT-STRING TO HDG-DATE.

       100-MAIN.
      ************************************************************
      * Main Routine                                             *
      ************************************************************
           DISPLAY '100-MAIN'.

      * only one processing type in this program

           PERFORM 200-PROCESS-DATA.

       200-PROCESS-DATA.
      ************************************************************
      * Main Process                                             *
      ************************************************************
           DISPLAY '200-PROCESS-DATA'.
           DISPLAY "INPUT-REC: " CLAIM-RECORD-WS.

           PERFORM 220-VALIDATE-DATA.
           ADD 1 TO WS-REC-KTR.
      * Process valid Data....
           IF VALID-DATA-IND
              THEN
              ADD 1 TO WS-REC-KTR-VALID
              PERFORM 230-PROCESS-CLAIM
              PERFORM 500-WRITE-REPORT
              PERFORM 280-INCREMENT-TOTALS
              PERFORM 400-READ-INPUT-FILE
      * ...else read next input record
           ELSE
              PERFORM 400-READ-INPUT-FILE *> Read Next
           END-IF.
       220-VALIDATE-DATA.
      ***********************************************************
      * Data validation for every input field                   *
      *                                                         *
      ***********************************************************
           DISPLAY '220-VALIDATE-DATA'.

      * Validating Policy No: hast to be > ZERO

           IF INSURED-POLICY-NO EQUAL TO ZERO
              THEN
              DISPLAY "** Key Field Error! ** " INSURED-POLICY-NO

              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.

           IF INSURED-LAST-NAME EQUAL TO SPACES
              THEN
              DISPLAY "** Last Name is blank ** " INSURED-LAST-NAME

              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.

           IF INSURED-FIRST-NAME EQUAL TO SPACES
              THEN
              DISPLAY "** First Name is blank ** " INSURED-FIRST-NAME

              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.

      * Validating Policy Type: has to be defined valid type

           EVALUATE TRUE
           WHEN PRIVATE
                MOVE 'EMPLOYER-PRIVATE'
                   TO DET-POLICY-TYPE
           WHEN MEDICARE
                MOVE 'STANDARD MEDICARE'
                   TO DET-POLICY-TYPE
           WHEN AFFORDABLE-CARE
                MOVE 'AFFORDABLE CARE ACT'
                   TO DET-POLICY-TYPE
           WHEN OTHER
                DISPLAY "** Policy Type not valid! ** " DET-POLICY-TYPE
                MOVE "N" TO VALID-DATA-STATUS
                EXIT PARAGRAPH
           END-EVALUATE.     

      * Validating Policy Date Fields: have to be defined value

                IF POLICY-BENEFIT-DATE-X EQUAL TO SPACES
                   THEN
                   DISPLAY "** Policy Date is blank ** "

                   MOVE "N" TO VALID-DATA-STATUS
                   EXIT PARAGRAPH
                END-IF.


           IF POLICY-YEAR < 1950 OR > WS-CURRENT-YEAR
              THEN
              DISPLAY "** Policy Year not valid  ** "
                      POLICY-BENEFIT-DATE-X
              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.

           IF POLICY-MONTH < 1 OR > 12
              THEN
              DISPLAY "** Policy Month not valid  ** "
                      POLICY-BENEFIT-DATE-X
              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.

           IF POLICY-DAY < 1 OR > 31
              THEN
              DISPLAY "** Policy Day not valid  ** "
                      POLICY-BENEFIT-DATE-X

              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.


      * Validating Policy Amount: hast to be numeric

           IF POLICY-AMOUNT NOT NUMERIC
              THEN
              DISPLAY "** Policy Amount not valid  ** "
                      POLICY-AMOUNT
              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.

      * Validating Policy Deductible paid: has to be numeric


           IF POLICY-DEDUCTIBLE-PAID NOT NUMERIC

              THEN
              DISPLAY "** Policy Deductible not valid  ** "
                      POLICY-DEDUCTIBLE-PAID
              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.

      * Validating Policy Coinsurance Deductible paid:has to be numeric

           IF POLICY-COINSURANCE NOT NUMERIC

              THEN
              DISPLAY "** Policy Coinsurance not valid  ** "
                      POLICY-COINSURANCE
              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.


      * Consistency test between policy-coinsurance and paid amount

           IF POLICY-COINSURANCE EQUAL TO ZERO AND
              POLICY-DEDUCTIBLE-PAID GREATER THAN ZERO
              THEN
              DISPLAY "** Coinsurance Data inconsistent ** "
                      POLICY-COINSURANCE
                      POLICY-DEDUCTIBLE-PAID
              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.

      * Validating Claim Amount: hast to be Numeric, > 0, < Max

           IF CLAIM-AMOUNT NOT NUMERIC
              OR CLAIM-AMOUNT EQUAL TO ZERO
              OR CLAIM-AMOUNT > ALLOWED-AMT
              OR CLAIM-AMOUNT > POLICY-AMOUNT
              THEN
              DISPLAY "** Claim Amount not valid  ** "
                      CLAIM-AMOUNT
              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.

           IF CLAIM-AMOUNT-PAID NOT NUMERIC

              THEN
              DISPLAY "** Claim Amount Paid not valid  ** "
                      CLAIM-AMOUNT-PAID
              MOVE "N" TO VALID-DATA-STATUS
              EXIT PARAGRAPH
           END-IF.


           MOVE "Y" TO VALID-DATA-STATUS.

       230-PROCESS-CLAIM.
      ***********************************************************
      * Processing result from para 240-COMPUTE-DEDUCTIBLES:    *
      * company's deductible is always applied                  *
      * whereas policy deductible is only                       *
      * applied, if client hasn't                               *
      * already paid up to deductible amount in the past        *
      ***********************************************************

           DISPLAY '230-PROCESS-CLAIM'.

           PERFORM 240-COMPUTE-DEDUCTIBLES.

           IF NOT DEDUCTIBLE-MET
              THEN
              ADD DEDUCTIBLE-POLICY-WS TO
                 DEDUCTIBLE-WS
           END-IF.

           PERFORM 280-INCREMENT-TOTALS.
      *
       240-COMPUTE-DEDUCTIBLES.
      * **********************************************************
      * Computing deductions from                                *
      * 1. policy deduction percentage                           *
      * 2. company's deduction percentage                        *
      ************************************************************

           DISPLAY '240-COMPUTE-DEDUCTIBLES'.

      * computing policy deductible

           COMPUTE DEDUCTIBLE-POLICY-WS ROUNDED =
              POLICY-COINSURANCE * CLAIM-AMOUNT.

      * computing companys deductible, testing if applicable

           COMPUTE DEDUCTIBLE-WS ROUNDED =
              CLAIM-AMOUNT * DEDUCTIBLE-PERC.

           COMPUTE CLAIM-PAID-WS = CLAIM-AMOUNT
              - DEDUCTIBLE-WS.

           IF POLICY-DEDUCTIBLE-PAID >= DEDUCTIBLE-WS
              MOVE "Y" TO POLICY-DEDUCTIBLE-MET-WS
           ELSE
              MOVE "N" TO POLICY-DEDUCTIBLE-MET-WS
           END-IF.

       280-INCREMENT-TOTALS.
      ************************************************************
      * Incrementing totals for report footer                    *
      ************************************************************
           DISPLAY '280-INCREMENT-TOTALS'.

           ADD DEDUCTIBLE-WS TO TOT-DEDUCTIBLE-PAID
           SIZE ERROR
              DISPLAY 'SIZE ERROR ON TOTAL CLAIM'
           END-ADD.
           ADD CLAIM-AMOUNT TO TOT-CLAIM-AMOUNT
           SIZE ERROR
              DISPLAY 'SIZE ERROR ON TOTAL CLAIM'
           END-ADD.
           ADD CLAIM-PAID-WS TO TOT-CLAIM-AMOUNT-PAID
           SIZE ERROR
              DISPLAY 'SIZE ERROR ON TOTAL CLAIM PAID'
           END-ADD.

       300-OPEN-FILES.
      ************************************************************
      * Open files                                               *
      ************************************************************
           DISPLAY '300-OPEN-FILES'.
           OPEN INPUT CLAIMFILE.
           IF NOT CLAIMFILE-OK
              THEN
              DISPLAY 'Input File Problem!'
              GOBACK
           END-IF.
           OPEN OUTPUT PRINTFILE.
           IF NOT PRINTFILE-OK
              DISPLAY 'Print Report Problem!'
              GOBACK
           END-IF.
       400-READ-INPUT-FILE.
      ************************************************************
      * Read Input record                                        *
      ************************************************************
           DISPLAY '400-READ-INPUT-FILE'.
           READ CLAIMFILE INTO CLAIM-RECORD-WS
           AT END
              MOVE "Y" TO CLAIMFILE-STATUS
           END-READ.
           DISPLAY CLAIM-RECORD-WS.

       500-WRITE-REPORT.
      ************************************************************
      * Filling detail line, writing report file                 *
      ************************************************************
           DISPLAY '500-WRITE-REPORT'.
           IF LINE-COUNT IS GREATER THAN LINES-PER-PAGE
              THEN
              PERFORM 510-PAGE-CHANGE-RTN
           END-IF

      * Moving Fields unchanged from Input to report
           MOVE INSURED-POLICY-NO TO DET-POLICY-NO.
           INSPECT DET-POLICY-NO REPLACING ALL ' ' BY '-'.

           MOVE SPACES TO DET-NAME.
           MOVE INSURED-LAST-NAME TO DET-LAST-NAME.
           MOVE INSURED-FIRST-NAME TO DET-FIRST-NAME.
           MOVE POLICY-BENEFIT-DATE-X TO DET-RENEW-DATE.
           MOVE POLICY-DEDUCTIBLE-MET-WS TO DET-DEDUCTIBLE-MET.
           MOVE POLICY-COINSURANCE TO DET-DEDUCTIBLE-PERC.
           MOVE CLAIM-AMOUNT TO DET-CLAIM-AMOUNT.

      * Moving computed fields from Input to report
           MOVE DEDUCTIBLE-WS TO DET-COINSURANCE.
           MOVE CLAIM-PAID-WS TO DET-CLAIM-PAID.


           WRITE PRINT-LINE FROM DETAIL-LINE
              AFTER ADVANCING 2 LINES
           ADD 1 TO LINE-COUNT.
       510-PAGE-CHANGE-RTN.
      ************************************************************
      * Page Break Routine                                       *
      ************************************************************
           DISPLAY '510-PAGE-CHANGE-RTN'.
           IF PAGE-COUNT GREATER ZERO *> not before 1st heading printed
              THEN
              PERFORM 520-PAGE-FOOTING
           END-IF.
           MOVE +1 TO LINE-COUNT.
           ADD +1 TO PAGE-COUNT.
           MOVE PAGE-COUNT TO HDG-PAGE-NUMBER.
           WRITE PRINT-LINE FROM HEADING-LINE-ONE.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.
           WRITE PRINT-LINE FROM HEADING-LINE-TWO.
           WRITE PRINT-LINE FROM HEADING-LINE-THREE.
           WRITE PRINT-LINE FROM HEADING-LINE-FOUR.

       520-PAGE-FOOTING.
      ***********************************************************
      * Page Footing                                            *
      ***********************************************************
           DISPLAY '520-PAGE-FOOTING'.
      * no page footing in this report

       700-WRITE-SUMMARY-REPORT.
      ***********************************************************
      * Write summary at end of report                          *
      ***********************************************************
           DISPLAY '700-WRITE-SUMMARY'.
           WRITE PRINT-LINE FROM TOTAL-DASH-LINE
              AFTER ADVANCING 2 LINES.
           MOVE TOT-CLAIM-AMOUNT TO TOT-CLAIM-AMOUNT-OUT
           MOVE TOT-DEDUCTIBLE-PAID TO TOT-DEDUCTIBLE-OUT
           MOVE TOT-CLAIM-AMOUNT-PAID TO TOT-CLAIM-AMOUNT-PAID-OUT
           WRITE PRINT-LINE FROM TOTAL-LINE-OUT.

           DISPLAY "Records read / valid  "
           DISPLAY WS-REC-KTR WS-REC-KTR-VALID.
           PERFORM 520-PAGE-FOOTING.
       800-CLOSE-FILES.
      ***********************************************************
      * Close all Files                                         *
      ***********************************************************
           DISPLAY '800-CLOSE-FILES'.
           CLOSE CLAIMFILE
           IF NOT CLAIMFILE-OK
              THEN
              DISPLAY 'Input File Problem!'
              GOBACK
           END-IF.

           CLOSE PRINTFILE.
           IF NOT PRINTFILE-OK
              DISPLAY 'Print Report Problem!'
              GOBACK
           END-IF.





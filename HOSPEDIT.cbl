       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOSPEDIT.
      ******************************************************************
      * HOSPEDIT: Processing of Hospitals Patients Data
      * Workshop 16.2
      * Program reads Input File with weekly patient data until EOF.
      * Input Fields: Patient Type and Insurance Type are tested for
      * valid Data.
      *
      * Valid Records are written to an Output File, invalid
      * Records are written to an Errorfile
      * A Report is written with a detail line for each valid records
      * and some statistics at end.
      *
      * initial Version HOSPEDIT copied
      * from DDS0001:                    08/06/2020 Ralf Straube
      * Additions WS 16.2 a to c,
      * Redesign/Restructuring:          08/12/2020 Ralf Straube
      *
      *
      ******************************************************************
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEV Center.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE
           ASSIGN TO HOSPIN
             FILE STATUS IS IFCODE.

           SELECT RPTFILE
           ASSIGN TO RPTFILE
             FILE STATUS IS RFCODE.

           SELECT OUTFILE
           ASSIGN TO HOSPOUT
             FILE STATUS IS OFCODE.

           SELECT ERRFILE
           ASSIGN TO ERRFILE
             FILE STATUS IS EFCODE.

      ****** New SELECT/ASSIGN for Insurance Type File
      * FD    Also - you should create another FILE STATUS
      **      Variable for this file in Working Storage
      * R.S.
      ******
           SELECT INSTYPE
           ASSIGN TO INSTYPE
             FILE STATUS IS ITCODE.



       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS IN-REC.
       01 IN-REC                    PIC X(100).

       FD  OUTFILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS OUT-REC.
       01 OUT-REC                   PIC X(133).

       FD  ERRFILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS ERR-REC.
       01 ERR-REC                   PIC X(133).

       FD  RPTFILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS RPT-REC.
       01 RPT-REC                   PIC X(133).

      ****** New Input File FD for Insurance Type Records
      * FD  INSTYPE
      * R.S.

       FD  INSTYPE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.

       01 INS-REC                   PIC X(80).

       WORKING-STORAGE SECTION.

       01 FILE-STATUS-CODES.
          05 IFCODE                 PIC X(2).
             88 CODE-READ                            VALUE SPACES.
             88 NO-MORE-DATA                         VALUE "10".
             88 STATUS-OK                            VALUE "00".
          05 OFCODE                 PIC X(2).
             88 CODE-WRITE                           VALUE SPACES.
             88 STATUS-OK                            VALUE "00".
          05 EFCODE                 PIC X(2).
             88 CODE-WRITE                           VALUE SPACES.
             88 STATUS-OK                            VALUE "00".
          05 RFCODE                 PIC X(2).
             88 CODE-WRITE                           VALUE SPACES.
             88 STATUS-OK                            VALUE "00".
          05 ITCODE                 PIC X(2).
             88 CODE-READ                            VALUE SPACES.
             88 NO-MORE-DATA                         VALUE "10".
             88 STATUS-OK                            VALUE "00".


       77 INS-COVERAGE-PERC         PIC 9(3)         VALUE 10.

       01 WS-OUTPUT-REC.
          05 PATIENT-NBR-O          PIC 9(5).
          05 FILLER                 PIC X(2)         VALUE SPACES.
          05 PATIENT-NAME-O         PIC X(22).
          05 FILLER                 PIC X(2)         VALUE SPACES.
          05 PATIENT-PHONE-O        PIC X(13).
          05 FILLER                 PIC X(2)         VALUE SPACES.
          05 PATIENT-TYPE-O         PIC X(2).
          05 FILLER                 PIC X(3)         VALUE SPACES.
          05 BED-IDENTITY-O         PIC ZZZ9.
          05 FILLER                 PIC X(2)         VALUE SPACES.
          05 CURR-DATE-O            PIC X(6).
          05 FILLER                 PIC X(2)         VALUE SPACES.
          05 PATIENT-AMT-PER-DAY-O  PIC $$,$$9.99.
          05 FILLER                 PIC X(2)         VALUE SPACES.
          05 DIAG-CODE-O            PIC 999.
          05 FILLER                 PIC X(3)         VALUE SPACES.
          05 INS-TYPE-O             PIC X(4).
          05 FILLER                 PIC X(3)         VALUE SPACES.
          05 HOSPITAL-STAY-LTH-O    PIC 999.
          05 FILLER                 PIC X(2)         VALUE SPACES.
          05 NETWORK-O              PIC X(7).
          05 FILLER                 PIC X(3)         VALUE SPACE.
          05 COPAY-O                PIC ZZZ9.
          05 FILLER                 PIC X(4)         VALUE SPACE.
          05 DEDUCT-O               PIC ZZZ9.
          05 FILLER                 PIC X(1)         VALUE SPACE.

       01 WS-OUTPUT-HDR-1.
          05 PATIENT-NBR-H          PIC X(5)         VALUE "PATNO".
          05 FILLER                 PIC X(2)         VALUE SPACES.
          05 PATIENT-NAME-H         PIC X(22)        VALUE "PATIENT".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 PATIENT-PHONE-H        PIC X(13)        VALUE "PHONE-NO".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 PATIENT-TYPE-H         PIC X(4)         VALUE "TYPE".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 BED-IDENTITY-H         PIC X(3)         VALUE "BED".
          05 FILLER                 PIC X(2)         VALUE SPACES.
          05 CURR-DATE-H            PIC X(6)         VALUE "ADMIT".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 PATIENT-AMT-PER-DAY-H  PIC X(9)         VALUE "DAILY AMT".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 DIAG-CODE-H            PIC X(4)         VALUE "DIAG".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 INS-TYPE-H             PIC X(4)         VALUE "INS".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 HOSPITAL-STAY-LTH-H    PIC X(4)         VALUE "STAY".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 NETWORK-H              PIC X(7)         VALUE "NETWORK".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 COPAY-H                PIC X(5)         VALUE "COPAY".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 DEDUCT-H               PIC X(6)         VALUE "DEDUCT".
          05 FILLER                 PIC X(2)         VALUE SPACE.

       01 WS-OUTPUT-HDR-2.
          05 FILLER                 PIC X(5)         VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACES.
          05 FILLER                 PIC X(22)        VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 FILLER                 PIC X(13)        VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 FILLER                 PIC X(4)         VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 FILLER                 PIC X(3)         VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACES.
          05 FILLER                 PIC X(6)         VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 FILLER                 PIC X(9)         VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 FILLER                 PIC X(4)         VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 FILLER                 PIC X(4)         VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 FILLER                 PIC X(4)         VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 FILLER                 PIC X(7)         VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 FILLER                 PIC X(5)         VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACE.
          05 FILLER                 PIC X(6)         VALUE ALL "=".
          05 FILLER                 PIC X(2)         VALUE SPACE.

       01 WS-TOTALS-REC-10.
          05 FILLER                 PIC X(14)        VALUE "Rec in: ".
          05 READ-OUT               PIC Z(4).
       01 WS-TOTALS-REC-11.
          05 FILLER                 PIC X(15)        VALUE
                "Rec written: ".
          05 WRITTEN-OUT            PIC Z(3).
       01 WS-TOTALS-REC-2.
          05 FILLER                 PIC X(15)        VALUE
                "Rec Errors: ".
          05 ERRORS-OUT             PIC Z(3).
       01 WS-TOTALS-REC-3.
          05 FILLER                 PIC X(15)        VALUE "Inpat: ".
          05 INPATIENTS-OUT         PIC Z(3).
       01 WS-TOTALS-REC-4.
          05 FILLER                 PIC X(15)        VALUE "Outpat:".
          05 OUTPATIENTS-OUT        PIC Z(3).
       01 WS-TOTALS-REC-51.
          05 FILLER                 PIC X(14)        VALUE "HMO: ".
          05 HMO-OUT                PIC Z(4).
       01 WS-TOTALS-REC-52.
          05 FILLER                 PIC X(14)        VALUE "PPO: ".
          05 PPO-OUT                PIC Z(4).
       01 WS-TOTALS-REC-53.
          05 FILLER                 PIC X(14)        VALUE "MED: ".
          05 MED-OUT                PIC Z(4).
       01 WS-TOTALS-REC-54.
          05 FILLER                 PIC X(14)        VALUE "PRI: ".
          05 PRI-OUT                PIC Z(4).
       01 WS-TOTALS-REC-55.
          05 FILLER                 PIC X(14)        VALUE "AFF: ".
          05 AFF-OUT                PIC Z(4).
       01 WS-TOTALS-REC-6.
          05 FILLER                 PIC X(15)        VALUE "No Cov: ".
          05 NO-COVERAGE-OUT        PIC ZZ9.
       01 WS-TOTALS-REC-7.
          05 FILLER                 PIC X(15)        VALUE "GROSS: ".
          05 TOTAL-GROSS-OUT        PIC $,$$$,$99.99.

       77 WS-DATE                   PIC 9(6).
       77 MORE-RECORDS-SW           PIC X(1)         VALUE SPACE.
       88 NO-MORE-RECORDS                            VALUE 'N'.
      *77  VALID-INS-TYPE   -- create a one-byte switch

       01 COUNTERS-AND-ACCUMULATORS.
          05 RECORDS-READ           PIC S9(4) COMP.
          05 RECORDS-WRITTEN        PIC S9(4) COMP.
          05 ERROR-RECS             PIC S9(4) COMP.
          05 NBR-INPATIENTS         PIC S9(4) COMP.
          05 NBR-OUTPATIENTS        PIC S9(4) COMP.
          05 NBR-HMO                PIC S9(4) COMP.
          05 NBR-STATE-FED          PIC S9(4) COMP.
          05 NBR-NO-COVERAGE        PIC S9(4) COMP.
          05 NBR-PPO                PIC S9(4) COMP.
          05 NBR-PRIVATE            PIC S9(4) COMP.
          05 NBR-AFFORDABLE         PIC S9(4) COMP.
          05 PAT-TOTAL-AMT-NET      PIC S9(7)V99 COMP-3.
          05 TOTAL-AMT-GROSS        PIC S9(7)V99 COMP-3.
          05 TOTAL-AMT-NET          PIC S9(7)V99 COMP-3.
          05 GROSS-TOTAL            PIC S9(7)V99 COMP-3.

      * Input Record (COPYBOOK)

       COPY HOSPINF.

      *****************************************************************
      **   1. A Working-Storage Table to store the 5 new INS-TYPE
      **      record - values that will be loaded from an input file
      **      You will need to create the input file
      **   2. Note the 88-level condition names in the table
      **      You will see/code references to these in paragraph 100
      **   3. Do you need to do anything with COBUCLD / COBUCLG ??
      *****************************************************************
       01 INS-TYPE-TABLE.
          05 INS-TYPE-ITEM OCCURS 50 TIMES
                DEPENDING ON T-IDX-MAX
                INDEXED BY T-IDX    PIC X(3).
                                                                        X

      * R.S. added new WS-Variables
       01 PGM-VARS-DIVERS.
          05 T-IDX-MAX              PIC 999 COMP     VALUE 1.
          05 WS-INS-TYPE            PIC X(3).
             88 HMO                                  VALUE 'HMO'.
             88 PRIVATE                              VALUE 'PRI'.
             88 PPO                                  VALUE 'PPO'.
             88 AFFORDABLE                           VALUE 'AFF'.
             88 MEDICARE                             VALUE 'MED'.
          05 WS-VALID-INS-TYPE-IND  PIC X(1).
             88 VALID-INS-TYPE                       VALUE "Y".


       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
              UNTIL NO-MORE-RECORDS.
           PERFORM 800-CLEANUP THRU 800-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
      ****************************************************************
      ***  Program initialization: Open files,
      ***  load table INS-TYPE-ITEM from Input file INSTYPE          *
      ****************************************************************
           DISPLAY "000-HOUSEKEEPING".

      *  Code your statement here to OPEN files

           PERFORM 070-OPEN-FILES THRU 070-EXIT.

           ACCEPT WS-DATE FROM DATE.


           INITIALIZE COUNTERS-AND-ACCUMULATORS,
                      WS-OUTPUT-REC.


      * Priming Read

           PERFORM 400-READ-INPUT-FILE THRU 400-EXIT.

      * Load  table of valid Insurance Types from JCL
      * Instream File

           PERFORM 050-LOAD-INS-TYPE-TABLE THRU 050-EXIT.

      * writing report header once (no page break logic provided)

           WRITE RPT-REC FROM WS-OUTPUT-HDR-1


           WRITE RPT-REC FROM WS-OUTPUT-HDR-2
              AFTER ADVANCING 2 LINES.

       000-EXIT.
           EXIT.
       100-MAINLINE.
      ****************************************************************
      ***  Main Processing Routine
      ***  PERFORMS Routines for Data Validation, Accumulation,
      ***  Report Writing and Reading Next Record
      ****************************************************************
           DISPLAY "100-MAINLINE".

           ADD +1 TO RECORDS-READ.

      *****
      **   Find if the input record contains a value INS-TYPE
      *****
      **   Code a COBOL SEARCH statement comparing INS-TYPE to the
      **     table values read in from the file (INS-TYPE-ITEM)
      **   When found move 'Y' to the VALID-INS-TYPE variable
      * R.S. coded as follows:
      *****
           PERFORM 220-DATA-VALIDATION THRU 220-EXIT.

      * Process only valid records


           IF VALID-TYPE AND VALID-INS-TYPE
              THEN
              PERFORM 300-WRITE-OUTFILE THRU 300-EXIT
              PERFORM 500-ACCUMULATE-COUNTERS THRU 500-EXIT
              PERFORM 510-WRITE-REPORT THRU 510-EXIT
           ELSE
              PERFORM 310-WRITE-ERRORFILE THRU 310-EXIT
           END-IF.
      * Read Next Record
           PERFORM 400-READ-INPUT-FILE THRU 400-EXIT.
       100-EXIT.
           EXIT.
       050-LOAD-INS-TYPE-TABLE.
      ****************************************************************
      *  Add an inline PERFORM to read the INS-TYPE-FILE and Load the
      *  values into the WORKING-STORAGE table (INS-TYPE-TABLE)
      *  R.S. coded as follows:
      *
      ****************************************************************
           DISPLAY "050-LOAD-INS-TYPE-TABLE".

           READ INSTYPE INTO WS-INS-TYPE.
           PERFORM VARYING T-IDX FROM 1 BY 1 UNTIL
              NO-MORE-DATA OF ITCODE
                   MOVE WS-INS-TYPE TO INS-TYPE-ITEM(T-IDX)
                   ADD 1 TO T-IDX-MAX
                   READ INSTYPE INTO WS-INS-TYPE
           END-PERFORM.

       050-EXIT.
           EXIT.
       070-OPEN-FILES.
      ****************************************************************
      * Open all Files, testing Files Status
      *
      ****************************************************************
           DISPLAY "070-OPEN-FILES".


           OPEN INPUT INFILE.
           IF NOT STATUS-OK OF IFCODE
              THEN
              DISPLAY 'Input File Problem!'
              GOBACK
           END-IF.

           OPEN OUTPUT OUTFILE.
           IF NOT STATUS-OK OF OFCODE
              THEN
              DISPLAY 'Output File Problem!'
              GOBACK
           END-IF.
           OPEN OUTPUT RPTFILE.
           IF NOT STATUS-OK OF RFCODE
              THEN
              DISPLAY 'Report File Problem!'
              GOBACK
           END-IF.
           OPEN OUTPUT ERRFILE.
           IF NOT STATUS-OK OF EFCODE
              THEN
              DISPLAY 'Error Record File Problem!'
              GOBACK
           END-IF.

      *****
      **   Add an open input for INSFILE
      * R.S. coded as follows:
      *****

           OPEN INPUT INSTYPE.
           IF NOT STATUS-OK OF ITCODE
              THEN
              DISPLAY 'Insurance Type File Problem!'
              GOBACK
           END-IF.

       070-EXIT.
           EXIT.
       220-DATA-VALIDATION.
      ****************************************************************
      ***  Data Validation Routine - only for one field:
      ***  tests Insurance Type from Input Records against Table
      ***  INS-TYPE-ITEM
      ****************************************************************
           DISPLAY "220-DATA-VALIDATION".

           SET T-IDX TO 1
           SEARCH INS-TYPE-ITEM
           AT END
              MOVE "N" TO WS-VALID-INS-TYPE-IND
           WHEN INS-TYPE-ITEM(T-IDX) = INS-TYPE
                MOVE "Y" TO WS-VALID-INS-TYPE-IND
           END-SEARCH.

           DISPLAY INS-TYPE "/" INS-TYPE-ITEM(T-IDX)
           DISPLAY WS-VALID-INS-TYPE-IND "/" PATIENT-TYPE.


       220-EXIT.
           EXIT.
       300-WRITE-OUTFILE.
      ****************************************************************
      ***  Writes Valid Records in Output File
      *
      ****************************************************************
           DISPLAY "300-WRITE-OUTFILE".

           MOVE WS-INPUT-REC TO OUT-REC
           WRITE OUT-REC.
           ADD +1 TO RECORDS-WRITTEN.
       300-EXIT.
           EXIT.
       310-WRITE-ERRORFILE.
      ****************************************************************
      ***  Writes Invalid Records to Error Files, counts them
      ****************************************************************
           DISPLAY "310-WRITE-ERRORFILE".
           MOVE WS-INPUT-REC TO ERR-REC.
           WRITE ERR-REC.
           ADD +1 TO ERROR-RECS.

       310-EXIT.
           EXIT.
       400-READ-INPUT-FILE.
      ****************************************************************
      ***  Reads a Record from Input file, does EOF Processing and
      ***  record counting
      ***
      ****************************************************************
           DISPLAY "400-READ-INPUT-FILE".
           READ INFILE INTO WS-INPUT-REC
           AT END
              MOVE "N" TO MORE-RECORDS-SW
           END-READ.

           DISPLAY WS-INPUT-REC.

       400-EXIT.
           EXIT.
       500-ACCUMULATE-COUNTERS.
      ****************************************************************
      ***  Accumulate all counters, partially depending on
      ***  case selections
      ****************************************************************
           DISPLAY "500-ACCUMULATE-COUNTERS".
      ****************************************************************
      *****  Inline Perform to add to report totals
      ****************************************************************
      * Q:This is an example of where a PERFORM VARYING might be a
      *   better design choice than a SEARCH.  Why?
      *
      * A: Because action has to be taken for each key in the table
      * it's better to use a PERFORM VARYING than a search.
      * All processing can be done by one sequential table scan instead
      * of mutiple scans when using SEARCH
      * **************************************************************
      * R.S. PERFORM VARYING eliminated. The counting of different
      * INS-TYPES depends on input record's value, it
      * does not depend on table items
      * ***************************************************************

           MOVE INS-TYPE TO WS-INS-TYPE.
           EVALUATE TRUE
           WHEN HMO
                ADD +1 TO NBR-HMO
           WHEN
              MEDICARE
                ADD +1 TO NBR-STATE-FED
           WHEN
              AFFORDABLE
                ADD +1 TO NBR-AFFORDABLE
           WHEN
              PPO
                ADD +1 TO NBR-PPO
           WHEN PRIVATE
                ADD +1 TO NBR-PRIVATE
           WHEN OTHER
                ADD +1 TO NBR-NO-COVERAGE
           END-EVALUATE.

           IF INPATIENT
              ADD +1 TO NBR-INPATIENTS
           ELSE
              ADD +1 TO NBR-OUTPATIENTS
           END-IF

           COMPUTE PAT-TOTAL-AMT-NET =
              (PATIENT-TOT-AMT +
              AMT-PER-DAY *((100 - INS-COVERAGE-PERC) / 100))
           END-COMPUTE

           ADD PAT-TOTAL-AMT-NET TO TOTAL-AMT-NET.
           ADD PATIENT-TOT-AMT TO TOTAL-AMT-GROSS.
       500-EXIT.
           EXIT.
       510-WRITE-REPORT.
      ****************************************************************
      ***  Data Validation Routine - only for one field:
      ***  tests Insurance Type from Input Records against Table
      ***  INS-TYPE-ITEM - No Page Break Processing
      ****************************************************************
           DISPLAY "510-WRITE-REPORT".

           MOVE PATIENT-NBR TO PATIENT-NBR-O.

           STRING
              PATIENT-NAME(1:10) ' ' PATIENT-NAME(11:10)
              DELIMITED BY SIZE
              INTO PATIENT-NAME-O
           END-STRING.


           STRING
              '(' PATIENT-PHONE(1:3) ')' PATIENT-PHONE(4:3) '-'
              PATIENT-PHONE(7:4)
              DELIMITED BY SIZE
              INTO PATIENT-PHONE-O
           END-STRING.



           MOVE PATIENT-TYPE TO PATIENT-TYPE-O.
           MOVE WS-DATE TO CURR-DATE-O.
           MOVE BED-IDENTITY TO BED-IDENTITY-O.
           ADD PAT-TOTAL-AMT-NET TO PATIENT-TOT-AMT
              GIVING PATIENT-AMT-PER-DAY-O.
           MOVE DIAGNOSTIC-CODE TO DIAG-CODE-O.
           MOVE INS-TYPE TO INS-TYPE-O.


           ADD +1 TO HOSPITAL-STAY-LTH
              GIVING HOSPITAL-STAY-LTH-O.

           DISPLAY "IN/OUT NETWORK: " IN-OUT-NETWORK

           IF IN-NETWORK THEN
              MOVE "IN" TO NETWORK-O
           ELSE
              MOVE "OUT" TO NETWORK-O
           END-IF.

           MOVE COPAY TO COPAY-O.
           MOVE DEDUCTIBLE TO DEDUCT-O.



           WRITE RPT-REC FROM WS-OUTPUT-REC.

       510-EXIT.
           EXIT.
       800-CLEANUP.
      ****************************************************************
      ***  Cleanup routine writes out accumulators at end of report.
      ***
      ****************************************************************
           DISPLAY "800-CLEANUP".


           MOVE RECORDS-READ TO READ-OUT.
           MOVE RECORDS-WRITTEN TO WRITTEN-OUT.
           MOVE ERROR-RECS TO ERRORS-OUT.
           MOVE NBR-INPATIENTS TO INPATIENTS-OUT.
           MOVE NBR-OUTPATIENTS TO OUTPATIENTS-OUT.
           MOVE NBR-HMO TO HMO-OUT.
           MOVE NBR-PPO TO PPO-OUT.
           MOVE NBR-PRIVATE TO PRI-OUT.
           MOVE NBR-AFFORDABLE TO AFF-OUT.
           MOVE NBR-STATE-FED TO MED-OUT.

           MOVE NBR-NO-COVERAGE TO NO-COVERAGE-OUT.
           MOVE TOTAL-AMT-GROSS TO TOTAL-GROSS-OUT.


           WRITE RPT-REC FROM WS-TOTALS-REC-10
              AFTER ADVANCING 4 LINES.
           WRITE RPT-REC FROM WS-TOTALS-REC-11
              AFTER ADVANCING 2 LINES.

           WRITE RPT-REC FROM WS-TOTALS-REC-2
              AFTER ADVANCING 2 LINES.

           WRITE RPT-REC FROM WS-TOTALS-REC-3
              AFTER ADVANCING 2 LINES.

           WRITE RPT-REC FROM WS-TOTALS-REC-4
              AFTER ADVANCING 2 LINES.

           WRITE RPT-REC FROM WS-TOTALS-REC-51
              AFTER ADVANCING 2 LINES.

           WRITE RPT-REC FROM WS-TOTALS-REC-52
              AFTER ADVANCING 2 LINES.

           WRITE RPT-REC FROM WS-TOTALS-REC-53
              AFTER ADVANCING 2 LINES.

           WRITE RPT-REC FROM WS-TOTALS-REC-54
              AFTER ADVANCING 2 LINES.

           WRITE RPT-REC FROM WS-TOTALS-REC-55
              AFTER ADVANCING 2 LINES.

           WRITE RPT-REC FROM WS-TOTALS-REC-6
              AFTER ADVANCING 2 LINES.

           WRITE RPT-REC FROM WS-TOTALS-REC-7
              AFTER ADVANCING 4 LINES.


           PERFORM 850-CLOSE-FILES THRU 850-EXIT.


           DISPLAY "NORMAL END OF JOB".
       800-EXIT.
           EXIT.
       850-CLOSE-FILES.
      ****************************************************************
      ***  Close all Files, testing File Status
      ***
      ****************************************************************
           DISPLAY "850-CLOSE-FILES".


      *     CLOSE OUTFILE, RPTFILE, ERRFILE, INFILE, INSTYPE.

           CLOSE INFILE.
           IF NOT STATUS-OK OF IFCODE
              THEN
              DISPLAY 'Input File Problem!'
              GOBACK
           END-IF.

           CLOSE OUTFILE.
           IF NOT STATUS-OK OF OFCODE
              THEN
              DISPLAY 'Output File Problem!'
              GOBACK
           END-IF.

           CLOSE RPTFILE.
           IF NOT STATUS-OK OF RFCODE
              THEN
              DISPLAY 'Report File Problem!'
              GOBACK
           END-IF.

           CLOSE ERRFILE.
           IF NOT STATUS-OK OF EFCODE
              THEN
              DISPLAY 'Error Record File Problem!'
              GOBACK
           END-IF.

           CLOSE INSTYPE.
           IF NOT STATUS-OK OF ITCODE
              THEN
              DISPLAY 'Insurance Type File Problem!'
              GOBACK
           END-IF.

       850-EXIT.
           EXIT.

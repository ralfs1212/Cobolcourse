       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRPT.
      * This programm reads input file until eof, computes
      * total-,average-
      * min- and max cost,  counts records read and
      * writes a report

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FAVIN  ASSIGN TO FAVIN.
           SELECT FAVOUT ASSIGN TO PRTLINE.
       DATA DIVISION.
       FILE SECTION.
       FD  FAVIN
           RECORDING MODE IS F
           DATA RECORD IS FAV-REC.
      * no declaration of record variable in WS necessary
       01 FAV-REC.
          05 ARTIST-NAME                PIC X(30).
          05 NUMBER-OF-MUSICIANS        PIC 9(2).
          05 MUSICAL-GENRE              PIC X(12).
          05 COST.
             10 CD-COST                 PIC 9(3)V99.
             10 SHIPPING-COST           PIC 9(2)V99.
             10 TAX                     PIC 9(2)V99.
          05 BAND-STATUS                PIC X(1).
             88 BAND-IS-STILL-TOGETHER                VALUE "Y".
             88 BAND-IS-DIVORCED                      VALUE "N".
          05 FILLER                     PIC X(22).

       FD  FAVOUT
           RECORDING MODE IS F
           DATA RECORD IS FAV-RPT.
      *  no declaration of record variable in WS necessary
       01 FAV-RPT.
          05 ARTIST-NAME-OUT            PIC X(30).
          05 FILLER                     PIC X(1).
          05 NUMBER-OF-MUSICIANS-OUT    PIC Z9.
          05 FILLER                     PIC X(1).
          05 MUSICAL-GENRE-OUT          PIC X(12).
          05 FILLER                     PIC X(1).
          05 COST.
             10 CD-COST-OUT             PIC $$$.99.
             10 FILLER                  PIC X(1).
             10 SHIPPING-COST-OUT       PIC $$.99.
             10 FILLER                  PIC X(1).
             10 TAX-OUT                 PIC Z9.99.
             10 FILLER                  PIC X(1).
          05 BAND-STATUS-OUT            PIC X(1).

       WORKING-STORAGE SECTION.
      * EOF indicator
       01 FAVIN-STATUS                  PIC X(01)     VALUE SPACE.
          88 FAVIN-EOF                                VALUE 'Y'.
      * Extensions Workshop 7.2
       77 REC-KTR                       PIC 99        VALUE ZERO.
       77 COST-TOTAL                    PIC 9(5)V99   VALUE ZERO.
       77 REC-KTR-OUT                   PIC Z9.
       77 COST-TOTAL-OUT                PIC $99999.99.
       77 AVG-COST                      PIC 9(3)V99   VALUE ZERO.
       77 AVG-COST-OUT                  PIC $999.99.
      * Optional fields für minimum / maximum cost
       77 MIN-COST                      PIC 999V99    VALUE 999.99.
       77 MIN-COST-OUT                  PIC $999.99.
       77 MAX-COST                      PIC 999V99    VALUE ZERO.
       77 MAX-COST-OUT                  PIC $999.99.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-MAIN UNTIL FAVIN-EOF
           PERFORM 600-CLOSE-FILES.
      * Extensions Workshop 7.2
           PERFORM 700-WRITE-REPORT.
           GOBACK.
       000-HOUSEKEEPING.
      * Priming Read
           PERFORM 300-OPEN-FILES.
      *
      * Initialization of Data Records
      * must take place after opening files to avoid S0C4
      *
           INITIALIZE FAV-REC, FAV-RPT.
           PERFORM 400-READ-FAVIN.
       100-MAIN.
           DISPLAY '100-MAIN'.
           DISPLAY "FAV-REC: " FAV-REC.
           PERFORM 200-PROCESS-DATA.
           PERFORM 500-WRITE-FAVOUT.
           PERFORM 400-READ-FAVIN.
       200-PROCESS-DATA.
           COMPUTE REC-KTR = REC-KTR + 1.

           COMPUTE CD-COST =
              CD-COST + CD-COST * TAX + SHIPPING-COST.

           COMPUTE COST-TOTAL = COST-TOTAL + CD-COST.

           IF CD-COST LESS THAN MIN-COST THEN
              MOVE CD-COST TO MIN-COST.

           IF CD-COST GREATER THAN MAX-COST THEN
              MOVE CD-COST TO MAX-COST.

      *  assigning field by field for numeric editing
           MOVE ARTIST-NAME TO ARTIST-NAME-OUT.
           MOVE NUMBER-OF-MUSICIANS TO NUMBER-OF-MUSICIANS-OUT.
           MOVE MUSICAL-GENRE TO MUSICAL-GENRE-OUT.
           MOVE CD-COST TO CD-COST-OUT.
           MOVE SHIPPING-COST TO SHIPPING-COST-OUT
           MOVE TAX TO TAX-OUT.
           MOVE BAND-STATUS TO BAND-STATUS-OUT.


           MOVE REC-KTR TO REC-KTR-OUT.


       300-OPEN-FILES.
           OPEN INPUT FAVIN.
           OPEN OUTPUT FAVOUT.
       400-READ-FAVIN.
           DISPLAY 'READ FAVIN'.
           READ FAVIN
      * Set AT END Switch
           AT END
              MOVE "Y" TO FAVIN-STATUS
           END-READ.

       500-WRITE-FAVOUT.
           DISPLAY 'WRITE FAVOUT'.
           WRITE FAV-RPT.
       600-CLOSE-FILES.
           CLOSE FAVIN, FAVOUT.
       700-WRITE-REPORT.
           MOVE REC-KTR TO REC-KTR-OUT.
           MOVE COST-TOTAL TO COST-TOTAL-OUT.
           MOVE MIN-COST TO MIN-COST-OUT.
           MOVE MAX-COST TO MAX-COST-OUT.

           IF REC-KTR GREATER ZERO THEN
              DIVIDE COST-TOTAL BY REC-KTR GIVING AVG-COST.

           MOVE AVG-COST TO AVG-COST-OUT.

           DISPLAY "Number of Records processed: " REC-KTR-OUT.

           DISPLAY "Total Cost: " COST-TOTAL-OUT.
           DISPLAY "Avg. Cost: " AVG-COST-OUT.
           DISPLAY "Min. Cost: " MIN-COST-OUT.
           DISPLAY "Max. Cost: " MAX-COST-OUT.

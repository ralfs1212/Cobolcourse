       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILECALC.
      * This program reads a file of input values into INVALS-WS
      * The operation read into the W-S structure drives the arithmetic
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVALS
           ASSIGN TO UT-S-INVALS
             ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INVALS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INVALS-REC.
       01 INVALS-REC                   PIC X(80).
       WORKING-STORAGE SECTION.
      * End of File switch
       01 INVALS-EOF                   PIC X(01)     VALUE SPACE.
       01 INVALS-WS.
          05 OPERATION                 PIC X(01).
      * 88-Level variables adde R.S.
             88 OPERATION-ADD                        VALUE "A".
             88 OPERATION-SUBTRACT                   VALUE "S".
             88 OPERATION-MULTIPLY                   VALUE "M".
             88 OPERATION-DIVIDE                     VALUE "D".
             88 OPERATION-SQUARE-ROOT                VALUE "R".
          05 INVALS-1                  PIC S99V99.
          05 INVALS-2                  PIC S99.
          05 INVALS-RESULT             PIC S99999V99.
          05 RESULT-FLD-EDIT           PIC 99999.99-.

       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-MAIN UNTIL INVALS-EOF = 'Y'.
           PERFORM 900-CLOSE-FILES.
           GOBACK.
       000-HOUSEKEEPING.
           INITIALIZE INVALS-WS.
           PERFORM 300-OPEN-FILES.
      * Priming Read
           PERFORM 400-READ-INVALS.
       100-MAIN.
           IF OPERATION-ADD
              PERFORM 500-ADD
           ELSE
              IF OPERATION-SUBTRACT
                 PERFORM 600-SUBTRACT
              ELSE
                 IF OPERATION-MULTIPLY
                    PERFORM 700-MULTIPLY
                 ELSE
      * Procedure 750-SQUARE-ROOT added R.S.
                    IF OPERATION-SQUARE-ROOT
                       PERFORM 750-SQUARE-ROOT
                    ELSE
                       IF OPERATION-DIVIDE
                          PERFORM 800-DIVIDE
      * Otherwise no arithmetics
                       ELSE
                          INITIALIZE INVALS-WS.


           MOVE INVALS-RESULT TO RESULT-FLD-EDIT.
           DISPLAY RESULT-FLD-EDIT.
           PERFORM 400-READ-INVALS.
       300-OPEN-FILES.
           OPEN INPUT INVALS.
       400-READ-INVALS.
           READ INVALS INTO INVALS-WS
      * Set AT END Switch
           AT END
              MOVE "Y" TO INVALS-EOF
           END-READ.
       500-ADD.
           ADD INVALS-1, INVALS-2 GIVING INVALS-RESULT.
       600-SUBTRACT.
           SUBTRACT INVALS-2 FROM INVALS-1 GIVING INVALS-RESULT.
       700-MULTIPLY.
           MULTIPLY INVALS-1 BY INVALS-2 GIVING INVALS-RESULT.
      * Procedure 750-SQUARE-ROOT added R.S.
       750-SQUARE-ROOT.
           COMPUTE INVALS-RESULT = FUNCTION SQRT(INVALS-2).
       800-DIVIDE.
           DIVIDE INVALS-2 BY INVALS-1 GIVING INVALS-RESULT.
       900-CLOSE-FILES.
           CLOSE INVALS.
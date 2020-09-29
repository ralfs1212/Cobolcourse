       IDENTIFICATION DIVISION.
       PROGRAM-ID. POEDIT.
      *** ------------------------------------------------------------
      *** Final Exam Subprogram Program POEDIT - Program Stub
      *** Called by PARTSUPP (Main)
      *** Check the passed PURCHORD-STRUCTURE data from the MAIM-PGM
      *** and send a  return code back to the MAIN.
      *** Return Code = 0 --> no error
      *** Return Code = 8 --> error, details are stored in the
      ***                     error structure
      *** General: only a maximum of four errors has to be processed.
      ***          If ERROR-SUB-MAX, used as subscript to ERROR-TABLE,
      ***          gets greater than four, all processing can be
      ***          stopped and control can be returned to calling
      ***          Program. ERROR-SUB-MAX has always to be observed,
      ***          because it may already have a value greater than
      ***          zero - but less than four - when entering the Pgm.
      ***
      *** created 05/09/2020                  Hilde Schmidbauer
      *** ------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-Z WITH DEBUGGING MODE.                      .
       SOURCE-COMPUTER. IBM-Z.     *> uncomment for suppressing Display
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 WFLD                    PIC X(01).

       01 WS-CHECK-FIELDS.
          05 WS-LEN               PIC  99   VALUE 0 .
          05 WS-ERROR-FIELD       PIC X(20) VALUE SPACES.
          05 WS-ERROR-MESSAGE     PIC X(20) VALUE SPACES.
          05 WS-TESTSTR           PIC X(20) VALUE SPACES. *> ANUM Test

      *   variable for validation of the input
       01 CHAR-FOUND         PIC X(01)    VALUE 'Y'.
       01 IDX                PIC 99       COMP.

      *   for check of the inputfields ANUM-VALID doesn't include BLANK,
      *   if you need to check with BLANK, use VALIDCHARACTER
       01 CHAR               PIC X(1).
          88 VALIDCHARACTER
             VALUE "A" THRU "Z" ,  "a" THRU "z", "0" THRU "9", " ".
          88 ANUM-VALID      VALUE "A" THRU "Z" , "0" THRU "9".

      *   variable for checking date with CEEDAYS- routine
       01 W-INPUT-DATE-INT        PIC 9(9) COMP.
       01 W-PICSTR-IN.
           10  W-PICSTR-LTH-IN     PIC S9(4) COMP VALUE 8.
           10  W-PICSTR-STR-IN     PIC X(8)  value 'YYYYMMDD'.
       01 W-DATE-IN-CEE.
          10  W-DATE-IN-LTH-CEE   PIC S9(4) COMP VALUE 8.
          10  W-DATE-IN-STR-CEE   PIC X(8).
       01 FC.
          10  FC-SEV              PIC S9(4) COMP.
          10  FC-MSG              PIC S9(4) COMP.
          10  FC-CTW              PIC X.
          10  FC-FAC              PIC X(3).
          10  FC-ISI              PIC S9(8) COMP.

      *   variable for data from MAIN-PGM
       01 PO-SUB                  PIC 9(01) COMP.

       LINKAGE SECTION.

       COPY PURCHL.

       PROCEDURE DIVISION USING
                 PURCHORD-STRUCTURE,
                 PO-SUB-MAX,
                 ERROR-SUB,
                 ERROR-SUB-MAX,
                 ERROR-STRUCTURE.
      ***--------------------------------------------------------------
      *** MAIN - ROUTINE
      ***--------------------------------------------------------------
      D    DISPLAY 'Start Sub POEDIT'.
      D    DISPLAY 'ERROR-SUB     : '    ERROR-SUB.
      D    DISPLAY 'PURCHORD-STRUCTURE ' PURCHORD-STRUCTURE.

           PERFORM 000-CHECK-PO-NUMBER.
           PERFORM 100-CHECK-BUYER-CODE.
           PERFORM 200-CHECK-QUANTITY.
           PERFORM 300-CHECK-UNIT-PRICE.
           PERFORM 400-CHECK-ORDER-DATE
           PERFORM 500-CHECK-DEILVERY-DATE.

           GOBACK.                         *> Control returned to MAIN
      ***---------------------------------------------------------------
      *** PO-NUMBER: has to be ANUM A to 9, uppercase and no
      *** special character.
      *** Validation will be done in 600-CHECK-ANUM
      *** If there is an error, move PO-NUMBER(PO_SUB)
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and call PERFORM 700-ERROR-MESSAGE
      ***---------------------------------------------------------------
       000-CHECK-PO-NUMBER.
      D    DISPLAY '000-CHECK-PO-NUMBER'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 1 TO PO-SUB.
           PERFORM UNTIL PO-SUB > PO-SUB-MAX
      D        DISPLAY 'PO-SUB   '  PO-SUB
      D        DISPLAY 'PO-NUMBER ' PO-NUMBER(PO-SUB)
               MOVE PO-NUMBER(PO-SUB) TO WS-TESTSTR
      *        value of PO-NUMBER has to be between A-Z and 0-9
               PERFORM 600-CHECK-ANUM
               IF CHAR-FOUND = 'N'
      *           fill the error field
                  MOVE 'PO-NUMBER '         TO WS-ERROR-FIELD
                  MOVE 'invalid characters' TO WS-ERROR-MESSAGE
                  PERFORM 700-ERROR-MESSAGE
               END-IF
               ADD +1 TO PO-SUB
           END-PERFORM.
      ***---------------------------------------------------------------
      *** BUYER-CODE:
      *** has to be ANUM A to 9, uppercase and no special character.
      *** Validation will be done in 600-CHECK-ANUM
      *** If there is an error, move WS-BUYER-CODE(PO_SUB)
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and call PERFORM 700-ERROR-MESSAGE
      ***---------------------------------------------------------------
       100-CHECK-BUYER-CODE.
      D    DISPLAY '100-CHECK-BUYER-CODE'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 1 TO PO-SUB.
           PERFORM UNTIL PO-SUB > PO-SUB-MAX
      D        DISPLAY 'PO-SUB     ' PO-SUB
      D        DISPLAY 'BUYER-CODE ' BUYER-CODE(PO-SUB)

               MOVE BUYER-CODE(PO-SUB) TO WS-TESTSTR
      *        value of BUYER-CODE has to be between A-Z and 0-9
               PERFORM 600-CHECK-ANUM
               IF CHAR-FOUND = 'N'
      *           fill the error field
                  MOVE 'BUYER-CODE '        TO WS-ERROR-FIELD
                  MOVE 'invalid characters' TO WS-ERROR-MESSAGE
                  PERFORM 700-ERROR-MESSAGE
               END-IF
               ADD +1 TO PO-SUB
           END-PERFORM.
      ***---------------------------------------------------------------
      *** QUANTITY:
      *** has to be numeric and >= 1. If the condition didnt' match,
      *** move WS-QUANTITY(PO-SUB) to ERROR-FLD(ERROR-SUB)
      *** and errormessage to ERROR-MSG(ERROR-SUB) and
      *** PERFORM 700-ERROR-MESSAGE
      ***---------------------------------------------------------------
       200-CHECK-QUANTITY.
      D    DISPLAY '200-CHECK-QUANTITY'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 1 TO PO-SUB.
           PERFORM UNTIL PO-SUB > PO-SUB-MAX
      D       DISPLAY 'PO-SUB     ' PO-SUB
      D       DISPLAY 'QUANTITY   ' QUANTITY(PO-SUB)

              IF QUANTITY(PO-SUB) < 1  OR
                 QUANTITY(PO-SUB)      NOT NUMERIC
      *          fill the error field
                 MOVE 'QUANTITY '         TO WS-ERROR-FIELD
                 MOVE 'zero/not numeric ' TO WS-ERROR-MESSAGE
                 PERFORM 700-ERROR-MESSAGE
              END-IF
              ADD +1 TO PO-SUB
           END-PERFORM.
      ***---------------------------------------------------------------
      *** UNIT-PRICE:
      *** has to be numeric and >= 0.01.
      *** If there is an error, move WS-UNIT-PRICE(PO-SUB)
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and PERFORM 700-ERROR-MESSAGE
      ***---------------------------------------------------------------
       300-CHECK-UNIT-PRICE.
      D    DISPLAY '300-CHECK-UNIT-PRICE'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 1 TO PO-SUB.
           PERFORM UNTIL PO-SUB > PO-SUB-MAX
      D        DISPLAY 'PO-SUB     ' PO-SUB
      D        DISPLAY 'UNIT-PRICE ' UNIT-PRICE(PO-SUB)

               IF UNIT-PRICE(PO-SUB) <= 0 OR
                  UNIT-PRICE(PO-SUB)      NOT NUMERIC
      *           fill the error field
                  MOVE 'UNIT-PRICE '       TO WS-ERROR-FIELD
                  MOVE 'zero/not numeric ' TO WS-ERROR-MESSAGE
                 PERFORM 700-ERROR-MESSAGE
              END-IF
               ADD +1 TO PO-SUB
           END-PERFORM.
      ***---------------------------------------------------------------
      *** ORDER-DATE:
      *** has to be a valid calendardate, check with CEEDAYS-routine.
      *** If not, move WS-DEILVERY-DATE(PO-SUB) to ERROR-FLD(ERROR-SUB)
      *** and errormessage to ERROR-MSG(ERROR-SUB)
      *** and call PERFORM 700-ERROR-MESSAGE
      ***---------------------------------------------------------------
       400-CHECK-ORDER-DATE.
      D    DISPLAY '400-CHECK-ORDER-DATE'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 1 TO PO-SUB.
           PERFORM UNTIL PO-SUB > PO-SUB-MAX
      D        DISPLAY 'PO-SUB     ' PO-SUB
      D        DISPLAY 'ORDER-DATE ' ORDER-DATE(PO-SUB)

               MOVE ORDER-DATE(PO-SUB) TO W-DATE-IN-STR-CEE

               CALL 'CEEDAYS' USING W-DATE-IN-CEE
                    W-PICSTR-IN, W-INPUT-DATE-INT, FC

               IF FC-SEV NOT = ZERO
                  MOVE 'ORDER-DATE ' TO WS-ERROR-FIELD
                  MOVE 'bad date'    TO WS-ERROR-MESSAGE
                  PERFORM 700-ERROR-MESSAGE
               END-IF
               ADD +1 TO PO-SUB
           END-PERFORM.
      ***---------------------------------------------------------------
      *** DEILVERY-DATE:
      *** has to be a valid calendardate, check with CEEDAYS-routine.
      *** If not, move WS-DEILVERY-DATE(PO-SUB) to ERROR-FLD(ERROR-SUB)
      *** and errormessage to ERROR-MSG(ERROR-SUB)
      *** and call PERFORM 700-ERROR-MESSAGE
      ***---------------------------------------------------------------
       500-CHECK-DEILVERY-DATE.
      D    DISPLAY '600-CHECK-DEILVERY-DATE'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 1 TO PO-SUB.
           PERFORM UNTIL PO-SUB > PO-SUB-MAX
      D        DISPLAY 'PO-SUB     ' PO-SUB
      D        DISPLAY 'DEILVERY-DATE ' DELIVERY-DATE(PO-SUB)

               MOVE DELIVERY-DATE(PO-SUB) TO W-DATE-IN-STR-CEE

               CALL 'CEEDAYS' USING W-DATE-IN-CEE
                     W-PICSTR-IN, W-INPUT-DATE-INT, FC

               IF FC-SEV NOT = ZERO
                  MOVE 'DEILVERY-DATE ' TO WS-ERROR-FIELD
                  MOVE 'bad date'       TO WS-ERROR-MESSAGE
                  PERFORM 700-ERROR-MESSAGE
               END-IF
               ADD +1 TO PO-SUB
           END-PERFORM.
      ***---------------------------------------------------------------
      *** CHECK-ANUM: check against 88-Values
      *** 88 ANUM-VALID      VALUE "A" THRU "Z" , "0" THRU "9".
      *** If it doesn't match this demand, move 'N' to CHAR-FOUND
      ***---------------------------------------------------------------
       600-CHECK-ANUM.
      D    DISPLAY '600-CHECK-ANUM'.

           MOVE 'Y' TO CHAR-FOUND              *> init
           MOVE  0  TO WS-LEN.                 *> init

      *    determinate the length of the value
           INSPECT FUNCTION REVERSE(WS-TESTSTR)
           TALLYING WS-LEN FOR LEADING SPACES
           COMPUTE  WS-LEN = LENGTH OF WS-TESTSTR - WS-LEN

      D    DISPLAY 'WS-TESTSTR ' WS-TESTSTR.

           PERFORM WITH TEST AFTER VARYING IDX FROM 1 BY 1 UNTIL
              IDX > WS-LEN OR NOT ANUM-VALID
                    MOVE WS-TESTSTR(IDX:1) TO CHAR
           END-PERFORM.

           IF IDX > WS-LEN
              THEN MOVE 'Y' TO CHAR-FOUND
              ELSE MOVE 'N' TO CHAR-FOUND
            END-IF.
      ***---------------------------------------------------------------
      *** WS-ERROR-FIELD and WS-ERROR-MESSAGE filled in the subroutine
      ***---------------------------------------------------------------
       700-ERROR-MESSAGE.
      D    DISPLAY '700-ERROR-MESSAGE'.

           COMPUTE ERROR-SUB  = ERROR-SUB  + 1.

           MOVE WS-ERROR-FIELD     TO ERROR-FLD(ERROR-SUB).
           MOVE WS-ERROR-MESSAGE   TO ERROR-MSG(ERROR-SUB).

      D    DISPLAY 'ERROR-SUB      = ' ERROR-SUB.
      D    DISPLAY 'error-fld      = ' ERROR-FLD(ERROR-SUB).
      D    DISPLAY 'error-message  = ' ERROR-MSG(ERROR-SUB).
      D    DISPLAY 'Return-Code    = ' RETURN-CODE.

            IF ERROR-SUB >= ERROR-SUB-MAX
               THEN MOVE 8 TO RETURN-CODE
                    GOBACK                    *> out of the loop  05.09.
               ELSE MOVE 0 TO RETURN-CODE
           END-IF.
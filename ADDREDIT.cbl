       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDREDIT.
      *** ------------------------------------------------------------
      *** Final Exam Subprogram Program ADDREDIT - Program Stub
      *** Called by PARTSUPP (Main)
      *** Check the passed ADDRESS-STRUCTURE data from the MAIM-PGM
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
      *** created 06/09/2020                  Hilde Schmidbauer
      *** ------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-Z WITH DEBUGGING MODE.
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
       01 CHAR-FOUND              PIC X(01)    VALUE 'Y'.
       01 IDX                     PIC 99       COMP.

      *   for check of the inputfields ANUM-VALID doesn't include BLANK,
      *   if you need to check with BLANK, use VALIDCHARACTER
       01 CHAR               PIC X(1).
          88 VALIDCHARACTER
             VALUE "A" THRU "Z" ,  "a" THRU "z", "0" THRU "9", " ",
                   "&", "-", "/", "+", ".", ":".
          88 ANUM-VALID
             VALUE "A" THRU "Z", "a" THRU "z", "0" THRU "9", " ".

      *   variable for data from MAIN-PGM ADDRESS-STRUCTURE
       01 A-SUB                   PIC 9(01) COMP.


       01 WS-WORKNUM                PIC S9(8) COMP. *> only for display
                                                    *> in test
      * check for combination of ADDR-STATE and ZIP-CODE
       01 WS-ZS-COMBI-VALID-IND     PIC X(1).
          88 ZS-COMBI-VALID                     VALUE 'Y'.
          88 ZS-COMBI-INVALID                   VALUE 'N'.

       LINKAGE SECTION.

       COPY ADDRL.

       PROCEDURE DIVISION USING
           ADDRESS-STRUCTURE,
           ZIP-STATE-STRUCTURE,
           ZS-TABLE-SUB-MAX,
           ADDR-TABLE-SUB-MAX,
           ERROR-SUB,
           ERROR-SUB-MAX,
           ERROR-STRUCTURE.
      ***--------------------------------------------------------------
      *** MAIN - ROUTINE
      ***--------------------------------------------------------------
      D    DISPLAY 'Start Sub ADDREDIT'.
      D    DISPLAY 'ERROR-SUB           : ' ERROR-SUB.
      D    DISPLAY 'ZS-TABLE-SUB-MAX    : ' ZS-TABLE-SUB-MAX.
      D    DISPLAY 'ADDRESS-STRUCTURE   : ' ADDRESS-STRUCTURE.
      D    DISPLAY 'ZIP-STATE-STRUCTURE : ' ZIP-STATE-STRUCTURE.

           PERFORM 000-CHECK-ADDRESS-TYPE.
           PERFORM 100-CHECK-ADDRESS-1.
           PERFORM 110-CHECK-ADDRESS-2.
           PERFORM 120-CHECK-ADDRESS-3.
           PERFORM 200-CHECK-CITY.
           PERFORM 300-CHECK-ZS-COMBI.

           GOBACK.                         *> Control returned to MAIN
      ***---------------------------------------------------------------
      *** ADDRESS-TYPE: has to be checked against the 88-Values in
      *** ADDRRL. 88 VALID-ADDRESS-TYPE VALUE '1' '2' '3'.
      *** If there is an error, move WS-ADDRESS-TYPE(PO_SUB)
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and call PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       000-CHECK-ADDRESS-TYPE.
      D    DISPLAY '000-CHECK-ADDRESS-TYPE'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 1 TO A-SUB.

           PERFORM UNTIL A-SUB > ADDR-TABLE-SUB-MAX
      D        DISPLAY 'A-SUB   '  A-SUB
      D        DISPLAY 'ADDRESS-TYPE ' ADDRESS-TYPE(A-SUB)
      *        valid value of ADDRESS-TYPE are '1' '2' '3'
               IF NOT VALID-ADDRESS-TYPE(A-SUB)
      *           fill the error field
                  MOVE 'ADDRESS-TYPE'   TO WS-ERROR-FIELD
                  MOVE 'does not exist' TO WS-ERROR-MESSAGE
                  PERFORM 800-ERROR-MESSAGE
               END-IF
               ADD +1 TO A-SUB
           END-PERFORM.
      ***---------------------------------------------------------------
      *** ADDRESS-1: - MUST be filled -
      *** it may contain any ANUM A to 9, special character
      *** "&", "-", "/", "+"  and embedded blanks.
      *** Validation will be done in 700-CHECK-VALIDCHARACTER
      *** If there is an error, move WS-ADDRESS-1(PO_SUB)
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and call PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       100-CHECK-ADDRESS-1.
      D    DISPLAY '100-ADDRESS-1'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 1 TO A-SUB.                             *> init

           PERFORM UNTIL A-SUB > ADDR-TABLE-SUB-MAX
              MOVE ADDRESS-1(A-SUB) TO WS-TESTSTR
              IF WS-TESTSTR = ' ' AND ADDRESS-TYPE(A-SUB) = '1'  *> SC01
                THEN
      *            fill the error field
                   MOVE 'ADDRESS-1 '         TO WS-ERROR-FIELD
                   MOVE 'blank not allowed ' TO WS-ERROR-MESSAGE
                   PERFORM 800-ERROR-MESSAGE
                ELSE
      *           check value of ADDRESS-1
                  PERFORM 700-CHECK-VALIDCHARACTER
                  IF CHAR-FOUND = 'N'
      *              fill the error field
                     MOVE 'ADDRESS-1 '         TO WS-ERROR-FIELD
                     MOVE 'invalid characters' TO WS-ERROR-MESSAGE
                     PERFORM 800-ERROR-MESSAGE
                  END-IF
                END-IF
                ADD +1 TO A-SUB
           END-PERFORM.
      ***---------------------------------------------------------------
      *** ADDRESS-2: - optional -
      *** it may contain any ANUM A to 9, special character
      *** "&", "-", "/", "+"  and embedded blanks.
      *** Validation will be done in 700-CHECK-VALIDCHARACTER
      *** If there is an error, move WS-ADDRESS-1(PO_SUB)
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and call PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       110-CHECK-ADDRESS-2.
      D    DISPLAY '110-ADDRESS-2'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 1 TO A-SUB.                             *> init

           PERFORM UNTIL A-SUB > ADDR-TABLE-SUB-MAX
              MOVE ADDRESS-2(A-SUB) TO WS-TESTSTR
              IF WS-TESTSTR > ' '
                 THEN
      *             check value of ADDRESS-2
                    PERFORM 700-CHECK-VALIDCHARACTER
                    IF CHAR-FOUND = 'N'
      *                fill the error field
                       MOVE 'ADDRESS-2 '         TO WS-ERROR-FIELD
                       MOVE 'invalid characters' TO WS-ERROR-MESSAGE
                       PERFORM 800-ERROR-MESSAGE
                    END-IF
                 ELSE DISPLAY 'ADDRESS-2 is blank'
                 END-IF
              ADD +1 TO A-SUB
           END-PERFORM.
      ***---------------------------------------------------------------
      *** ADDRESS-3: - optional -
      *** it may contain any ANUM A to 9, special character
      *** "&", "-", "/", "+"  and embedded blanks.
      *** Validation will be done in 700-CHECK-VALIDCHARACTER
      *** If there is an error, move WS-ADDRESS-1(PO_SUB)
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and call PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       120-CHECK-ADDRESS-3.
      D    DISPLAY '120-ADDRESS-3'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 1 TO A-SUB.                             *> init

           PERFORM UNTIL A-SUB > ADDR-TABLE-SUB-MAX
              MOVE ADDRESS-3(A-SUB) TO WS-TESTSTR
              IF WS-TESTSTR > ' '
                 THEN
      *             check value of ADDRESS-3
                    PERFORM 700-CHECK-VALIDCHARACTER
                    IF CHAR-FOUND = 'N'
      *                fill the error field
                       MOVE 'ADDRESS-3 '         TO WS-ERROR-FIELD
                       MOVE 'invalid characters' TO WS-ERROR-MESSAGE
                       PERFORM 800-ERROR-MESSAGE
                    END-IF
                 ELSE DISPLAY 'ADDRESS-3  is blank'
                 END-IF
              ADD +1 TO A-SUB
           END-PERFORM.
      ***---------------------------------------------------------------
      *** CITY:  - MUST be filled -
      *** it may contain any ANUM A to 9, special character
      *** "&", "-", "/", "+"  and embedded blanks.
      *** Validation will be done in 750-CHECK-ANUM
      *** If there is an error, move WS-ADDRESS-1(PO_SUB)
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and call PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       200-CHECK-CITY.
      D    DISPLAY '200-CHECK-CITY'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 1 TO A-SUB.                             *> init

           PERFORM UNTIL A-SUB > ADDR-TABLE-SUB-MAX
              MOVE CITY(A-SUB) TO WS-TESTSTR
              IF WS-TESTSTR = ' ' AND ADDRESS-TYPE(A-SUB) = '1'  *> SC01
                THEN
      *            fill the error field
                   MOVE 'CITY '              TO WS-ERROR-FIELD
                   MOVE 'blank not allowed ' TO WS-ERROR-MESSAGE
                   PERFORM 800-ERROR-MESSAGE
                ELSE
      *           check value of CITY
                  PERFORM 750-CHECK-ANUM
                  IF CHAR-FOUND = 'N'
      *              fill the error field
                     MOVE 'CITY '              TO WS-ERROR-FIELD
                     MOVE 'invalid characters' TO WS-ERROR-MESSAGE
                     PERFORM 800-ERROR-MESSAGE
                  END-IF
                END-IF
                ADD +1 TO A-SUB
           END-PERFORM.
      ***---------------------------------------------------------------
      *** CHECK-ZS-COMBI:
      *** The combination of Fields ZIP-CODE and ZIP-STATE-CODE
      *** has to be tested by a non-binary table search:
      *** ADDRESS-TABLE.ZIP-CODE(A-SUB) = ZIP-STATE-TABLE.ZIP-STATE-CODE
      *** AND ADDRESS-TABLE.ZIP-CODE(SUB)  between
      ***         ZIP-STATE-TABLE.ZIPCODE-RANGE-LO  and
      ***         ZIP-STATE-TABLE.ZIPCODE-RANGE-HI
      *** Otherwise set RETURN-CODE to ERROR,
      *** move STATE/ZIP Combination to ERROR-FLD(ERROR-SUB)
      *** and not valid to ERROR-MSG(ERROR-SUB)
      *** and call PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       300-CHECK-ZS-COMBI.
      D    DISPLAY '300-CHECK-ADDR-STATE'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           PERFORM VARYING A-SUB FROM 1 BY 1
              UNTIL A-SUB > ADDR-TABLE-SUB-MAX
                   SET ZIP-STATE-IDX TO 1
      D            DISPLAY "Search Argument: "
      D                     ZIP-CODE(A-SUB) '/' ADDR-STATE(A-SUB)
                   SEARCH ZIP-STATE-TABLE
                          AT END
                          MOVE 'N' TO WS-ZS-COMBI-VALID-IND
                   WHEN ZIP-CODE(A-SUB)  GREATER THAN
                        ZIPCODE-RANGE-LO IN ZIP-STATE-TABLE
                                            (ZIP-STATE-IDX)
                       AND
                         ZIP-CODE(A-SUB)  LESS THAN
                         ZIPCODE-RANGE-HI IN ZIP-STATE-TABLE
                                             (ZIP-STATE-IDX)
                      AND
                         ADDR-STATE(A-SUB) =
                         ZIP-STATE-CODE IN
                         ZIP-STATE-TABLE(ZIP-STATE-IDX)
                            MOVE 'Y' TO WS-ZS-COMBI-VALID-IND
                   END-SEARCH

      D            DISPLAY "Loop: " A-SUB
      D            SET WS-WORKNUM TO ZIP-STATE-IDX
      D            DISPLAY "Index :" WS-WORKNUM
      D            DISPLAY 'ZIPCODE-RANGE-HI(ZIP-STATE-IDX) :'
      D                      ZIPCODE-RANGE-HI(ZIP-STATE-IDX)
      D             DISPLAY 'ZIPCODE-RANGE-LO(ZIP-STATE-IDX) :'
      D                      ZIPCODE-RANGE-LO(ZIP-STATE-IDX)
      D             DISPLAY "Search Argument: "
      D                     ZIP-CODE(A-SUB) '/' ADDR-STATE(A-SUB)
      D
      D            DISPLAY "Table Entry :"
      D                    ZIP-STATE-TABLE(ZIP-STATE-IDX)
      D
      D            IF ZS-COMBI-VALID THEN
      D               DISPLAY '**ZS Combination valid**'
      D            ELSE
      D               DISPLAY '**Error! ZS Combination invalid**'
      D            END-IF
           END-PERFORM.

           IF WS-ZS-COMBI-VALID-IND = 'N'
                 THEN
                    MOVE 'ADDR-STATE/ZIP-CODE ' TO WS-ERROR-FIELD
                    MOVE 'not valid     '       TO WS-ERROR-MESSAGE
                    PERFORM 800-ERROR-MESSAGE
                 ELSE
                    DISPLAY 'valid ADDR STATE /ZIP combination '
                 END-IF.
      ***---------------------------------------------------------------
      *** CHECK-VALIDCHARACTER: check against 88-Values
      *** 88 VALIDCHARACTER
      ***     VALUE "A" THRU "Z" ,  "a" THRU "z", "0" THRU "9", " ",
      ***             "&", "-", "/", "+", ".", ":"
      *** If it doesn't match this demand, move 'N' to CHAR-FOUND
      ***---------------------------------------------------------------
       700-CHECK-VALIDCHARACTER.
      D    DISPLAY '700-CHECK-VALIDCHARACTER'.

           MOVE 'Y' TO CHAR-FOUND              *> init
           MOVE  0 TO WS-LEN.                  *> init

      *    determinate the length of the value
           INSPECT FUNCTION REVERSE(WS-TESTSTR)
           TALLYING WS-LEN FOR LEADING SPACES
           COMPUTE  WS-LEN = LENGTH OF WS-TESTSTR - WS-LEN

      D    DISPLAY 'WS-TESTSTR ' WS-TESTSTR.

           PERFORM WITH TEST AFTER VARYING IDX FROM 1 BY 1 UNTIL
              IDX > WS-LEN OR NOT VALIDCHARACTER
                    MOVE WS-TESTSTR(IDX:1) TO CHAR
           END-PERFORM.

           IF IDX > WS-LEN
              THEN MOVE 'Y' TO CHAR-FOUND
              ELSE MOVE 'N' TO CHAR-FOUND
            END-IF.
      ***---------------------------------------------------------------
      *** CHECK-ANUM: check against 88-Values
      *** 88 ANUM-VALID      VALUE "A" THRU "Z" , "0" THRU "9".
      *** If it doesn't match this demand, move 'N' to CHAR-FOUND
      ***---------------------------------------------------------------
       750-CHECK-ANUM.
      D    DISPLAY '750-CHECK-ANUM'.

           MOVE 'Y' TO CHAR-FOUND              *> init
           MOVE  0 TO WS-LEN.                  *> init

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
       800-ERROR-MESSAGE.
      D    DISPLAY '800-ERROR-MESSAGE'.

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

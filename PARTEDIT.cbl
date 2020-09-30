       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTEDIT.
      *** ------------------------------------------------------------
      *** Final Exam Subprogram Program PARTEDIT - Program Stub
      *** Called by PARTSUPP (Main)
      *** Check the passed variable from the MAIM-PGM
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
      ***
      *** created 01/09/2020                  Hilde Schmidbauer
      *** ------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-Z WITH DEBUGGING MODE.
       SOURCE-COMPUTER. IBM-Z.     *> uncomment for suppressing Display
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 WFLD                  PIC X(01).

       01 WS-CHECK-FIELDS.
          05 WS-PART-NUMBER1    PIC 9(07).
          05 WS-PART-NUMBER2    PIC 9(07).
          05 WS-PART-NUMBER3    PIC 9(07).
          05 WS-LEN             PIC  99   VALUE 0 .
          05 WS-ERROR-FIELD     PIC X(20) VALUE SPACES.
          05 WS-ERROR-MESSAGE   PIC X(20) VALUE SPACES.
          05 WS-VEHICLE-MODEL   PIC X(05) VALUE SPACES.
          05 WS-TESTSTR         PIC X(20) VALUE SPACES. *> ANUM Test

      *   variable for ANUM-CHECK
       01 CHAR-FOUND         PIC X(01)    VALUE 'Y'.
       01 IDX                PIC 99       COMP.

      *   for ANUM check of the inputfields
       01 CHAR               PIC X(1).
          88 ANUM-VALID      VALUE "A" THRU "Z" , "0" THRU "9".

       01 DATE-VARS.
           05 CURRENT-YEAR      PIC X(4).
           05 CURRENT-MON       PIC X(2).
           05 CURRENT-DAY       PIC X(2).

       LINKAGE SECTION.

       COPY PARTSL.

       PROCEDURE DIVISION USING
           PART-NUMBER,
           BLUEPRINT-NUMBER,
           UNIT-OF-MEASURE,
           WEEKS-LEAD-TIME,
           VEHICLE-MAKE,
           VEHICLE-MODEL,
           VEHICLE-YEAR,
           ERROR-SUB,
           ERROR-SUB-MAX,
           ERROR-STRUCTURE.

      ***--------------------------------------------------------------
      *** Main Routine
      ***--------------------------------------------------------------
      D    DISPLAY 'Start Sub PARTEDIT'.
      D    DISPLAY 'ERROR-SUB: ' ERROR-SUB.

           PERFORM 000-CHECK-PART-NUMBER.
           PERFORM 100-CHECK-BLUEPRINT-NBR.
           PERFORM 200-CHECK-UNIT-OF-MEASURE.
           PERFORM 300-CHECK-WEEKS-LEAD-TIME.
           PERFORM 400-CHECK-VEHICLE-MAKE.
           PERFORM 500-CHECK-VEHICLE-MODEL.
           PERFORM 600-CHECK-VEHICLE-YEAR.

           GOBACK.                         *> Control returned to MAIN
      ***---------------------------------------------------------------
      *** PART-NUMBER: has to be numeric, either mask
      *** 9999999/9999999/9999999 or
      *** 9999999-9999999-9999999
      *** If there is an error, move PART-NUMBER to
      *** ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and call PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       000-CHECK-PART-NUMBER.
      D    DISPLAY '000-CHECK-PART-NUMBER'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           UNSTRING PART-NUMBER DELIMITED BY '/' or '-'
                    INTO WS-PART-NUMBER1,
                         WS-PART-NUMBER2,
                         WS-PART-NUMBER3
           END-UNSTRING.

           IF WS-PART-NUMBER1 IS NOT NUMERIC OR
              WS-PART-NUMBER2 IS NOT NUMERIC OR
              WS-PART-NUMBER3 IS NOT NUMERIC

      *       fill the error field
              MOVE 'PART-NUMBER '       TO WS-ERROR-FIELD
              MOVE 'has invalid scheme' TO WS-ERROR-MESSAGE
              PERFORM 800-ERROR-MESSAGE
           END-IF.
      ***---------------------------------------------------------------
      *** BLUEPRINT-NUMBER: has to be ANUM A to 9, max 5 CHAR, uppercase,
      *** no special character.
      *** If there is an error, move BLUEPRINT-NUMBER
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and call PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       100-CHECK-BLUEPRINT-NBR.
      D    DISPLAY '100-CHECK-BLUEPRINT-NBR'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

      *    return field real field length
           MOVE 0 TO WS-LEN.                             *> init
           INSPECT FUNCTION REVERSE(BLUEPRINT-NUMBER)
           TALLYING WS-LEN FOR LEADING SPACES.

           COMPUTE WS-LEN = LENGTH OF BLUEPRINT-NUMBER - WS-LEN.

           IF WS-LEN > 5
              THEN
      *          fill the error field
                 MOVE 'BLUEPRINT-NUMBER ' TO WS-ERROR-FIELD
                 MOVE 'field is too long' TO WS-ERROR-MESSAGE
                 PERFORM 800-ERROR-MESSAGE
              ELSE
                 MOVE BLUEPRINT-NUMBER TO WS-TESTSTR
                 PERFORM 700-CHECK-ANUM
                 IF CHAR-FOUND = 'N'
      *             fill the error field
                    MOVE 'BLUEPRINT-NUMBER '  TO WS-ERROR-FIELD
                    MOVE 'invalid characters' TO WS-ERROR-MESSAGE
                    PERFORM 800-ERROR-MESSAGE
                END-IF
              END-IF.
      ***---------------------------------------------------------------
      *** UNIT-OF-MEASURE:
      *** has to be tested against 88-Values in copybook PARTSL.
      *** 01 UNIT-OF-MEASURE PIC X(3).
      ***    88 VALID-UNIT                  VALUE 'CM ' 'INC'.
      *** If the content of field doesn't match,
      *** move UNIT-OF-MEASURE to ERROR-FLD(ERROR-SUB)
      *** and errormessage to ERROR-MSG(ERROR-SUB) and
      *** PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       200-CHECK-UNIT-OF-MEASURE.
      D    DISPLAY '200-CHECK-UNIT-OF-MEASURE'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

            IF NOT VALID-UNIT THEN
      *       fill the error field
              MOVE 'UNIT-OF-MEASURE ' TO WS-ERROR-FIELD
              MOVE 'wrong value'     TO WS-ERROR-MESSAGE
              PERFORM 800-ERROR-MESSAGE
           END-IF.
      ***---------------------------------------------------------------
      *** WEEKS-LEAD-TIME:
      *** has to be greater zero
      *** If there is an error, move WEEKS-LEAD-TIME
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       300-CHECK-WEEKS-LEAD-TIME.
      D    DISPLAY '300-CHECK-WEEKS-LEAD-TIME'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           IF WEEKS-LEAD-TIME <= 0
      *       fill the error field
              MOVE 'WEEKS-LEAD-TIME ' TO WS-ERROR-FIELD
              MOVE 'zero/not numeric' TO WS-ERROR-MESSAGE
              PERFORM 800-ERROR-MESSAGE
           END-IF.
      ***---------------------------------------------------------------
      *** VEHICLE-MAKE:
      *** has to be tested against 88-Values in copybook PARTSL.
      *** 88 VALID-MAKE
      ***    VALUE 'CHR' 'FOR' 'GM ' 'VW' 'TOY' 'JAG' 'PEU' 'BMW'.
      *** If the content of field doesn't match,
      *** move VEHICLE-MAKE to ERROR-FLD(ERROR-SUB) and
      *** errormessage to ERROR-MSG(ERROR-SUB)
      ***  and PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       400-CHECK-VEHICLE-MAKE.
      D    DISPLAY '400-CHECK-VEHICLE-MAKE'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           IF NOT VALID-MAKE THEN
      *       fill the error field
              MOVE 'VEHICLE-MAKE '   TO WS-ERROR-FIELD
              MOVE 'does not exist'  TO WS-ERROR-MESSAGE
              PERFORM 800-ERROR-MESSAGE
           END-IF.
      ***---------------------------------------------------------------
      *** VEHICLE-MODEL:
      *** has to be ANUM, including special characters
      *** and blanks, up to a max. count of 5 characters.
      *** If the count is greater than five,
      *** the field has to be truncated --> for the MAIN
      *** This has to be done for further processing in the MAIN-PGM
      *** For this field no error is produced.
      ***---------------------------------------------------------------
       500-CHECK-VEHICLE-MODEL.
           DISPLAY '500-CHECK-VEHICLE-MODEL'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 0 TO WS-LEN.                             *> init
           COMPUTE WS-LEN = LENGTH OF VEHICLE-MODEL.
      ***  If WS-LEN greater than five, the field has to be truncated
      ***  DCL of WS-VEHICLE-MODEL PIC X(05)
           IF WS-LEN > 5
              THEN MOVE VEHICLE-MODEL    TO WS-VEHICLE-MODEL
                   MOVE WS-VEHICLE-MODEL TO VEHICLE-MODEL
              END-IF.
      ***---------------------------------------------------------------
      *** VEHICLE-YEAR:
      *** has to be numeric, between 1900 and current year.
      *** If it doesn't match this demand,
      *** move VEHICLE-YEAR to ERROR-FLD(ERROR-SUB) and
      *** errormessage to ERROR-MSG(ERROR-SUB)
      ***  and PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       600-CHECK-VEHICLE-YEAR.
      D    DISPLAY '600-CHECK-VEHICLE-YEAR'.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE FUNCTION CURRENT-DATE TO DATE-VARS.

           IF VEHICLE-YEAR IS NOT NUMERIC
              THEN
      *          fill the error field
                 MOVE 'VEHICLE-YEAR '  TO WS-ERROR-FIELD
                 MOVE 'not numeric  '  TO WS-ERROR-MESSAGE
                 PERFORM 800-ERROR-MESSAGE
              ELSE
                 IF FUNCTION NUMVAL(VEHICLE-YEAR) < 1900 OR
                                    VEHICLE-YEAR > CURRENT-YEAR
                    MOVE 'VEHICLE-YEAR '      TO WS-ERROR-FIELD
                    MOVE 'year out of range'  TO WS-ERROR-MESSAGE
                    PERFORM 800-ERROR-MESSAGE
                END-IF
           END-IF.
      ***---------------------------------------------------------------
      *** CHECK-ANUM:
      *** check against 88-Values "A" THRU "Z" , "0" THRU "9".
      *** If it doesn't match this demand, move 'N' to CHAR-FOUND
      ***---------------------------------------------------------------
       700-CHECK-ANUM.
      D    DISPLAY '700-CHECK-ANUM'.

           DISPLAY 'WS-TESTSTR '   WS-TESTSTR.

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

           DISPLAY 'ERROR-SUB      = ' ERROR-SUB.
           DISPLAY 'error-fld      = ' ERROR-FLD(ERROR-SUB).
           DISPLAY 'error-message  = ' ERROR-MSG(ERROR-SUB).
           DISPLAY 'Return-Code    = ' RETURN-CODE.

            IF ERROR-SUB >= ERROR-SUB-MAX
               THEN MOVE 8 TO RETURN-CODE
               ELSE MOVE 0 TO RETURN-CODE
           END-IF.

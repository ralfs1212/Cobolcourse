       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPEDIT.
      *** ------------------------------------------------------------
      *** Final Exam Subprogram Program SUPPEDIT - Program Stub
      *** Called by PARTSUPP (Main)
      *** Check the passed SUPPLIER data from the MAIM-PGM
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
      *** created 01/09/2020                  Hilde Schmidbauer
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
             VALUE "A" THRU "Z" ,  "a" THRU "z", "0" THRU "9",
                   " ", "-", "&", "/", ".", ":".
          88 ANUM-VALID      VALUE "A" THRU "Z" , "0" THRU "9".

      *   variable for checking date with CEEDAYS- routine
       01 W-INPUT-DATE-INT         PIC 9(9) COMP.
       01 W-PICSTR-IN.
           10  W-PICSTR-LTH-IN     PIC S9(4) COMP VALUE 8.
           10  W-PICSTR-STR-IN     PIC X(8)  value 'YYYYMMDD'.
       01 W-DATE-IN-CEE.
          10  W-DATE-IN-LTH-CEE    PIC S9(4) COMP VALUE 8.
          10  W-DATE-IN-STR-CEE    PIC X(8).
       01 FC.
          10  FC-SEV               PIC S9(4) COMP.
          10  FC-MSG               PIC S9(4) COMP.
          10  FC-CTW               PIC X.
          10  FC-FAC               PIC X(3).
          10  FC-ISI               PIC S9(8) COMP.

       LINKAGE SECTION.

       COPY SUPLIERL.

       PROCEDURE DIVISION USING
           SUPPLIER-CODE,
           SUPPLIER-TYPE,
           SUPPLIER-NAME,
           SUPPLIER-PERF,
           SUPPLIER-RATING,
           SUPPLIER-STATUS,
           SUPPLIER-ACT-DATE,
           ERROR-SUB,
           ERROR-SUB-MAX,
           ERROR-STRUCTURE.
      ***--------------------------------------------------------------
      *** MAIN - ROUTINE
      ***--------------------------------------------------------------
      D    DISPLAY 'Start Sub SUPPEDIT'.
      D    DISPLAY 'ERROR-SUB: ' ERROR-SUB.

           PERFORM 000-CHECK-SUPPLIER-CODE.
           PERFORM 100-CHECK-SUPPLIER-TYPE.
           PERFORM 200-CHECK-SUPPLIER-NAME.
           PERFORM 300-CHECK-SUPPLIER-PERF.
           PERFORM 400-CHECK-SUPPLIER-RATING.
           PERFORM 500-CHECK-SUPPLIER-STATUS.
           PERFORM 600-CHECK-SUPPLIER-ACT-DATE.

           GOBACK.                         *> Control returned to MAIN
      ***---------------------------------------------------------------
      *** SUPPLIER-CODE:
      *** has to be ANUM A to 9, max 5 CHAR, uppercase, no special char
      *** check will be done in 700-CHECK-ANUM
      *** If there is an error, move SUPPLIER-TYPE
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and call PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       000-CHECK-SUPPLIER-CODE.
      D    DISPLAY '000-CHECK-SUPPLIER-CODE'.
      D    DISPLAY 'SUPPLIER-CODE = ' SUPPLIER-CODE.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE 0 TO WS-LEN.                             *> init
           INSPECT FUNCTION REVERSE(SUPPLIER-CODE)
           TALLYING WS-LEN FOR LEADING SPACES.

           COMPUTE WS-LEN = LENGTH OF SUPPLIER-CODE - WS-LEN.

            IF WS-LEN > 5
      *        fill the error field
               MOVE 'SUPPLIER-CODE '       TO WS-ERROR-FIELD
               MOVE 'too many characters'  TO WS-ERROR-MESSAGE
               PERFORM 800-ERROR-MESSAGE
            ELSE
      *        value of SUPPLIER-CODE has to be between A-Z and 0-9
               MOVE SUPPLIER-CODE TO WS-TESTSTR
               PERFORM 700-CHECK-ANUM
               IF CHAR-FOUND = 'N'
      *           fill the error field
                  MOVE 'SUPPLIER-CODE '     TO WS-ERROR-FIELD
                  MOVE 'invalid characters' TO WS-ERROR-MESSAGE
                  PERFORM 800-ERROR-MESSAGE
               END-IF
            END-IF.
      ***---------------------------------------------------------------
      *** SUPPLIER-TYPE:
      *** has to be tested against 88-Values in copybook SUPLIERL.
      *** If the content of field doesn't match,
      *** move SUPPLIER-TYPE to ERROR-FLD(ERROR-SUB)
      *** and errormessage to ERROR-MSG(ERROR-SUB)
      *** and call PERFORM 800-ERROR-MESSAGE
      *** 01 SUPPLIER-TYPE       PIC       X(01).
      ***       88 VALID-SUPPLIER-TYPE VALUE 'S' 'D' 'M' 'I'.
      ***---------------------------------------------------------------
       100-CHECK-SUPPLIER-TYPE.
      D    DISPLAY '100-CHECK-SUPPLIER-TYPE'.
      D    DISPLAY 'SUPPLIER-TYPE = ' SUPPLIER-TYPE.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           IF NOT VALID-SUPPLIER-TYPE THEN
      *        fill the error field
               MOVE 'SUPPLIER-TYPE '  TO WS-ERROR-FIELD
               MOVE 'does not exist'  TO WS-ERROR-MESSAGE
               PERFORM 800-ERROR-MESSAGE
           END-IF.
      ***---------------------------------------------------------------
      *** SUPPLIER-NAME:
      *** has to be alhanumeric, including embedded blanks.
      *** check will be done in 750-CHECK-VALID-CHARACTER
      *** If the entire field is blank
      *** move SUPPLIER-NAME to ERROR-FLD(ERROR-SUB)
      *** and errormessage to ERROR-MSG(ERROR-SUB) and
      *** PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       200-CHECK-SUPPLIER-NAME.
      D    DISPLAY '200-CHECK-SUPPLIER-NAME'.
      D    DISPLAY 'SUPPLIER-NAME = ' SUPPLIER-NAME.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           IF SUPPLIER-NAME = ' '
              THEN
                 MOVE 'SUPPLIER-NAME ' TO WS-ERROR-FIELD
                 MOVE 'is blank '      TO WS-ERROR-MESSAGE
                 PERFORM 800-ERROR-MESSAGE
              ELSE
                 MOVE 0 TO WS-LEN.                             *> init
                 COMPUTE WS-LEN = LENGTH OF SUPPLIER-NAME
                 MOVE SUPPLIER-NAME      TO WS-TESTSTR

      *          value of has to be between A-Z, a-z, 0-9, ' '
                 PERFORM 750-CHECK-VALID-CHARACTER
                 IF CHAR-FOUND = 'N'
      *          fill the error field
                 MOVE 'SUPPLIER-NAME '     TO WS-ERROR-FIELD
                 MOVE 'invalid characters' TO WS-ERROR-MESSAGE
                 PERFORM 800-ERROR-MESSAGE
              END-IF.
      ***---------------------------------------------------------------
      *** SUPPLIER-PERF:
      *** has to be numeric.
      *** no numeric check because of declaration PIC S9
      *** If there is an error, move SUPPLIER-PERF
      *** to ERROR-FLD(ERROR-SUB) and errormessage to
      *** ERROR-MSG(ERROR-SUB) and PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       300-CHECK-SUPPLIER-PERF.
      D    DISPLAY '300-CHECK-SUPPLIER-PERF'.
      D    DISPLAY 'SUPPLIER-PERF = ' SUPPLIER-PERF.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           IF SUPPLIER-PERF IS NOT NUMERIC
      *       fill the error field
              MOVE 'SUPPLIER-PERF ' TO WS-ERROR-FIELD
              MOVE 'not numeric'    TO WS-ERROR-MESSAGE
              PERFORM 800-ERROR-MESSAGE
           END-IF.
      ***---------------------------------------------------------------
      *** SUPPLIER-RATING:
      *** has to be tested against 88-Values in copybook SUPLIERL.
      *** 01 SUPPLIER-RATING     PIC  X(01).
      ***       88 VALID-SUPPLIER-RATING VALUE '1' '2' '3'.
      *** If the content of field doesn't match,
      *** move SUPPLIER-RATING to ERROR-FLD(ERROR-SUB) and
      *** errormessage to ERROR-MSG(ERROR-SUB)
      *** and PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       400-CHECK-SUPPLIER-RATING.
      D    DISPLAY '400-CHECK-SUPPLIER-RATING'.
      D    DISPLAY 'SUPPLIER-RATING = ' SUPPLIER-RATING.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           IF NOT VALID-SUPPLIER-RATING THEN
      *       fill the error field
              MOVE 'SUPPLIER-RATING ' TO WS-ERROR-FIELD
              MOVE 'does not exist'   TO WS-ERROR-MESSAGE
              PERFORM 800-ERROR-MESSAGE
           END-IF.
      ***---------------------------------------------------------------
      *** SUPPLIER-STATUS:
      *** has to be tested against 88-Values in copybook SUPLIERL.
      *** 01 SUPPLIER-STATUS     PIC X(01).
      ***    88 VALID-SUPPLIER-STATUS VALUE '1' '2' '3'.
      *** If the content of field doesn't match,
      *** move SUPPLIER-RATING to ERROR-FLD(ERROR-SUB) and
      *** errormessage to ERROR-MSG(ERROR-SUB)
      *** and PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       500-CHECK-SUPPLIER-STATUS.
      D    DISPLAY '500-CHECK-SUPPLIER-STATUS'.
      D    DISPLAY 'SUPPLIER-STATUS = ' SUPPLIER-STATUS.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           IF NOT VALID-SUPPLIER-STATUS THEN
      *       fill the error field
              MOVE 'SUPPLIER-STATUS ' TO WS-ERROR-FIELD
              MOVE 'does not exist'   TO WS-ERROR-MESSAGE
              PERFORM 800-ERROR-MESSAGE
           END-IF.
      ***---------------------------------------------------------------
      *** SUPPLIER-ACT-DATE:
      *** has to be a valid calendardate, check with CEEDAYS- routine.
      *** If not, move SUPPLIER-ACT-DATE to ERROR-FLD(ERROR-SUB)
      *** and errormessage to ERROR-MSG(ERROR-SUB)
      *** and call PERFORM 800-ERROR-MESSAGE
      ***---------------------------------------------------------------
       600-CHECK-SUPPLIER-ACT-DATE.
      D    DISPLAY '600-CHECK-SUPPLIER-ACT-DATE'.
      D    DISPLAY 'SUPPLIER-ACT-DATE = ' SUPPLIER-ACT-DATE.

           IF ERROR-SUB >= ERROR-SUB-MAX
              THEN GOBACK
           END-IF.

           MOVE SUPPLIER-ACT-DATE TO W-DATE-IN-STR-CEE

           CALL 'CEEDAYS' USING W-DATE-IN-CEE
                 W-PICSTR-IN, W-INPUT-DATE-INT, FC

           IF FC-SEV NOT = ZERO
              MOVE 'SUPPLIER-ACT-DATE '       TO WS-ERROR-FIELD
              MOVE 'Bad Date'  TO WS-ERROR-MESSAGE
              PERFORM 800-ERROR-MESSAGE
           END-IF.
      ***---------------------------------------------------------------
      *** CHECK-ANUM:
      *** check against 88-Values "A" THRU "Z" , "0" THRU "9".
      *** If it doesn't match this demand, move 'N' to CHAR-FOUND
      ***---------------------------------------------------------------
       700-CHECK-ANUM.
      D    DISPLAY '700-CHECK-ANUM'.

           MOVE 'Y' TO CHAR-FOUND

           PERFORM WITH TEST AFTER VARYING IDX FROM 1 BY 1 UNTIL
              IDX > WS-LEN OR NOT ANUM-VALID
                   MOVE WS-TESTSTR(IDX:1) TO CHAR
           END-PERFORM.

           IF IDX > WS-LEN
              THEN MOVE 'Y' TO CHAR-FOUND
              ELSE MOVE 'N' TO CHAR-FOUND
           END-IF.
      ***---------------------------------------------------------------
      *** CHECK-VALID-CHARACTER: check against 88-Values
      *** 88 VALIDCHARACTER VALUE "A" THRU "Z" ,  "a" THRU "z",
      ***                         "0" THRU "9"," ", "-", "&", "/".
      *** If it doesn't match this demand, move 'N' to CHAR-FOUND
      ***---------------------------------------------------------------
       750-CHECK-VALID-CHARACTER.
      D    DISPLAY '750-CHECK-VALID-CHARACTER'.
      D    DISPLAY 'WS-TESTSTR = ' WS-TESTSTR.

           MOVE 'Y' TO CHAR-FOUND

           PERFORM WITH TEST AFTER VARYING IDX FROM 1 BY 1 UNTIL
              IDX > WS-LEN OR NOT VALIDCHARACTER
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTSUPX.
      * ------------------------------------------------------------
      * Final Exam for COBOL Course by Jon Sayles
      * *****************************************************************
      * PARTSUPX:
      * This Program reads sorted file input containing
      * four separate data groups:
      * PARTS, SUPPLIERS, ADRESSES and PURCHASE-ORDERS,
      * a 473 byte long record.It writes a report and
      * one output file for every data group, to be used
      * as load file for a corresponding DB2 table.
      * It performs control break
      * processing on the sort-key of the input file
      * (PART-NUMBER). A summary report is produced, one
      * block of information for every distinct PART-NUMBER,
      * all Purchase Orders within same part-number are
      * accumulated. No further
      * summary reporting is produced.
      *
      * Only valid data will be processed for report
      * printing, control break detection and file output.
      * Data validation is done by calling four
      * external data-edit subprograms: PARTEDIT,
      * ADDREDIT, SUPPEDIT, PO-EDIT.
      *
      * An error file is written with up to four leading
      * records containing error messages followed by a
      * data record containing  all fields of input
      * record in-error. A file with valid ZIP/ US State
      * combinations is used to load an internal table
      * which is passed to subrogram ADDREDIT.
      * Several data type conversion is done for
      * moving the input data to the
      * output files.
      *
      * The Go-Job consists of three steps:
      * 1.     Delete output files
      * 2. Sort input File (1,23,CH,A)
      * 3. Run PARTSUPP program
      *
      * Annotation: We assumed, that the data groups
      * PARTS and SUPPLIERS both depend on the sort
      * key PART-NUMBER. Nevertheless the program
      * is prepared to do control break processing
      * for PART-NUMBER + SUPPLIER CODE if requested.
      * Additional key fields (Primary and Foreign Keys)
      * have been added to the four output file records
      * for supporting join selects in case of loading
      * data into DB2.
      *
      *  Design, Implementation an testing:
      *  Team Hilde Schmidbauer/ Ralf Straube
      *
      *  VERSION 1.0     2020/09/09
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.  *> comment for suppress display
      * SOURCE-COMPUTER. IBM-Z WITH DEBUGGING MODE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO PARTSUPP
           FILE STATUS IS INPUT-FILE-ST.
           SELECT ZIP-STATE-FILE ASSIGN TO ZIPSTAT
           FILE STATUS IS ZIP-STATE-FILE-ST.

           SELECT ERROR-FILE ASSIGN TO BADRECS
           FILE STATUS IS ERROR-FILE-ST.

           SELECT PARTS-FILE ASSIGN TO PARTSFIL
           FILE STATUS IS PARTS-FILE-ST.

           SELECT SUPPLIERS-FILE ASSIGN TO SUPLFILE
           FILE STATUS IS SUPPLIERS-FILE-ST.

           SELECT ADDRESSES-FILE ASSIGN TO ADDRFILE
           FILE STATUS IS ADDRESSES-FILE-ST.

           SELECT PURCHORDS-FILE ASSIGN TO POFILE
           FILE STATUS IS PURCHORDS-FILE-ST.

           SELECT REPORT-FILE  ASSIGN TO PRTLINE
           FILE STATUS IS REPORT-FILE-ST.


       DATA DIVISION.
       FILE SECTION.
      * Input Files
       FD  INPUT-FILE
           RECORDING MODE IS F.
       01 PART-SUPP-ADDR-PO.
           COPY PARTSUPP.

       FD  ZIP-STATE-FILE
           RECORDING MODE IS F.
       01 ZIP-STATE-REC.
           COPY ADDRZIP.

       FD  ERROR-FILE
           RECORDING MODE IS F.
       01 ERROR-FILE-REC               PIC X(473).

       FD  PARTS-FILE
           RECORDING MODE IS F.
           COPY PARTS.
       FD  SUPPLIERS-FILE
           RECORDING MODE IS F.
           COPY SUPLIERS.
       FD  ADDRESSES-FILE
           RECORDING MODE IS F.
           COPY ADRESSES.
       FD  PURCHORDS-FILE
           RECORDING MODE IS F.
           COPY PURCHRDS.

      * Report
       FD  REPORT-FILE
           RECORDING MODE IS F.
       01 OUTPUT-LINE                  PIC X(132).


       WORKING-STORAGE SECTION.
       01 WFLD                         PIC 9(01).
       01 WS-CALLPGM                   PIC X(8).
       01 WS-WORKNUM                   PIC S9(8) COMP.
       01 WS-SUB                       PIC S9(8) COMP.
       01 WS-ZIPCODE-NUM-EDIT          PIC ZZZZZZZZZ9.


      * Error Table - filled by subprograms
       01 ERROR-SUB                    PIC 9(1) COMP.
       01 ERROR-SUB-MAX                PIC 9(1) COMP
                                                       VALUE 4.
       01 ERROR-STRUCTURE              PIC X(160).
       01 ERROR-ITEMS REDEFINES ERROR-STRUCTURE.
          05 ERROR-TABLE OCCURS 4 TIMES.
             10 ERROR-FLD              PIC X(20).
             10 ERROR-MSG              PIC X(20).

      * ZIP-STATE Table - loaded from input file, handed over
      * to ADDREDIT
       01 ZIP-STATE-SUB                PIC S9(3) COMP.
       01 ZIP-STATE-SUB-MAX            PIC S9(3) COMP
                                                       VALUE +200.
       01 ZIP-STATE-STRUCTURE          PIC X(6800).
                                          *> 34 * 200 Records
       01 ZIP-STATE-ITEMS REDEFINES ZIP-STATE-STRUCTURE.
          05 ZIP-STATE-TABLE OCCURS 200 TIMES INDEXED BY ZIP-STATE-IDX.
             10 STATE-NAME             PIC X(16).
             10 STATE-CODE             PIC X(2).
             10 ZIPCODE-RANGE-LO       PIC 9(8).
             10 ZIPCODE-RANGE-HI       PIC 9(8).
       01 ZS-TABLE-SUB-MAX             PIC 9(03) COMP
                                                       VALUE 200.
      * Maximum Value of Subcripts for table processing in
      * subprograms
       01 ADDR-TABLE-SUB-MAX           PIC 9(01) COMP
                                                       VALUE 3.
       01 WS-ADDR-SUB                  PIC 9(01).
       01 PO-SUB-MAX                   PIC 9(01) COMP
                                                       VALUE 3.
       01 WS-ZS-COMBI-VALID-IND        PIC X(1).
          88 ZS-COMBI-VALID                            VALUE 'Y'.
          88 ZS-COMBI-INVALID                          VALUE 'N'.


       01 PROGRAM-SWITCHES.
          05 INPUT-FILE-ST             PIC X(2).
             88 INPUT-FILE-OK                          VALUE '00'.
          05 REPORT-FILE-ST            PIC X(2).
             88 REPORT-FILE-OK                         VALUE '00'.
          05 ZIP-STATE-FILE-ST         PIC X(2).
             88 ZIP-STATE-FILE-OK                      VALUE '00'.
          05 ERROR-FILE-ST             PIC X(2).
             88 ERROR-FILE-OK                          VALUE '00'.
          05 PARTS-FILE-ST             PIC X(2).
             88 PARTS-FILE-OK                          VALUE '00'.
          05 SUPPLIERS-FILE-ST         PIC X(2).
             88 SUPPLIERS-FILE-OK                      VALUE '00'.
          05 ADDRESSES-FILE-ST         PIC X(2).
             88 ADDRESSES-FILE-OK                      VALUE '00'.
          05 PURCHORDS-FILE-ST         PIC X(2).
             88 PURCHORDS-FILE-OK                      VALUE '00'.
      * EOF indicators (Input files only)
          05 INPUT-FILE-STATUS         PIC X(01)       VALUE SPACE.
             88 INPUT-FILE-EOF                         VALUE 'Y'.
          05 ZIP-STATE-FILE-STATUS     PIC X(01)       VALUE SPACE.
             88 ZIP-STATE-FILE-EOF                     VALUE 'Y'.
      * Valid Data  indicator
          05 VALID-DATA-STATUS         PIC X(01)       VALUE SPACE.
             88 VALID-DATA-IND                         VALUE 'Y'.

       01 ACCUMS-AND-COUNTERS.
          05 WS-REC-KTR                PIC 9(7)        VALUE 0.         .
          05 WS-REC-KTR-VALID          PIC 9(7)        VALUE 0.
          05 CTR-LINES                 PIC 99          VALUE 0.
          05 CTR-PAGES                 PIC 999         VALUE 0.
          05 CTR-LINES-MAX             PIC 99          VALUE 3.
       01 CONTROL-GROUP-FIELDS.
      * Control Break indicator
          05 CTRLBRK-STATUS            PIC X(01)       VALUE SPACE.
             88 CTRLBRK-IND                            VALUE 'Y'.
          05 WS-GRP1-CTR-ACCUM.
             10 WS-GRP1-REC-CTR        PIC 9(7)        VALUE 0.
             10 PO-QUANT-ACCUM         PIC S9(11)      VALUE +0.
             10 PO-UNIT-PRICE-ACCUM    PIC S9(11)V99   VALUE +0.
             10 PO-COUNT               PIC 999         VALUE 0.
       01 WS-SAVE-GRP.
          10 PART-NUMBER-S             PIC        X(23)
                                                       VALUE SPACES.
          10 PART-NAME-S               PIC        X(14)
                                                       VALUE SPACES.
          10 WEEKS-LEAD-TIME-S         PIC        9(03)
                                                       VALUE 0.
          10 VEHICLE-MAKE-S            PIC        X(03)
                                                       VALUE SPACES.
             88 CHRYSLER-S                             VALUE 'CHR'.
             88 FORD-S                                 VALUE 'FOR'.
             88 GM-S                                   VALUE 'GM '.
             88 VOLKSWAGON-S                           VALUE 'VW '.
             88 TOYOTA-S                               VALUE 'TOY'.
             88 JAGUAR-S                               VALUE 'JAG'.
             88 PEUGEOT-S                              VALUE 'PEU'.
             88 BMW-S                                  VALUE 'BMW'.
          10 SUPPLIER-CODE-S           PIC X(5)        VALUE SPACES.
          10 SUPPLIER-NAME-S           PIC X(15)       VALUE SPACES.
          10 SUPPLIER-RATING-S         PIC        X(01)
                                                       VALUE SPACES.
             88 HIGHEST-QUALITY-S                      VALUE '3'.
             88 AVERAGE-QUALITY-S                      VALUE '2'.
             88 LOWEST-QUALITY-S                       VALUE '1'.
       05 ADDRESS-STRUCTURE-S          PIC X(219).
                                                 *> 73 * 3 Records
          05 ADDRESS-ITEMS-S REDEFINES ADDRESS-STRUCTURE-S.
             10 SUPP-ADDRESS-S OCCURS 3 TIMES INDEXED BY ADDRS-IDX.
                15 ADDRESS-TYPE-S      PIC X(01).
                   88 ORDER-ADDRESS-S                  VALUE '1'.
                   88 SCHED-ADDRESS-S                  VALUE '2'.
                   88 REMIT-ADDRESS-S                  VALUE '3'.
                15 ADDRESS-1-S         PIC        X(15).

                15 ADDRESS-2-S         PIC        X(15).
                15 ADDRESS-3-S         PIC        X(15).
                15 CITY-S              PIC        X(15).
                15 ADDR-STATE-S        PIC        X(02).
                15 ZIP-CODE-S          PIC        9(10).
          05 PURCHORD-STRUCTURE-S      PIC X(123).
                                              *> 41 * 3 Records
          05 PURCHORDS-ITEMS-S REDEFINES PURCHORD-STRUCTURE-S.
             10 PURCHASE-ORDER-S OCCURS 3 TIMES INDEXED BY POS-IDX.
                15 PO-NUMBER-S         PIC        X(06)
           .
                15 BUYER-CODE-S        PIC        X(03)
           .
                15 QUANTITY-S          PIC        S9(7)
           .
                15 UNIT-PRICE-S        PIC S9(7)V99.
                15 ORDER-DATE-S        PIC        9(08)
           .
                15 DELIVERY-DATE-S     PIC        9(08)
           .


      * Alternate I/O Records
       01 ERROR-REC.
          05 PART-NUMBER               PIC X(23).
          05 FILLER                    PIC X(2).
          05 ERROR-FLD                 PIC X(20).
          05 FILLER                    PIC X(2).
          05 ERROR-MSG                 PIC X(20).
          05 FILLER                    PIC X(406).

      * Print Lines
      * Header line
       01 HEADING-1.
          05 FILLER                    PIC X(25)       VALUE SPACE.
          05 FILLER                    PIC X(50)       VALUE
                '            PARTS/ SUPPLIER   R E P O R T         '.

          05 FILLER                    PIC X(8)        VALUE SPACES.
          05 FILLER                    PIC X(10)       VALUE 'PAGE NO'.
          05 PAGE-NUM                  PIC ZZ9         VALUE ZERO.
          05 FILLER                    PIC X(36)       VALUE SPACES.

       01 HEADING-2.
          05 FILLER                    PIC X(15)       VALUE
                'Part Name'.
          05 FILLER                    PIC X(05)       VALUE SPACES.
          05 FILLER                    PIC X(15)       VALUE
                'Weeks Lead Time'.
          05 FILLER                    PIC X(05)       VALUE SPACES.
          05 FILLER                    PIC X(12)       VALUE
                'Vehicle Make'.
          05 FILLER                    PIC X(05)       VALUE SPACES.
          05 FILLER                    PIC X(13)       VALUE
                'Supplier Name'.
          05 FILLER                    PIC X(07)       VALUE SPACES.
          05 FILLER                    PIC X(15)       VALUE
                'Supplier Rating'.
          05 FILLER                    PIC X(40)       VALUE SPACES.

       01 HEADING-3.
          05 FILLER                    PIC X(15)       VALUE ALL "=".
          05 FILLER                    PIC X(05)       VALUE SPACES.
          05 FILLER                    PIC X(15)       VALUE ALL "=".
          05 FILLER                    PIC X(05)       VALUE SPACES.
          05 FILLER                    PIC X(12)       VALUE ALL "=".
          05 FILLER                    PIC X(05)       VALUE SPACES.
          05 FILLER                    PIC X(13)       VALUE ALL "=".
          05 FILLER                    PIC X(07)       VALUE SPACES.
          05 FILLER                    PIC X(15)       VALUE ALL "=".
          05 FILLER                    PIC X(40)       VALUE SPACES.



       01 DETAIL-LINE.
          05 PART-NAME-O               PIC X(15).
          05 FILLER                    PIC X(05)       VALUE SPACES.
          05 WEEK-LEAD-TIME-O          PIC X(15).
          05 FILLER                    PIC X(05)       VALUE SPACES.
          05 VEHICLE-MAKE-O            PIC X(12).
          05 FILLER                    PIC X(05)       VALUE SPACES.
          05 SUPPLIER-NAME-O           PIC X(13).
          05 FILLER                    PIC X(07)       VALUE SPACES.
          05 SUPPLIER-RATING-O         PIC X(15).
          05 FILLER                    PIC X(40)       VALUE SPACES.

       01 BLANK-LINE.
          05 FILLER                    PIC X(132)      VALUE SPACES.

      * declare for the output statistic at the end of the report
       01 WS-TOTALS-REC1.
          05 FILLER                    PIC X(16)       VALUE
                'Order Address : '.
          05 ORDER-ADDR-O              PIC X(15).
          05 FILLER                    PIC X(01).
          05 O-CITY-O                  PIC X(15).
          05 FILLER                    PIC X(01).
          05 O-STATE-NAME-O            PIC X(16).
          05 FILLER                    PIC X(01).
          05 O-ZIP-CODE-O              PIC Z9(09).
          05 FILLER                    PIC X(57).

       01 WS-TOTALS-REC2.
          05 FILLER                    PIC X(16)       VALUE
                'Sched Address : '.
          05 SCHED-ADDR-O              PIC X(15).
          05 FILLER                    PIC X(01).
          05 S-CITY-O                  PIC X(15).
          05 FILLER                    PIC X(01).
          05 S-STATE-NAME-O            PIC X(16).
          05 FILLER                    PIC X(01).
          05 S-ZIP-CODE-O              PIC Z9(09).
          05 FILLER                    PIC X(57).
      *    address lines
       01 WS-TOTALS-REC3.
          05 FILLER                    PIC X(16)       VALUE
                'Remit Address : '.
          05 REMIT-ADDR-O              PIC X(15).
          05 FILLER                    PIC X(01).
          05 R-CITY-O                  PIC X(15).
          05 FILLER                    PIC X(01).
          05 R-STATE-NAME-O            PIC X(16).
          05 FILLER                    PIC X(01).
          05 R-ZIP-CODE-O              PIC Z9(09).
          05 FILLER                    PIC X(57).

      *    statistic  lines
       01 WS-TOTALS-REC4.
          05 FILLER                    PIC X(35)       VALUE
                'Total # Purchase Orders          :'.
          05 FILLER                    PIC X(9)        VALUE SPACES.
          05 TOTAL-PO-O                PIC ZZZZ9.
          05 FILLER                    PIC X(81)       VALUE SPACES.

       01 WS-TOTALS-REC5.
          05 FILLER                    PIC X(35)       VALUE
                'Total Price Purchade Orders      :'.
          05 TOTAL-PRICE-PO-O          PIC $$$,$$$,$$9.99.
          05 FILLER                    PIC X(83)       VALUE SPACES.

       01 WS-TOTALS-REC6.
          05 FILLER                    PIC X(34)       VALUE
                'Total Quantity in Purchase Orders:'.
          05 FILLER                    PIC X(4)        VALUE SPACES.
          05 TOTAL-QUANTITY-PO-O       PIC ZZZ,ZZZ,ZZ9.

          05 FILLER                    PIC X(83)       VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN.
      ************************************************************
      * Main                                                     *
      ************************************************************
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-MAIN UNTIL INPUT-FILE-EOF.
           PERFORM 700-WRITE-SUMMARY-REPORT.
           PERFORM 800-CLOSE-FILES.
           GOBACK.
       000-HOUSEKEEPING.
      ************************************************************
      * Inititialization Routine                                 *
      ************************************************************
      D    DISPLAY '000-HOUSEKEEPING'.
           PERFORM 300-OPEN-FILES.

           PERFORM 070-LOAD-ZS-TABLE.

           MOVE 31 TO CTR-LINES. *> Force Pagebreak at Start
      * Priming Read:
           PERFORM 400-READ-INPUT-FILE.
       070-LOAD-ZS-TABLE.
      ************************************************************
      * Load Table with ZIP State Combination from input file
      ************************************************************
      D    DISPLAY '070-LOAD-ZS-TABLE'.

      *    Init Zip-State-Table before loading
           INITIALIZE ZIP-STATE-ITEMS
              REPLACING NUMERIC DATA BY 0
              ALPHANUMERIC DATA BY " ".
      *    Load Table with ZIP Ranges and US States from File
           MOVE +1 TO ZIP-STATE-SUB.
           PERFORM UNTIL ZIP-STATE-FILE-EOF OR
              ZIP-STATE-SUB > ZIP-STATE-SUB-MAX
                   READ ZIP-STATE-FILE
                   AT END
                      MOVE "Y" TO ZIP-STATE-FILE-STATUS
                   END-READ

                   MOVE STATE-NAME IN ZIP-STATE-REC TO
                      STATE-NAME IN ZIP-STATE-TABLE(ZIP-STATE-SUB)

                   MOVE STATE-CODE IN ZIP-STATE-REC TO
                      STATE-CODE IN ZIP-STATE-TABLE(ZIP-STATE-SUB)

      *    Convert left-bounded pic x to pic 9

                   COMPUTE ZIPCODE-RANGE-LO IN ZIP-STATE-TABLE
                      (ZIP-STATE-SUB) = FUNCTION NUMVAL
                      (ZIPCODE-RANGE-LO IN ZIP-STATE-REC)

                   COMPUTE ZIPCODE-RANGE-HI IN ZIP-STATE-TABLE
                      (ZIP-STATE-SUB) = FUNCTION NUMVAL
                      (ZIPCODE-RANGE-HI IN ZIP-STATE-REC)

      *            DISPLAY ZIP-STATE-SUB
      *                    " /Rec: "
      *                    STATE-NAME IN ZIP-STATE-REC
      *                     "/"
      *                    STATE-CODE IN ZIP-STATE-REC
      *                    "/"
      *                    ZIPCODE-RANGE-LO IN ZIP-STATE-REC
      *                    "/"
      *                    ZIPCODE-RANGE-HI IN ZIP-STATE-REC
      *
      *                    "Table: "
      *                     STATE-NAME IN ZIP-STATE-TABLE(ZIP-STATE-SUB)
      *                     "/"
      *                    STATE-CODE IN ZIP-STATE-TABLE(ZIP-STATE-SUB)
      *                    "/"
      *                    ZIPCODE-RANGE-LO IN ZIP-STATE-TABLE
      *               (ZIP-STATE-SUB)
      *                    "/"
      *                    ZIPCODE-RANGE-HI IN ZIP-STATE-TABLE
      *               (ZIP-STATE-SUB)

                   ADD +1 TO ZIP-STATE-SUB
           END-PERFORM.
       100-MAIN.
      ************************************************************
      * Main Routine                                             *
      ************************************************************
      D    DISPLAY '100-MAIN'.
           PERFORM 210-PREPROC-GRP1.
           PERFORM 250-GRP1-PROCESSING UNTIL CTRLBRK-IND OR
              INPUT-FILE-EOF.
           IF WS-GRP1-REC-CTR > 0 THEN   *> only when at least
              PERFORM 280-POSTPROC-GRP1  *> one valid record in
           END-IF.                       *> GRP1
       220-VALIDATE-DATA.
      ************************************************************
      * Data validation by calling subprograms                   *
      ************************************************************
      D    DISPLAY '220-VALIDATE-DATA'.
      * Initialize error table filled by data edit subprograms
           MOVE ZERO TO ERROR-SUB.
           MOVE SPACES TO ERROR-STRUCTURE.
      * Default: No Errrors
           MOVE "Y" TO VALID-DATA-STATUS.


      * Call first data edit subprogram

           MOVE 0 TO RETURN-CODE.
           MOVE 'PARTEDIT' TO WS-CALLPGM.
           CALL WS-CALLPGM USING
              PART-NUMBER IN PART-SUPP-ADDR-PO,
              BLUEPRINT-NUMBER IN PART-SUPP-ADDR-PO,
              UNIT-OF-MEASURE IN PART-SUPP-ADDR-PO,
              WEEKS-LEAD-TIME IN PART-SUPP-ADDR-PO,
              VEHICLE-MAKE IN PART-SUPP-ADDR-PO,
              VEHICLE-MODEL IN PART-SUPP-ADDR-PO,
              VEHICLE-YEAR IN PART-SUPP-ADDR-PO,
              ERROR-SUB,
              ERROR-SUB-MAX,
              ERROR-STRUCTURE.


      D    DISPLAY "PARTEDIT called : " WS-REC-KTR '/' ERROR-SUB.

           IF RETURN-CODE > 0 OR ERROR-SUB > 0
              THEN
              MOVE "N" TO VALID-DATA-STATUS
           END-IF.


      * Call next data edit subprogram only when maximimum error
      * count not reached

           IF ERROR-SUB < ERROR-SUB-MAX THEN
              MOVE 'SUPPEDIT' TO WS-CALLPGM
              CALL WS-CALLPGM USING
                 SUPPLIER-CODE IN PART-SUPP-ADDR-PO
                 SUPPLIER-TYPE IN PART-SUPP-ADDR-PO
                 SUPPLIER-NAME IN PART-SUPP-ADDR-PO
                 SUPPLIER-PERF IN PART-SUPP-ADDR-PO
                 SUPPLIER-RATING IN PART-SUPP-ADDR-PO
                 SUPPLIER-STATUS IN PART-SUPP-ADDR-PO
                 SUPPLIER-ACT-DATE IN PART-SUPP-ADDR-PO
                 ERROR-SUB,
                 ERROR-SUB-MAX,
                 ERROR-STRUCTURE
           END-IF.

      D    DISPLAY "SUPPEDIT called  : " WS-REC-KTR '/' ERROR-SUB.

           IF RETURN-CODE > 0 OR ERROR-SUB > 0
              THEN
              MOVE "N" TO VALID-DATA-STATUS
           END-IF.


      * Call next data edit subprogram only when maximimum error
      * count not reached

           IF ERROR-SUB < ERROR-SUB-MAX THEN
              MOVE 'ADDREDIT' TO WS-CALLPGM
              CALL WS-CALLPGM USING
                 ADDRESS-STRUCTURE,
                 ZIP-STATE-STRUCTURE
                 ZS-TABLE-SUB-MAX
                 ADDR-TABLE-SUB-MAX
                 ERROR-SUB
                 ERROR-SUB-MAX
                 ERROR-STRUCTURE
           END-IF.


      D    DISPLAY "ADDREDIT called : " WS-REC-KTR '/' ERROR-SUB.

           IF RETURN-CODE > 0 OR ERROR-SUB > 0
              THEN
              MOVE "N" TO VALID-DATA-STATUS
           END-IF.


      * Call next data edit subprogram only when maximimum error
      * count not reached

           IF ERROR-SUB < ERROR-SUB-MAX THEN

              MOVE 'POEDIT' TO WS-CALLPGM
              CALL WS-CALLPGM USING
                 PURCHORD-STRUCTURE,
                 PO-SUB-MAX,
                 ERROR-SUB,
                 ERROR-SUB-MAX,
                 ERROR-STRUCTURE
           END-IF.

      D    DISPLAY "POEDIT called : " WS-REC-KTR '/' ERROR-SUB.

           IF RETURN-CODE > 0 OR ERROR-SUB > 0
              THEN
              MOVE "N" TO VALID-DATA-STATUS
           END-IF.

      D    DISPLAY '220-VALIDATE-DATA-END'
      D             '/'
      D             WS-REC-KTR
      D             '/'
      D             ERROR-SUB.
      D    DISPLAY '/' ERROR-STRUCTURE.
       210-PREPROC-GRP1.
      ************************************************************
      * Pre Process Control Group 1
      ************************************************************
      D    DISPLAY '210-PREPROC-GRP1'.

           INITIALIZE WS-GRP1-CTR-ACCUM.

      * Set up Group Control - saving fields for use
      * in Group Control, printing values of saved record
      * in 500-WRITE-REPORT and writing DB2 Load Files in
      * 610-WRITE-DB2LOAD-FILES


           MOVE PART-NUMBER IN PART-SUPP-ADDR-PO TO PART-NUMBER-S

      *  Candidates for group control are PART-NUMBER + SUPPLIER-CODE
      *  At the moment, only PART-NUMBER is used, according to
      *  specificatioh. This can be easily expanded by additional
      *  comparisons in 250-GRP1-PROCESSING

           MOVE SUPPLIER-CODE IN PART-SUPP-ADDR-PO TO SUPPLIER-CODE-S.

           MOVE PART-NAME IN PART-SUPP-ADDR-PO TO PART-NAME-S.
           MOVE WEEKS-LEAD-TIME IN PART-SUPP-ADDR-PO TO
              WEEKS-LEAD-TIME-S.
           MOVE VEHICLE-MAKE IN PART-SUPP-ADDR-PO TO VEHICLE-MAKE-S.
           MOVE SUPPLIER-NAME IN PART-SUPP-ADDR-PO TO SUPPLIER-NAME-S.
           MOVE SUPPLIER-RATING IN PART-SUPP-ADDR-PO TO
              SUPPLIER-RATING-S.

           MOVE ADDRESS-STRUCTURE TO ADDRESS-STRUCTURE-S.
           MOVE PURCHORD-STRUCTURE TO PURCHORD-STRUCTURE-S.

       250-GRP1-PROCESSING.
      ************************************************************
      * Main Process Control Group 1
      ************************************************************
      D     DISPLAY '250-GRP1-PROCESSING'.


      D    DISPLAY PART-NUMBER IN PART-SUPP-ADDR-PO
      D             '/'
      D             PART-NUMBER-S.
           ADD 1 TO WS-REC-KTR.
      D    DISPLAY 'Record#: ' WS-REC-KTR.

           PERFORM 220-VALIDATE-DATA.

      * Process only valid Data
           IF VALID-DATA-IND THEN
              ADD 1 TO WS-REC-KTR-VALID *> count valid recorsds only
              ADD 1 TO WS-GRP1-REC-CTR
      D       DISPLAY 'Valid Record##: ' WS-REC-KTR-VALID
              PERFORM 600-FILL-DB2LOAD-FILES
              PERFORM 270-ACCUMULATE-ORDERS
      * Read next record
              PERFORM 400-READ-INPUT-FILE
      * Test for Control Break (last step in group processing)
              IF PART-NUMBER IN PART-SUPP-ADDR-PO
                 NOT EQUAL TO PART-NUMBER-S
                 THEN
                 PERFORM 260-CONTROL-BREAK-GRP1
              END-IF
      * Process invalid data
           ELSE
              PERFORM 650-WRITE-ERROR-RECORDS
              PERFORM 400-READ-INPUT-FILE *> next Record, no Output
              PERFORM 210-PREPROC-GRP1 *>
           END-IF.

       260-CONTROL-BREAK-GRP1.
      ************************************************************
      * Control Break Processing Control Group1
      ************************************************************
      D    DISPLAY '260-CONTROL-BREAK-GRP1'.
      D    DISPLAY "** Control Break Group1 **".
      D    DISPLAY PART-NUMBER IN PART-SUPP-ADDR-PO.
      D    DISPLAY PART-NUMBER-S.
           MOVE "Y" TO CTRLBRK-STATUS.


       270-ACCUMULATE-ORDERS.
      ***********************************************************
      * Accumulate Orders: 3 for every record in group          *
      ***********************************************************
      D    DISPLAY '270-ACCUMULATE-ORDERS'.

           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL
              WS-SUB > PO-SUB-MAX
                   ADD QUANTITY IN PURCHORDS-ITEMS(WS-SUB)
                      TO PO-QUANT-ACCUM
                   ADD UNIT-PRICE IN PURCHORDS-ITEMS(WS-SUB)
                      TO PO-UNIT-PRICE-ACCUM
                   ADD +1 TO PO-COUNT
           END-PERFORM.

       280-POSTPROC-GRP1.
      ************************************************************
      * Post Processing Control Group1
      ************************************************************
      D    DISPLAY '280-POSTPROC-GRP1'.

      D    DISPLAY 'Record#: ' WS-REC-KTR.
      D    DISPLAY 'Valid Record##: ' WS-REC-KTR-VALID.
      D    DISPLAY "Record Count Group1###: " WS-GRP1-REC-CTR.

      * Write Report columns

           PERFORM 500-WRITE-REPORT.

      * Write DB2LOAD-FILES

           PERFORM 610-WRITE-DB2LOAD-FILES.

      * Write report totals

           MOVE PO-COUNT TO TOTAL-PO-O.
           MOVE PO-UNIT-PRICE-ACCUM TO TOTAL-PRICE-PO-O.
           MOVE PO-QUANT-ACCUM TO TOTAL-QUANTITY-PO-O.

           WRITE OUTPUT-LINE FROM WS-TOTALS-REC4
              AFTER ADVANCING 4.
           WRITE OUTPUT-LINE FROM WS-TOTALS-REC5
              AFTER ADVANCING 2.
           WRITE OUTPUT-LINE FROM WS-TOTALS-REC6
              AFTER ADVANCING 2.

           ADD +6 TO CTR-LINES.

           MOVE "N" TO CTRLBRK-STATUS.
           INITIALIZE WS-GRP1-CTR-ACCUM.

       300-OPEN-FILES.
      *************************************************************
      * Open files                                                *
      *************************************************************
      D    DISPLAY '300-OPEN-FILES'.
           OPEN INPUT INPUT-FILE.
           IF NOT INPUT-FILE-OK
              THEN
              DISPLAY 'Input File Problem: ' INPUT-FILE-ST
              GOBACK
           END-IF.

           OPEN INPUT ZIP-STATE-FILE.
           IF NOT ZIP-STATE-FILE-OK
              THEN
              DISPLAY 'ZIP-STATE-File Problem: ' ZIP-STATE-FILE-ST
              GOBACK
           END-IF.


           OPEN OUTPUT REPORT-FILE.
           IF NOT REPORT-FILE-OK
              THEN
              DISPLAY ' Report-FILE Problem: ' REPORT-FILE-ST
              GOBACK
           END-IF.

           OPEN OUTPUT ERROR-FILE.
           IF NOT ERROR-FILE-OK
              THEN
              DISPLAY 'ERROR-FILE Problem: ' ERROR-FILE-ST
              GOBACK
           END-IF.

           OPEN OUTPUT PARTS-FILE.
           IF NOT PARTS-FILE-OK
              THEN
              DISPLAY ' PARTS-FILE Problem: ' PARTS-FILE-ST
              GOBACK
           END-IF.

           OPEN OUTPUT SUPPLIERS-FILE.
           IF NOT ZIP-STATE-FILE-OK
              THEN
              DISPLAY ' SUPPLIERS-FILE Problem: ' SUPPLIERS-FILE-ST
              GOBACK
           END-IF.

           OPEN OUTPUT ADDRESSES-FILE.
           IF NOT ADDRESSES-FILE-OK
              THEN
              DISPLAY 'ADDRESSES-FILE Problem: ' ADDRESSES-FILE-ST
              GOBACK
           END-IF.

           OPEN OUTPUT PURCHORDS-FILE.
           IF NOT PURCHORDS-FILE-OK
              THEN
              DISPLAY 'ZIP-STATE-File Problem: ' PURCHORDS-FILE-ST
              GOBACK
           END-IF.

       400-READ-INPUT-FILE.
      ************************************************************
      * Read Input record                                        *
      ************************************************************
      D    DISPLAY '400-READ-INPUT-FILE'.
           READ INPUT-FILE
           AT END
              MOVE "Y" TO INPUT-FILE-STATUS
           END-READ.

       500-WRITE-REPORT.
      ************************************************************
      * Filling detail line from saved record                    *
      ************************************************************
      D    DISPLAY '500-WRITE-REPORT'.
           IF CTR-LINES IS GREATER THAN CTR-LINES-MAX
              THEN
              PERFORM 510-PAGE-CHANGE-RTN
           END-IF.

      *  Fill Columns of Report from saved record in WS-SAVE-GRP.
           MOVE PART-NAME-S TO PART-NAME-O.

           MOVE WEEKS-LEAD-TIME-S TO
              WEEK-LEAD-TIME-O.

           EVALUATE TRUE
           WHEN CHRYSLER-S
                MOVE 'CHRYSLER' TO VEHICLE-MAKE-O
           WHEN FORD-S
                MOVE 'FORD' TO VEHICLE-MAKE-O
           WHEN GM-S
                MOVE 'GM' TO VEHICLE-MAKE-O
           WHEN VOLKSWAGON-S
                MOVE 'VOLKSWAGON' TO VEHICLE-MAKE-O
           WHEN TOYOTA-S
                MOVE 'TOYOTA' TO VEHICLE-MAKE-O
           WHEN PEUGEOT-S
                MOVE 'PEUGEOT' TO VEHICLE-MAKE-O
           WHEN BMW-S
                MOVE 'BMW' TO VEHICLE-MAKE-O
           WHEN OTHER
                MOVE 'UNKNOWN' TO VEHICLE-MAKE-O
           END-EVALUATE.

           MOVE SUPPLIER-NAME-S TO SUPPLIER-NAME-O.

           EVALUATE TRUE
           WHEN HIGHEST-QUALITY-S
                MOVE 'HIGHEST QUALITY' TO
                   SUPPLIER-RATING-O
           WHEN AVERAGE-QUALITY-S
                MOVE 'AVERAGE QUALITY' TO
                   SUPPLIER-RATING-O
           WHEN LOWEST-QUALITY-S
                MOVE 'LOWEST QUALITY' TO
                   SUPPLIER-RATING-O
           WHEN OTHER
                MOVE 'UNKNOWN' TO SUPPLIER-RATING-O
           END-EVALUATE.

           WRITE OUTPUT-LINE FROM DETAIL-LINE
              AFTER ADVANCING 3.

           ADD 4 TO CTR-LINES.

      * Filling Address Block by sequential table search
      * Only Address Line 1 (ADDRESS-1) is printed
      *
      * init outputfields with spaces                  SC01
           MOVE SPACES TO ORDER-ADDR-O.
           MOVE SPACES TO O-CITY-O.
           MOVE SPACES TO O-STATE-NAME-O.
           MOVE ZERO TO O-ZIP-CODE-O.

           MOVE SPACES TO SCHED-ADDR-O.
           MOVE SPACES TO S-CITY-O.
           MOVE SPACES TO S-STATE-NAME-O.
           MOVE ZERO TO S-ZIP-CODE-O.

           MOVE SPACES TO REMIT-ADDR-O.
           MOVE SPACES TO R-CITY-O.
           MOVE SPACES TO R-STATE-NAME-O.
           MOVE ZERO TO R-ZIP-CODE-O.
      * end init                                       SC01

           SET ADDRS-IDX TO 1.
           PERFORM VARYING ADDRS-IDX FROM 1 BY 1     *> SC01
              UNTIL ADDRS-IDX > ADDR-TABLE-SUB-MAX   *> SC01
                   SEARCH SUPP-ADDRESS-S
                   WHEN ORDER-ADDRESS-S(ADDRS-IDX)
                        MOVE ADDRESS-1-S(ADDRS-IDX) TO ORDER-ADDR-O
                        MOVE CITY-S(ADDRS-IDX) TO O-CITY-O
                        MOVE ADDR-STATE-S(ADDRS-IDX) TO O-STATE-NAME-O
                        MOVE ZIP-CODE-S(ADDRS-IDX) TO O-ZIP-CODE-O

                   WHEN SCHED-ADDRESS-S(ADDRS-IDX)         *>SC01
                        MOVE ADDRESS-1-S(ADDRS-IDX) TO SCHED-ADDR-O
                        MOVE CITY-S(ADDRS-IDX) TO S-CITY-O
                        MOVE ADDR-STATE-S(ADDRS-IDX) TO S-STATE-NAME-O
                        MOVE ZIP-CODE-S(ADDRS-IDX) TO S-ZIP-CODE-O

                   WHEN REMIT-ADDRESS-S(ADDRS-IDX)         *> SC01
                        MOVE ADDRESS-1-S(ADDRS-IDX) TO REMIT-ADDR-O
                        MOVE CITY-S(ADDRS-IDX) TO R-CITY-O
                        MOVE ADDR-STATE-S(ADDRS-IDX) TO R-STATE-NAME-O
                        MOVE ZIP-CODE-S(ADDRS-IDX) TO R-ZIP-CODE-O
                   END-SEARCH
           END-PERFORM.          *> SC01

           WRITE OUTPUT-LINE FROM WS-TOTALS-REC1
              AFTER ADVANCING 6.
           WRITE OUTPUT-LINE FROM WS-TOTALS-REC2
              AFTER ADVANCING 2.
           WRITE OUTPUT-LINE FROM WS-TOTALS-REC3
              AFTER ADVANCING 2.

           ADD 8 TO CTR-LINES.

       510-PAGE-CHANGE-RTN.
      ************************************************************
      * Page Break Routine                                       *
      ************************************************************
      D    DISPLAY '510-PAGE-CHANGE-RTN'.

           ADD 1 TO CTR-PAGES.
           MOVE CTR-PAGES TO PAGE-NUM.

           WRITE OUTPUT-LINE FROM HEADING-1
              AFTER ADVANCING PAGE.
           WRITE OUTPUT-LINE FROM HEADING-2
              AFTER ADVANCING 4.
           WRITE OUTPUT-LINE FROM HEADING-3
              AFTER ADVANCING 2.

           MOVE ZERO TO CTR-LINES.

       600-FILL-DB2LOAD-FILES.
      ***********************************************************
      * Fill I/O Areas for files as assigned in 600-FILL-DB2... *
      * Files are written in 610-WRITE-DB2..                    *
      * after group change is detected                          *
      ***********************************************************
      D    DISPLAY '600-FILL-DB2LOAD-FILES'.



             *> some receiving fields smaller, content is validated
             *> -> MOD-REF
           MOVE SUPPLIER-CODE IN PART-SUPP-ADDR-PO(1:5)
              TO SUPPLIER-CODE IN PARTS-FILE.
           MOVE PART-NUMBER IN PART-SUPP-ADDR-PO
              TO PART-NUMBER IN PARTS-FILE.
           MOVE PART-NAME IN PART-SUPP-ADDR-PO
              TO PART-NAME IN PARTS-FILE.
           MOVE SPEC-NUMBER IN PART-SUPP-ADDR-PO
              TO SPEC-NUMBER IN PARTS-FILE.
           MOVE GOVT-COMML-CODE IN PART-SUPP-ADDR-PO
              TO GOVT-COMML-CODE IN PARTS-FILE.
           MOVE BLUEPRINT-NUMBER IN PART-SUPP-ADDR-PO(1:5)
              TO BLUEPRINT-NUMBER IN PARTS-FILE.
           MOVE UNIT-OF-MEASURE IN PART-SUPP-ADDR-PO
              TO UNIT-OF-MEASURE IN PARTS-FILE.
           MOVE WEEKS-LEAD-TIME IN PART-SUPP-ADDR-PO
              TO WEEKS-LEAD-TIME IN PARTS-FILE.
           MOVE VEHICLE-MAKE IN PART-SUPP-ADDR-PO
              TO VEHICLE-MAKE IN PARTS-FILE.
           MOVE VEHICLE-MODEL IN PART-SUPP-ADDR-PO(1:5)
              TO VEHICLE-MODEL IN PARTS-FILE.
           MOVE VEHICLE-YEAR IN PART-SUPP-ADDR-PO
              TO VEHICLE-YEAR IN PARTS-FILE.


      *    Supplier File assign all fields

              *> receiving field smaller, content is validated
           MOVE SUPPLIER-CODE IN PART-SUPP-ADDR-PO(1:5)
              TO SUPPLIER-CODE IN SUPPLIERS-FILE.
           MOVE SUPPLIER-TYPE IN PART-SUPP-ADDR-PO
              TO SUPPLIER-TYPE IN SUPPLIERS-FILE.
           MOVE SUPPLIER-NAME IN PART-SUPP-ADDR-PO TO
              SUPPLIER-NAME IN SUPPLIERS-FILE.
           MOVE SUPPLIER-PERF IN PART-SUPP-ADDR-PO TO
              SUPPLIER-PERF IN SUPPLIERS-FILE.
           MOVE SUPPLIER-RATING IN PART-SUPP-ADDR-PO TO
              SUPPLIER-RATING IN SUPPLIERS-FILE.
           MOVE SUPPLIER-STATUS IN PART-SUPP-ADDR-PO TO
              SUPPLIER-STATUS IN SUPPLIERS-FILE.
           *> moving  PIC x to PIC 9 (equal length)
           MOVE SUPPLIER-ACT-DATE IN PART-SUPP-ADDR-PO
              TO SUPPLIER-ACT-DATE IN SUPPLIERS-FILE.

      *    Adresses File and PURCHORDER File are completely filled
      *    from WS-SAVE-GRP-FIELDS before writing



       610-WRITE-DB2LOAD-FILES.
      ***********************************************************
      * Write Files after control break detection               *
      * for loading DB2 tables according to a normalized        *
      * Data Model with Primary and Foreign Keys                *
      ***********************************************************
      D    DISPLAY '610-WRITE-DB2LOAD-FILES'.

      D    DISPLAY "PART-FILE"
      D    DISPLAY PARTS IN PARTS-FILE.


      * PARTS File: write record from previously filled i/o structure
      * in 250-GRP1-Processing, filled from last record of input file
      * before control break was detected

           WRITE PARTS IN PARTS-FILE.
           IF NOT PARTS-FILE-OK
              THEN
              DISPLAY ' PARTS-FILE Problem: ' PARTS-FILE-ST
              GOBACK
           END-IF.

      * suppliers file: same as above

      D    DISPLAY "SUPPLIERS-FILE"
      D    DISPLAY SUPPLIERS IN SUPPLIERS-FILE.

           WRITE SUPPLIERS IN SUPPLIERS-FILE.
           IF NOT SUPPLIERS-FILE-OK
              THEN
              DISPLAY ' SUPPLIERS-FILE Problem: ' SUPPLIERS-FILE-ST
              GOBACK
           END-IF.

      * Adresses: write n records from  entries
      * saved in WS-SAVE-GRP before control break was detected

           PERFORM VARYING WS-SUB FROM 1 BY 1
              UNTIL WS-SUB > ADDR-TABLE-SUB-MAX

      * Assigning additional Primary Key Supplier Code for DB2

                   MOVE SUPPLIER-CODE-S(1:5)
                      TO SUPPLIER-CODE IN ADDRESSES-FILE

      * Assigning Address Details


                   MOVE ADDRESS-TYPE-S(WS-SUB)
                      TO ADDRESS-TYPE IN SUPP-ADRESS-DETAILS
                   MOVE ADDRESS-1-S(WS-SUB)
                      TO ADDRESS-1 IN SUPP-ADRESS-DETAILS
                   MOVE ADDRESS-2-S(WS-SUB)
                      TO ADDRESS-2 IN SUPP-ADRESS-DETAILS
                   MOVE ADDRESS-3-S(WS-SUB) TO
                      ADDRESS-3 IN SUPP-ADRESS-DETAILS
                   MOVE CITY-S(WS-SUB)
                      TO CITY IN SUPP-ADRESS-DETAILS
                   MOVE ADDR-STATE-S(WS-SUB)
                      TO ADDR-STATE IN SUPP-ADRESS-DETAILS
                  *> Converting PIC 9(10) to PIC X(5)

                   MOVE ZIP-CODE-S(WS-SUB)
                      TO WS-ZIPCODE-NUM-EDIT
                   MOVE WS-ZIPCODE-NUM-EDIT(6:5) TO
                      ZIP-CODE IN SUPP-ADRESS-DETAILS                   ar) .


      D            DISPLAY "Loop :" WS-SUB
      D            DISPLAY "ADRESSES-FILE"
      D            DISPLAY SUPP-ADDRESS IN ADDRESSES-FILE

      *     Write  record

                   WRITE SUPP-ADDRESS IN ADDRESSES-FILE
                   IF NOT ADDRESSES-FILE-OK
                      THEN
                      DISPLAY ' ADDRESSES-FILE Problem: '
                              ADDRESSES-FILE-ST
                      GOBACK
                   END-IF
           END-PERFORM.

      * PRCHRDS: write n records from   entries
      * saved in WS-SAVE-GRP before control break was detected

           PERFORM VARYING WS-SUB FROM 1 BY 1
              UNTIL WS-SUB > PO-SUB-MAX

      *     Assigning additional foreign keys for DB2

                   MOVE PART-NUMBER-S
                      TO PART-NUMBER IN PURCHASE-ORDER
                   MOVE SUPPLIER-CODE-S(1:5)
                      TO SUPPLIER-CODE IN PURCHASE-ORDER

      *     Assignig Purchord Details


                   MOVE PO-NUMBER-S(WS-SUB) TO
                      PO-NUMBER IN ORDER-DETAILS
                   MOVE BUYER-CODE-S(WS-SUB)
                      TO BUYER-CODE IN ORDER-DETAILS

                  *> converting binary, packed decimal to
                  *> zoned decimal
                   COMPUTE QUANTITY IN ORDER-DETAILS = QUANTITY-S
                      (WS-SUB)

                   COMPUTE UNIT-PRICE IN ORDER-DETAILS = UNIT-PRICE-S
                      (WS-SUB)

                   *> Converting PIC X to PIC 9


                   MOVE ORDER-DATE-S(WS-SUB)
                      TO ORDER-DATE IN ORDER-DETAILS
                   MOVE DELIVERY-DATE-S(WS-SUB)
                      TO DELIVERY-DATE IN ORDER-DETAILS






      D            DISPLAY "Loop :" WS-SUB
      D            DISPLAY "PURCHORDS-FILE"
      D            DISPLAY PURCHASE-ORDER IN PURCHORDS-FILE


      *     Write  record

                   WRITE PURCHASE-ORDER IN PURCHORDS-FILE
                   IF NOT PURCHORDS-FILE-OK
                      THEN
                      DISPLAY ' PURCHORDS-FILE Problem: '
                              PURCHORDS-FILE-ST
                      GOBACK
                   END-IF
           END-PERFORM.



       650-WRITE-ERROR-RECORDS.
      ***********************************************************
      * Write max 5 records to Error File:                      *
      * 1 to 4 heading error message records followed by        *       *
      * complete input record                                   *
      ***********************************************************
      D    DISPLAY '650-WRITE-ERROR-RECORDS'.

           MOVE SPACES TO ERROR-FILE-REC.

      *    Move Error Messages from Data Edits to Error-File
           IF RETURN-CODE > 0 OR ERROR-SUB > 0
              THEN
              PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL
                 WS-SUB > ERROR-SUB
                      MOVE PART-NUMBER IN PART-SUPP-ADDR-PO TO
                         PART-NUMBER IN ERROR-REC

                      MOVE ERROR-FLD IN ERROR-TABLE(WS-SUB)
                         TO ERROR-FLD IN ERROR-REC

                      MOVE ERROR-MSG IN ERROR-TABLE(WS-SUB)
                         TO ERROR-MSG IN ERROR-REC

      D               DISPLAY "Loop : " WS-SUB
      D               DISPLAY 'ERROR-FILE : '
      D               DISPLAY ERROR-REC


                      WRITE ERROR-FILE-REC FROM ERROR-REC

                      IF NOT ERROR-FILE-OK
                         THEN
                         DISPLAY ' Error-FILE Problem: '
                                 ERROR-FILE-ST
                         GOBACK
                      END-IF
              END-PERFORM.

      *    Move Input Record to ERROR File                        *
           MOVE PART-SUPP-ADDR-PO TO ERROR-FILE-REC.

           WRITE ERROR-FILE-REC.


           IF NOT ERROR-FILE-OK
              THEN
              DISPLAY ' Error-FILE Problem: '
                      ERROR-FILE-ST
              GOBACK
           END-IF.
       700-WRITE-SUMMARY-REPORT.
      ***********************************************************
      * Write summary at end of report                          *
      ***********************************************************
      D    DISPLAY '700-WRITE-SUMMARY'.
      D    DISPLAY "Records read / valid :".

      D    DISPLAY WS-REC-KTR " / " WS-REC-KTR-VALID.

       800-CLOSE-FILES.
      ***********************************************************
      * Close all Files                                         *
      ***********************************************************
      D    DISPLAY '800-CLOSE-FILES'.
           CLOSE INPUT-FILE.
           IF NOT INPUT-FILE-OK
              THEN
              DISPLAY 'Input File Problem: ' INPUT-FILE-ST
              GOBACK
           END-IF.

           CLOSE ZIP-STATE-FILE.
           IF NOT ZIP-STATE-FILE-OK
              THEN
              DISPLAY 'ZIP-STATE-File Problem: ' ZIP-STATE-FILE-ST
              GOBACK
           END-IF.

           CLOSE REPORT-FILE.
           IF NOT REPORT-FILE-OK
              THEN
              DISPLAY 'Output File Problem: ' REPORT-FILE-ST
              GOBACK
           END-IF.


           CLOSE ERROR-FILE.
           IF NOT ERROR-FILE-OK
              THEN
              DISPLAY 'ERROR-FILE Problem: ' ERROR-FILE-ST
              GOBACK
           END-IF.

           CLOSE PARTS-FILE.
           IF NOT PARTS-FILE-OK
              THEN
              DISPLAY ' PARTS-FILE Problem: ' PARTS-FILE-ST
              GOBACK
           END-IF.

           CLOSE SUPPLIERS-FILE.
           IF NOT ZIP-STATE-FILE-OK
              THEN
              DISPLAY ' SUPPLIERS-FILE Problem: ' SUPPLIERS-FILE-ST
              GOBACK
           END-IF.

           CLOSE ADDRESSES-FILE.
           IF NOT ADDRESSES-FILE-OK
              THEN
              DISPLAY 'ADDRESSES-FILE Problem: ' ADDRESSES-FILE-ST
              GOBACK
           END-IF.

           CLOSE PURCHORDS-FILE.
           IF NOT PURCHORDS-FILE-OK
              THEN
              DISPLAY 'ZIP-STATE-File Problem: ' PURCHORDS-FILE-ST
              GOBACK
           END-IF.

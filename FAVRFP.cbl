       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRFP.
      *
      * This programm reads input file,
      * validates input and writes either a reportline
      * or an error report for each input record.
      * At end a summary ist printed.
      * FINAL VERSION 07/11/2020
      * RALF STRAUBE
      *


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RFPIN  ASSIGN TO FAVRFP.
           SELECT PROPOSAL ASSIGN TO PRTLINE.
           SELECT ERROR-REPORT ASSIGN TO FAVERR.
       DATA DIVISION.
       FILE SECTION.
       FD  RFPIN
           RECORDING MODE IS F
           DATA RECORD IS RFP-REC.
       01 RFP-REC.
          05 ARTIST-ACCT-NO             PIC X(08).

          05 ARTIST-MUSICAL-GENRE       PIC X(09).
             88 ROCK                                   VALUE 'ROCK'.
             88 JAZZ                                   VALUE 'JAZZ'.
             88 FUSION                                 VALUE 'FUSION'.
             88 FOLK                                   VALUE 'FOLK'.
             88 CLASSICAL                              VALUE 'CLASSICAL'
           .
             88 FUSION                                 VALUE 'FUSION'.
          05 MUSICIAN.
             10 MUSICIAN-LNAME          PIC X(15).
             10 MUSICIAN-FNAME          PIC X(15).
          05 MUSICIAN-INSTRUMENT-TYPE   PIC X(06).
             88 KEYBOARD                               VALUE "KEYS".
             88 VOCALS                                 VALUE "VOCALS".
             88 GUITAR                                 VALUE "GUITAR".
             88 BASS                                   VALUE "BASS".
             88 DRUMS                                  VALUE "DRUMS".
             88 PERCUSSION                             VALUE "PERC".    .
          05 INSTRUMENT-QUALITY         PIC X(01).
             88 USED-FLAG                              VALUE "U".
             88 NEW-FLAG                               VALUE "N".
             88 PREMIUM-FLAG                           VALUE "P".
          05 MAX-MUSICIAN-BUDGET-AMOUNT PIC 9(5)V99.
          05 MAX-MUSICIAN-BUDGET-AMT-REDEF REDEFINES
                MAX-MUSICIAN-BUDGET-AMOUNT
                                        PIC X(7).
          05 SHIP-TO                    PIC X(03).
             88 IN-COUNTRY                             VALUE "IN".
             88 OUT-OF-COUNTRY                         VALUE "OUT".
          05 FILLER                     PIC X(16).


       FD  PROPOSAL
           RECORDING MODE IS F
           DATA RECORD IS PROP-LINE.
       01 PROP-LINE                     PIC X(132).

       FD ERROR-REPORT
           RECORDING MODE IS F.
       01 ERROR-LINE                    PIC X(132)     VALUE SPACES.


       WORKING-STORAGE SECTION.
      * EOF indicator
       77 RFPIN-STATUS                  PIC X(01)      VALUE SPACE.
       88 RFPIN-EOF                                    VALUE 'Y'.
      * Valid data indicator
       77 VALID-DATA-IND                PIC X(01)      VALUE SPACE.
       88 VALID-INPUT-DATA                             VALUE 'Y'.

       77 WS-INSTRUMENT-PRICE           PIC S9(5)V99.
      * TERMS & CONDITIONS   for maintenance  *      ******************
       77 KEYBOARD-PRICE                PIC 9(7)V99    VALUE 3017.89.
       77 VOCALS-PRICE                  PIC 9(7)V99    VALUE 599.05.
       77 BASS-PRICE                    PIC 9(7)V99    VALUE 18761.00.
       77 GUITAR-PRICE                  PIC 9(7)V99    VALUE 2648.99.
       77 DRUMS-PRICE                   PIC 9(7)V99    VALUE 3087.22.
       77 PERCUSSION-PRICE              PIC 9(7)V99    VALUE 799.99.

       77 TAX-RATE                      PIC 9(3)V99    VALUE 0.08.
       77 QUALITY-UPLIFT                PIC SV99       VALUE 0.20.
       77 QUALITY-DISCOUNT              PIC SV99       VALUE -0.20.
       77 SHIP-NAT                      PIC SV99       VALUE 0.10.
       77 SHIP-OFFSHOR                  PIC SV99       VALUE 0.20.
      *****************************************************************
       77 WS-INST-PRICE-BEFORE-TAX      PIC S9(7)V99   VALUE ZERO.
       77 WS-INST-PRICE-FINAL           PIC S9(7)V99   VALUE ZERO.
       77 WS-PRICE-ADJUSTMT             PIC S9(5)V99   VALUE ZERO.
       77 WS-SHIPPING-COSTS             PIC 9(4)V99    VALUE ZERO.
       77 WS-TAX                        PIC 9(4)V99    VALUE ZERO.
       77 WS-REC-KTR                    PIC 99         VALUE ZERO.
       77 WS-REC-KTR-VALID              PIC 99         VALUE ZERO.
       77 WS-REC-KTR-BAD                PIC 99         VALUE ZERO.
       77 WS-GROSS-AMOUNT               PIC 9(7)V99    VALUE ZERO.




      * Report Detail Line
       01 WS-PROP-LINE.
          05 ARTIST-ACCT-NO-O           PIC X(08).
          05 FILLER                     PIC    X(1).
          05 ARTIST-MUSICAL-GENRE-O     PIC X(09).
          05 FILLER                     PIC    X(1).
          05 MUSICIAN-O.
             10 MUSICIAN-LNAME-O        PIC X(15).
             10 FILLER                  PIC    X(1).
             10 MUSICIAN-FNAME-O        PIC X(15).
             10 FILLER                  PIC    X(1).
          05 MUSICIAN-INSTRUMENT-TYPE-O PIC X(12).
          05 FILLER                     PIC    X(1).
          05 INSTRUMENT-QUALITY-O       PIC X(10).
          05 FILLER                     PIC    X(1).
          05 SHIP-TO-O                  PIC X(03).
          05 FILLER                     PIC    X(5).
          05 COST-PER-INSTRUMENT-O      PIC $Z,ZZZ,ZZ9.99-.
          05 FILLER                     PIC    X(1).
          05 ADDITIONAL-COSTS-O.
             10 SHIPPING-COSTS-O        PIC $Z,ZZ9.99.
             10 FILLER                  PIC    X(1).
             10 TAX-O                   PIC $Z,ZZ9.99.
          05 FILLER                     PIC X(01).
          05 REMARKS-O                  PIC X(11).
          05 FILLER                     PIC X(22).

      * Report Headline
       01 WS-HEADLINE.
          05 ACCT-LIT                   PIC X(08)      VALUE "Account".
          05 FILLER                     PIC X(1).
          05 GENRE-LIT                  PIC X(09)      VALUE "Genre".
          05 FILLER                     PIC X(1).
          05 LNAME-LIT                  PIC X(15)      VALUE "Lastname".
          05 FILLER                     PIC X(1).
          05 FNAME-LIT                  PIC X(15)      VALUE "Firstname"
           .
          05 FILLER                     PIC X(1).
          05 INSTRUMENT-TYPE-LIT        PIC X(12)      VALUE
                "Instrument".
          05 FILLER                     PIC X(1).
          05 QUALITY-LIT                PIC X(10)      VALUE "Quality".
          05 FILLER                     PIC X(1).
          05 SHIP-LIT                   PIC X(07)      VALUE "Ship To".
          05 FILLER                     PIC X(1).
          05 COST-LIT                   PIC X(14)      VALUE "Price".
          05 FILLER                     PIC X(1).
          05 SHIPPING-LIT               PIC X(9)       VALUE "Ship Cost"
           .
          05 FILLER                     PIC X(1).
          05 TAX-LIT                    PIC X(9)       VALUE "Tax".
          05 FILLER                     PIC X(01).
          05 REM-LIT                    PIC X(11)      VALUE "Remarks".
          05 FILLER                     PIC X(22).

       01 WS-SUMMARY-REPORT.
          05 FILLER                     PIC X(15)
                                                       VALUE
                "Records read: ".
          05 FILLER                     PIC X(1).
          05 WS-REC-KTR-OUT             PIC Z9.
          05 FILLER                     PIC X(3).

          05 FILLER                     PIC X(21)
                                                       VALUE
                "Valid Input Records: ".
          05 FILLER                     PIC X(1).
          05 WS-REC-KTR-VALID-O         PIC Z9.
          05 FILLER                     PIC X(3).
          05 FILLER                     PIC X(19)
                                                       VALUE
                "Bad Input Records: ".
          05 FILLER                     PIC X(1).
          05 WS-REC-KTR-BAD-O           PIC Z9.
          05 FILLER                     PIC X(3).
          05 FILLER                     PIC X(25)
                                                       VALUE
                "Grand Total Instruments: ".
          05 FILLER                     PIC X(1).
          05 WS-GROSS-AMOUNT-O          PIC $Z,ZZZ,ZZZ.ZZ.

      * Tables with valid content and Output Content Xlation

       01 WS-INSTRUMENT-TABLE.
          05 FILLER                     PIC X(16)      VALUE
                "KEYS  KEYS      ".
          05 FILLER                     PIC X(16)      VALUE
                "VOCALSVOCALS    ".
          05 FILLER                     PIC X(16)      VALUE
                "GUITARGUITAR    ".
          05 FILLER                     PIC X(16)      VALUE
                "BASS  BASS      ".
          05 FILLER                     PIC X(16)      VALUE
                "DRUMS DRUMS     ".
          05 FILLER                     PIC X(16)      VALUE
                "PERC  PERCUSSION".
       01 INSTRUMENT-TABLE-REDEF REDEFINES WS-INSTRUMENT-TABLE.
          05 WS-INSTRUMENT OCCURS 6 TIMES INDEXED BY I.
             10 WS-INSTRUMENT-SHORTNAME PIC X(6).
             10 WS-INSTRUMENT-LONGNAME  PIC X(10).

       01 WS-GENRE-TABLE.
          05 FILLER                     PIC X(9)       VALUE
                "ROCK  ".
          05 FILLER                     PIC X(9)       VALUE
                "JAZZ  ".
          05 FILLER                     PIC X(9)       VALUE
                "FUSION".
          05 FILLER                     PIC X(9)       VALUE
                "FOLK  ".
          05 FILLER                     PIC X(9)       VALUE
                "CLASSICAL".
          05 FILLER                     PIC X(9)       VALUE
                "COUNTRY".
       01 GENRE-TABLE-REDEF REDEFINES WS-GENRE-TABLE.
          05 WS-GENRE                   PIC X(9) OCCURS 6 TIMES INDEXED
                BY J.

       01 WS-INST-QUALITY-TABLE.
          05 FILLER                     PIC X(8)       VALUE
                "UUsed   ".
          05 FILLER                     PIC X(8)       VALUE
                "NNew    ".
          05 FILLER                     PIC X(8)       VALUE
                "PPremium".

       01 INST-QUALITY-TABLE-REDEF REDEFINES WS-INST-QUALITY-TABLE.
          05 WS-INST-QUALITY OCCURS 3 TIMES INDEXED BY K.
             10 WS-QUALITY-SHORTNAME    PIC X(1).
             10 WS-QUALITY-LONGNAME     PIC X(7).




       PROCEDURE DIVISION.
       MAIN.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-MAIN UNTIL RFPIN-EOF
           PERFORM 700-WRITE-SUMMARY-REPORT.
           PERFORM 800-CLOSE-FILES.
           GOBACK.
       000-HOUSEKEEPING.
           DISPLAY '000-HOUSEKEEPING'.
           PERFORM 300-OPEN-FILES.
           INITIALIZE RFP-REC PROP-LINE.
      * Priming Read

           PERFORM 400-READ-RFPIN.
       100-MAIN.
           DISPLAY '100-MAIN'.
           DISPLAY "RFP-REC: " RFP-REC.
           PERFORM 150-VALIDATE-DATA.
           IF VALID-INPUT-DATA THEN
              PERFORM 200-PROCESS-DATA
              PERFORM 500-WRITE-REPORT
           END-IF.
           PERFORM 400-READ-RFPIN.

       150-VALIDATE-DATA.
           DISPLAY '150-VALIDATE-DATA'.
           MOVE "N" TO VALID-DATA-IND.
           INITIALIZE ERROR-LINE.
           ADD 1 TO WS-REC-KTR.
           MOVE RFP-REC TO ERROR-LINE.


      * Extensions Workshop 8.1d, 8.1.e
      * Validating Input Record Fields -> first error found
      * will end procedure

      * Validating Account Number

           IF NOT ARTIST-ACCT-NO NUMERIC THEN
              MOVE "<-Acct# not numeric **" TO ERROR-LINE(9:22)
              WRITE ERROR-LINE
              EXIT PARAGRAPH
           END-IF.

      * Validating Genre for valid table entry

           SET J TO 1.
           SEARCH WS-GENRE
           AT END
              MOVE
                 "<-Genre not valid**" TO ERROR-LINE(18:19)
              WRITE ERROR-LINE
              EXIT PARAGRAPH
           WHEN WS-GENRE(J) = ARTIST-MUSICAL-GENRE
                CONTINUE
           END-SEARCH.

      * Validating Last Name
           IF MUSICIAN-LNAME = SPACE THEN
              MOVE
                 "<- Lastname empty**" TO ERROR-LINE(33:19)
              WRITE ERROR-LINE
              EXIT PARAGRAPH
           END-IF.

      * Validating First Name
           IF MUSICIAN-FNAME = SPACE THEN
              MOVE
                 "**Firstname empty  ->" TO ERROR-LINE(12:21)
              WRITE ERROR-LINE
              EXIT PARAGRAPH
           END-IF.

      * Validating Instrument Type with Table Search + lookup Longname
           SET I TO 1.
           SEARCH WS-INSTRUMENT
           AT END
              MOVE
                 "**Instr.Type not valid ->" TO ERROR-LINE(23:25)
              WRITE ERROR-LINE
              EXIT PARAGRAPH
           WHEN WS-INSTRUMENT-SHORTNAME(I) = MUSICIAN-INSTRUMENT-TYPE
                MOVE WS-INSTRUMENT-LONGNAME(I) TO
                   MUSICIAN-INSTRUMENT-TYPE-O
           END-SEARCH.

      * Validating Instr. Quality with Table Search + lookup Longname
           SET K TO 1.
           SEARCH WS-INST-QUALITY
           AT END
              MOVE
                 "**Instr.Qual not valid ->" TO ERROR-LINE(29:25)
              WRITE ERROR-LINE
              EXIT PARAGRAPH
           WHEN WS-QUALITY-SHORTNAME(K) = INSTRUMENT-QUALITY
                MOVE WS-QUALITY-LONGNAME(K) TO
                   INSTRUMENT-QUALITY-O
           END-SEARCH.

      * Validating Budget
           IF NOT (MAX-MUSICIAN-BUDGET-AMOUNT >= 0 AND <=
              9999.99) THEN
              MOVE
                 "**Max. Budget not valid ->" TO ERROR-LINE(29:26)
              WRITE ERROR-LINE
              EXIT PARAGRAPH
           END-IF.

      * Validating Ship To
           IF NOT (SHIP-TO = "IN" OR SHIP-TO = "OUT")
              THEN
              MOVE
                 "**ShipTo not valid ->" TO ERROR-LINE(41:21)
              WRITE ERROR-LINE
              EXIT PARAGRAPH
           END-IF.

      * If this statement is reached, no errors were found

           MOVE "Y" TO VALID-DATA-IND.

       200-PROCESS-DATA.

      * Calculating Output Fields and write report detail line

           DISPLAY '200-PROCESS-DATA'
           ADD 1 TO WS-REC-KTR-VALID.

      *  ASSIGN UNCHANGED INPUT FIELDS to Detail Line

           MOVE ARTIST-ACCT-NO TO ARTIST-ACCT-NO-O.
           MOVE ARTIST-MUSICAL-GENRE TO ARTIST-MUSICAL-GENRE-O.
           MOVE MUSICIAN-LNAME TO MUSICIAN-LNAME-O.
           MOVE MUSICIAN-FNAME TO MUSICIAN-FNAME-O.
      * Instrument Type  Quality already assigned in 150-VALIDATE-DATA
           MOVE SHIP-TO TO SHIP-TO-O.

      * DETERMINE PRICE FOR EACH INSTRUMENT

           EVALUATE TRUE
           WHEN KEYBOARD
                MOVE KEYBOARD-PRICE TO WS-INSTRUMENT-PRICE
           WHEN VOCALS
                MOVE VOCALS-PRICE TO WS-INSTRUMENT-PRICE
           WHEN GUITAR
                MOVE GUITAR-PRICE TO WS-INSTRUMENT-PRICE
           WHEN BASS
                MOVE BASS-PRICE TO WS-INSTRUMENT-PRICE
           WHEN DRUMS
                MOVE DRUMS-PRICE TO WS-INSTRUMENT-PRICE
           WHEN PERCUSSION
                MOVE PERCUSSION-PRICE TO WS-INSTRUMENT-PRICE
           WHEN OTHER
                DISPLAY "Invalid Instrument Type"
                MOVE ZERO TO WS-INSTRUMENT-PRICE
           END-EVALUATE.

      * ADJUST PRICE ACCORDING TO REQUIRED QUALITY

           EVALUATE TRUE
           WHEN USED-FLAG
      * USED: discount
                COMPUTE WS-PRICE-ADJUSTMT
                   ROUNDED =(WS-INSTRUMENT-PRICE * QUALITY-DISCOUNT)
           WHEN NEW-FLAG
      * NEW: price remains unadjusted
                MOVE ZERO TO WS-PRICE-ADJUSTMT
           WHEN PREMIUM-FLAG
      * PREMIUM: uplift
                COMPUTE WS-PRICE-ADJUSTMT
                   ROUNDED =(WS-INSTRUMENT-PRICE * QUALITY-UPLIFT)
           WHEN OTHER
                DISPLAY "INVALID QUALITY FLAG"
                MOVE ZERO TO WS-PRICE-ADJUSTMT
           END-EVALUATE.

      * Workshop 8.1d: shipping independent from
      * instrument quality price adjustmt

           EVALUATE TRUE
           WHEN OUT-OF-COUNTRY
                COMPUTE WS-SHIPPING-COSTS
                   ROUNDED =(WS-INSTRUMENT-PRICE * SHIP-OFFSHOR)

           WHEN IN-COUNTRY
                COMPUTE WS-SHIPPING-COSTS
                   ROUNDED =(WS-INSTRUMENT-PRICE * SHIP-NAT)
           WHEN OTHER
                DISPLAY "INVALID Shipping FLAG"
                MOVE ZERO TO WS-SHIPPING-COSTS
           END-EVALUATE.

      * Adding Price Components before Tax

           ADD WS-INSTRUMENT-PRICE
               WS-PRICE-ADJUSTMT
               WS-SHIPPING-COSTS GIVING WS-INST-PRICE-BEFORE-TAX.

      * Tax based on total
           COMPUTE WS-TAX ROUNDED = WS-INST-PRICE-BEFORE-TAX * TAX-RATE.

      * Calculating FINAL PRICE including tax

           ADD WS-TAX WS-INST-PRICE-BEFORE-TAX
              GIVING WS-INST-PRICE-FINAL.

      * Summing up instrument prices

           ADD WS-INST-PRICE-FINAL TO WS-GROSS-AMOUNT.


      * ASSIGNING RESULTS TO Detail Line

           MOVE WS-INST-PRICE-FINAL TO COST-PER-INSTRUMENT-O.

      * just to let input record budget not to be left unused

           IF WS-INST-PRICE-FINAL
              > MAX-MUSICIAN-BUDGET-AMOUNT
              THEN
              MOVE "Over Budget" TO REMARKS-O
           ELSE
              MOVE SPACES TO REMARKS-O
           END-IF.

           MOVE WS-SHIPPING-COSTS TO SHIPPING-COSTS-O.
           MOVE WS-TAX TO TAX-O.
       300-OPEN-FILES.
           DISPLAY '300-OPEN-FILES'.
           OPEN INPUT RFPIN.
           OPEN OUTPUT PROPOSAL ERROR-REPORT.
       400-READ-RFPIN.
           DISPLAY '400-READ-RFPIN'.
           READ RFPIN
      * Set AT END Switch
           AT END
              MOVE "Y" TO RFPIN-STATUS
           END-READ.
       500-WRITE-REPORT.
           DISPLAY '500-WRITE-REPORT'.

           IF WS-REC-KTR-VALID = 1 THEN
              WRITE PROP-LINE FROM WS-HEADLINE
           END-IF.

           WRITE PROP-LINE FROM WS-PROP-LINE.


       700-WRITE-SUMMARY-REPORT.

      * Workshop 8.1.e
           DISPLAY '700-WRITE-SUMMARY'.

           MOVE WS-REC-KTR TO WS-REC-KTR-OUT.
           MOVE WS-REC-KTR-VALID TO WS-REC-KTR-VALID-O.

           SUBTRACT WS-REC-KTR-VALID FROM WS-REC-KTR
              GIVING WS-REC-KTR-BAD.

           MOVE WS-REC-KTR-BAD TO WS-REC-KTR-BAD-O.
           MOVE WS-GROSS-AMOUNT TO WS-GROSS-AMOUNT-O

           WRITE PROP-LINE FROM WS-SUMMARY-REPORT
              AFTER ADVANCING 5 LINES.

       800-CLOSE-FILES.
           DISPLAY '800-CLOSE-FILES'.
           CLOSE RFPIN, PROPOSAL, ERROR-REPORT.





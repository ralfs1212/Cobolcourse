       IDENTIFICATION DIVISION.
      * This programm reads 3 input records, computes a value and
      * writes a report
       PROGRAM-ID. FAVRPT.
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
       01 FAV-REC.
          05 ARTIST-NAME                PIC X(30).
          05 NUMBER-OF-MUSICIANS        PIC 9(2).
          05 MUSICAL-GENRE              PIC X(12).
          05 COST.
             10 CD-COST                 PIC 9(3)V99.
             10 SHIPPING-COST           PIC 9(2)V99.
             10 TAX                     PIC 9(2)V99.
          05 BAND-STATUS                PIC X(1).
             88 BAND-IS-STILL-TOGETHER              VALUE "Y".
             88 BAND-IS-DIVORCED                    VALUE "N".
          05 FILLER                     PIC X(22).

       FD  FAVOUT
           RECORDING MODE IS F
           DATA RECORD IS FAV-RPT.
       01 FAV-RPT.
          05 ARTIST-NAME-OUT            PIC X(30).
          05 NUMBER-OF-MUSICIANS-OUT    PIC Z9.
          05 MUSICAL-GENRE-OUT          PIC X(12).
          05 COST.
             10 CD-COST-OUT             PIC 9(3)V99.
             10 SHIPPING-COST-OUT       PIC 9(2)V99.
             10 TAX-OUT                 PIC 9(2)V99.
          05 BAND-STATUS-OUT            PIC X(1).

       PROCEDURE DIVISION.

      * initial processing

           OPEN INPUT FAVIN.
           OPEN OUTPUT FAVOUT.

      * iteration 1

           READ FAVIN.

           COMPUTE CD-COST =
              CD-COST + CD-COST * TAX + SHIPPING-COST.


           MOVE FAV-REC TO FAV-RPT.
           WRITE FAV-RPT.

      * iteration 2

           READ FAVIN.

           COMPUTE CD-COST =
              CD-COST + CD-COST * TAX + SHIPPING-COST.

           MOVE FAV-REC TO FAV-RPT.
           WRITE FAV-RPT.

      * iteration 3

           READ FAVIN.

           COMPUTE CD-COST =
              CD-COST + CD-COST * TAX + SHIPPING-COST.

           MOVE FAV-REC TO FAV-RPT.
           WRITE FAV-RPT.

      * end processing

           CLOSE FAVIN FAVOUT.

           GOBACK.
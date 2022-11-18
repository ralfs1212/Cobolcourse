      * Copy Book PURCHL - Calling Parameters for PGM POEDIT.
      * Include Copybook in LINKAGE SECTION of calledProgram
      *
      * COPY PURCHL.
      *
          01 PURCHORD-STRUCTURE PIC X(123). *> 41 * 3 Records
          01 PURCHORDS-ITEMS REDEFINES PURCHORD-STRUCTURE.
          05 PURCHORD-TABLE OCCURS 3 TIMES.
            10 PO-NUMBER                PIC X(06).
            10 BUYER-CODE               PIC X(03).
            10 QUANTITY                 PIC S9(7).
            10 UNIT-PRICE               PIC S9(7)V99.
            10 ORDER-DATE               PIC 9(08).
            10 DELIVERY-DATE            PIC 9(08).
          01 PO-SUB-MAX                 PIC 9     COMP   VALUE 3.
          01 ERROR-SUB                  PIC 9(01) COMP.
          01 ERROR-SUB-MAX              PIC 9(01) COMP   VALUE 4.
          01 ERROR-STRUCTURE            PIC X(160).
          01 ERROR-ITEMS REDEFINES ERROR-STRUCTURE.
            05 ERROR-TABLE OCCURS 4 TIMES.
               10 ERROR-FLD             PIC X(20).
               10 ERROR-MSG             PIC X(20).

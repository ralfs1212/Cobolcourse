      * Copy Book ADDRL - Calling Parameters for PGM ADDREDIT.
      * Include Copybook in LINKAGE SECTION of called- and
      * WORKING-STORAGE SECTION of calling Program
      *
      * COPY ADDRL.
      *
          01 ADDRESS-STRUCTURE PIC X(219).            *> 73 * 3 Records
          01 ADDRESS-ITEMS REDEFINES ADDRESS-STRUCTURE.
          05 ADDRESS-TABLE OCCURS 3 TIMES.
             10 ADDRESS-TYPE        PIC X(01).
               88 VALID-ADDRESS-TYPE VALUE '1' '2' '3'.
               88 ORDER-ADDRESS                       VALUE 'l'.
               88 SCHED-ADDRESS                       VALUE '2'.
               88 REMIT-ADDRESS                       VALUE '3'.
             10 ADDRESS-1           PIC X(15).
             10 ADDRESS-2           PIC X(15).
             10 ADDRESS-3           PIC X(15).
             10 CITY                PIC X(15).
             10 ADDR-STATE          PIC X(02).
             10 ZIP-CODE            PIC 9(10).
          01 ZIP-STATE-STRUCTURE PIC X(6800). *> 34 * 200 Records
          01 ZIP-STATE-ITEMS REDEFINES ZIP-STATE-STRUCTURE.
           05 ZIP-STATE-TABLE OCCURS 200 TIMES INDEXED BY ZIP-STATE-IDX.
           10 ZIP-STATE-NAME        PIC X(16).
           10 ZIP-STATE-CODE        PIC X(02).
           10 ZIPCODE-RANGE-LO      PIC 9(08).
           10 ZIPCODE-RANGE-HI      PIC 9(08).
          01 ZS-TABLE-SUB-MAX       PIC 9(03) COMP VALUE 200.
          01 ADDR-TABLE-SUB-MAX     PIC 9(01) COMP VALUE 3.
          01 ERROR-SUB              PIC 9(01) COMP.
          01 ERROR-SUB-MAX          PIC 9(01) COMP VALUE 4.
          01 ERROR-STRUCTURE PIC X(160).
          01 ERROR-ITEMS REDEFINES ERROR-STRUCTURE.
            05 ERROR-TABLE OCCURS 4 TIMES.
              10 ERROR-FLD          PIC X(20).
              10 ERROR-MSG          PIC X(20).



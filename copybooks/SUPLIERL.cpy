      * Copy Book SUPLIERL - Calling Parameters for PGM SUPPEDIT.
      * Include Copybook in LINKAGE SECTION of called- and
      * WORKING-STORAGE SECTION of calling Program
      *
      * COPY SUPLIERL.

          01 SUPPLIER-CODE            PIC       X(10).
          01 SUPPLIER-TYPE            PIC       X(01).
             88 VALID-SUPPLIER-TYPE              VALUE 'S' 'D' 'M' 'I'.
             88 SUBCONTRACTOR                    VALUE 'S'.
             88 DISTRIBUTOR                      VALUE 'D'.
             88 MANUFACTURER                     VALUE 'M'.
             88 IMPORTER                         VALUE 'l'.
          01 SUPPLIER-NAME            PIC  X(15).
          01 SUPPLIER-PERF            PIC  9(03).
          01 SUPPLIER-RATING          PIC  X(01).
             88 VALID-SUPPLIER-RATING            VALUE '1' '2' '3'.
             88 HIGHEST-QUALITY                  VALUE '3'.
             88 AVERAGE-QUALITY                  VALUE '2'.
             88 LOWEST-QUALITY                   VALUE 'l'.
          01 SUPPLIER-STATUS          PIC X(01).
             88 VALID-SUPPLIER-STATUS            VALUE '1' '2' '3'.
             88 GOVT-COMM                        VALUE 'l'.
             88 GOVT-ONLY                        VALUE '2'.
             88 COMMERCIAL-ONLY                  VALUE '3'.
          01 SUPPLIER-ACT-DATE        PIC 9(08).
          01 ERROR-SUB                PIC 9(1)   COMP.
          01 ERROR-SUB-MAX            PIC 9(1)   COMP VALUE 4.
          01 ERROR-STRUCTURE          PIC X(160).
          01 ERROR-ITEMS REDEFINES ERROR-STRUCTURE.
            05 ERROR-TABLE OCCURS 4 TIMES.
               10 ERROR-FLD           PIC X(20).
               10 ERROR-MSG           PIC X(20).


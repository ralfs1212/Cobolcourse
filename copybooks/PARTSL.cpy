      * Copy Book PARTSL - Calling Parameters for PGM PARTEDIT.
      * Include Copybook in LINKAGE SECTION of called- and
      * WORKING-STORAGE SECTION of calling Program
      *
      * COPY PARTSL.

          01 PART-NUMBER PIC X(23).
          01 BLUEPRINT-NUMBER    PIC X(10).
          01 UNIT-OF-MEASURE PIC X(3).
             88 VALID-UNIT                  VALUE 'CM ' 'INC'.
             88 CM                          VALUE 'CM '.
             88 INCH                        VALUE 'INC'.
          01 WEEKS-LEAD-TIME     PIC 9(03) COMP.
          01 VEHICLE-MAKE        PIC X(03).
             88 VALID-MAKE
                VALUE 'CHR' 'FOR' 'GM ' 'VW' 'TOY' 'JAG' 'PEU' 'BMW'.
             88 CHRYSLER                     VALUE 'CHR'.
             88 FORD                         VALUE 'FOR'.
             88 GM                           VALUE 'GM '.
             88 VOLKSWAGON                   VALUE 'VW '.
             88 TOYOTA                       VALUE 'TOY'.
             88 JAGUAR                       VALUE 'JAG'.
             88 PEUGEOT                      VALUE 'PEU'.
             88 BMW                          VALUE 'BMW'.
          01 VEHICLE-MODEL       PIC X(10).
          01 VEHICLE-YEAR        PIC X(04).
          01 ERROR-SUB                 PIC 9(1) COMP.
          01 ERROR-SUB-MAX             PIC 9(1) COMP
                                                VALUE 4.
          01 ERROR-STRUCTURE           PIC X(160).
          01 ERROR-ITEMS REDEFINES ERROR-STRUCTURE.
            05 ERROR-TABLE OCCURS 4 TIMES.
              10 ERROR-FLD           PIC X(20).
              10 ERROR-MSG           PIC X(20).


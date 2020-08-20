       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLES2Z.
      ****************************************************************
      * TABLES2Z, Table Processing Programm                          *
      * This programm establishes a static storage table,            *
      * performs a few table searches and writes result to SYSOUT    *  *
      *                                                              *
      * RALF STRAUBE                                                 *
      * Workshop 17.2 b (Second Part)                                *
      * Initial Version  08/19/2020                                  *
      *                                                              *
      ****************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 SUBSCRIPTS-AND-COUNTERS.

          05 STUDENT-SUB                PIC 99    VALUE 0 COMP.
          05 STUDENT-SUB-MAX            PIC 99    VALUE 5 COMP.
          05 COURSES-SUB                PIC 99    VALUE 0 COMP.
          05 COURSES-SUB-MAX            PIC 99    VALUE 6 COMP.

      * R.S. Exam for practicing coninuation lines :-)
       01 WS-STUDENT-RECORD.
            05 ROW1  PIC X(68) VALUE
           'ROBERT K KAHN       ANTH101BCALC687ASOCS200CALGB12BAPHYS002A
      -    'FLUT140C'.
           05 ROW2  PIC X(68) VALUE
           'LISA CRUDUP         BIOL201ATRIG551DSHAK213APSYC23ABBIOL002C
      -    'DRUM310C'.
           05 ROW3  PIC X(68) VALUE
           'RICHARD HILDEBRAND  POLY555CGEOM231ARLIT560DBIOL13DBMECH002A
      -    'ACCO140B'.
           05 ROW4  PIC X(68) VALUE
           'LORETTA PANTOLINE   TUBA567ASTAT043CSHOP980ACHEM534HASTR002B
      -    'VIOL610B'.
           05 ROW5  PIC X(68) VALUE
           'SALLY HARRIS        MEDC522CPIAN003BSPAN760AEART164BRUSS002B
      -    'PIAN170A'.
       01 WS-STUDENT-RECORD-RDF REDEFINES WS-STUDENT-RECORD.
          05 WS-STUDENT-TABLE-RDF OCCURS 5 TIMES INDEXED BY STD-IDX.
             10 WS-STUDENT-NAME-RDF     PIC X(20).
             10 WS-STUDENT-COURSE-TAB-RDF OCCURS 6 TIMES
                   INDEXED BY CRS-IDX.
                15 WS-COURSE-NBR-RDF    PIC X(7).
                15 WS-COURSE-GRADE-RDF  PIC X(1).
                   88 A-GRADE-T                   VALUE "A".
                   88 B-GRADE-T                   VALUE "B".
                   88 C-GRADE-T                   VALUE "C".
                   88 D-GRADE-T                   VALUE "D".
                   88 F-GRADE-T                   VALUE "F".


       PROCEDURE DIVISION.
       MAIN.


           PERFORM 250-PROCESS-SEARCHES.

           GOBACK.


       250-PROCESS-SEARCHES.
      ************************************************************
      *  Process various Table Searches                          *
      *  All searches are serial because table not sorted        *
      ************************************************************
           DISPLAY '250-PROCESS-SEARCHES'.

           DISPLAY "Student TABLE-RDF:".

           DISPLAY WS-STUDENT-TABLE-RDF(1).
           DISPLAY WS-STUDENT-TABLE-RDF(2).
           DISPLAY WS-STUDENT-TABLE-RDF(3).
           DISPLAY WS-STUDENT-TABLE-RDF(4).
           DISPLAY WS-STUDENT-TABLE-RDF(5).

      * Find anyone who's studied TRIG551 or DRUM310

           SET STD-IDX, CRS-IDX TO 1.

      * Search index-increment only works for
      * one index - every index for
      * table elements of higher order has to be set explicity

           PERFORM VARYING STD-IDX FROM 1 BY 1 UNTIL STD-IDX >
              STUDENT-SUB-MAX
                   SET CRS-IDX TO 1   *>R.S. Index Reset is important!
                   SEARCH WS-STUDENT-COURSE-TAB-RDF VARYING CRS-IDX

                   AT END
                      DISPLAY "Search1: No TRIG551 or DRUM310 found"
                   WHEN WS-COURSE-NBR-RDF(STD-IDX CRS-IDX) = "TRIG551"
                      OR WS-COURSE-NBR-RDF(STD-IDX CRS-IDX) = "DRUM3101"

                        DISPLAY "Search1 - Found: "
                                WS-COURSE-NBR-RDF(STD-IDX CRS-IDX)
                        DISPLAY WS-STUDENT-NAME-RDF(STD-IDX)
                   END-SEARCH
           END-PERFORM.


      * Is SALLY HARRIS taking ear training (EART164)?
      * nested search with std-idx positioned on Student found
           SET STD-IDX, CRS-IDX TO 1.

           SEARCH WS-STUDENT-TABLE-RDF
           AT END
              DISPLAY "Search2: No SALLY Harris found"
           WHEN WS-STUDENT-NAME-RDF(STD-IDX) =
              "SALLY HARRIS"
                DISPLAY "Search2 - Found: "
                        WS-STUDENT-NAME-RDF(STD-IDX)

                SEARCH WS-STUDENT-COURSE-TAB-RDF
                AT END
                   DISPLAY "Search2: No EART164 found"
                WHEN WS-COURSE-NBR-RDF(STD-IDX CRS-IDX) =
                   "EART164"
                     DISPLAY "Search2 - Found: "
                             WS-COURSE-NBR-RDF(STD-IDX CRS-IDX)
                END-SEARCH
           END-SEARCH.

      * What did LISA CRUDUP get in PSYCH23A?
      * nested search with std-idx positioned on Student found
      * crs-idx from crs-nbr-searched used to fetch course grade

           SET STD-IDX, CRS-IDX TO 1.

           SEARCH WS-STUDENT-TABLE-RDF
           AT END
              DISPLAY "Search3: No LISA CRUDUP found"
           WHEN WS-STUDENT-NAME-RDF(STD-IDX) =
              "LISA CRUDUP"
                DISPLAY "Search3 - Found: "
                        WS-STUDENT-NAME-RDF(STD-IDX)

                SEARCH WS-STUDENT-COURSE-TAB-RDF
                AT END
                   DISPLAY "Search3: No PSYC23A found"
                WHEN WS-COURSE-NBR-RDF(STD-IDX CRS-IDX) =
                   "PSYC23A"
                     DISPLAY "Search3 - Found: "
                             WS-COURSE-NBR-RDF(STD-IDX CRS-IDX)
                     DISPLAY "Grade: "
                             WS-COURSE-GRADE-RDF(STD-IDX CRS-IDX)
                END-SEARCH
           END-SEARCH.


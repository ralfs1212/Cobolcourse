       01 CLAIM-RECORD-WS.
           05 INSURED-DETAILS.
               10 INSURED-POLICY-NO      PIC 9(07).
               10 INSURED-LAST-NAME      PIC X(15).
               10 INSURED-FIRST-NAME     PIC X(10).
           05 POLICY-DETAILS.
               10 POLICY-TYPE            PIC 9.
                   88 PRIVATE           VALUE 1.
                   88 MEDICARE          VALUE 2.
                   88 AFFORDABLE-CARE   VALUE 3.
               10 POLICY-BENEFIT-DATE-NUM PIC 9(08).
               10 POLICY-BENEFIT-DATE-X REDEFINES
                         POLICY-BENEFIT-DATE-NUM PIC X(08).
               10 POLICY-BENEFIT-PERIOD REDEFINES
                         POLICY-BENEFIT-DATE-NUM.
                   15 POLICY-YEAR        PIC 9(04).
                   15 POLICY-MONTH       PIC 9(02).
                   15 POLICY-DAY         PIC 9(02).
               10 POLICY-AMOUNT          PIC S9(7)V99.
               10 POLICY-DEDUCTIBLE-PAID PIC S9(4).
               10 POLICY-COINSURANCE     PIC V99.
           05 CLAIM-DETAILS.
               10 CLAIM-AMOUNT           PIC S9(7)V99.
               10 CLAIM-AMOUNT-PAID      PIC S9(7)V99.
           05 FILLER                     PIC X(6).


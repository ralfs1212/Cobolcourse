      **************************************************************
      * HOSPINF - Copy Book for Input Record PGM HOSPEDIT
      **************************************************************

       01 WS-INPUT-REC.
          05 PATIENT-NBR            PIC 9(5).
          05 PATIENT-NAME.
             10 LAST-NAME           PIC X(10).
             10 FIRST-NAME          PIC X(10).
          05 PATIENT-PHONE          PIC X(10).
          05 PATIENT-TYPE           PIC X(1).
             88 INPATIENT                            VALUE "I".
             88 OUTPATIENT                           VALUE "O".
             88 VALID-TYPE                           VALUES ARE "I"
                                            , "O".
          05 BED-IDENTITY           PIC 9(4).
          05 DATE-ADMIT             PIC X(10).
          05 AMT-PER-DAY            PIC 9(5)V99.
          05 DIAGNOSTIC-CODE        PIC 999.
          05 INS-TYPE               PIC X(3).
      *****
      **    Remove the 88-Level condition/fields from the Input Record
      *****
      * R.S.   88 VALID-INS-TYPE VALUES ARE "HMO", "PPO", "POS" "MAN".
      *        88 Managed-Care value "MAN".
          05 HOSPITAL-STAY-LTH      PIC 999.
          05 PATIENT-TOT-AMT        PIC 9(7)V99.
          05 PCP-ID                 PIC X(6).
          05 IN-OUT-NETWORK         PIC X(1).
             88 IN-NETWORK                           VALUE "N".
             88 OUT-OF-NETWORK                       VALUE "O".
          05 COPAY                  PIC S9(3)        VALUE 0.
          05 DEDUCTIBLE             PIC S9(4)        VALUE 0.
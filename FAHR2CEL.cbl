       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAHR2CEL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEMPS-VARS.
           05 WS-FAHRENHEIT     PIC 999.
           05 WS-CELSIUS        PIC 999V99.
           05 WS-CELSIUS-OUT    PIC 999.99.
       PROCEDURE DIVISION.
           MOVE 300 TO WS-FAHRENHEIT.
           COMPUTE WS-CELSIUS ROUNDED =
                  ( (WS-FAHRENHEIT - 32) * 5 ) / 9.
           DISPLAY "Fahrenheit: " WS-FAHRENHEIT.
           MOVE WS-CELSIUS TO WS-CELSIUS-OUT.
           DISPLAY "Celsius: " WS-CELSIUS-OUT.
           GOBACK.


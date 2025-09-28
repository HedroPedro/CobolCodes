       IDENTIFICATION DIVISION.
       PROGRAM-ID. LUHN.
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-I.
       OBJECT-COMPUTER. IBM-I.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
        01 WS-CREDIT-CARD.
           05 WS-DIGITS   PIC X(16) .
        01 WS-IS-PAIR     PIC 9    VALUE ZERO.
        01 WS-COUNTER     PIC 9(2) VALUE ZERO.
        01 WS-TOTAL       PIC 9(4) VALUE ZERO.
        01 WS-DIGIT       PIC 9(2) VALUE ZERO.
        01 WS-TEMP        PIC 9(2) VALUE ZERO.
        01 WS-RESULT      PIC X(3) VALUE SPACES.
       PROCEDURE DIVISION.
       000-MAIN.
           MOVE '1111111111111111' TO WS-DIGITS 
           PERFORM VARYING WS-COUNTER FROM 16 BY -1 UNTIL WS-COUNTER < 1
              MOVE FUNCTION NUMVAL(WS-DIGITS(WS-COUNTER:1)) TO WS-DIGIT
              IF WS-IS-PAIR EQUAL 1
                 MOVE 0 TO WS-IS-PAIR
                 MULTIPLY WS-DIGIT BY 2 GIVING WS-TEMP
                 IF WS-TEMP > 9
                 SUBTRACT 9 FROM WS-TEMP GIVING WS-TEMP
                 END-IF                 
                 ADD WS-TEMP TO WS-TOTAL
              ELSE
                 MOVE 1 TO WS-IS-PAIR 
                 ADD WS-DIGIT TO WS-TOTAL
              END-IF 
           END-PERFORM
           
           MOVE 'NO ' TO WS-RESULT.
           IF FUNCTION MOD(WS-TOTAL 10) = 0
              MOVE 'YES' TO WS-RESULT
           END-IF.
           DISPLAY 'IS ' WS-DIGITS ' VALID?' WS-RESULT. 
           GOBACK.
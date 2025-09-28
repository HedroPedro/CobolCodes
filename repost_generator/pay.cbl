       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CURRENCY SIGN IS 'R$' WITH PICTURE SYMBOL '$'
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTA-REC ASSIGN TO CONTAREC.
           SELECT OUT-REC   ASSIGN TO OUTREC.
       DATA DIVISION.
       FILE SECTION.
       FD  CONTA-REC  RECORDING MODE F.
       01  CONTA-FIELD.
           05 CONTA-NUM      PIC 9(4).
           05 SOBRENOME      PIC X(15).
           05 PRIM-NOME      PIC X(15).
           05 SALDO          PIC S9(7)V99 COMP-3.
           05 LIMITE         PIC S9(7)V99 COMP-3.
           05 FILLER         PIC X(36).

       FD  OUT-REC    RECORDING MODE F.
       01  OUT-FIELD.
           05  OUT-NUMERO         PIC 9(4).
           05  FILLER             PIC X(5) VALUE SPACES.
           05  OUT-SOBRENOME      PIC X(15).
           05  FILLER             PIC X(1) VALUE SPACES.
           05  OUT-PRIM-NOME      PIC X(15).
           05  FILLER             PIC X(1) VALUE SPACES.
           05  OUT-LIMITE         PIC $.$$$.$$9,99.
           05  FILLER             PIC X(1) VALUE SPACES.
           05  OUT-SALDO          PIC $.$$$.$$9,99.

       WORKING-STORAGE SECTION.
       01  FLAGS.
           05 EOF            PIC X VALUE SPACE.

       01  WT-DATE.
           05 WT-ANO         PIC 9(4).
           05 WT-MES         PIC 9(2).
           05 WT-DIA         PIC 9(2).

       01  WT-CURRENCY.
           05 TOTAL-SALDO    PIC S9(9)V99 COMP-3 VALUE ZERO.
           05 TOTAL-LIMITE   PIC S9(9)V99 COMP-3 VALUE ZERO.

       01  CAB-1.
           05 FILLER         PIC X(20) VALUE 'RELATORIO FINANCEIRO'.
           05 FILLER         PIC X(60) VALUE SPACES.

       01  CAB-2.
           05 FILLER         PIC X(4) VALUE 'ANO '.
           05 CAB-ANO        PIC 9(4).
           05 FILLER         PIC X(2) VALUE SPACES.
           05 FILLER         PIC X(4) VALUE 'MES '.
           05 CAB-MES        PIC 9(2).
           05 FILLER         PIC X(4) VALUE SPACES.
           05 FILLER         PIC X(4) VALUE 'DIA '.
           05 CAB-DIA        PIC 9(2).

       01  CAB-3.
           05 FILLER         PIC X(8)  VALUE 'N. CONTA'.
           05 FILLER         PIC X(1)  VALUE SPACES.
           05 FILLER         PIC X(9)  VALUE 'SOBRENOME'.
           05 FILLER         PIC X(7)  VALUE SPACES.
           05 FILLER         PIC X(13) VALUE 'PRIMEIRO NOME'.
           05 FILLER         PIC X(4)  VALUE SPACES.
           05 FILLER         PIC X(6)  VALUE 'LIMITE'.
           05 FILLER         PIC X(8)  VALUE SPACES.
           05 FILLER         PIC X(5)  VALUE 'SALDO'.

       01  CAB-4.
           05 FILLER         PIC X(08) VALUE '--------'.
           05 FILLER         PIC X(01) VALUE SPACES.
           05 FILLER         PIC X(15) VALUE '---------------'.
           05 FILLER         PIC X(01) VALUE SPACES.
           05 FILLER         PIC X(15) VALUE '---------------'.
           05 FILLER         PIC X(02) VALUE SPACES.
           05 FILLER         PIC X(12) VALUE '------------'.
           05 FILLER         PIC X(02) VALUE SPACES.
           05 FILLER         PIC X(12) VALUE '------------'.
       
       01  ROD-1.
           05 FILLER         PIC X(6)  VALUE 'TOTAL '.
           05 FILLER         PIC X(2)  VALUE SPACES.
           05 FILLER         PIC X(7)  VALUE 'LIMITE '.
           05 TOTAL-LIM-O    PIC $$$.$$$.$$9,99.
           05 FILLER         PIC X(2)  VALUE SPACES.
           05 FILLER         PIC X(6)  VALUE 'SALDO '.
           05 TOTAL-SALDO-O  PIC $$$.$$$.$$9,99.

       PROCEDURE DIVISION.
       000-SETUP.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WT-DATE.
           OPEN INPUT CONTA-REC.
           OPEN OUTPUT OUT-REC.
           MOVE WT-ANO TO CAB-ANO.
           MOVE WT-MES TO CAB-MES.
           MOVE WT-DIA TO CAB-DIA.
           
       010-MAIN.
           WRITE OUT-FIELD FROM CAB-1.
           WRITE OUT-FIELD FROM CAB-2.
           WRITE OUT-FIELD FROM CAB-3.
           WRITE OUT-FIELD FROM CAB-4.
           MOVE SPACES TO OUT-FIELD.

       020-RELAT.
           PERFORM LER-ARQ.
           PERFORM UNTIL EOF = 'T'
              PERFORM ESC-ARQ
              PERFORM SOMA-TOTAIS
              PERFORM LER-ARQ
           END-PERFORM.
           MOVE TOTAL-SALDO  TO TOTAL-SALDO-O.
           MOVE TOTAL-LIMITE TO TOTAL-LIM-O.
           MOVE SPACES TO OUT-FIELD.
           WRITE OUT-FIELD FROM CAB-4.
           MOVE SPACES TO OUT-FIELD.
           WRITE OUT-FIELD FROM ROD-1.

       030-END.    
           CLOSE OUT-REC.
           CLOSE CONTA-REC.
           GOBACK.

      *    PROCEDURES
       LER-ARQ.
           READ CONTA-REC
              AT END MOVE 'T' TO EOF
           END-READ.

       ESC-ARQ.
           MOVE CONTA-NUM TO OUT-NUMERO.
           MOVE SOBRENOME TO OUT-SOBRENOME.
           MOVE PRIM-NOME TO OUT-PRIM-NOME.
           MOVE LIMITE    TO OUT-LIMITE.
           MOVE SALDO     TO OUT-SALDO.
           WRITE OUT-FIELD.

       SOMA-TOTAIS.
           COMPUTE TOTAL-SALDO  = TOTAL-SALDO + SALDO   END-COMPUTE.
           COMPUTE TOTAL-LIMITE = TOTAL-LIMITE + LIMITE END-COMPUTE.
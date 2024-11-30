       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSFOR.
      *******************************
      * PROGRAMA PARA CONSULTA DE FORNCEDORES *
      *******************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT CADFOR ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY IS CNPJ
                       ALTERNATE RECORD KEY IS 
                       NOMEFANTASIA WITH DUPLICATES
                       FILE STATUS  IS ST-ERRO.
       DATA DIVISION.
       FILE SECTION.
      *
       FD CADFOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADFOR.DAT".
       01 REGFOR.
           03 CNPJ              PIC 9(14).
           03 RAZAOSOCIAL       PIC X(35).
           03 NOMEFANTASIA      PIC X(12).
           03 FCEP               PIC 9(09).
           03 NUMERO            PIC 9(05).
           03 COMPL             PIC X(12).
           03 DDD               PIC 9(02).
           03 TEL               PIC 9(09).
           03 EMAIL             PIC X(35).
           03 RAMO              PIC 9(01).
           03 RAMOTXT           PIC X(20).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL        PIC 9(01) VALUE ZEROS.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT        PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(55) VALUE SPACES.
       01 SOLIC        PIC X(20) VALUE SPACES.
       01 CONLIN       PIC 9(03) VALUE 001.
       01 CNPJENTR      PIC 9(14) VALUE ZEROS.
       01 NOMEENTR     PIC X(12) VALUE SPACES.
      *-----------------------------------------------------------------
      *
      ******************
      * REPORT SECTION *
      ******************
      *
       01 DET2.
           03 FILLER    PIC X(6) VALUE "CNPJ: ".
           03 DETCNPJ   PIC 9(14) VALUE ZEROS.
           03 FILLER    PIC X(02) VALUE SPACES.
           03 FILLER    PIC X(07) VALUE "RAZAO: ".
           03 DETRAZAO  PIC X(35) VALUE SPACES.

       01 DET3.
           03 FILLER     PIC X(06) VALUE "NOME: ".
           03 DETNOME PIC X(12) VALUE SPACES.
           03 FILLER     PIC X(05) VALUE "CEP: ".
           03 DETCEP    PIC 9(09) VALUE ZEROS.
           03 FILLER     PIC X(12) VALUE " TELEFONE: (".
           03 DETDDD  PIC 9(02) VALUE ZEROS.
           03 FILLER     PIC X(02) VALUE ") ".
           03 DETTEL PIC 9(09) VALUE ZEROS.

       01 DET4.
           03 FILLER     PIC X(07) VALUE "EMAIL: ".
           03 DETEMAIL     PIC X(35) VALUE SPACES.
           03 FILLER     PIC X(08) VALUE " RAMO: ".
           03 DETRAMO   PIC X(20) VALUE SPACES.         

       SCREEN SECTION.

       01  TELAPRO.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  " FATEC ZONA LESTE         ***   CONSULTA".
           05  LINE 02  COLUMN 41 
               VALUE  " DE FORNCEDORES  ***".
           05  LINE 04  COLUMN 01 
               VALUE  " CNPJ :           NOME FANTASIA : ".
           05  LINE 23  COLUMN 01 
               VALUE  "  MENSAGEM :".
           05  TCNPJENTR
               LINE 04  COLUMN 09  PIC 9(06)
               USING  CNPJENTR.
           05  TNOMEENTR
               LINE 04  COLUMN 35  PIC X(30)
               USING  NOMEENTR.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  CADFOR
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADFOR NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADFOR"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
           ELSE
               NEXT SENTENCE.

       INC-001.
           MOVE ZEROS TO CNPJENTR
           MOVE SPACES TO NOMEENTR
           DISPLAY  TELAPRO.
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA  F2=CONSULTA P/NOME FANTASIA."
           ACCEPT TCNPJENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADFOR
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CNPJENTR TO CNPJ
           START CADFOR KEY IS NOT LESS CNPJ INVALID KEY
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.

       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA     F2=CONSULTA P/CNPJ "
           ACCEPT TNOMEENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADFOR
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001A.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE NOMEENTR TO NOMEFANTASIA
           START CADFOR KEY IS NOT LESS NOMEFANTASIA INVALID KEY
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
       
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ CADFOR NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADFOR"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       ROT-MONTAR.
           MOVE CNPJ TO DETCNPJ
           MOVE RAZAOSOCIAL  TO DETRAZAO
           MOVE NOMEFANTASIA TO DETNOME
           MOVE FCEP TO DETCEP
           MOVE DDD     TO DETDDD
           MOVE TEL     TO DETTEL
           MOVE EMAIL     TO DETEMAIL
           MOVE RAMOTXT TO DETRAMO
           COMPUTE LIN = CONLIN + 3
           DISPLAY (LIN, 01) DET2
           COMPUTE LIN = CONLIN + 3
           ADD 1 TO CONLIN
           COMPUTE LIN = CONLIN + 3
           DISPLAY (LIN, 01) DET3
           ADD 1 TO CONLIN
           COMPUTE LIN = CONLIN + 3
           DISPLAY (LIN, 01) DET4
           ADD 3 TO CONLIN
           IF CONLIN < 17
                  GO TO INC-RD2.

       ROT-SOL.
           DISPLAY (23, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELAPRO
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO INC-001.
      *
       NOV-SOL.
           DISPLAY (23, 13) " *** NOVA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  GO TO INC-001.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE CADFOR.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *

      *---------------------*** FIM DE PROGRAMA ***--------------------*
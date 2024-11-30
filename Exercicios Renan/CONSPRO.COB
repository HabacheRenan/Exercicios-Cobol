       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSPRO.
      *******************************
      * CONSULTA DE ENDERECAMENTO CEP *
      *******************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT CADPRO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DESC
                                   WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
      *
       FD CADPRO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADPRO.DAT".
       01 REGPRO.
           03 CODIGO            PIC 9(06).
           03 DESC              PIC X(30).
           03 UNIDADE           PIC X(02).
           03 UNIDADETXT        PIC X(15).
           03 TIPOPRODUTO       PIC 9(01).
           03 TIPOTXT          PIC X(15).
           03 FCNPJ             PIC 9(14).
           03 QUANTIDADE        PIC 9(06).
           03 PRECO             PIC 9(09)V99.
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
       01 CODENTR      PIC 9(09) VALUE ZEROS.
       01 DESCENTR     PIC X(35) VALUE SPACES.
      *-----------------------------------------------------------------
      *
      ******************
      * REPORT SECTION *
      ******************
      *
       01 DET2.
           03 FILLER    PIC X(09) VALUE "CODIGO: ".
           03 DETCODIGO PIC 9(06) VALUE ZEROS.
           03 FILLER    PIC X(02) VALUE SPACES.
           03 FILLER    PIC X(11) VALUE "DESCRICAO: ".
           03 DETDESCR  PIC X(35) VALUE SPACES.
           03 FILLER     PIC X(09) VALUE "UNIDADE: ".
           03 DETUNIDADE PIC X(15) VALUE SPACES.

       01 DET3.
           03 FILLER     PIC X(06) VALUE "TIPO: ".
           03 DETTIPO    PIC X(20) VALUE SPACES.
           03 FILLER     PIC X(18) VALUE " CNPJ FORNECEDOR: ".
           03 DETCNPJ    PIC 9(14) VALUE ZEROS.

       01 DET4.
           03 FILLER     PIC X(13) VALUE "QUANTIDADE: ".
           03 DETQTD     PIC 9(06) VALUE ZEROS.
           03 FILLER     PIC X(08) VALUE " PRECO: ".
           03 DETPRECO   PIC 9(09)V99 VALUE ZEROS.         

       SCREEN SECTION.

       01  TELAPRO.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  " FATEC ZONA LESTE         ***   CONSULTA".
           05  LINE 02  COLUMN 41 
               VALUE  " DE PRODUTO  ***".
           05  LINE 04  COLUMN 01 
               VALUE  " CODIGO :               DESCRICAO : ".
           05  LINE 23  COLUMN 01 
               VALUE  "  MENSAGEM :".
           05  TCODENTR
               LINE 04  COLUMN 09  PIC 9(06)
               USING  CODENTR.
           05  TDESCENTR
               LINE 04  COLUMN 35  PIC X(30)
               USING  DESCENTR.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  CADPRO
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADPRO NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADPRO"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
           ELSE
               NEXT SENTENCE.

       INC-001.
           MOVE ZEROS TO CODENTR
           MOVE SPACES TO DESCENTR
           DISPLAY  TELAPRO.
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA   F2=CONSULTA P/DESCRICAO."
           ACCEPT TCODENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADPRO
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CODENTR TO CODIGO
           START CADPRO KEY IS NOT LESS CODIGO INVALID KEY
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.

       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA     F2=CONSULTA P/CODIGO "
           ACCEPT TDESCENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADPRO
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001A.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE DESCENTR TO DESC
           START CADPRO KEY IS NOT LESS DESC INVALID KEY
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
       
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ CADPRO NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADPRO"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.

       ROT-MONTAR.
           MOVE CODIGO TO DETCODIGO
           MOVE DESC  TO DETDESCR
           MOVE UNIDADETXT TO DETUNIDADE
           MOVE TIPOTXT TO DETTIPO
           MOVE FCNPJ     TO DETCNPJ
           MOVE QUANTIDADE     TO DETQTD
           MOVE PRECO     TO DETPRECO
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
           CLOSE CADPRO.
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
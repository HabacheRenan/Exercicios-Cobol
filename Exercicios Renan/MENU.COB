       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU.
      **********************
      * MENU DOS PROGRAMAS *
      **********************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    SELECT CLA000   ASSIGN TO DISK
      *        ORGANIZATION IS INDEXED
      *        ACCESS MODE  IS DYNAMIC
      *        RECORD KEY   IS CLK00101
      *        FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      * COPY CDA000.BOK
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL1       PIC 9(02) VALUE ZEROS.
       01 W-SEL2       PIC 9(02) VALUE ZEROS.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT        PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(50) VALUE SPACES.
       01 W-PROGR      PIC X(07) VALUE SPACES.
       01 W-EMPRESA    PIC X(20) VALUE "FATEC ZONA LESTE".
      *-----------------------------------------------------------------
      *              *** CAMPOS PARA TRATAMENTO DE DATA ***
       01 DATA-EQ.
          03 DIA-EQ    PIC 99 VALUE ZEROS.
          03 MES-EQ    PIC 99 VALUE ZEROS.
          03 ANO-EQ    PIC 99 VALUE ZEROS.
       01 DATA-N.
          03 DIA       PIC 99.
          03 MES       PIC 99.
          03 ANO       PIC 99.
       01 DATA-INV.
          03 ANO-INV   PIC 99.
          03 MES-INV   PIC 99.
          03 DIA-INV   PIC 99.
       01 DATA-FOR.
          03 DIA-FOR   PIC 99.
          03 BR1-FOR   PIC X(01) VALUE "/".
          03 MES-FOR   PIC 99.
          03 BR2-FOR   PIC X(01) VALUE "/".
          03 ANO-FOR   PIC 99.
       01 W-01         PIC 9(04).
       01 FLAG1        PIC 9(01).
      *-----------------------------------------------------------------
       01 TAB-PROGR.
           03 TAB-PROR-ITEM OCCURS 9 TIMES.
               05 TAB-PROGR PIC X(08).
      **********************
       01 ARQ-ERRO.
          03 FILLER     PIC X(05) VALUE "OP > ".
          03 ERRO-OPE   PIC X(07).
          03 FILLER     PIC X(07) VALUE " ARQ > ".
          03 ERRO-ARQ   PIC X(08).
          03 FILLER     PIC X(06) VALUE " LB > ".
          03 ERRO-LAB   PIC X(08).
          03 FILLER     PIC X(06) VALUE " ER > ".
          03 ERRO-COD   PIC X(02).
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       
       01  TELAMENU.
           05  LINE 01  COLUMN 01 
               VALUE  "************************************ MEN".
           05  LINE 01  COLUMN 41 
               VALUE  "U **************************************".
           05  LINE 02  COLUMN 01 
               VALUE  "         CADASTRAR           |     CONSU".
           05  LINE 02  COLUMN 41 
               VALUE  "LTAR             |  TRANSFORMAR EM TXT".
           05  LINE 03  COLUMN 01 
               VALUE  "                             |".
           05  LINE 03  COLUMN 41 
               VALUE  "                 |".
           05  LINE 04  COLUMN 01 
               VALUE  " 01 - CADASTRAR FUNCIONARIO  | 05 - CONS".
           05  LINE 04  COLUMN 41 
               VALUE  "ULTAR FORNECEDOR | 07 - ARQUIVO DE PRO".
           05  LINE 05  COLUMN 01 
               VALUE  "                             |".
           05  LINE 05  COLUMN 41 
               VALUE  "                 |".
           05  LINE 06  COLUMN 01 
               VALUE  " 02 - CADASTRAR ENDERECO     | 06 - CONS".
           05  LINE 06  COLUMN 41 
               VALUE  "ULTAR PRODUTOS   | 08 - ARQUIVO DE FOR".
           05  LINE 07  COLUMN 01 
               VALUE  "                             |".
           05  LINE 07  COLUMN 41 
               VALUE  "                 |".
           05  LINE 08  COLUMN 01 
               VALUE  " 03 - CADASTRAR FORNECEDOR   |".
           05  LINE 08  COLUMN 41 
               VALUE  "                 | 09 - ARQUIVO DE END".
           05  LINE 09  COLUMN 01 
               VALUE  "                             |".
           05  LINE 09  COLUMN 41 
               VALUE  "                 |".
           05  LINE 10  COLUMN 01 
               VALUE  " 04 - CADASTRAR PRODUTOS     |".
           05  LINE 10  COLUMN 41 
               VALUE  "                 |".
           05  LINE 11  COLUMN 01 
               VALUE  "                             |".
           05  LINE 11  COLUMN 41 
               VALUE  "                 |".
           05  LINE 12  COLUMN 01 
               VALUE  "                             |".
           05  LINE 12  COLUMN 41 
               VALUE  "                 |".
           05  LINE 13  COLUMN 01 
               VALUE  "                             |".
           05  LINE 13  COLUMN 41 
               VALUE  "                 |".
           05  LINE 14  COLUMN 01 
               VALUE  "                             |".
           05  LINE 14  COLUMN 41 
               VALUE  "                 |".
           05  LINE 15  COLUMN 01 
               VALUE  "                             |".
           05  LINE 15  COLUMN 41 
               VALUE  "                 |".
           05  LINE 16  COLUMN 01 
               VALUE  "                             |".
           05  LINE 16  COLUMN 41 
               VALUE  "                 |".
           05  LINE 17  COLUMN 01 
               VALUE  "                             |".
           05  LINE 17  COLUMN 41 
               VALUE  "                 |".
           05  LINE 18  COLUMN 01 
               VALUE  "                             |".
           05  LINE 18  COLUMN 41 
               VALUE  "                 |".
           05  LINE 19  COLUMN 01 
               VALUE  "                             |".
           05  LINE 19  COLUMN 41 
               VALUE  "                 |".
           05  LINE 20  COLUMN 01 
               VALUE  "                             |".
           05  LINE 20  COLUMN 41 
               VALUE  "                 |".
           05  LINE 21  COLUMN 01 
               VALUE  "                             |".
           05  LINE 21  COLUMN 41 
               VALUE  "                 |".
           05  LINE 22  COLUMN 01 
               VALUE  "                             |".
           05  LINE 22  COLUMN 41 
               VALUE  "                 |".
           05  LINE 23  COLUMN 01 
               VALUE  "                             |".
           05  LINE 23  COLUMN 41 
               VALUE  "                 |".
           05  LINE 24  COLUMN 01 
               VALUE  "                             |".
           05  LINE 24  COLUMN 41 
               VALUE  "                 |".

      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "PROGRFP" TO TAB-PROR-ITEM(1)
           MOVE "SCP001" TO TAB-PROR-ITEM(2)
           MOVE "SCP002" TO TAB-PROR-ITEM(3)
           MOVE "SCP003" TO TAB-PROR-ITEM(4)
           MOVE "CONSFOR" TO TAB-PROR-ITEM(5)
           MOVE "CONSPRO" TO TAB-PROR-ITEM(6)
           MOVE "PROTXT" TO TAB-PROR-ITEM(7)
           MOVE "FORTXT" TO TAB-PROR-ITEM(8)
           MOVE "ENDTXT" TO TAB-PROR-ITEM(9).

           ACCEPT DATA-INV FROM DATE
           MOVE DIA-INV TO DIA DIA-FOR
           MOVE MES-INV TO MES MES-FOR
           MOVE ANO-INV TO ANO ANO-FOR.
           MOVE DATA-N  TO DATA-EQ.
      *
      ****************************
      * ROTINA DE SELECAO NIVEL1 *
      ****************************
      *
       NIVEL1.
           DISPLAY TELAMENU.
           DISPLAY (23 20) "*** SELECIONE A OPCAO : .. ***".
           ACCEPT (23 44) W-SEL2.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      GO TO ROT-FIM.
           IF W-SEL2 > 9 OR W-SEL2 = ZEROS
                 MOVE "*** FUNCAO NAO DISPONIVEL *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NIVEL1.
           MOVE TAB-PROR-ITEM(W-SEL2) TO W-PROGR
           IF W-PROGR = SPACES
                 MOVE "*** FUNCAO NAO IMPLEMENTADA *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NIVEL1.
      *
       ROT-EXEC.
           CALL W-PROGR ON OVERFLOW
              MOVE "* ERRO NA CHAMADA DO PROGRAMA SELECIONADO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ROT-EXEC1.
           CANCEL W-PROGR.
       ROT-EXEC1.
           GO TO NIVEL1.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           EXIT PROGRAM.
       ROT-FIM1.
           DISPLAY (01, 01) ERASE.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23 12) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23 12) LIMPA.
       ROT-MENS-FIM.
      *
      *---------------------*** FIM DE PROGRAMA ***--------------------*
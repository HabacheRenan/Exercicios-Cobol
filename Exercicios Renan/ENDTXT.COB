       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENDTXT.
      ************************
      * PROGRAMA PARA TRANSFORMAR OS CADASTROS DE ENDERECO EM TXT *
      ***********************
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADEND ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY IS CEP
                       FILE STATUS  IS ST-ERRO.

           SELECT CADCEPS ASSIGN TO DISK
                       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADEND
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADEND.DAT".
       01 REGEND.
           03 CEP            PIC 9(09).
           03 LOGRADOURO     PIC X(35).
           03 BAIRRO         PIC X(20).
           03 CIDADE         PIC X(20).
           03 UF             PIC X(02).
           03 UFTXT          PIC X(20).

       FD CADCEPS  
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADCEPS.TXT".
       01 REGCEPS            PIC X(86).

       WORKING-STORAGE SECTION.
       77 W-ACT                PIC 9(02) VALUE ZEROS.
       77 W-CONT               PIC 9(06) VALUE ZEROS.
       77 W-OPCAO              PIC X(01) VALUE SPACES.
       77 MENS                 PIC X(50) VALUE SPACES.
       01 W-SEL                PIC 9(01) VALUE ZEROS.
       01 ST-ERRO              PIC X(02) VALUE "00".

       SCREEN SECTION.
       01 LIMPAR.
           05 BLANK SCREEN.
           05 LINE 4 COLUMN 4
               VALUE "O CEP ".
           05 LINE 4 COLUMN 20
               VALUE " FOI CADASTRADO NO ARQUIVO".  

       PROCEDURE DIVISION.
       R0.
           OPEN INPUT CADEND.
           IF ST-ERRO NOT = "00"  
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADEND" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R0A.
           OPEN OUTPUT CADCEPS.

       LER-ARQUIVO.
           READ CADEND NEXT
           IF ST-ERRO = "10"
                MOVE "FIM DE ARQUIVO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.    

       INC-WR1.   
           MOVE REGEND TO REGCEPS
           DISPLAY LIMPAR
           DISPLAY (4, 10) CEP
           WRITE REGCEPS
           IF ST-ERRO = "00" OR "02"
               MOVE "CEP GRAVADO " TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO LER-ARQUIVO. 
       ROT-FIM.
           CLOSE CADEND.
           CLOSE CADCEPS.
           STOP RUN.
       
       ROT-MENS.
           MOVE ZEROS TO W-CONT.

       ROT-MENS1.
           DISPLAY (22, 25) MENS.
       
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
              GO TO ROT-MENS2
           ELSE
              MOVE SPACES TO MENS
              DISPLAY (22, 25) MENS.

       ROT-MENS-FIM.
           EXIT.                                
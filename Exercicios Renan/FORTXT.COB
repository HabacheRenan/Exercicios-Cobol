       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORTXT.
      ************************
      * PROGRAMA PARA TRANSFORMAR OS CADASTROS DE FORNECEDOR EM TXT *
      ***********************
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADFOR ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY IS CNPJ
                       ALTERNATE RECORD KEY IS 
                       NOMEFANTASIA WITH DUPLICATES
                       FILE STATUS  IS ST-ERRO.

           SELECT CADFORS ASSIGN TO DISK
                       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
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
       FD CADFORS  
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADFORS.TXT".
       01 REGFORS            PIC X(160).

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
           05 LINE 4 COLUMN 3
               VALUE "O CNPJ ".
           05 LINE 4 COLUMN 24
               VALUE " FOI CADASTRADO NO ARQUIVO".   

       PROCEDURE DIVISION.
       R0.
           OPEN INPUT CADFOR.
           IF ST-ERRO NOT = "00"  
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADFOR" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R0A.
           OPEN OUTPUT CADFORS.

       LER-ARQUIVO.
           READ CADFOR NEXT
           IF ST-ERRO = "10"
                MOVE "FIM DE ARQUIVO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.    

       INC-WR1.   
           MOVE REGFOR TO REGFORS
           DISPLAY LIMPAR
           DISPLAY (4, 10) CNPJ
           WRITE REGFORS
           IF ST-ERRO = "00" OR "02"
               MOVE "CNPJ GRAVADO NO ARQUIVO " TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO LER-ARQUIVO. 
       ROT-FIM.
           CLOSE CADFOR.
           CLOSE CADFORS.
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROTXT.
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
           SELECT CADPRO ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE IS DYNAMIC
                       RECORD KEY IS CODIGO
                       FILE STATUS IS ST-ERRO
                       ALTERNATE RECORD KEY IS DESC
                                   WITH DUPLICATES.

           SELECT CADPROS ASSIGN TO DISK
                       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
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
       FD CADPROS  
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADPROS.TXT".
       01 REGPROS            PIC X(100).

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
           05 LINE 4 COLUMN 1
               VALUE "O PRODUTO DO CODIGO ".
           05 LINE 4 COLUMN 28
               VALUE " FOI CADASTRADO NO ARQUIVO".   

       PROCEDURE DIVISION.
       R0.
           OPEN INPUT CADPRO.
           IF ST-ERRO NOT = "00"  
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADPRO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R0A.
           OPEN OUTPUT CADPROS.

       LER-ARQUIVO.
           READ CADPRO NEXT
           IF ST-ERRO = "10"
                MOVE "FIM DE ARQUIVO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.    

       INC-WR1.   
           MOVE REGPRO TO REGPROS
           DISPLAY LIMPAR.
           DISPLAY (4, 21) CODIGO
           WRITE REGPROS
           IF ST-ERRO = "00" OR "02"
               MOVE "PRODUTO GRAVADO NO ARQUIVO " TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO LER-ARQUIVO. 
       ROT-FIM.
           CLOSE CADPRO.
           CLOSE CADPROS.
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
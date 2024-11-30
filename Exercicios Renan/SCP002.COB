       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCP002.
      ************************
      * PROGRAMA PARA CADASTRAR FORNECEDORES *
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
      *
           SELECT CADEND ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE IS DYNAMIC
                       RECORD KEY IS CEP
                       FILE STATUS IS ST-ERRO. 
      *-----------------------------------------------------------------
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

       WORKING-STORAGE SECTION.
       77 W-ACT                PIC 9(02) VALUE ZEROS.
       77 W-CONT               PIC 9(06) VALUE ZEROS.
       77 W-OPCAO              PIC X(01) VALUE SPACES.
       01 MENS.
          02 MENS1             PIC X(48) VALUE SPACES.
          02 MENS2             PIC ZZZ.
       01 W-SEL                PIC 9(01) VALUE ZEROS.
       01 ST-ERRO              PIC X(02) VALUE "00". 

       01 TBRAMO.
           03 TBRAMO-ITEM OCCURS 9 TIMES.
               05 TBRAMO PIC X(20).

       SCREEN SECTION.
       01  CADASTROFOR.
           05 BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "****************************** CADASTRO".
           05  LINE 01  COLUMN 41 
               VALUE  "DE FORNECEDOR **************************".
           05  LINE 02  COLUMN 01 
               VALUE  "CNPJ:".
           05  LINE 04  COLUMN 01 
               VALUE  "RAZAO SOCIAL:".
           05  LINE 04  COLUMN 41 
               VALUE  "            NOME FANTASIA:".
           05  LINE 06  COLUMN 01 
               VALUE  "CEP:".
           05  LINE 08  COLUMN 01 
               VALUE  "LOGRADOURO:".
           05  LINE 08  COLUMN 41 
               VALUE  "         BAIRRO:".
           05  LINE 10  COLUMN 01 
               VALUE  "CIDADE:                       UF:    -".
           05  LINE 10  COLUMN 41 
               VALUE  "                    NUMERO:".
           05  LINE 12  COLUMN 01 
               VALUE  "COMPL:".
           05  LINE 14  COLUMN 01 
               VALUE  "TELEFONE: (  )".
           05  LINE 16  COLUMN 01 
               VALUE  "EMAIL:".
           05  LINE 18  COLUMN 01 
               VALUE  "RAMO ATIVIDADE:    -".
           05  TCNPJ
               LINE 02  COLUMN 07  PIC 9(14)
               USING  CNPJ.
           05  TRAZAOSOCIAL
               LINE 04  COLUMN 15  PIC X(35)
               USING  RAZAOSOCIAL.
           05  TNOMEFANTASIA
               LINE 04  COLUMN 68  PIC X(12)
               USING  NOMEFANTASIA.
           05  TCEP
               LINE 06  COLUMN 06  PIC 9(09)
               USING  FCEP.
           05  TLOG
               LINE 08  COLUMN 13  PIC X(35)
               USING  LOGRADOURO.
           05  TBAIRRO
               LINE 08  COLUMN 58  PIC X(20)
               USING  BAIRRO.
           05  TCIDADE
               LINE 10  COLUMN 09  PIC X(20)
               USING  CIDADE.
           05  TUF
               LINE 10  COLUMN 35  PIC X(02)
               USING  UF.
           05  TUFTXT
               LINE 10  COLUMN 40  PIC X(20)
               USING  UFTXT.
           05  TNUMERO
               LINE 10  COLUMN 69  PIC 9(05)
               USING  NUMERO.
           05  TCOMPL
               LINE 12  COLUMN 08  PIC X(12)
               USING  COMPL.
           05  TDD
               LINE 14  COLUMN 12  PIC 9(02)
               USING  DDD.
           05  TTEL
               LINE 14  COLUMN 16  PIC 9(09)
               USING  TEL.
           05  TEMAIL
               LINE 16  COLUMN 08  PIC X(35)
               USING  EMAIL.
           05  TRAMO
               LINE 18  COLUMN 17  PIC 9(02)
               USING  RAMO.
           05  TRAMOTXT
               LINE 18  COLUMN 22  PIC X(25)
               USING  RAMOTXT.

       01 TELA2.
           05  LINE 15 COLUMN 50
               VALUE "1- METAL".
           05  LINE 16 COLUMN 50
               VALUE "2- TEXTO".
           05  LINE 17 COLUMN 50
               VALUE "3- AGRONEGOCIO".
           05  LINE 18 COLUMN 50
               VALUE "4- SAUDE".
           05  LINE 19 COLUMN 50
               VALUE "5- EDUCACAO".
           05  LINE 20 COLUMN 50
               VALUE "6- INDUSTRIA".
           05  LINE 21 COLUMN 50
               VALUE "7- VAREJO".
           05  LINE 22 COLUMN 50
               VALUE "8- TURISMO".
           05  LINE 23 COLUMN 50
               VALUE "9- ELETRONICA".        

       PROCEDURE DIVISION.
       VETOR.
           MOVE "METAL" TO TBRAMO-ITEM(1)
           MOVE "TEXTO" TO TBRAMO-ITEM(2)
           MOVE "AGRONEGOCIO" TO TBRAMO-ITEM(3)
           MOVE "SAUDE" TO TBRAMO-ITEM(4)
           MOVE "EDUCACAO" TO TBRAMO-ITEM(5)
           MOVE "INDUSTRIA" TO TBRAMO-ITEM(6)
           MOVE "VAREJO" TO TBRAMO-ITEM(7)
           MOVE "TURISMO" TO TBRAMO-ITEM(8)
           MOVE "ELETRONICA" TO TBRAMO-ITEM(9).

       R0.
           OPEN I-O CADFOR.
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADFOR
                 CLOSE CADFOR
                 MOVE "*** CADFOR FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DE CADFOR" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       RA0.
           OPEN INPUT CADEND.
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADEND
                 CLOSE CADEND
                 MOVE "*** CADEND FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADEND" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       INICIO.
           MOVE ZEROS TO CNPJ FCEP NUMERO DDD TEL RAMO CEP
           MOVE SPACES TO RAZAOSOCIAL NOMEFANTASIA COMPL EMAIL RAMOTXT
           MOVE SPACES TO LOGRADOURO BAIRRO CIDADE UF UFTXT.

       DISPLAY-SCREEN.
           DISPLAY CADASTROFOR.

       R1.
           ACCEPT TCNPJ
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.     

       LER-FORNECEDOR.
           READ CADFOR
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                MOVE "*** FORNECEDOR JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA DE CADFOR" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** FORNECEDOR AINDA NAO EXISTE ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM. 

       R2.
           ACCEPT TRAZAOSOCIAL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1.

       R3.
           ACCEPT TNOMEFANTASIA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
       R4.
           ACCEPT TCEP     
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.

       R4A.
           MOVE FCEP TO CEP
           READ CADEND
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "* ESSE CEP AINDA NAO FOI CADASTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R3
              ELSE
                 MOVE "ERRO NA LEITURA DO CEP " TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
           DISPLAY CADASTROFOR.          

       R5.                    
           ACCEPT TNUMERO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
               GO TO R4.

       R6. ACCEPT TCOMPL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
               GO TO R5.      

       R7. ACCEPT TDD    
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
               GO TO R6.   

       R8. ACCEPT TTEL
           IF W-ACT = 01 
               GO TO R7.  

       R9. ACCEPT TEMAIL
           IF W-ACT = 01 
               GO TO R8.    

       R10. 
           DISPLAY TELA2.
           ACCEPT TRAMO   
           IF W-ACT = 01 
               GO TO R9.
           MOVE TBRAMO-ITEM(RAMO) TO RAMOTXT.
           DISPLAY CADASTROFOR.

           IF W-SEL = 1 
                GO TO ALT-OPC. 

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R8.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   DISPLAY CADASTROFOR
                   GO TO INICIO.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGFOR
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      DISPLAY CADASTROFOR
                      GO TO INICIO.
                IF ST-ERRO = "22"
                  MOVE "* FORNECEDOR JA EXISTE!!!*" 
                  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO INICIO
                ELSE
                      MOVE "ERRO NA GRAVACAO DE FORNCEDOR"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO INICIO  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      DISPLAY CADASTROFOR
                      GO TO R2.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   DISPLAY CADASTROFOR
                   GO TO INICIO.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADFOR RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO FORNECEDOR EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   DISPLAY CADASTROFOR
                   GO TO INICIO.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R5.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   DISPLAY CADASTROFOR
                   GO TO INICIO.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGFOR
                IF ST-ERRO = "00" OR "02"
                   MOVE " REGISTRO DE FORNECEDOR ALTERADO      " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   DISPLAY CADASTROFOR
                   GO TO INICIO.
                MOVE "ERRO NA EXCLUSAO DE FORNECEDOR"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADFOR
           CLOSE CADEND.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.                               
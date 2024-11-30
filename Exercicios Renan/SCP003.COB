       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCP003.
      ************************
      * PROGRAMA PARA CADASTRAR PRODUTOS *
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
      *
           SELECT CADFOR ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY IS CNPJ
                       ALTERNATE RECORD KEY IS NOMEFANTASIA 
                       WITH DUPLICATES
                       FILE STATUS IS ST-ERRO.
      *
           SELECT CADEND ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE IS DYNAMIC
                       RECORD KEY IS CEP
                       FILE STATUS IS ST-ERRO.
      *-----------------------------------------------------------------
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

       FD CADFOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADFOR.DAT".
       01 REGFOR.
           03 CNPJ              PIC 9(14).
           03 RAZAOSOCIAL       PIC X(35).
           03 NOMEFANTASIA      PIC X(12).
           03 FCEP              PIC 9(09).
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

       01 TBUNIDADE.
           03 TBUNIDADE-ITEM OCCURS 4 TIMES.
               05 TBUNIDADE PIC X(15).
       01 TBTIPO.
           03 TBTIPO-ITEM OCCURS 9 TIMES.
               05 TBTIPO PIC X(20).

       SCREEN SECTION.     
       01  CADASTROPRODUTO.
           05 BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "**************************** CADASTRO DE".
           05  LINE 01  COLUMN 41 
               VALUE  " PRODUTOS ******************************".
           05  LINE 02  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 02  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 03  COLUMN 01 
               VALUE  " CODIGO:         DESCRICAO PRODUTO:".
           05  LINE 05  COLUMN 01 
               VALUE  " UNIDADE DO PRODUTO:    -".
           05  LINE 05  COLUMN 41 
               VALUE  "   TIPO DO PRODUTO:   -".
           05  LINE 07  COLUMN 01 
               VALUE  " CNPJ DO FORNECEDOR:                 NOM".
           05  LINE 07  COLUMN 41 
               VALUE  "E FANTASIA:".
           05  LINE 09  COLUMN 01 
               VALUE  " RAZAO SOCIAL:".
           05  LINE 09  COLUMN 41 
               VALUE  "               UF:".
           05  LINE 11  COLUMN 01 
               VALUE  " CIDADE:".
           05  LINE 13  COLUMN 01 
               VALUE  " QUANTIDADE DO PRODUTO:".
           05  LINE 15  COLUMN 01 
               VALUE  " PRECO DO PRODUTO:".
           05  TCOD
               LINE 03  COLUMN 10  PIC 9(06)
               USING  CODIGO.
           05  TDESC
               LINE 03  COLUMN 37  PIC X(30)
               USING  DESC.
           05  TUNIDADE
               LINE 05  COLUMN 22  PIC X(02)
               USING  UNIDADE.
           05  TUNIDADETXT
               LINE 05  COLUMN 27  PIC X(15)
               USING  UNIDADETXT.
           05  TTIPOPRODUTO
               LINE 05  COLUMN 61  PIC 9(01)
               USING  TIPOPRODUTO.
           05  TTIPOTXT
               LINE 05  COLUMN 65  PIC X(15)
               USING  TIPOTXT.
           05  TFCNPJ
               LINE 07  COLUMN 22  PIC 9(14)
               USING  FCNPJ.
           05  TFNOME
               LINE 07  COLUMN 53  PIC X(12)
               USING  NOMEFANTASIA.
           05  TRAZAO
               LINE 09  COLUMN 16  PIC X(35)
               USING  RAZAOSOCIAL.
           05  TUF
               LINE 09  COLUMN 60  PIC X(02)
               USING  UF.
           05  TCIDADE
               LINE 11  COLUMN 10  PIC X(20)
               USING  CIDADE.
           05  TQUANTIDADE
               LINE 13  COLUMN 25  PIC 9(06)
               USING  QUANTIDADE.
           05  TPRECO
               LINE 15  COLUMN 20  PIC 9(09)V99
               USING  PRECO.

       01  TELA1.
           05  LINE 20  COLUMN 41 
               VALUE  "                     LT - LITRO   ".
           05  LINE 21  COLUMN 41 
               VALUE  "                     KG - KILO    ".
           05  LINE 22  COLUMN 41 
               VALUE  "                     TN - TONELADA".
           05  LINE 23  COLUMN 41 
               VALUE  "                     AR - ARROBA  ".  

       01  TELA3.
           05  LINE 16  COLUMN 41 
               VALUE  "                   01 - FERRAMENTAS      ".
           05  LINE 17  COLUMN 41 
               VALUE  "                   02 - MOVEIS   ".
           05  LINE 18  COLUMN 41 
               VALUE  "                   03 - ROUPAS".
           05  LINE 19  COLUMN 41 
               VALUE  "                   04 - MEDICAMENTO    ".
           05  LINE 20  COLUMN 41 
               VALUE  "                   05 - LIMPEZA  ".
           05  LINE 21  COLUMN 41 
               VALUE  "                   06 - BRINQUEDO ".
           05  LINE 22  COLUMN 41 
               VALUE  "                   07 - COSMETICO  ".
           05  LINE 23  COLUMN 41 
               VALUE  "                   08 - ELETRONICO  ".
           05  LINE 24  COLUMN 41 
               VALUE  "                   09 - ALIMENTO ".
           

       PROCEDURE DIVISION.
       VETOR.
           MOVE "LITRO" TO TBUNIDADE-ITEM(1)
           MOVE "KILO" TO TBUNIDADE-ITEM(2)
           MOVE "TONELADA" TO TBUNIDADE-ITEM(3)
           MOVE "ARROBA" TO TBUNIDADE-ITEM(4).

           MOVE "FERRAMENTAS" TO TBTIPO-ITEM(1)
           MOVE "MOVEIS" TO TBTIPO-ITEM(2)
           MOVE "ROUPAS" TO TBTIPO-ITEM(3)
           MOVE "MEDICAMENTO" TO TBTIPO-ITEM(4)
           MOVE "LIMPEZA" TO TBTIPO-ITEM(5)
           MOVE "BRINQUEDO" TO TBTIPO-ITEM(6)
           MOVE "COSMETICO" TO TBTIPO-ITEM(7)
           MOVE "ELETRONICO" TO TBTIPO-ITEM(8)
           MOVE "ALIMENTO" TO TBTIPO-ITEM(9).        

       R00.
           OPEN I-O CADPRO.
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPRO
                 CLOSE CADPRO
                 MOVE "*** ARQUIVO CADPRO FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R00
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADPRO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       RA0.
           OPEN INPUT CADFOR.
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADFOR
                 CLOSE CADFOR
                 MOVE "*** ARQUIVO CADFOR FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R00
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADFOR" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       RB0.
           OPEN INPUT CADEND.
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADEND
                 CLOSE CADEND
                 MOVE "*** ARQUIVO CADEND FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R00
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADEND" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       INICIO.
           MOVE ZEROS TO FCNPJ TIPOPRODUTO CODIGO QUANTIDADE 
           MOVE ZEROS TO PRECO CNPJ
           MOVE SPACES TO DESC UNIDADE UF CIDADE RAZAOSOCIAL 
           MOVE SPACES TO NOMEFANTASIA UNIDADETXT TIPOTXT.

           DISPLAY CADASTROPRODUTO.

       R0.
           ACCEPT TCOD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.

       LER-PRODUTO.
           READ CADPRO
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                MOVE "*** PRODUTO JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO AO LER CADPRO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** PRODUTO NAO FOI CADASTRADO AINDA***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.   

       R1. 
           ACCEPT TDESC                                    
           IF W-ACT = 01
                   GO TO R0.
       R2.
           DISPLAY TELA1
           ACCEPT TUNIDADE
           IF UNIDADE = "LT" 
               MOVE TBUNIDADE-ITEM(01) TO UNIDADETXT
           ELSE
               IF UNIDADE = "KG"
                   MOVE TBUNIDADE-ITEM(02) TO UNIDADETXT
               ELSE
                   IF UNIDADE = "TL"
                       MOVE TBUNIDADE-ITEM(03) TO UNIDADETXT
                   ELSE
                       IF UNIDADE = "AR"
                           MOVE TBUNIDADE-ITEM(04) TO UNIDADETXT
                       ELSE
                           GO TO R2.
           DISPLAY CADASTROPRODUTO.                             
           IF W-ACT = 01
                   GO TO R1.

       R3. 
           DISPLAY TELA3
           ACCEPT TTIPOPRODUTO
           MOVE TBTIPO-ITEM(TIPOPRODUTO) TO TIPOTXT
           DISPLAY CADASTROPRODUTO.
           IF W-ACT = 01
                   GO TO R2.  

       R4.
           ACCEPT TFCNPJ
           IF W-ACT = 01
                   GO TO ROT-FIM. 

       R4A.
           MOVE FCNPJ TO CNPJ
           READ CADFOR
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "* CNPJ NAO EXISTE *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
              ELSE
                 MOVE "ERRO NA LEITURA DO CNPJ " TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
           DISPLAY CADASTROPRODUTO.  

       R4B.
           MOVE FCEP TO CEP
           READ CADEND
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "* CEP NAO EXISTE *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
              ELSE
                 MOVE "ERRO NA LEITURA DO CEP " TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
           DISPLAY CADASTROPRODUTO.     

       R5.
           ACCEPT TQUANTIDADE
            IF W-ACT = 01
                   GO TO R4.

       R6.
           ACCEPT TPRECO
            IF W-ACT = 01
                   GO TO R5. 
           
           IF W-SEL = 1 
                GO TO ALT-OPC. 

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INICIO.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPRO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INICIO.
                IF ST-ERRO = "22"
                  MOVE "* PRODUTO JA EXISTE!!! *"
                  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO INICIO
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
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
                      DISPLAY CADASTROPRODUTO
                      GO TO R1.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INICIO.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADPRO RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO PRODUTO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INICIO.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INICIO.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPRO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INICIO.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO FORNECEDOR"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADPRO
           CLOSE CADFOR
           CLOSE CADEND.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (20, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (20, 12) MENS.
       ROT-MENS-FIM.
                EXIT.                                       
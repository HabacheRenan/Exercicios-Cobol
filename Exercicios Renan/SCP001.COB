       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCP001.
      ************************
      * PROGRAMA PARA CADASTRAR ENDEREÇOS *
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
      *
      *-----------------------------------------------------------------
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

       WORKING-STORAGE SECTION.
       77 W-ACT                PIC 9(02) VALUE ZEROS.
       77 W-CONT               PIC 9(06) VALUE ZEROS.
       77 W-OPCAO              PIC X(01) VALUE SPACES.
       77 MENS                 PIC X(50) VALUE SPACES.
       01 W-SEL                PIC 9(01) VALUE ZEROS.
       01 ST-ERRO              PIC X(02) VALUE "00".

       01 TBESTADOS.
           03 TBESTADOS-ITEM OCCURS 26 TIMES.
               05 TBESTADOS PIC X(20).

       SCREEN SECTION. 
       01  CADASTROENDERECO.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "****************************** CADASTRO".
           05  LINE 01  COLUMN 41 
               VALUE  "DE ENDERECO ****************************".
           05  LINE 03  COLUMN 01 
               VALUE  "CEP:           LOGRADOURO:".
           05  LINE 05  COLUMN 01 
               VALUE  "BAIRRO:".
           05  LINE 07  COLUMN 01 
               VALUE  "CIDADE:".
           05  LINE 07  COLUMN 41 
               VALUE  "        UF:    -".
           05  TCEP
               LINE 03  COLUMN 06  PIC 9(09)
               USING  CEP.
           05  TLOG
               LINE 03  COLUMN 28  PIC X(35)
               USING  LOGRADOURO.
           05  TBAIRRO
               LINE 05  COLUMN 09  PIC X(20)
               USING  BAIRRO.
           05  TCIDADE
               LINE 07  COLUMN 09  PIC X(20)
               USING  CIDADE.
           05  TUF
               LINE 07  COLUMN 53  PIC X(02)
               USING  UF.
           05  TUFTXT
               LINE 07  COLUMN 58  PIC X(20)
               USING  UFTXT.

       PROCEDURE DIVISION.
       VETOR.
           MOVE "ACRE" TO TBESTADOS-ITEM(1)
           MOVE "ALAGOAS" TO TBESTADOS-ITEM(2)
           MOVE "AMAPA" TO TBESTADOS-ITEM(3)
           MOVE "AMAZONAS" TO TBESTADOS-ITEM(4)
           MOVE "BAHIA" TO TBESTADOS-ITEM(5)
           MOVE "CEARA" TO TBESTADOS-ITEM(6)
           MOVE "ESPIRITO SANTO" TO TBESTADOS-ITEM(7)
           MOVE "GOIAS" TO TBESTADOS-ITEM(8)
           MOVE "MARANHAO" TO TBESTADOS-ITEM(9)
           MOVE "MATO GROSSO" TO TBESTADOS-ITEM(10)
           MOVE "MATO GROSSO DO SUL" TO TBESTADOS-ITEM(11)
           MOVE "MINAS GERAIS" TO TBESTADOS-ITEM(12)
           MOVE "PARA" TO TBESTADOS-ITEM(13)
           MOVE "PARAIBA" TO TBESTADOS-ITEM(14)
           MOVE "PARANA" TO TBESTADOS-ITEM(15)
           MOVE "PERNAMBUCO" TO TBESTADOS-ITEM(16)
           MOVE "PIAUI" TO TBESTADOS-ITEM(17)
           MOVE "RIO DE JANEIRO" TO TBESTADOS-ITEM(18)
           MOVE "RIO GRANDE DO NORTE" TO TBESTADOS-ITEM(19)
           MOVE "RIO GRANDE DO SUL" TO TBESTADOS-ITEM(20)
           MOVE "RONDONIA" TO TBESTADOS-ITEM(21)
           MOVE "RORAIMA" TO TBESTADOS-ITEM(22)
           MOVE "SANTA CATARINA" TO TBESTADOS-ITEM(23)
           MOVE "SAO PAULO" TO TBESTADOS-ITEM(24)
           MOVE "SERGIPE" TO TBESTADOS-ITEM(25)
           MOVE "TOCANTINS" TO TBESTADOS-ITEM(26).
       R0. 
           OPEN I-O CADEND.
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADEND
                 CLOSE CADEND
                 MOVE "*** ARQUIVO CADEND FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADEND" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       INICIO.
           MOVE ZEROS TO CEP W-SEL
           MOVE SPACES TO LOGRADOURO BAIRRO CIDADE UF UFTXT.

           DISPLAY CADASTROENDERECO.

       R1.
           ACCEPT TCEP
           IF CEP = ZEROS
               GO TO R0.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1.

       LER-ENDERECO.
           READ CADEND
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                MOVE "*** ENDERECO JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA DE CADEND" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** ENDERECO NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.    

       R2.
           ACCEPT TLOG
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R1.
       R3.
           ACCEPT TBAIRRO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
       R4.
           ACCEPT TCIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
       R5.
           ACCEPT TUF.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
           IF UF = "AC"
               MOVE TBESTADOS-ITEM(1) TO UFTXT
           ELSE     IF UF = "AL"
               MOVE TBESTADOS-ITEM(2) TO UFTXT
           ELSE     IF UF = "AP"
               MOVE TBESTADOS-ITEM(3) TO UFTXT
           ELSE     IF UF = "AM"
               MOVE TBESTADOS-ITEM(4) TO UFTXT
           ELSE     IF UF = "BA"
               MOVE TBESTADOS-ITEM(5) TO UFTXT
           ELSE     IF UF = "CE"
               MOVE TBESTADOS-ITEM(6) TO UFTXT
           ELSE     IF UF = "ES"
               MOVE TBESTADOS-ITEM(7) TO UFTXT
           ELSE     IF UF = "GO"
               MOVE TBESTADOS-ITEM(8) TO UFTXT
           ELSE     IF UF = "MA"
               MOVE TBESTADOS-ITEM(9) TO UFTXT
           ELSE     IF UF = "MT"
               MOVE TBESTADOS-ITEM(10) TO UFTXT
           ELSE     IF UF = "MS"
               MOVE TBESTADOS-ITEM(11) TO UFTXT
           ELSE     IF UF = "MG"
               MOVE TBESTADOS-ITEM(12) TO UFTXT
           ELSE     IF UF = "PA"
               MOVE TBESTADOS-ITEM(13) TO UFTXT
           ELSE     IF UF = "PB"
               MOVE TBESTADOS-ITEM(14) TO UFTXT
           ELSE     IF UF = "PR"
               MOVE TBESTADOS-ITEM(15) TO UFTXT
           ELSE     IF UF = "PE"
               MOVE TBESTADOS-ITEM(16) TO UFTXT
           ELSE     IF UF = "PI"
               MOVE TBESTADOS-ITEM(17) TO UFTXT
           ELSE     IF UF = "RJ"
               MOVE TBESTADOS-ITEM(18) TO UFTXT
           ELSE     IF UF = "RN"
               MOVE TBESTADOS-ITEM(19) TO UFTXT
           ELSE     IF UF = "RS"
               MOVE TBESTADOS-ITEM(20) TO UFTXT
           ELSE     IF UF = "RO"
               MOVE TBESTADOS-ITEM(21) TO UFTXT
           ELSE     IF UF = "RR"
               MOVE TBESTADOS-ITEM(22) TO UFTXT
           ELSE     IF UF = "SC"
               MOVE TBESTADOS-ITEM(23) TO UFTXT
           ELSE     IF UF = "SP"
               MOVE TBESTADOS-ITEM(24) TO UFTXT
           ELSE     IF UF = "SE"
               MOVE TBESTADOS-ITEM(25) TO UFTXT
           ELSE     IF UF = "TO"
               MOVE TBESTADOS-ITEM(26) TO UFTXT
           ELSE    
               MOVE "*** UF INVALIDA ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R5.  
           DISPLAY CADASTROENDERECO.

           IF W-SEL = 1 
                GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (22, 25) "DADOS OK (S/N): ".
                ACCEPT (22, 48) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R4.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   DISPLAY CADASTROENDERECO
                   GO TO INICIO.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC. 
       INC-WR1.
                WRITE REGEND
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      DISPLAY CADASTROENDERECO
                      GO TO INICIO.
                IF ST-ERRO = "22"
           MOVE "* ENDERECO JA EXISTE!!! *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  DISPLAY CADASTROENDERECO
                  GO TO INICIO
                ELSE
                  MOVE "ERRO NA GRAVACAO DO ARQUIVO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

       ACE-001.
                DISPLAY (22, 25) 
                "N=NOVO REGISTRO A=ALTERAR E=EXCLUIR".
                ACCEPT (22, 66) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (22, 25) MENS
                IF W-OPCAO = "N"
                   GO TO INICIO
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      DISPLAY CADASTROENDERECO
                      GO TO R2.
       EXC-OPC.
                DISPLAY (22, 25) "EXCLUIR   (S/N) : ".
                ACCEPT (22, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   DISPLAY CADASTROENDERECO
                   GO TO INICIO.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.

       EXC-DL1.
                DELETE CADEND RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   DISPLAY CADASTROENDERECO
                   GO TO INICIO.
                MOVE "ERRO NA EXCLUSAO DO ENDERECO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ALT-OPC.
                DISPLAY (22, 25) "ALTERAR? (S/N) : ".
                ACCEPT (22, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R4.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** ENDERECO NAO ALTERADO *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   DISPLAY CADASTROENDERECO
                   GO TO INICIO.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.

       ALT-RW1.
                REWRITE REGEND
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INICIO.
                MOVE "ERRO NA EXCLUSAO DO ENDERECO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
               
       ROT-FIM.
           CLOSE CADEND.
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
                                                        
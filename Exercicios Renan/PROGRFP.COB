       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRFP.
      ************************
      * PROGRAMA PARA CADASTRAR FUNCIONÁRIOS *
      ***********************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADFOLHA ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY IS CODIGO
                       FILE STATUS  IS ST-ERRO.
      *
      *----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADFOLHA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADFOLHA.DAT".
       01 REGFOLHA.
           03 CODIGO        PIC 9(06).
           03 NOME          PIC X(30).
           03 TIPOSALARIO   PIC X(01).
           03 SALARIOBASE   PIC 9(06)V99.
           03 NUMFILHOS     PIC 9(02).
           03 DEPARTAMENTO  PIC 9(02).
           03 FUNC          PIC X(01).
           03 BRUTO         PIC 9(09)V99.
           03 DEPTXT        PIC X(10).
           03 TXTFUNC       PIC X(25).
           03 INSS          PIC 9(06)V99.
           03 IMPOSTO       PIC 9(06)V99.
           03 SALFAMI       PIC 9(06)V99.
           03 SALIQ         PIC 9(06)V99.
       
      *----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-ACT                PIC 9(02) VALUE ZEROS.
       77 W-CONT               PIC 9(06) VALUE ZEROS.
       77 W-OPCAO              PIC X(01) VALUE SPACES.
       77 MENS                 PIC X(50) VALUE SPACES.
       01 CONTROLE             PIC 9(06)V99 VALUE ZEROS.
       01 W-SEL                PIC 9(01) VALUE ZEROS.
       01 ST-ERRO              PIC X(02) VALUE "00".
       01 X                    PIC 9(03) VALUE ZEROS.
       01 Y                    PIC 9(02) VALUE ZEROS.
       01 Z                    PIC 9(02) VALUE ZEROS.

       01 TBFUNC.
           03 TBFUNC-ITEM OCCURS 8 TIMES.
               05 TBFUNC    PIC X(25).
       01  TBDEPTO.
           03 TBDEPTO-ITEM OCCURS 10 TIMES.
               05 TBDEPTO      PIC X(30).
       
      *----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA.
           05 BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "****************************  CADASTRO D".
           05  LINE 01  COLUMN 41 
               VALUE  "E FUNCIONARIOS *************************".
           05  LINE 02  COLUMN 01 
               VALUE  "CODIGO:".
           05  LINE 04  COLUMN 01 
               VALUE  "NOME:                                 TI".
           05  LINE 04  COLUMN 41 
               VALUE  "PO SALRIO:    SALARIOS BASE:".
           05  LINE 06  COLUMN 01 
               VALUE  "NUMERO DE FILHOS:".
           05  LINE 08  COLUMN 01 
               VALUE  "DEPARTAMENTO:    -".
           05  LINE 10  COLUMN 01 
               VALUE  "FUNCAO:   -".
           05  LINE 12  COLUMN 01 
               VALUE  "SALARIO BRUTO -".
           05  LINE 14  COLUMN 01 
               VALUE  "INSS -".
           05  LINE 16  COLUMN 01 
               VALUE  "IMPOSTO DE RENDA -".
           05  LINE 18  COLUMN 01 
               VALUE  "SALARIO DE FAMILIA -".
           05  LINE 20  COLUMN 01 
               VALUE  "SALARIO LIQUIDO -".
           05  TCODIGO
               LINE 02  COLUMN 09  PIC 9(06)
               USING  CODIGO.
           05  TNOME
               LINE 04  COLUMN 07  PIC X(30)
               USING  NOME.
           05  TTIPOSALARIO
               LINE 04  COLUMN 52  PIC X(01)
               USING  TIPOSALARIO.
           05  TSALARIO
               LINE 04  COLUMN 70  PIC 9(06)V99
               USING  SALARIOBASE.
           05  TFILHO
               LINE 06  COLUMN 19  PIC 9(02)
               USING  NUMFILHOS.
           05  TDEP
               LINE 08  COLUMN 15  PIC 9(02)
               USING  DEPARTAMENTO.
           05  TDEPTXT
               LINE 08  COLUMN 20  PIC X(10)
               USING  DEPTXT.
           05  TFUNC
               LINE 10  COLUMN 09  PIC X(01)
               USING  FUNC.
           05  TTXTFUNC
               LINE 10  COLUMN 13  PIC X(30)
               USING  TXTFUNC.
           05  TBRUTO
               LINE 12  COLUMN 17  PIC 9(06)V99
               USING  BRUTO.
           05  TINSS
               LINE 14  COLUMN 08  PIC 9(06)V99
               USING  INSS.
           05  TIMPOSTO
               LINE 16  COLUMN 20  PIC 9(06)V99
               USING  IMPOSTO.
           05  TSALFAMI
               LINE 18  COLUMN 22  PIC 9(06)V99
               USING  SALFAMI.
           05  TSALIQ
               LINE 20  COLUMN 19  PIC 9(06)V99
               USING  SALIQ.
       
       01  TELA1.
           05  LINE 11  COLUMN 41 
               VALUE  "        A - ANALISTA".
           05  LINE 12  COLUMN 41 
               VALUE  "        B - ASSISTENTE".
           05  LINE 13  COLUMN 41 
               VALUE  "        G - GERENTE".
           05  LINE 14  COLUMN 41 
               VALUE  "        C - COORDENADOR DE VENDAS".
           05  LINE 15  COLUMN 41 
               VALUE  "        P - PESQUISADOR".
           05  LINE 16  COLUMN 41 
               VALUE  "        L - LOGISTICO".
           05  LINE 17  COLUMN 41 
               VALUE  "        R - REPRESENTANTE DE VENDAS".
           05  LINE 18  COLUMN 41 
               VALUE  "        S - SUPERVISOR JURÍDICO".

       01  TELA2.
           05  LINE 11  COLUMN 41 
               VALUE  "        01 - FINANCEIRO".
           05  LINE 12  COLUMN 41 
               VALUE  "        02 - RH".
           05  LINE 13  COLUMN 41 
               VALUE  "        03 - COMPRAS".
           05  LINE 14  COLUMN 41 
               VALUE  "        04 - VENDAS".
           05  LINE 15  COLUMN 41 
               VALUE  "        05 - LOGISTICA".
           05  LINE 16  COLUMN 41 
               VALUE  "        06 - TI".
           05  LINE 17  COLUMN 41 
               VALUE  "        07 - MARKETING".
           05  LINE 18  COLUMN 41 
               VALUE  "        08 - PRODUCAO".
           05  LINE 19  COLUMN 41 
               VALUE  "        09 - QUALIDADE".
           05  LINE 20  COLUMN 41 
               VALUE  "        10 - JURIDICO".        

       01  TELA3.
           05  LINE 11  COLUMN 41 
               VALUE  "        H - HORISTA".
           05  LINE 12  COLUMN 41 
               VALUE  "        D - DIARISTA".
           05  LINE 13  COLUMN 41 
               VALUE  "        M - MENSALISTA".
       
      *----------------------------------------------------------------
       PROCEDURE DIVISION.
       TABELAS.
      * TABELA DE FUNCOES ---------------------------------------------- 
           MOVE "ANALISTA" TO TBFUNC-ITEM(1)
           MOVE "ASSISTENTE" TO TBFUNC-ITEM(2)
           MOVE "GERENTE" TO TBFUNC-ITEM(3)
           MOVE "COORDENADOR DE VENDAS" TO TBFUNC-ITEM(4)
           MOVE "PESQUISADOR" TO TBFUNC-ITEM(5)
           MOVE "LOGISTICO" TO TBFUNC-ITEM(6)
           MOVE "REPRESENTANTE DE VENDAS" TO TBFUNC-ITEM(7)
           MOVE "SUPERVISOR JURÍDICO" TO TBFUNC-ITEM(8).

      * TABELA DE DEPARTAMENTOS ----------------------------------------
           MOVE "FINANCEIRO" TO TBDEPTO-ITEM(1)
           MOVE "RH" TO TBDEPTO-ITEM(2)
           MOVE "COMPRAS" TO TBDEPTO-ITEM(3)
           MOVE "VENDAS" TO TBDEPTO-ITEM(4)
           MOVE "LOGISTICA" TO TBDEPTO-ITEM(5)
           MOVE "TI" TO TBDEPTO-ITEM(6)
           MOVE "MARKETING" TO TBDEPTO-ITEM(7)
           MOVE "PRODUCAO" TO TBDEPTO-ITEM(8)
           MOVE "QUALIDADE" TO TBDEPTO-ITEM(9)
           MOVE "JURIDICO" TO TBDEPTO-ITEM(10).

       INICIO.       
           OPEN I-O CADFOLHA.
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADFOLHA
                 CLOSE CADFOLHA
                 MOVE " CADFOLHA CRIADO! " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INICIO
              ELSE
                 MOVE "OCORREU UM ERRO AO ABRIR CADFOLHA" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       
       INITIALIZE-RECORD.
           MOVE SPACES TO NOME TIPOSALARIO FUNC DEPTXT TXTFUNC
           MOVE ZEROS TO CODIGO SALARIOBASE NUMFILHOS DEPARTAMENTO
           MOVE ZEROS TO BRUTO INSS IMPOSTO SALFAMI SALIQ W-SEL.

       
      *------------------------------------------------------------------
       DISPLAY-SCREEN.
           DISPLAY TELA.

      *------------------------------------------------------------------
       INPUT-DATA.
       R0.
           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
       LER-FOLHA.
           READ CADFOLHA
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                MOVE "*** FUNCIONARIO JA CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADFOLHA" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** FUNCIONARIO NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R1.                     
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R0.
          
       R2.
           DISPLAY TELA3
           ACCEPT TTIPOSALARIO
               ACCEPT W-ACT FROM ESCAPE KEY
               IF W-ACT = 01
                   GO TO R1.
           IF TIPOSALARIO = "H" OR
               TIPOSALARIO = "D" OR
               TIPOSALARIO = "M"
               IF TIPOSALARIO = "H" OR TIPOSALARIO = "h"
                   MOVE 220 TO X
               ELSE 
                   IF TIPOSALARIO = "D" OR TIPOSALARIO = "d"
                       MOVE 30 TO X
                   ELSE
                       MOVE 1 TO X
           ELSE
               MOVE "*** DIGITE APENAS H, D OU M ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R2.
           DISPLAY TELA.    

       R3.
           ACCEPT TSALARIO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
               GO TO R2.

           IF SALARIOBASE < 2500,01
               MOVE 8 TO Y
           ELSE 
               IF SALARIOBASE > 2500,00 AND SALARIOBASE < 6300,01
                   MOVE 9 TO Y
               ELSE
                   MOVE 10 TO Y.
           COMPUTE BRUTO = SALARIOBASE * X
           DIVIDE BRUTO BY 100 GIVING INSS
           COMPUTE INSS = INSS * Y 

           COMPUTE CONTROLE = BRUTO - INSS    

           IF CONTROLE < 5000,01
               MOVE 0 TO Z
           ELSE 
               IF CONTROLE > 5000,00 AND CONTROLE < 12000,01
                   MOVE 5 TO Z
               ELSE 
                   MOVE 10 TO Z.  
           COMPUTE IMPOSTO = BRUTO - INSS.
           DIVIDE IMPOSTO BY 100 GIVING IMPOSTO.
           COMPUTE IMPOSTO = IMPOSTO * Z.        

       R4.
           ACCEPT TFILHO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
               GO TO R3.
           COMPUTE SALFAMI = NUMFILHOS * 20,00
           COMPUTE SALIQ = BRUTO - INSS
           COMPUTE SALIQ = (SALIQ - IMPOSTO) + SALFAMI.
           DISPLAY TELA.

       R5.
           DISPLAY TELA2
           ACCEPT TDEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
               GO TO R4.
           MOVE TBDEPTO-ITEM(DEPARTAMENTO) TO DEPTXT.
           DISPLAY TELA.    

       R6.
           DISPLAY TELA1
           ACCEPT TFUNC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
               GO TO R5.
           IF FUNC = "A"
                   MOVE TBFUNC-ITEM(1) TO TXTFUNC
               ELSE
               IF FUNC = "B"
                   MOVE TBFUNC-ITEM(2) TO TXTFUNC
               ELSE
               IF FUNC = "G"
                   MOVE TBFUNC-ITEM(3) TO TXTFUNC
               ELSE
               IF FUNC = "C"
                   MOVE TBFUNC-ITEM(4) TO TXTFUNC
               ELSE
               IF FUNC = "P"
                   MOVE TBFUNC-ITEM(5) TO TXTFUNC
               ELSE
               IF FUNC = "R"
                   MOVE TBFUNC-ITEM(6) TO TXTFUNC
               ELSE 
               IF FUNC = "L"
                   MOVE TBFUNC-ITEM(7) TO TXTFUNC 
               ELSE
               IF FUNC = "S"
                   MOVE TBFUNC-ITEM(8) TO TXTFUNC
               ELSE 
                   MOVE "*** DIGITE UMA DAS FUNCOES AO LADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R6.
           DISPLAY TELA.  

           IF W-SEL = 1 
                GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (22, 30) "DADOS OK (S/N): ".
                ACCEPT (22, 48) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INITIALIZE-RECORD.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC. 
       INC-WR1.
                WRITE REGFOLHA
                IF ST-ERRO = "00" OR ST-ERRO = "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      DISPLAY TELA
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INITIALIZE-RECORD.
                IF ST-ERRO = "22"
           MOVE "* FUNCIONARIO JA EXISTE, DADOS NAO GRAVADOS *" TO MENS
                  DISPLAY TELA
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO INITIALIZE-RECORD
                ELSE
                  MOVE "ERRO NA GRAVACAO DO ARQUIVO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

       ACE-001.
                DISPLAY (22, 30) 
                "N=NOVO REGISTRO A=ALTERAR E=EXCLUIR".
                ACCEPT (22, 66) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (22, 20) MENS
                IF W-OPCAO = "N"
                   GO TO INITIALIZE-RECORD
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      DISPLAY TELA
                      GO TO R1.
       EXC-OPC.
                DISPLAY (22, 30) "EXCLUIR   (S/N) : ".
                ACCEPT (22, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   DISPLAY TELA
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INITIALIZE-RECORD.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.

       EXC-DL1.
                DELETE CADFOLHA RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO FUNCIONARIO EXCLUIDO ***" TO MENS
                   DISPLAY TELA
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INITIALIZE-RECORD.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ALT-OPC.
                DISPLAY (22, 30) "ALTERAR?   (S/N) : ".
                ACCEPT (22, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   DISPLAY TELA
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INITIALIZE-RECORD.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.

       ALT-RW1.
                REWRITE REGFOLHA
                IF ST-ERRO = "00" OR "02"
                  MOVE "***REGISTRO DE FUNCIONATIO ALTERADO ***" TO MENS
                   DISPLAY TELA
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INITIALIZE-RECORD.
                MOVE "ERRO NA EXCLUSAO DO FUNCIONARIO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
               
       ROT-FIM.
           CLOSE CADFOLHA.
           STOP RUN.
       
       ROT-MENS.
           MOVE ZEROS TO W-CONT.

       ROT-MENS1.
           DISPLAY (22, 30) MENS.
       
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
              GO TO ROT-MENS2
           ELSE
              MOVE SPACES TO MENS
              DISPLAY (22, 30) MENS.

       ROT-MENS-FIM.
           EXIT. 
           
      *-----------------------------------------------------------------
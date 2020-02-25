       IDENTIFICATION DIVISION.
       PROGRAM-ID. leitor-rfb.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-EMPRESAS ASSIGN TO "dados2/empresas.dat"
                   ORGANIZATION INDEXED
                   ACCESS MODE DYNAMIC
                   RECORD KEY IS FRE-CNPJ-ID
                   STATUS ST-ARQUIVO-EMP.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-EMPRESAS.
       COPY "FD-REG-EMPRESA.cpy".

       WORKING-STORAGE SECTION.
       
       01 ST-ARQUIVO-SOC            PIC XX.
       01 ST-ARQUIVO-EMP            PIC XX.
       01 WS-EOF                    PIC X VALUE 'N'.
       01 WS-COMANDO                PIC X VALUE ' '.
       01 WS-CNPJ-ED.
          03 CNPJ1                  PIC 9(002).
          03 FILLER                 PIC X VALUE '.'.
          03 CNPJ2                  PIC 9(003).
          03 FILLER                 PIC X VALUE '.'.
          03 CNPJ3                  PIC 9(003). 
          03 FILLER                 PIC X VALUE '/'.
          03 CNPJ4                  PIC 9(004).
          03 FILLER                 PIC X VALUE '-'.
          03 CNPJ5                  PIC 9(002). 
       01 WS-MENSAGEM               PIC X(070).
       01 WS-MATRIZ-FILIAL          PIC X.
       01 WS-SITUACAO-CADASTRAL     PIC X(010).
       01 WS-DATA-SIT-CADASTRAL.
          03 DATA-SIT-DD         PIC 99.
          03 FILLER                 PIC X VALUE '/'.
          03 DATA-SIT-MM         PIC 99.
          03 FILLER                 PIC X VALUE '/'.
          03 DATA-SIT-AA         PIC 99.

       COPY 'REGISTRO-RFB-EMPRESA.cpy'.

       SCREEN SECTION.
       01 SC-TELA-LIMPA BLANK SCREEN.
       01 SC-REGUA-COMANDO.
          05 VALUE '    COMANDO ===>                                   '
           LINE 2 COL 2 FOREGROUND-COLOR IS 3.
          05 COMANDO-INPUT         LINE 2 COL 19 FOREGROUND-COLOR IS 2
           PIC X TO WS-COMANDO.
       01 SC-MENU-PRINCIPAL.
          05 VALUE '--------------------- *** LEITOR DE DADOS ABERTOS RF
      -     'B *** -----------------------'
           LINE 1 COL 1 FOREGROUND-COLOR IS 3.
          05 VALUE '1 - BUSCAR PESSOA JURÍDICA.' LINE 4 COL 2
           FOREGROUND-COLOR IS 2.
          05 VALUE '2 - BUSCAR PESSOA FÍSICA.' LINE 5 COL 2
           FOREGROUND-COLOR IS 2.
          05 VALUE 'S-SAIR'        LINE 24 COL 2 FOREGROUND-COLOR IS 3. 
       01 SC-ERRO-COMANDO.
          05 VALUE '*** COMANDO INCORRETO ***' LINE 25 COL 2
           FOREGROUND-COLOR IS 7 BACKGROUND-COLOR IS 4.
       01 SC-MENSAGEM.
          05 MSG-INPUT LINE 25 COL 2 FOREGROUND-COLOR IS 1
           FROM WS-MENSAGEM.
       01 SC-BUSCAR-PJ.
          05 VALUE '--------------------- *** BUSCAR PESSOA JURÍDICA **
      -     '*  ----------------' LINE 1 COL 1 FOREGROUND-COLOR IS 3.
          05 VALUE '**ENTRE COM OS DADOS DE BUSCA E PRESSIONE ENTER**' 
            LINE 2 COL 2 FOREGROUND-COLOR IS 3.
          05 LINE 4 COL 2 VALUE 'CNPJ: ' FOREGROUND-COLOR IS 2.
          05 CNPJ-INPUT LINE 4 COL 8     FOREGROUND-COLOR IS 7
           FROM RRE-CNPJ TO RRE-CNPJ.
          05 LINE 5 COL 2 VALUE 'RAZAO SOCIAL: ' FOREGROUND-COLOR IS 2.
          05 RAZAO-INPUT LINE 5 COL 17   FOREGROUND-COLOR IS 7
           FROM RRE-RAZAO-SOCIAL TO RRE-RAZAO-SOCIAL.
       01 SC-MENU-BUSCA-PJ.
          05 LINE 24 COL 2 VALUE '1-BUSCAR POR CNPJ  2-BUSCAR POR RAZAO
      -      'SOCIAL  V-VOLTAR'
           FOREGROUND-COLOR IS 3.
       01 SC-EXIBIR-PJ.
          05 VALUE '------------------- *** LISTAR DADOS PESSOA JURÍDIC
      -     'A *** -------------' LINE 1 COL 1 FOREGROUND-COLOR IS 3.
          05 LINE 4 COL 2 VALUE 'CNPJ: ' FOREGROUND-COLOR IS 2.
          05 CNPJ-INPUT LINE 4 COL 8    FOREGROUND-COLOR IS 7
           FROM WS-CNPJ-ED.
          05 LINE 4 COL 35 VALUE 'MATRIZ/FILIAL: ' FOREGROUND-COLOR IS
               2.
          05 MF-INPUT LINE 4 COL 50 FOREGROUND-COLOR IS 7
           FROM WS-MATRIZ-FILIAL.
          05 LINE 5 COL 2 VALUE 'RAZAO SOCIAL: ' FOREGROUND-COLOR IS 2.
          05 RAZAO-INPUT LINE 5 COL 17 FOREGROUND-COLOR IS 7
           FROM RRE-RAZAO-SOCIAL.
          05 LINE 6 COL 2 VALUE 'NOME FANTASIA: ' FOREGROUND-COLOR IS 2.
          05 NMF-INPUT LINE 6 COL 17 FOREGROUND-COLOR IS 7
           FROM RRE-NOME-FANTASIA.
          05 LINE 7 COL 2 VALUE 'LOGRADOURO: ' FOREGROUND-COLOR IS 2.
          05 LOGRA-INPUT LINE 7 COL 14 FOREGROUND-COLOR IS 7
           FROM RRE-LOGRADOURO.
          05 LINE 8 COL 2 VALUE 'SIT.CADASTRAL: ' FOREGROUND-COLOR IS 2.
          05 SITCAD-INPUT LINE 8 COL 17 FOREGROUND-COLOR IS 7
           FROM WS-SITUACAO-CADASTRAL.
          05 LINE 8 COL 35 VALUE 'DATA SIT.CADASTRAL: ' FOREGROUND-COLOR
           IS 2.
          05 DTSITCAD-INPUT LINE 8 COL 55 FOREGROUND-COLOR IS 7
           FROM WS-DATA-SIT-CADASTRAL.

       PROCEDURE DIVISION.
       0001-MAIN-PARA.
           DISPLAY SC-TELA-LIMPA.
           PERFORM UNTIL WS-COMANDO IS EQUAL TO 'S'
             DISPLAY SC-MENU-PRINCIPAL
             DISPLAY SC-REGUA-COMANDO
             ACCEPT SC-REGUA-COMANDO
             EVALUATE WS-COMANDO
                     WHEN 1
                             PERFORM 0002-MENU-BUSCAR-PJ
                     WHEN 2
                             DISPLAY 'BUSCAR PESSOA FÍSICA'
                     WHEN 'S'
                             MOVE ' ' TO WS-MENSAGEM
                     WHEN OTHER
                             DISPLAY SC-ERRO-COMANDO
             END-EVALUATE
           END-PERFORM.
           STOP RUN.
       0002-MENU-BUSCAR-PJ.
           DISPLAY SC-TELA-LIMPA.
           DISPLAY SC-BUSCAR-PJ.
           ACCEPT SC-BUSCAR-PJ.
           PERFORM UNTIL WS-COMANDO IS EQUAL TO 'V'
             DISPLAY SC-REGUA-COMANDO
             DISPLAY SC-MENU-BUSCA-PJ
             ACCEPT SC-REGUA-COMANDO
             EVALUATE WS-COMANDO
                   WHEN 1
                           PERFORM 0004-BUSCAR-PJ-CNPJ
                   WHEN 2
                           PERFORM 0005-BUSCAR-PJ-RAZAO
                   WHEN OTHER
                           DISPLAY SC-ERRO-COMANDO
             END-EVALUATE
           END-PERFORM.
           DISPLAY SC-TELA-LIMPA.
       0003-EXIBIR-PJ.
      ************TRATA OS DADOS APLICANDO MASCARAS*************     
           MOVE CORRESPONDING RRE-CNPJ TO WS-CNPJ-ED.
           EVALUATE RRE-IND-MATRIZ-FILIAL
                   WHEN 1
                           MOVE 'M' TO WS-MATRIZ-FILIAL
                   WHEN 2
                           MOVE 'F' TO WS-MATRIZ-FILIAL
                   WHEN OTHER
                           INITIALIZE WS-MATRIZ-FILIAL
           END-EVALUATE.
           EVALUATE RRE-SIT-CADASTRAL
                   WHEN '01'
                           MOVE 'NULA' TO WS-SITUACAO-CADASTRAL
                   WHEN '02'
                           MOVE 'ATIVA' TO WS-SITUACAO-CADASTRAL
                   WHEN '03'
                           MOVE 'SUSPENSA' TO WS-SITUACAO-CADASTRAL
                   WHEN '04'
                           MOVE 'INAPTA' TO WS-SITUACAO-CADASTRAL
                   WHEN '08'
                           MOVE 'BAIXADA' TO WS-SITUACAO-CADASTRAL
                   WHEN OTHER
                           INITIALIZE WS-SITUACAO-CADASTRAL
           END-EVALUATE.
           MOVE CORRESPONDING RRE-DATA-SIT-CADASTRAL TO
            WS-DATA-SIT-CADASTRAL.
      **********************************************************
           DISPLAY SC-TELA-LIMPA.
           DISPLAY SC-EXIBIR-PJ.
           DISPLAY SC-REGUA-COMANDO.
           MOVE '            ***** PRESSIONE ENTER PARA CONTINUAR ***'
             TO WS-MENSAGEM.
           IF ST-ARQUIVO-EMP IS NOT EQUAL TO '00' THEN
                   MOVE ST-ARQUIVO-EMP TO WS-MENSAGEM
           END-IF.
           DISPLAY SC-MENSAGEM.
           ACCEPT SC-REGUA-COMANDO.
           INITIALIZE REGISTRO-RFB-EMPRESA.
           MOVE 'V' TO WS-COMANDO.
       0004-BUSCAR-PJ-CNPJ.
           OPEN INPUT ARQ-EMPRESAS.
           MOVE RRE-CNPJ TO FRE-CNPJ-ID.
           INITIALIZE REGISTRO-RFB-EMPRESA.
           READ ARQ-EMPRESAS
                   KEY IS FRE-CNPJ-ID
                   INVALID KEY PERFORM
                           MOVE '            *** NAO ENCONTRADO ***' TO
                             RRE-RAZAO-SOCIAL
                   END-PERFORM
                   NOT INVALID KEY PERFORM
                           MOVE FD-REG-EMPRESA TO
                           REGISTRO-RFB-EMPRESA
                   END-PERFORM
           END-READ.
           CLOSE ARQ-EMPRESAS.
           PERFORM 0003-EXIBIR-PJ.
       0005-BUSCAR-PJ-RAZAO.
           OPEN INPUT ARQ-EMPRESAS.
           INITIALIZE WS-EOF.
           PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
             READ ARQ-EMPRESAS
                   AT END PERFORM
                           MOVE 'Y' TO WS-EOF
                           MOVE '*** NAO ENCONTRADO ***'
                             TO RRE-RAZAO-SOCIAL
                   END-PERFORM
                     NOT AT END PERFORM
                           DISPLAY FRE-RAZAO-SOCIAL LINE 5 COL 17
                           IF FRE-RAZAO-SOCIAL IS EQUAL TO
                                   RRE-RAZAO-SOCIAL THEN
                              MOVE 'Y' TO WS-EOF
                              MOVE FD-REG-EMPRESA TO
                                REGISTRO-RFB-EMPRESA
                           END-IF
                   END-PERFORM
           END-PERFORM
           CLOSE ARQ-EMPRESAS.
           PERFORM 0003-EXIBIR-PJ.


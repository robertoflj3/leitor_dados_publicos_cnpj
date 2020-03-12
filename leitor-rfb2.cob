       IDENTIFICATION DIVISION.
       PROGRAM-ID. leitor-rfb2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-SOCIOS ASSIGN TO "dados/socios2teste.dat"
                   ORGANIZATION INDEXED
                   ACCESS MODE DYNAMIC
                   RECORD KEY IS FRS-COD-SOCIO
                   ALTERNATE RECORD KEY IS FRS-NOME-SOCIO WITH
                   DUPLICATES
                   ALTERNATE RECORD KEY IS FRS-CNPJ WITH DUPLICATES
                   STATUS ST-ARQUIVO-SOC.

           SELECT ARQ-EMPRESAS ASSIGN TO "dados/empresas2teste.dat"
                   ORGANIZATION INDEXED
                   ACCESS MODE DYNAMIC
                   RECORD KEY IS FRE-CNPJ-ID
                   ALTERNATE RECORD KEY IS FRE-RAZAO-SOCIAL WITH
                    DUPLICATES
                   STATUS ST-ARQUIVO-EMP.

           SELECT ARQ-MOT-SIT-CAD ASSIGN TO
                   "TABELASRFB/MOTIVO-SIT-CADASTRAL"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQ-MOT-SIT-CAD.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-SOCIOS.
           COPY "FD-REG-SOCIO.cpy".

       FD ARQ-EMPRESAS.
           COPY "FD-REG-EMPRESA.cpy".

       FD ARQ-MOT-SIT-CAD.
       01 FD-MOT-SIT-CAD.
           03 FD-MOT-COD            PIC 99.
           03 FD-MOT-DESCRICAO      PIC X(088).

       WORKING-STORAGE SECTION.
       
       01 ST-ARQUIVO-SOC            PIC XX.
       01 ST-ARQUIVO-EMP            PIC XX.
       01 ST-ARQ-MOT-SIT-CAD        PIC XX.
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
          03 DATA-SIT-DD            PIC 99.
          03 FILLER                 PIC X VALUE '/'.
          03 DATA-SIT-MM            PIC 99.
          03 FILLER                 PIC X VALUE '/'.
          03 DATA-SIT-AA            PIC 99.
       01 WS-MOTIVO-SIT-CADASTRAL   PIC X(088).
       01 WS-DATA-INICIO-ATIVIDADE.
          03 DATA-INI-ATI-DD        PIC 99.
          03 FILLER                 PIC X VALUE '/'.
          03 DATA-INI-ATI-MM        PIC 99.
          03 FILLER                 PIC X VALUE '/'.
          03 DATA-INI-ATI-AAAA      PIC 9999.
       01 WS-COMPLEMENTO            PIC X(015).
       01 WS-PORTE                  PIC X(025).
       01 WS-OPCAO-SIMPLES          PIC X(020).
       01 WS-CAPITAL-SOCIAL         PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
       77 WS-CONTINUA-BUSCA         PIC X VALUE 'N'.
       77 WS-RAZAO-SOCIAL-BUSCA     PIC X(150) VALUE SPACES.
       77 WS-CNPJ-BUSCA             PIC 9(014) VALUE ZEROES.
       77 WS-NOME-SOCIO-BUSCA       PIC X(150) VALUE SPACES.

       COPY 'REGISTRO-RFB-EMPRESA.cpy'.
       COPY 'REGISTRO-RFB-SOCIO.cpy'.

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
          05 VALUE '1 - BUSCAR PESSOA JURIDICA.' LINE 4 COL 2
           FOREGROUND-COLOR IS 2.
          05 VALUE '2 - BUSCAR PESSOA FISICA.' LINE 5 COL 2
           FOREGROUND-COLOR IS 2.
          05 VALUE 'S-SAIR'        LINE 24 COL 2 FOREGROUND-COLOR IS 3. 

       01 SC-ERRO-COMANDO.
          05 VALUE '*** COMANDO INCORRETO ***' LINE 23 COL 30
           FOREGROUND-COLOR IS 7 BACKGROUND-COLOR IS 4.

       01 SC-MENSAGEM.
          05 MSG-INPUT LINE 24 COL 2 FOREGROUND-COLOR IS 3
           FROM WS-MENSAGEM.

       01 SC-BUSCAR-PJ.
          05 VALUE '--------------------- *** BUSCAR PESSOA JURIDICA **
      -     '*  ----------------' LINE 1 COL 1 FOREGROUND-COLOR IS 3.
          05 VALUE '** ENTRE COM OS DADOS DE BUSCA E PRESSIONE ENTER **'
            LINE 2 COL 10 FOREGROUND-COLOR IS 3.
          05 LINE 4 COL 2 VALUE 'CNPJ: ' FOREGROUND-COLOR IS 2.
          05 CNPJ-INPUT LINE 4 COL 8     FOREGROUND-COLOR IS 7
           FROM RRE-CNPJ TO RRE-CNPJ.
          05 LINE 5 COL 2 VALUE 'RAZAO SOCIAL: ' FOREGROUND-COLOR IS 2.
          05 RAZAO-INPUT LINE 5 COL 17   FOREGROUND-COLOR IS 7
           FROM RRE-RAZAO-SOCIAL TO RRE-RAZAO-SOCIAL.
       
       01 SC-BUSCAR-PF.
          05 VALUE '--------------------- *** BUSCAR PESSOA FISICA *** -
      -     '----------------' LINE 1 COL 1 FOREGROUND-COLOR IS 3.
          05 VALUE '** ENTRE COM OS DADOS DE BUSCA E PRESSIONE ENTER **'
            LINE 2 COL 10 FOREGROUND-COLOR IS 3.
          05 LINE 4 COL 2 VALUE 'CNPJ DA EMPRESA: '
            FOREGROUND-COLOR IS 2.
          05 CNPJ-EMPRESA-INPUT LINE 4 COL 19     FOREGROUND-COLOR IS 7
           FROM RRS-CNPJ TO RRS-CNPJ.
          05 LINE 5 COL 2 VALUE 'NOME DO SOCIO: ' FOREGROUND-COLOR IS 2.
          05 NOME-SOCIO-INPUT LINE 5 COL 17   FOREGROUND-COLOR IS 7
           FROM RRS-NOME-SOCIO TO RRS-NOME-SOCIO.

      * 01 SC-MENU-BUSCA-PJ.
      *    05 LINE 24 COL 2 VALUE '1-BUSCAR POR CNPJ  2-BUSCAR POR RAZAO
      *-      'SOCIAL  V-VOLTAR'
      *     FOREGROUND-COLOR IS 3.
       
      * 01 SC-MENU-BUSCA-PF.
      *    05 LINE 24 COL 2 VALUE '1-BUSCAR POR CNPJ DA EMPRESA  2-BUSCAR
      *-      ' POR NOME DO SOCIO V-VOLTAR'
      *     FOREGROUND-COLOR IS 3.

       01 SC-EXIBIR-PJ.
          05 VALUE '------------------- *** LISTAR DADOS PESSOA JURIDICA
      -     ' *** -------------' LINE 1 COL 1 FOREGROUND-COLOR IS 3.
          05 LINE 3 COL 2 VALUE 'CNPJ: ' FOREGROUND-COLOR IS 2.
          05 CNPJ-INPUT LINE 3 COL 8    FOREGROUND-COLOR IS 7
           FROM WS-CNPJ-ED.
          05 LINE 3 COL 35 VALUE 'MATRIZ/FILIAL: ' FOREGROUND-COLOR IS
               2.
          05 MF-INPUT LINE 3 COL 50 FOREGROUND-COLOR IS 7
           FROM WS-MATRIZ-FILIAL.
          05 LINE 4 COL 2 VALUE 'RAZAO SOCIAL: ' FOREGROUND-COLOR IS 2.
          05 RAZAO-INPUT LINE 4 COL 17 FOREGROUND-COLOR IS 4
           FROM RRE-RAZAO-SOCIAL.
          05 LINE 5 COL 2 VALUE 'NOME FANTASIA: ' FOREGROUND-COLOR IS 2.
          05 NMF-INPUT LINE 5 COL 17 FOREGROUND-COLOR IS 7
           FROM RRE-NOME-FANTASIA.
          05 LINE 6 COL 2 VALUE 'SIT.CADASTRAL: ' FOREGROUND-COLOR IS 2.
          05 SITCAD-INPUT LINE 6 COL 17 FOREGROUND-COLOR IS 7
           FROM WS-SITUACAO-CADASTRAL.
          05 LINE 6 COL 35 VALUE 'DATA SIT.CADASTRAL: ' FOREGROUND-COLOR
           IS 2.
          05 DTSITCAD-INPUT LINE 6 COL 55 FOREGROUND-COLOR IS 7
           FROM WS-DATA-SIT-CADASTRAL.
          05 LINE 7 COL 2 VALUE 'MOTIVO SIT.CADASTRAL: '
           FOREGROUND-COLOR IS 2.
          05 MOTSIT-INPUT LINE 7 COL 23 FOREGROUND-COLOR IS 7
           FROM WS-MOTIVO-SIT-CADASTRAL.
          05 LINE 8 COL 2 VALUE 'NOME CIDADE EXTERIOR: '
           FOREGROUND-COLOR IS 2.
          05 CIDEXT-INPUT LINE 8 COL 24 FOREGROUND-COLOR IS 7
           FROM RRE-NM-CIDADE-EXTERIOR.
          05 LINE 9 COL 2 VALUE 'PAIS: '
           FOREGROUND-COLOR IS 2.
          05 COPAIS-INPUT LINE 9 COL 8 FOREGROUND-COLOR IS 7
           FROM RRE-CO-PAIS.
          05 NMPAIS-INPUT LINE 9 COL 12 FOREGROUND-COLOR IS 7
           FROM RRE-NM-PAIS.
          05 LINE 10 COL 2 VALUE 'NATUREZA JURIDICA: '
           FOREGROUND-COLOR IS 2.
          05 NATJUR-INPUT LINE 10 COL 22 FOREGROUND-COLOR IS 7
           FROM RRE-COD-NAT-JURIDICA.
          05 LINE 11 COL 2 VALUE 'INICIO DA ATIVIDADE: '
           FOREGROUND-COLOR IS 2.
          05 DTINIATIV-INPUT LINE 11 COL 23 FOREGROUND-COLOR IS 7
           FROM WS-DATA-INICIO-ATIVIDADE.
          05 LINE 11 COL 35 VALUE 'CNAE FISCAL: '
           FOREGROUND-COLOR IS 2.
          05 CNAE-INPUT LINE 11 COL 48 FOREGROUND-COLOR IS 7
           FROM RRE-CNAE-FISCAL.
          05 LINE 12 COL 2 VALUE 'TIPO LOGRADOURO: '
           FOREGROUND-COLOR IS 2.
          05 TIPOLOG-INPUT LINE 12 COL 19 FOREGROUND-COLOR IS 7
           FROM RRE-TIPO-LOGRADOURO.
          05 LINE 13 COL 2 VALUE 'LOGRADOURO: ' FOREGROUND-COLOR IS 2.
          05 LOGRA-INPUT LINE 13 COL 13 FOREGROUND-COLOR IS 7
           FROM RRE-LOGRADOURO.
          05 LINE 14 COL 2 VALUE 'No: ' FOREGROUND-COLOR IS 2.
          05 NM-INPUT LINE 14 COL 6 FOREGROUND-COLOR IS 7
           FROM RRE-NUMERO.
          05 LINE 14 COL 13 VALUE 'COMPLEMENTO: ' FOREGROUND-COLOR IS 2.
          05 COMP-INPUT LINE 14 COL 28 FOREGROUND-COLOR IS 7
           FROM WS-COMPLEMENTO.
          05 LINE 15 COL 2 VALUE 'BAIRRO: ' FOREGROUND-COLOR IS 2.
          05 BAIRRO-INPUT LINE 15 COL 10 FOREGROUND-COLOR IS 7
           FROM RRE-BAIRRO.
          05 LINE 15 COL 61 VALUE 'CEP: ' FOREGROUND-COLOR IS 2.
          05 CEP-INPUT LINE 15 COL 66 FOREGROUND-COLOR IS 7
           FROM RRE-CEP.
          05 LINE 16 COL 2 VALUE 'UF: ' FOREGROUND-COLOR IS 2.
          05 UF-INPUT LINE 16 COL 6 FOREGROUND-COLOR IS 7
           FROM RRE-UF.
          05 LINE 16 COL 9 VALUE 'MUNICIPIO: ' FOREGROUND-COLOR IS 2.
          05 CDMUN-INPUT LINE 16 COL 20 FOREGROUND-COLOR IS 7
           FROM RRE-COD-MUNICIPIO.
          05 NMMUN-INPUT LINE 16 COL 25 FOREGROUND-COLOR IS 7
           FROM RRE-MUNICIPIO.
          05 LINE 17 COL 2 VALUE 'TEL1: ' FOREGROUND-COLOR IS 2.
          05 TEL1-INPUT LINE 17 COL 8  FOREGROUND-COLOR IS 7
           FROM RRE-DDD-TELEFONE-1.
          05 LINE 17 COL 21 VALUE 'TEL2: ' FOREGROUND-COLOR IS 2.
          05 TEL2-INPUT LINE 17 COL 27 FOREGROUND-COLOR IS 7
           FROM RRE-DDD-TELEFONE-2.
          05 LINE 17 COL 42 VALUE 'FAX: ' FOREGROUND-COLOR IS 2.
          05 FAX-INPUT LINE 17 COL 47 FOREGROUND-COLOR IS 7
           FROM RRE-DDD-FAX.
          05 LINE 18 COL 2 VALUE 'EMAIL: ' FOREGROUND-COLOR IS 2.
          05 EMAIL-INPUT LINE 18 COL 9 FOREGROUND-COLOR IS 7
           FROM RRE-CORREIO-ELETRONICO.
          05 LINE 19 COL 2 VALUE 'QUALI.RESP.: ' FOREGROUND-COLOR IS 2.
          05 QUALIRESP-INPUT LINE 19 COL 15 FOREGROUND-COLOR IS 7
           FROM RRE-QUALIFICACAO-RESPONSAVEL.
          05 LINE 19 COL 19 VALUE 'CAPITAL SOCIAL: ' FOREGROUND-COLOR IS
               2.
          05 CAPSOCIAL-INPUT LINE 19 COL 35 FOREGROUND-COLOR IS 7
           FROM WS-CAPITAL-SOCIAL.
          05 LINE 20 COL 2 VALUE 'PORTE: ' FOREGROUND-COLOR IS 2.
          05 PORTE-INPUT LINE 20 COL 9 FOREGROUND-COLOR IS 7
           FROM WS-PORTE.
          05 LINE 20 COL 36 VALUE 'OPCAO SIMPLES: ' FOREGROUND-COLOR IS
               2.
          05 OPSIMPLES-INPUT LINE 20 COL 51 FOREGROUND-COLOR IS 7
           FROM WS-OPCAO-SIMPLES.
          05 LINE 21 COL 2 VALUE 'DT OPCAO SIMPLES: '
           FOREGROUND-COLOR IS 2.
          05 DTOPSIMPLES-INPUT LINE 21 COL 20 FOREGROUND-COLOR IS 7
           FROM RRE-DATA-OPCAO-SIMPLES.
          05 LINE 21 COL 31 VALUE 'DT EXCLUSAO SIMPLES: '
           FOREGROUND-COLOR IS 2.
          05 DTEXSIMPLES-INPUT LINE 21 COL 52 FOREGROUND-COLOR IS 7
           FROM RRE-DATA-EXCLUSAO-SIMPLES.
          05 LINE 23 COL 30 VALUE 'OPCAO MEI: '
           FOREGROUND-COLOR IS 2.
          05 OPMEI-INPUT LINE 23 COL 41 FOREGROUND-COLOR IS 7
           FROM RRE-OPCAO-MEI.
          05 LINE 22 COL 2 VALUE 'SIT.ESPECIAL: '
           FOREGROUND-COLOR IS 2.
          05 SITESPECIAL-INPUT LINE 22 COL 16 FOREGROUND-COLOR IS 7
           FROM RRE-SIT-ESPECIAL.
          05 LINE 23 COL 2 VALUE 'DT SIT.ESPECIAL: '
           FOREGROUND-COLOR IS 2.
          05 DTSITESPECIAL-INPUT LINE 23 COL 19 FOREGROUND-COLOR IS 7
           FROM RRE-DATA-SIT-ESPECIAL.          
       
       01 SC-EXIBIR-PF.
          05 VALUE '------------------- *** LISTAR DADOS PESSOA FISICA
      -     ' *** -------------' LINE 1 COL 1 FOREGROUND-COLOR IS 3.
          05 LINE 3 COL 2 VALUE 'CNPJ DA EMPRESA:'
           FOREGROUND-COLOR IS 2.
          05 CNPJ-EMPRESA-INPUT LINE 3 COL 19 FOREGROUND-COLOR IS 7
           FROM WS-CNPJ-ED.
          05 LINE 4 COL 2 VALUE 'NOME DO SOCIO:'
           FOREGROUND-COLOR IS 2.
          05 NOME-SOCIO-INPUT LINE 4 COL 17 FOREGROUND-COLOR IS 4
           FROM RRS-NOME-SOCIO.
          05 LINE 6 COL 2 VALUE 'CPF DO SOCIO:'
           FOREGROUND-COLOR IS 2.
          05 CPF-SOCIO-INPUT LINE 6 COL 16 FOREGROUND-COLOR IS 7
           FROM RRS-CPF-SOCIO.
          05 LINE 7 COL 2 VALUE 'QUALIFICACAO:'
           FOREGROUND-COLOR IS 2.
          05 QUALIF-SOCIO-INPUT LINE 7 COL 16 FOREGROUND-COLOR IS 7
           FROM RRS-COD-QUALIFICACAO-SOCIO.
          05 LINE 8 COL 2 VALUE 'PERCENTUAL DE CAPITAL:'
           FOREGROUND-COLOR IS 2.
          05 PERC-CAPITAL-INPUT LINE 8 COL 25 FOREGROUND-COLOR IS 7
           FROM RRS-PERCENTUAL-CAPITAL.
          05 LINE 9 COL 2 VALUE 'DATA ENTRADA SOCIEDADE:'
           FOREGROUND-COLOR IS 2.
          05 DT-ENT-SOC-INPUT LINE 9 COL 26 FOREGROUND-COLOR IS 7
           FROM RRS-DATA-ENTRADA-SOCIEDADE.
          05 LINE 10 COL 2 VALUE 'CODIGO PAIS:'
           FOREGROUND-COLOR IS 2.
          05 CD-PAIS-INPUT LINE 10 COL 15 FOREGROUND-COLOR IS 7
           FROM RRS-CODIGO-PAIS.
          05 LINE 11 COL 2 VALUE 'NOME PAIS:'
           FOREGROUND-COLOR IS 2.
          05 NM-PAIS-INPUT LINE 11 COL 13 FOREGROUND-COLOR IS 7
           FROM RRS-NOME-PAIS-SOCIO.
          05 LINE 12 COL 2 VALUE 'CPF REPRESENTANTE:'
           FOREGROUND-COLOR IS 2.
          05 CPF-REPRESENTANTE-INPUT LINE 12 COL 21
           FOREGROUND-COLOR IS 7 FROM RRS-CPF-REPRESENTANTE.
          05 LINE 13 COL 2 VALUE 'NOME REPRESENTANTE:'
           FOREGROUND-COLOR IS 2.
          05 NM-REPRESENTANTE-INPUT LINE 13 COL 22
           FOREGROUND-COLOR IS 7 FROM RRS-NOME-REPRESENTANTE.
          05 LINE 14 COL 2 VALUE 'COD. QUALIFIC. REPRESENTANTE:'
           FOREGROUND-COLOR IS 2.
          05 CD-QUALIF-REP-INPUT LINE 14 COL 32
            FOREGROUND-COLOR IS 7 FROM RRS-COD-QUALIFICACAO-REP.

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
                              PERFORM 0020-MENU-BUSCAR-PF
                     WHEN 'S'
                             MOVE ' ' TO WS-MENSAGEM
                     WHEN OTHER
                             DISPLAY SC-ERRO-COMANDO
             END-EVALUATE
           END-PERFORM.
           STOP RUN.

       0002-MENU-BUSCAR-PJ.
           INITIALIZE RRE-CNPJ.
           INITIALIZE RRE-RAZAO-SOCIAL.
           DISPLAY SC-TELA-LIMPA.

           DISPLAY SC-BUSCAR-PJ
           ACCEPT SC-BUSCAR-PJ

           IF RRE-CNPJ IS NOT EQUAL TO ZEROES THEN
              PERFORM 0004-BUSCAR-PJ-CNPJ
           ELSE
             IF RRE-RAZAO-SOCIAL IS NOT EQUAL TO SPACES THEN
               PERFORM 0005-BUSCAR-PJ-RAZAO
             END-IF
           END-IF.
           DISPLAY SC-TELA-LIMPA.

       0020-MENU-BUSCAR-PF.
           INITIALIZE RRS-CNPJ.
           INITIALIZE RRS-NOME-SOCIO.
           DISPLAY SC-TELA-LIMPA.

           DISPLAY SC-BUSCAR-PF
           ACCEPT SC-BUSCAR-PF

           IF RRS-CNPJ IS NOT EQUAL TO ZEROES THEN
              PERFORM 0040-BUSCAR-PF-CNPJ
           ELSE
             IF RRS-NOME-SOCIO IS NOT EQUAL TO SPACES THEN
               PERFORM 0050-BUSCAR-PF-NOME
             END-IF
           END-IF.
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
           PERFORM 0006-BUSCAR-MOTIVO-SIT-CADASTRAL.
           MOVE CORRESPONDING RRE-DATA-INICIO-ATIVIDADE TO
           WS-DATA-INICIO-ATIVIDADE.
           MOVE RRE-COMPLEMENTO TO WS-COMPLEMENTO.
           EVALUATE RRE-PORTE-EMPRESA
                   WHEN '00'
                           MOVE 'NAO INFORMADO' TO WS-PORTE
                   WHEN '01'
                           MOVE 'MICRO EMPRESA' TO WS-PORTE
                   WHEN '03'
                           MOVE 'EMPRESA DE PEQUENO PORTE'
                           TO WS-PORTE
                   WHEN '05'
                           MOVE 'DEMAIS' TO WS-PORTE
                   WHEN OTHER
                           INITIALIZE WS-PORTE
           END-EVALUATE.
           EVALUATE RRE-OPCAO-SIMPLES
                   WHEN ' '
                           MOVE 'NAO OPTANTE' TO WS-OPCAO-SIMPLES
                   WHEN '0'
                           MOVE 'NAO OPTANTE' TO WS-OPCAO-SIMPLES
                   WHEN '5'
                           MOVE 'OPTANTE' TO WS-OPCAO-SIMPLES
                   WHEN '7'
                           MOVE 'OPTANTE' TO WS-OPCAO-SIMPLES
                   WHEN '6'
                           MOVE 'EXCLUIDO DO SIMPLES' TO
                           WS-OPCAO-SIMPLES
                    WHEN '8'
                           MOVE 'EXCLUIDO DO SIMPLES' TO
                           WS-OPCAO-SIMPLES
                    WHEN OTHER
                            INITIALIZE WS-OPCAO-SIMPLES
           END-EVALUATE.
           MOVE RRE-CAPITAL-SOCIAL-EMP TO WS-CAPITAL-SOCIAL.
      **********************************************************
           DISPLAY SC-TELA-LIMPA.
           DISPLAY SC-EXIBIR-PJ.
           DISPLAY SC-REGUA-COMANDO.
           IF WS-CONTINUA-BUSCA IS EQUAL TO 'Y' THEN
             MOVE '    *** PRESSIONE ENTER PARA CONTINUAR. V-VOLTAR ***'
               TO WS-MENSAGEM
           ELSE
             MOVE '        *** FIM DA BUSCA, PRESSIONE ENTER PARA VOLTAR
      -       '***' TO WS-MENSAGEM
           END-IF
           IF ST-ARQUIVO-EMP IS NOT EQUAL TO '00' THEN
                   MOVE ST-ARQUIVO-EMP TO WS-MENSAGEM
           END-IF.
           DISPLAY SC-MENSAGEM.
           
           MOVE ' ' TO WS-COMANDO.
           ACCEPT SC-REGUA-COMANDO.
           INITIALIZE REGISTRO-RFB-EMPRESA.

       0030-EXIBIR-PF.
      **************TRATA OS DADOS APLICANDO MASCARAS************
           MOVE CORRESPONDING RRS-CNPJ TO WS-CNPJ-ED.
      ***********************************************************   
           DISPLAY SC-TELA-LIMPA.
           DISPLAY SC-EXIBIR-PF.
           DISPLAY SC-REGUA-COMANDO.
           IF WS-CONTINUA-BUSCA IS EQUAL TO 'Y' THEN
             MOVE '    *** PRESSIONE ENTER PARA CONTINUAR. V-VOLTAR ***'
               TO WS-MENSAGEM
           ELSE
             MOVE '        *** FIM DA BUSCA, PRESSIONE ENTER PARA VOLTAR
      -       '***' TO WS-MENSAGEM
           END-IF
           IF ST-ARQUIVO-SOC IS NOT EQUAL TO '00' THEN
                   MOVE ST-ARQUIVO-SOC TO WS-MENSAGEM
           END-IF.
           DISPLAY SC-MENSAGEM.
           
           MOVE ' ' TO WS-COMANDO.
           ACCEPT SC-REGUA-COMANDO.
           INITIALIZE REGISTRO-RFB-SOCIO.

       0004-BUSCAR-PJ-CNPJ.
           OPEN INPUT ARQ-EMPRESAS.
           MOVE RRE-CNPJ TO FRE-CNPJ-ID.
           INITIALIZE REGISTRO-RFB-EMPRESA.
           MOVE 'N' TO WS-CONTINUA-BUSCA.
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
       
       0040-BUSCAR-PF-CNPJ.
           OPEN INPUT ARQ-SOCIOS.
           MOVE RRS-CNPJ TO WS-CNPJ-BUSCA.
           MOVE WS-CNPJ-BUSCA TO FRS-CNPJ.
           INITIALIZE REGISTRO-RFB-SOCIO.

           START ARQ-SOCIOS
             KEY IS EQUAL TO FRS-CNPJ
             INVALID KEY PERFORM
               MOVE 'N' TO WS-CONTINUA-BUSCA
               MOVE '           *** NAO ENCONTRADO ***' TO
                 RRS-NOME-SOCIO
               MOVE '00' TO ST-ARQUIVO-SOC
               PERFORM 0030-EXIBIR-PF
             END-PERFORM
             NOT INVALID KEY MOVE 'Y' TO WS-CONTINUA-BUSCA
           END-START.

           PERFORM UNTIL WS-CONTINUA-BUSCA IS EQUAL TO 'N'
             READ ARQ-SOCIOS
               AT END PERFORM
                 MOVE 'N' TO WS-CONTINUA-BUSCA
                 PERFORM 0030-EXIBIR-PF
               END-PERFORM
               NOT AT END PERFORM
                 IF WS-CNPJ-BUSCA IS EQUAL TO FRS-CNPJ THEN
                   MOVE FD-REG-SOCIO TO REGISTRO-RFB-SOCIO
                 ELSE
                   MOVE 'N' TO WS-CONTINUA-BUSCA
                 END-IF
                 PERFORM 0030-EXIBIR-PF
                 IF WS-COMANDO IS EQUAL TO 'V' MOVE 'N' TO
                         WS-CONTINUA-BUSCA
               END-PERFORM
             END-READ
           END-PERFORM.
           CLOSE ARQ-SOCIOS.
           MOVE SPACES TO WS-COMANDO.
       
       0005-BUSCAR-PJ-RAZAO.
           OPEN INPUT ARQ-EMPRESAS.
           MOVE FUNCTION UPPER-CASE(RRE-RAZAO-SOCIAL)
             TO WS-RAZAO-SOCIAL-BUSCA.
           MOVE WS-RAZAO-SOCIAL-BUSCA TO FRE-RAZAO-SOCIAL.
           INITIALIZE REGISTRO-RFB-EMPRESA.

           START ARQ-EMPRESAS
             KEY IS EQUAL TO FRE-RAZAO-SOCIAL
             INVALID KEY PERFORM
               MOVE 'N' TO WS-CONTINUA-BUSCA
               MOVE '           *** NAO ENCONTRADO ***' TO
                 RRE-RAZAO-SOCIAL
               MOVE '00' TO ST-ARQUIVO-EMP
               PERFORM 0003-EXIBIR-PJ
             END-PERFORM
             NOT INVALID KEY MOVE 'Y' TO WS-CONTINUA-BUSCA
           END-START.

           PERFORM UNTIL WS-CONTINUA-BUSCA IS EQUAL TO 'N'
             READ ARQ-EMPRESAS
               AT END PERFORM
                 MOVE 'N' TO WS-CONTINUA-BUSCA
                 PERFORM 0003-EXIBIR-PJ      
               END-PERFORM
               NOT AT END PERFORM
                 IF WS-RAZAO-SOCIAL-BUSCA IS EQUAL TO FRE-RAZAO-SOCIAL
                 THEN
                   MOVE FD-REG-EMPRESA TO REGISTRO-RFB-EMPRESA
                 ELSE
                   MOVE 'N' TO WS-CONTINUA-BUSCA
                 END-IF
                 PERFORM 0003-EXIBIR-PJ
                 IF WS-COMANDO IS EQUAL TO 'V' MOVE 'N' TO
                         WS-CONTINUA-BUSCA
               END-PERFORM
             END-READ
           END-PERFORM.
           CLOSE ARQ-EMPRESAS.
           MOVE SPACES TO WS-COMANDO.
       
       0050-BUSCAR-PF-NOME.
           OPEN INPUT ARQ-SOCIOS.
           MOVE FUNCTION UPPER-CASE(RRS-NOME-SOCIO)
             TO WS-NOME-SOCIO-BUSCA.
           MOVE WS-NOME-SOCIO-BUSCA TO FRS-NOME-SOCIO.
           INITIALIZE REGISTRO-RFB-SOCIO.

           START ARQ-SOCIOS
             KEY IS EQUAL TO FRS-NOME-SOCIO
             INVALID KEY PERFORM
               MOVE 'N' TO WS-CONTINUA-BUSCA
               MOVE '           *** NAO ENCONTRADO ***' TO
                 RRS-NOME-SOCIO
               MOVE '00' TO ST-ARQUIVO-SOC
               PERFORM 0030-EXIBIR-PF
             END-PERFORM
             NOT INVALID KEY MOVE 'Y' TO WS-CONTINUA-BUSCA
           END-START.

           PERFORM UNTIL WS-CONTINUA-BUSCA IS EQUAL TO 'N'
             READ ARQ-SOCIOS
               AT END PERFORM
                 MOVE 'N' TO WS-CONTINUA-BUSCA
                 PERFORM 0030-EXIBIR-PF
               END-PERFORM
               NOT AT END PERFORM
                 IF WS-NOME-SOCIO-BUSCA IS EQUAL TO FRS-NOME-SOCIO THEN
                   MOVE FD-REG-SOCIO TO REGISTRO-RFB-SOCIO
                 ELSE
                   MOVE 'N' TO WS-CONTINUA-BUSCA
                 END-IF
                 PERFORM 0030-EXIBIR-PF
                 IF WS-COMANDO IS EQUAL TO 'V' MOVE 'N' TO
                         WS-CONTINUA-BUSCA
               END-PERFORM
             END-READ
           END-PERFORM.
           CLOSE ARQ-SOCIOS.
           MOVE SPACES TO WS-COMANDO.

       0006-BUSCAR-MOTIVO-SIT-CADASTRAL.
           INITIALIZE WS-MOTIVO-SIT-CADASTRAL.
           INITIALIZE WS-EOF.
           OPEN INPUT ARQ-MOT-SIT-CAD.
           PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
             READ ARQ-MOT-SIT-CAD
                     AT END MOVE 'Y' TO WS-EOF
                     NOT AT END PERFORM
                        IF FD-MOT-COD IS EQUAL TO
                                FRE-MOTIVO-SIT-CADASTRAL THEN
                          MOVE 'Y' TO WS-EOF
                          MOVE FD-MOT-DESCRICAO TO
                            WS-MOTIVO-SIT-CADASTRAL
                        END-IF
                     END-PERFORM
           END-PERFORM.
           CLOSE ARQ-MOT-SIT-CAD.

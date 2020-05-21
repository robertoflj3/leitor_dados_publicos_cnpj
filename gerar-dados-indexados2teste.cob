       IDENTIFICATION DIVISION.
       PROGRAM-ID. GERAR-DADOS-INDEXADOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-SOCIOS ASSIGN TO "dados/socios2teste.dat"
                   ORGANIZATION INDEXED
                   ACCESS MODE DYNAMIC
                   RECORD KEY IS FRS-COD-SOCIO
                   ALTERNATE RECORD KEY IS
                     FRS-NOME-SOCIO WITH DUPLICATES
                   ALTERNATE RECORD KEY IS
                     FRS-CNPJ WITH DUPLICATES
                   STATUS ST-ARQUIVO-SOC.

           SELECT ARQ-EMPRESAS ASSIGN TO "dados/empresas2teste.dat"
                   ORGANIZATION INDEXED
                   ACCESS MODE DYNAMIC
                   RECORD KEY IS FRE-CNPJ
                   ALTERNATE KEY IS FRE-RAZAO-SOCIAL WITH DUPLICATES
                   STATUS ST-ARQUIVO-EMP.

           SELECT ARQ-CNAE ASSIGN TO "dados/cnae2teste.dat"
                   ORGANIZATION INDEXED
                   ACCESS MODE DYNAMIC
                   RECORD KEY IS FRC-CNPJ
                   ALTERNATE RECORD KEY IS
                     FRC-CNAE-SECUNDARIA WITH DUPLICATES
                   STATUS ST-ARQUIVO-CNAE.

           SELECT ARQ1 ASSIGN TO "dados/K3241.K03200DV.D00124.L00001"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ2 ASSIGN TO "dados/K3241.K03200DV.D00124.L00002"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ3 ASSIGN TO "dados/K3241.K03200DV.D00124.L00003"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ4 ASSIGN TO "dados/K3241.K03200DV.D00124.L00004"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ5 ASSIGN TO "dados/K3241.K03200DV.D00124.L00005"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ6 ASSIGN TO "dados/K3241.K03200DV.D00124.L00006"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ7 ASSIGN TO "dados/K3241.K03200DV.D00124.L00007"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ8 ASSIGN TO "dados/K3241.K03200DV.D00124.L00008"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ9 ASSIGN TO "dados/K3241.K03200DV.D00124.L00009"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ10 ASSIGN TO "dados/K3241.K03200DV.D00124.L00010"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ11 ASSIGN TO "dados/K3241.K03200DV.D00124.L00011"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ12 ASSIGN TO "dados/K3241.K03200DV.D00124.L00012"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ13 ASSIGN TO "dados/K3241.K03200DV.D00124.L00013"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ14 ASSIGN TO "dados/K3241.K03200DV.D00124.L00014"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ15 ASSIGN TO "dados/K3241.K03200DV.D00124.L00015"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ16 ASSIGN TO "dados/K3241.K03200DV.D00124.L00016"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ17 ASSIGN TO "dados/K3241.K03200DV.D00124.L00017"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ18 ASSIGN TO "dados/K3241.K03200DV.D00124.L00018"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ19 ASSIGN TO "dados/K3241.K03200DV.D00124.L00019"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.
           SELECT ARQ20 ASSIGN TO "dados/K3241.K03200DV.D00124.L00020"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-SOCIOS.
       COPY "FD-REG-SOCIO.cpy".

       FD ARQ-EMPRESAS.
       COPY "FD-REG-EMPRESA.cpy".
        	   
       FD ARQ-CNAE.
       COPY "FD-REG-CNAE.cpy".

       FD ARQ1.
       01 FD-REG-ARQ1.
               03 FILLER                  PIC X(1200).
       FD ARQ2.
       01 FD-REG-ARQ2.
               03 FILLER                  PIC X(1200).
       FD ARQ3.
       01 FD-REG-ARQ3.
               03 FILLER                  PIC X(1200).
       FD ARQ4.
       01 FD-REG-ARQ4.
               03 FILLER                  PIC X(1200).
       FD ARQ5.
       01 FD-REG-ARQ5.
               03 FILLER                  PIC X(1200).
       FD ARQ6.
       01 FD-REG-ARQ6.
               03 FILLER                  PIC X(1200).
       FD ARQ7.
       01 FD-REG-ARQ7.
               03 FILLER                  PIC X(1200).
       FD ARQ8.
       01 FD-REG-ARQ8.
               03 FILLER                  PIC X(1200).
       FD ARQ9.
       01 FD-REG-ARQ9.
               03 FILLER                  PIC X(1200).
       FD ARQ10.
       01 FD-REG-ARQ10.
               03 FILLER                  PIC X(1200).
       FD ARQ11.
       01 FD-REG-ARQ11.
               03 FILLER                  PIC X(1200).
       FD ARQ12.
       01 FD-REG-ARQ12.
               03 FILLER                  PIC X(1200).
       FD ARQ13.
       01 FD-REG-ARQ13.
               03 FILLER                  PIC X(1200).
       FD ARQ14.
       01 FD-REG-ARQ14.
               03 FILLER                  PIC X(1200).
       FD ARQ15.
       01 FD-REG-ARQ15.
               03 FILLER                  PIC X(1200).
       FD ARQ16.
       01 FD-REG-ARQ16.
               03 FILLER                  PIC X(1200).
       FD ARQ17.
       01 FD-REG-ARQ17.
               03 FILLER                  PIC X(1200).
       FD ARQ18.
       01 FD-REG-ARQ18.
               03 FILLER                  PIC X(1200).
       FD ARQ19.
       01 FD-REG-ARQ19.
               03 FILLER                  PIC X(1200).
       FD ARQ20.
       01 FD-REG-ARQ20.
               03 FILLER                  PIC X(1200).

       WORKING-STORAGE SECTION.

       01 ST-ARQUIVO-SOC              PIC XX.
       01 ST-ARQUIVO-LOTE             PIC XX.
       01 ST-ARQUIVO-EMP              PIC XX.
       01 ST-ARQUIVO-CNAE             PIC XX.
       01 WS-EOF                      PIC X VALUE 'N'.
       01 WS-COD-SOCIO                PIC 9(011) VALUE 1.
       01 WS-PASSO-ARQUIVO            PIC 9 VALUE 0.

       COPY "REGISTRO-RFB.cpy".
       COPY "REGISTRO-RFB-SOCIO.cpy".
       COPY "REGISTRO-RFB-EMPRESA.cpy".
       COPY "REGISTRO-RFB-CNAE.cpy".

       PROCEDURE DIVISION.
       0001-MAIN-PARA.
           DISPLAY 'INICIANDO A LEITURA DOS ARQUIVOS ...'.
           PERFORM 0002-ABRIR-ARQUIVO.
           GOBACK.
       0002-ABRIR-ARQUIVO.
           PERFORM 2 TIMES
             ADD 1 TO WS-PASSO-ARQUIVO GIVING WS-PASSO-ARQUIVO

             IF WS-PASSO-ARQUIVO IS EQUAL TO 1 THEN
                     DISPLAY 'EXECUTANDO PASSO 1/2 - EMPRESAS'
                     OPEN OUTPUT ARQ-EMPRESAS
             END-IF
             IF WS-PASSO-ARQUIVO IS EQUAL TO 2 THEN
                     DISPLAY 'EXECUTANDO PASSO 2/2 - SOCIOS'
                     OPEN OUTPUT ARQ-SOCIOS
             END-IF

             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00001 ...'
             OPEN INPUT ARQ1
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ1
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ1 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ1
             MOVE 'N' TO WS-EOF

             IF WS-PASSO-ARQUIVO IS EQUAL TO 1 THEN
                     CLOSE ARQ-EMPRESAS
             END-IF
             IF WS-PASSO-ARQUIVO IS EQUAL TO 2 THEN
                     CLOSE ARQ-SOCIOS
             END-IF
           END-PERFORM
           DISPLAY 'FIM DA GERACAO DOS ARQUIVOS.'.
       0003-GRAVAR-REGISTRO.
           IF RRF-TIPO-REGISTRO IS EQUAL TO 2 AND WS-PASSO-ARQUIVO IS
                   EQUAL TO 2 THEN
                   MOVE REGISTRO-RFB TO REGISTRO-RFB-SOCIO
                   PERFORM 0004-GRAVAR-REGISTRO-SOCIO
           END-IF.
           IF RRF-TIPO-REGISTRO IS EQUAL TO 1 AND WS-PASSO-ARQUIVO IS
                   EQUAL TO 1 THEN
                   MOVE REGISTRO-RFB TO REGISTRO-RFB-EMPRESA
                   PERFORM 0005-GRAVAR-REGISTRO-EMPRESA
           END-IF.
           IF RRF-TIPO-REGISTRO IS EQUAL TO 6 AND WS-PASSO-ARQUIVO IS
                   EQUAL TO 3 THEN
                   MOVE REGISTRO-RFB TO REGISTRO-RFB-CNAE
                   PERFORM 0006-GRAVAR-REGISTRO-CNAE
           END-IF.
       0004-GRAVAR-REGISTRO-SOCIO.
           MOVE WS-COD-SOCIO TO RRS-COD-SOCIO.
           MOVE REGISTRO-RFB-SOCIO TO FD-REG-SOCIO.
           WRITE FD-REG-SOCIO.
           ADD 1 WS-COD-SOCIO GIVING WS-COD-SOCIO.
       0005-GRAVAR-REGISTRO-EMPRESA.
           MOVE REGISTRO-RFB-EMPRESA TO FD-REG-EMPRESA.
           WRITE FD-REG-EMPRESA.
       0006-GRAVAR-REGISTRO-CNAE.
           MOVE REGISTRO-RFB-CNAE TO FD-REG-CNAE.
           WRITE FD-REG-CNAE.

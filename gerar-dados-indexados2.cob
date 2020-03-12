       IDENTIFICATION DIVISION.
       PROGRAM-ID. GERAR-DADOS-INDEXADOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-SOCIOS ASSIGN TO "dados/socios2.dat"
                   ORGANIZATION INDEXED
                   ACCESS MODE DYNAMIC
                   RECORD KEY IS FRS-COD-SOCIO
                   ALTERNATE RECORD KEY IS
                     FRS-NOME-SOCIO WITH DUPLICATES
                   ALTERNATE RECORD KEY IS
                     FRS-CNPJ WITH DUPLICATES
                   STATUS ST-ARQUIVO-SOC.

           SELECT ARQ-EMPRESAS ASSIGN TO "dados/empresas2.dat"
                   ORGANIZATION INDEXED
                   ACCESS MODE DYNAMIC
                   RECORD KEY IS FRE-CNPJ
                   ALTERNATE KEY IS FRE-RAZAO-SOCIAL WITH DUPLICATES
                   STATUS ST-ARQUIVO-EMP.

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
       01 WS-EOF                      PIC X VALUE 'N'.
       01 WS-COD-SOCIO                PIC 9(011) VALUE 1.    
       01 WS-PASSO-ARQUIVO            PIC 9 VALUE 0.

       COPY "REGISTRO-RFB.cpy".
       COPY "REGISTRO-RFB-SOCIO.cpy".
       COPY "REGISTRO-RFB-EMPRESA.cpy".

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
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00002 ...'
             OPEN INPUT ARQ2
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ2
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ2 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ2
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00003 ...'
             OPEN INPUT ARQ3
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ3
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ3 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ3
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00004 ...'
             OPEN INPUT ARQ4
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ4
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ4 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ4
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00005 ...'
             OPEN INPUT ARQ5
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ5
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ5 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ5
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00006 ...'
             OPEN INPUT ARQ6
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ6
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ6 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ6
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00007 ...'
             OPEN INPUT ARQ7
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ7
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ7 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ7
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00008 ...'
             OPEN INPUT ARQ8
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ8
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ8 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ8
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00009 ...'
             OPEN INPUT ARQ9
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ9
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ9 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ9
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00010 ...'
             OPEN INPUT ARQ10
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ10
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ10 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ10
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00011 ...'
             OPEN INPUT ARQ11
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ11
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ11 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ11
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00012 ...'
             OPEN INPUT ARQ12
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ12
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ12 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ12
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00013 ...'
             OPEN INPUT ARQ13
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ13
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ13 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ13
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00014 ...'
             OPEN INPUT ARQ14
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ14
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ14 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ14
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00015 ...'
             OPEN INPUT ARQ15
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ15
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ15 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ15
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00016 ...'
             OPEN INPUT ARQ16
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ16
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ16 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ16
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00017 ...'
             OPEN INPUT ARQ17
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ17
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ17 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ17
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00018 ...'
             OPEN INPUT ARQ18
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ18
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ18 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ18
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00019 ...'
             OPEN INPUT ARQ19
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ19
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ19 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ19
             MOVE 'N' TO WS-EOF
             DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00020 ...'
             OPEN INPUT ARQ20
             PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ20
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM
                      MOVE FD-REG-ARQ20 TO REGISTRO-RFB
                      PERFORM 0003-GRAVAR-REGISTRO
                   END-PERFORM
             END-PERFORM
             CLOSE ARQ20

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
       0004-GRAVAR-REGISTRO-SOCIO.
           MOVE WS-COD-SOCIO TO RRS-COD-SOCIO.
           MOVE REGISTRO-RFB-SOCIO TO FD-REG-SOCIO.
           WRITE FD-REG-SOCIO.
           ADD 1 WS-COD-SOCIO GIVING WS-COD-SOCIO.
       0005-GRAVAR-REGISTRO-EMPRESA.
           MOVE REGISTRO-RFB-EMPRESA TO FD-REG-EMPRESA.
           WRITE FD-REG-EMPRESA.

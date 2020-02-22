       IDENTIFICATION DIVISION.
       PROGRAM-ID. GERAR-DADOS-INDEXADOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-SOCIOS ASSIGN TO "dados/socios.dat"
                   ORGANIZATION INDEXED
                   ACCESS MODE DYNAMIC
                   RECORD KEY IS FD-CHAVE-SOCIO
                   ALTERNATE KEY IS FD-CPF-SOCIO WITH DUPLICATES
                   STATUS ST-ARQUIVO.

           SELECT ARQ1 ASSIGN TO "dados/K3241.K03200DV.D00124.L00001"
                   ORGANIZATION LINE SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   STATUS ST-ARQUIVO-LOTE.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-SOCIOS.
       01 FD-REG-SOCIO.
               03 FILLER              PIC X(003).
               03 FD-CHAVE-SOCIO.
                   05 FD-CNPJ-SOCIO   PIC 9(014).
                   05 FD-IDENT-SOCIO  PIC 9.
                   05 FD-NOME-SOCIO   PIC X(150).
               03 FD-CPF-SOCIO        PIC 9(014).
               03 FILLER              PIC X(1018).
       FD ARQ1.
       01 FD-REG-ARQ1.
               03 FD-TIPO-REGISTRO        PIC 9.
               03 FILLER                  PIC X(1199).

       WORKING-STORAGE SECTION.

       01 ST-ARQUIVO                  PIC XX.
       01 ST-ARQUIVO-LOTE             PIC XX.
       01 WS-EOF                      PIC X VALUE 'N'.

           COPY "REGISTRO-RFB.cpy".
           COPY "REGISTRO-RFB-SOCIO.cpy".

       PROCEDURE DIVISION.
       0001-MAIN-PARA.
           DISPLAY 'INICIANDO A LEITURA DOS ARQUIVOS ...'.
           PERFORM 0002-ABRIR-ARQUIVO.
           GOBACK.
       0002-ABRIR-ARQUIVO.
           DISPLAY 'LENDO ARQUIVO K3241.K03200DV.D00124.L00001 ...'
           OPEN INPUT ARQ1.
           OPEN OUTPUT ARQ-SOCIOS.
           PERFORM UNTIL WS-EOF IS EQUAL TO 'Y'
                   READ ARQ1
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM 0003-GRAVAR-REGISTRO
           END-PERFORM.
           CLOSE ARQ1.
           CLOSE ARQ-SOCIOS.
       0003-GRAVAR-REGISTRO.
           IF FD-TIPO-REGISTRO IS EQUAL TO 2 THEN
                   MOVE FD-REG-ARQ1 TO REGISTRO-RFB-SOCIO
                   PERFORM 0004-GRAVAR-REGISTRO-SOCIO
           END-IF.
       0004-GRAVAR-REGISTRO-SOCIO.
           MOVE REGISTRO-RFB-SOCIO TO FD-REG-SOCIO.
           WRITE FD-REG-SOCIO.

       01 REGISTRO-RFB-SOCIO.
           03 RRS-TIPO-REGISTRO          PIC 9.
           03 RRS-IND-FULL-DIARIO        PIC X.
           03 RRS-TIPO-ATUALIZACAO       PIC X.
           03 RRS-CNPJ.
              05 CNPJ1                   PIC 9(002).
              05 CNPJ2                   PIC 9(003).
              05 CNPJ3                   PIC 9(003).
              05 CNPJ4                   PIC 9(004).
              05 CNPJ5                   PIC 9(002).
           03 RRS-IDENTIFICADOR-SOCIO    PIC 9.
           03 RRS-NOME-SOCIO             PIC X(150).
           03 RRS-CPF-SOCIO              PIC 9(014).
           03 RRS-COD-QUALIFICACAO-SOCIO PIC XX.
           03 RRS-PERCENTUAL-CAPITAL     PIC 9(005).
           03 RRS-DATA-ENTRADA-SOCIEDADE PIC 9(008).
           03 RRS-CODIGO-PAIS            PIC XXX.
           03 RRS-NOME-PAIS-SOCIO        PIC X(070).
           03 RRS-CPF-REPRESENTANTE      PIC 9(011).
           03 RRS-NOME-REPRESENTANTE     PIC X(060).
           03 RRS-COD-QUALIFICACAO-REP   PIC XX.
           03 RRS-COD-SOCIO              PIC 9(011).
           03 FILLER                     PIC X(844).
           03 RRS-FIM-REGISTRO           PIC X.


       01 REGISTRO-RFB-CNAE.
           03 RRC-TIPO-REGISTRO          PIC 9.
           03 RRC-INDICADOR-FULL-DIARIO  PIC X.
           03 RRC-TIPO-ATUALIZACAO       PIC X.
           03 RRC-CNPJ.
              05 CNPJ1                   PIC 9(002).
              05 CNPJ2                   PIC 9(003).
              05 CNPJ3                   PIC 9(003).
              05 CNPJ4                   PIC 9(004).
              05 CNPJ5                   PIC 9(002).
           03 RRC-CNAE-SECUNDARIA        PIC 9(007) OCCURS 99 TIMES.
           03 FILLER                     PIC X(489).
           03 RRC-FIM-REGISTRO           PIC X.
   

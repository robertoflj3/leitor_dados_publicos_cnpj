       01 FD-REG-CNAE.
           03 FRC-TIPO-REGISTRO          PIC 9.
           03 FRC-INDICADOR-FULL-DIARIO  PIC X.
           03 FRC-TIPO-ATUALIZACAO       PIC X.
		   03 FRC-CNPJ                   PIC 9(014).
           03 FRC-CNAE-SECUNDARIA        PIC 9(007) OCCURS 99 TIMES.
		   03 FILLER                     PIC X(489).
		   03 FRC-FIM-REGISTRO           PIC X.
		   
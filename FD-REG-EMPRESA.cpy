       01 FD-REG-EMPRESA.
           03 FRE-TIPO-REGISTRO          PIC 9.
           03 FRE-INDICADOR-FULL-DIARIO  PIC X.
           03 FRE-TIPO-ATUALIZACAO       PIC X.
           03 FRE-CNPJ-ID.
              05 FRE-CNPJ                   PIC 9(014).
           03 FRE-IND-MATRIZ-FILIAL      PIC 9.
           03 FRE-RAZAO-SOCIAL           PIC X(150).
           03 FRE-NOME-FANTASIA          PIC X(055).
           03 FRE-SIT-CADASTRAL          PIC 99.
           03 FRE-DATA-SIT-CADASTRAL     PIC 9(008).
           03 FRE-MOTIVO-SIT-CADASTRAL   PIC 99.
           03 FRE-NM-CIDADE-EXTERIOR     PIC X(055).
           03 FRE-CO-PAIS                PIC XXX.
           03 FRE-NM-PAIS                PIC X(070).
           03 FRE-COD-NAT-JURIDICA       PIC 9999.
           03 FRE-DATA-INICIO-ATIVIDADE  PIC 9(008).
           03 FRE-CNAE-FISCAL            PIC 9(007).
           03 FRE-TIPO-LOGRADOURO        PIC X(020).
           03 FRE-LOGRADOURO             PIC X(060).
           03 FRE-NUMERO                 PIC X(006).
           03 FRE-COMPLEMENTO            PIC X(156).
           03 FRE-BAIRRO                 PIC X(050).
           03 FRE-CEP                    PIC 9(008).
           03 FRE-UF                     PIC X(002).
           03 FRE-COD-MUNICIPIO          PIC 9999.
           03 FRE-MUNICIPIO              PIC X(050).
           03 FRE-DDD-TELEFONE-1.
              05 FRE-DDD-1               PIC X(04).
              05 FRE-TELEFONE-1          PIC X(08).
           03 FRE-DDD-TELEFONE-2.
              05 FRE-DDD-2               PIC X(04).
              05 FRE-TELEFONE-2          PIC X(08).
           03 FRE-DDD-FAX.
              05 FRE-NU-DDD-FAX          PIC X(04).
              05 FRE-NU-FAX              PIC X(08).
           03 FRE-CORREIO-ELETRONICO     PIC X(115).
           03 FRE-QUALIFICACAO-RESPONSAVEL PIC 99.
           03 FRE-CAPITAL-SOCIAL-EMP     PIC 9(12)V99.
           03 FRE-PORTE-EMPRESA          PIC XX.
           03 FRE-OPCAO-SIMPLES          PIC X.
           03 FRE-DATA-OPCAO-SIMPLES     PIC 9(08).
           03 FRE-DATA-EXCLUSAO-SIMPLES  PIC 9(08).
           03 FRE-OPCAO-MEI              PIC X.
           03 FRE-SIT-ESPECIAL           PIC X(23).
           03 FRE-DATA-SIT-ESPECIAL      PIC 9(08).
           03 FILLER                     PIC X(243).
           03 FRE-FIM-REGISTRO           PIC X.

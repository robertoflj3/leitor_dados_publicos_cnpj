       01 REGISTRO-RFB-EMPRESA.
           03 RRE-TIPO-REGISTRO          PIC 9.
           03 RRE-INDICADOR-FULL-DIARIO  PIC X.
           03 RRE-TIPO-ATUALIZACAO       PIC X.
           03 RRE-CNPJ.
              05 CNPJ1                   PIC 9(002).
              05 CNPJ2                   PIC 9(003).
              05 CNPJ3                   PIC 9(003).
              05 CNPJ4                   PIC 9(004).
              05 CNPJ5                   PIC 9(002).
           03 RRE-IND-MATRIZ-FILIAL      PIC 9.
           03 RRE-RAZAO-SOCIAL           PIC X(150).
           03 RRE-NOME-FANTASIA          PIC X(055).
           03 RRE-SIT-CADASTRAL          PIC 99.
           03 RRE-DATA-SIT-CADASTRAL.
              05 FILLER		         PIC XX.
              05 DATA-SIT-AA             PIC 99.
              05 DATA-SIT-MM             PIC 99.
              05 DATA-SIT-DD             PIC 99.
           03 RRE-MOTIVO-SIT-CADASTRAL   PIC 99.
           03 RRE-NM-CIDADE-EXTERIOR     PIC X(055).
           03 RRE-CO-PAIS                PIC XXX.
           03 RRE-NM-PAIS                PIC X(070).
           03 RRE-COD-NAT-JURIDICA       PIC 9999.
           03 RRE-DATA-INICIO-ATIVIDADE.
              05 DATA-INI-ATI-AAAA       PIC 9999.
              05 DATA-INI-ATI-MM         PIC 99.
              05 DATA-INI-ATI-DD         PIC 99.
           03 RRE-CNAE-FISCAL            PIC 9(007).
           03 RRE-TIPO-LOGRADOURO        PIC X(020).
           03 RRE-LOGRADOURO             PIC X(060).
           03 RRE-NUMERO                 PIC X(006).
           03 RRE-COMPLEMENTO            PIC X(156).
           03 RRE-BAIRRO                 PIC X(050).
           03 RRE-CEP                    PIC 9(008).
           03 RRE-UF                     PIC X(002).
           03 RRE-COD-MUNICIPIO          PIC 9999.
           03 RRE-MUNICIPIO              PIC X(050).
           03 RRE-DDD-TELEFONE-1.
              05 RRE-DDD-1               PIC X(04).
              05 RRE-TELEFONE-1          PIC X(08).
           03 RRE-DDD-TELEFONE-2.
              05 RRE-DDD-2               PIC X(04).
              05 RRE-TELEFONE-2          PIC X(08).
           03 RRE-DDD-FAX.
              05 RRE-NU-DDD-FAX          PIC X(04).
              05 RRE-NU-FAX              PIC X(08).
           03 RRE-CORREIO-ELETRONICO     PIC X(115).
           03 RRE-QUALIFICACAO-RESPONSAVEL PIC 99.
           03 RRE-CAPITAL-SOCIAL-EMP     PIC 9(12)V99.
           03 RRE-PORTE-EMPRESA          PIC XX.
           03 RRE-OPCAO-SIMPLES          PIC X.
           03 RRE-DATA-OPCAO-SIMPLES     PIC 9(08).
           03 RRE-DATA-EXCLUSAO-SIMPLES  PIC 9(08).
           03 RRE-OPCAO-MEI              PIC X.
           03 RRE-SIT-ESPECIAL           PIC X(23).
           03 RRE-DATA-SIT-ESPECIAL      PIC 9(08).
           03 FILLER                     PIC X(243).
           03 RRE-FIM-REGISTRO           PIC X.

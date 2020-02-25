# Leitor de dados publicos CNPJ
Leitor de dados públicos da Receita Federal do Brasil em COBOL

O que o aplicativo faz:
* Busca dados de empresas na base da Receita Federal.

Implementações a serem feitas em um futuro próximo:
* Busca dados de sócios na base da Receita Federal.

Passos para utilização (Exemplo utilizando Ubuntu Linux).

- Compilar os aplicativos utilizando OpenCobol.
  - $>cobc -x gerar-dados-indexados.cob
  - $>cobc -x leitor-rfb.cob

- Baixar os 20 arquivos da receita federal no diretório "dados" e descompactar: http://receita.economia.gov.br/orientacao/tributaria/cadastros/cadastro-nacional-de-pessoas-juridicas-cnpj/dados-publicos-cnpj

- Gerar o banco de dados indexado com o seguinte comando(dependendo do nível de processamento da máquina poderá demorar dias):
  - $>./gerar-dados-indexados

- Rodar o aplicativo para efetuar as pesquisas.
  - $>./leitor-rfb

Telas de exemplo de uso:

![Tela inicial]
(https://github.com/robertoflj3/leitor_dados_publicos_cnpj/blob/master/screenshots/tela_inicial.png)

![Tela de busca de pessoa jurídica]
(https://github.com/robertoflj3/leitor_dados_publicos_cnpj/blob/master/screenshots/tela_busca_pj.png)

![Tela de detalhamento de pessoa jurídica]
(https://github.com/robertoflj3/leitor_dados_publicos_cnpj/blob/master/screenshots/tela_lista_pj.png)

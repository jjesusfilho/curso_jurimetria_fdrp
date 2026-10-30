## instrucoes inciciais

O token do GitHub está em `~/.Renviron` (`GITHUB_PAT`), lido diretamente pelo R. Para Python, shell e outros programas, o `.env` local (fora do git, permissão 600) repete o valor como `GITHUB_PAT`, `GITHUB_TOKEN` e `GH_TOKEN`. Nunca copiar o token para código ou arquivos versionados.

A cada tarefa lógica concluída: commit e push, com mensagens no padrão Conventional Commits (`feat:`, `fix:`, `docs:`, `test:`, `chore:`).

Repositório público: https://github.com/jjesusfilho/curso_jurimetria_fdrp. Dados brutos (`data/`) e `.env` ficam fora do git; só código, documentação e resultados agregados.

## Projeto


Estimar em dias entre a distribuição da ação e a primeira decisão de tutela de urgência em processos de fornecimento de medicamentos de alto custo de 2022 em diante.

Queremos comparar o período anterior e posterior à publicação do Tema 1234 do STF.

Classe:procedimento cível comum.

código do assunto: 12495 das TPUs do CNJ: fornecimento de medicamento não padronizado


Para tanto, vamos usar o pacote datajud: https://github.com/jjesusfilho/datajud


Teste se o datajud está funcionando.


### metodologia

O propósito é realizar uma análise de sobrevivência. Essa análise deve levar em conta os riscos concorrentes: acordo, desistência da ação, perda de objeto etc.

### Ferramentas

Use os pacotes do R como tidvyerse, survival e outros pacotes sobre geracao gráfica de análise de sobrevivência.
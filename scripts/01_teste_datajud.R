# Teste de funcionamento do pacote datajud
# A chave da API pública vem de DATAJUD_API_KEY (~/.Renviron).

library(datajud)
library(tidyverse)

tribunais <- list_tribunals()
print(tribunais)

# Amostra: medicamento não padronizado (assunto 12495), ajuizados desde 2022
amostra <- search_processes(
  tribunal = "tjsp",
  assuntos_codigo = 12495,
  dataAjuizamento_start = "2022-01-01",
  size = 5
)

glimpse(amostra)

# Datas vêm em dois formatos (ISO e compacto); conferir a leitura
amostra |>
  transmute(numeroProcesso, dataAjuizamento, ajuizamento = parse_datajud_date(dataAjuizamento))

# Movimentos e assuntos em formato longo
movimentos <- extract_movements(amostra)
glimpse(movimentos)

# Classes presentes numa amostra maior de 1º grau
amostra_g1 <- search_processes(
  tribunal = "tjsp",
  assuntos_codigo = 12495,
  grau = "G1",
  dataAjuizamento_start = "2022-01-01",
  size = 200
)

amostra_g1 |>
  mutate(classe = map_chr(classe, \(x) paste(x[[1]], x[[2]]))) |>
  count(classe, sort = TRUE)

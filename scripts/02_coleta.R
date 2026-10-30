# Coleta dos processos de medicamento não padronizado (assunto 12495) no 1º grau
# de todos os TJs estaduais. Retomável: rode de novo até não haver erros.
#
# nohup Rscript scripts/02_coleta.R > data/coleta.log 2>&1 &

library(tidyverse)
source("R/coleta.R")

log_coleta <- function(msg) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg, "\n")
}

tribunais <- list_tribunals() |>
  filter(type == "TJ") |>
  pull(alias) |>
  str_remove("^api_publica_")

# Rodadas sucessivas para retomar tribunais que falharam por instabilidade
for (rodada in 1:3) {
  pendentes <- tribunais[!file.exists(file.path("data/brutos", tribunais, "_completo"))]
  if (length(pendentes) == 0) break
  log_coleta(sprintf("rodada %d: %d tribunais pendentes", rodada, length(pendentes)))
  walk(pendentes, \(t) coletar_tribunal(t, log = log_coleta))
}

pendentes <- tribunais[!file.exists(file.path("data/brutos", tribunais, "_completo"))]
log_coleta(sprintf("fim: %d tribunais pendentes %s", length(pendentes),
                   paste(pendentes, collapse = " ")))

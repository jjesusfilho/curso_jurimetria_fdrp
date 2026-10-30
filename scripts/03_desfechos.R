# Base analítica: um processo por linha, com tempo até a primeira decisão de
# tutela ou até o primeiro risco concorrente

source("R/desfechos.R")

processos <- ler_brutos() |> filtrar_processos()
eventos <- construir_eventos(processos)

saveRDS(eventos, "data/eventos.rds")

count(eventos, periodo, evento) |> print(n = 30)
count(eventos, tribunal, sort = TRUE) |> print(n = 30)
summary(eventos$dias)

# Construção do tempo até a primeira decisão de tutela e dos riscos concorrentes
#
# Códigos da Tabela Processual Unificada (movimentos) conferidos nos dados:
# 12444 ("deferimento") e 12455 ("indeferimento") ficaram de fora porque, sem
# complemento, aparecem junto de outras decisões (ex.: gratuidade da justiça).

library(tidyverse)
library(datajud)

marco_tema1234 <- as.Date("2024-09-19")

codigos_tutela <- tribble(
  ~movimento_codigo, ~tutela,
  332L, "concedida",
  889L, "concedida em parte",
  785L, "negada",
  339L, "concedida",
  892L, "concedida em parte",
  792L, "negada"
)

codigos_concorrentes <- tribble(
  ~movimento_codigo, ~desfecho,
  466L,   "acordo",
  463L,   "desistência",
  12325L, "perda do objeto",
  464L,   "óbito",
  11380L, "óbito",
  454L,   "extinção sem mérito",
  458L,   "extinção sem mérito",
  459L,   "extinção sem mérito",
  460L,   "extinção sem mérito",
  461L,   "extinção sem mérito",
  941L,   "incompetência",
  11379L, "incompetência",
  219L,   "sentença sem tutela",
  220L,   "sentença sem tutela",
  221L,   "sentença sem tutela"
)

ler_brutos <- function(dir_base = "data/brutos") {
  list.files(dir_base, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE) |>
    map(readRDS) |>
    list_rbind()
}

# Um registro por processo de 1º grau, procedimento comum cível, ajuizado desde 2022
filtrar_processos <- function(brutos, classe_alvo = 7L, desde = as.Date("2022-01-01")) {
  brutos |>
    mutate(
      classe_codigo = map_int(classe, \(x) as.integer(x[[1]])),
      ajuizamento = as.Date(parse_datajud_date(dataAjuizamento)),
      atualizacao = as.Date(parse_datajud_date(dataHoraUltimaAtualizacao))
    ) |>
    filter(grau == "G1", classe_codigo == classe_alvo, ajuizamento >= desde) |>
    arrange(desc(atualizacao)) |>
    distinct(numeroProcesso, .keep_all = TRUE)
}

construir_eventos <- function(processos) {
  movimentos <- extract_movements(processos) |>
    mutate(data = as.Date(parse_datajud_date(movimento_data))) |>
    select(numeroProcesso, movimento_codigo, data)

  base <- processos |>
    select(numeroProcesso, tribunal, ajuizamento, atualizacao)

  primeira_tutela <- movimentos |>
    inner_join(codigos_tutela, by = "movimento_codigo") |>
    inner_join(base, by = "numeroProcesso") |>
    filter(data >= ajuizamento) |>
    slice_min(data, n = 1, with_ties = FALSE, by = numeroProcesso) |>
    select(numeroProcesso, data_tutela = data, tutela)

  # Incompetência só encerra a observação se nenhuma decisão de tutela vier
  # depois (o processo pode seguir em outra vara do mesmo tribunal)
  primeiro_concorrente <- movimentos |>
    inner_join(codigos_concorrentes, by = "movimento_codigo") |>
    inner_join(base, by = "numeroProcesso") |>
    filter(data >= ajuizamento) |>
    left_join(primeira_tutela, by = "numeroProcesso") |>
    filter(!(desfecho == "incompetência" & !is.na(data_tutela) & data_tutela > data)) |>
    slice_min(data, n = 1, with_ties = FALSE, by = numeroProcesso) |>
    select(numeroProcesso, data_concorrente = data, desfecho)

  base |>
    left_join(primeira_tutela, by = "numeroProcesso") |>
    left_join(primeiro_concorrente, by = "numeroProcesso") |>
    mutate(
      # Empate no mesmo dia: a decisão de tutela prevalece
      evento = case_when(
        !is.na(data_tutela) & (is.na(data_concorrente) | data_tutela <= data_concorrente) ~ "tutela",
        !is.na(data_concorrente) ~ desfecho,
        .default = "censurado"
      ),
      data_fim = case_when(
        evento == "tutela" ~ data_tutela,
        evento == "censurado" ~ atualizacao,
        .default = data_concorrente
      ),
      dias = as.numeric(data_fim - ajuizamento),
      periodo = factor(if_else(ajuizamento < marco_tema1234, "antes", "depois"),
                       levels = c("antes", "depois")),
      ano = year(ajuizamento),
      uf = str_remove(tribunal, "^TJ"),
      tutela = if_else(evento == "tutela", tutela, NA_character_)
    ) |>
    select(numeroProcesso, tribunal, uf, ajuizamento, ano, periodo,
           evento, tutela, data_fim, dias)
}

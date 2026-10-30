# Análise de sobrevivência com riscos concorrentes: tempo do ajuizamento até a
# primeira decisão de tutela, antes e depois do Tema 1234 (19/09/2024).
# Grava só resultados agregados em relatorio/resultados/ (versionados).

library(tidyverse)
library(survival)
library(tidycmprsk)
library(ggsurvfit)

dir_res <- "relatorio/resultados"
dir.create(dir_res, recursive = TRUE, showWarnings = FALSE)

horizontes <- c(7, 30, 90, 180, 365)

eventos <- readRDS("data/eventos.rds") |>
  mutate(
    desfecho = case_when(
      evento == "tutela" ~ "decisão de tutela",
      evento %in% c("censurado", "incompetência", "sentença sem tutela") ~ evento,
      .default = "outros encerramentos"
    ),
    desfecho = factor(desfecho, levels = c("censurado", "decisão de tutela", "incompetência",
                                           "sentença sem tutela", "outros encerramentos")),
    # Tribunais com poucos processos agrupados para os modelos
    tribunal_modelo = fct_lump_min(tribunal, min = 50, other_level = "outros")
  )

# Contagens descritivas ---------------------------------------------------

eventos |>
  count(periodo, evento) |>
  pivot_wider(names_from = periodo, values_from = n, values_fill = 0) |>
  write_csv(file.path(dir_res, "contagem_eventos.csv"))

eventos |>
  count(tribunal, periodo) |>
  pivot_wider(names_from = periodo, values_from = n, values_fill = 0) |>
  mutate(total = antes + depois) |>
  arrange(desc(total)) |>
  write_csv(file.path(dir_res, "contagem_tribunais.csv"))

eventos |>
  filter(evento == "tutela") |>
  count(periodo, tutela) |>
  mutate(prop = n / sum(n), .by = periodo) |>
  write_csv(file.path(dir_res, "resultado_tutela.csv"))

# Incidência acumulada (Aalen-Johansen) -----------------------------------

ajuste_cif <- cuminc(Surv(dias, desfecho) ~ periodo, data = eventos)

tidy(ajuste_cif, times = horizontes) |>
  select(outcome, strata, time, estimate, conf.low, conf.high, n.risk) |>
  write_csv(file.path(dir_res, "cif_horizontes.csv"))

glance(ajuste_cif) |>
  write_csv(file.path(dir_res, "teste_gray.csv"))

grafico_tutela <- ajuste_cif |>
  ggcuminc(outcome = "decisão de tutela", linewidth = 0.8) +
  add_confidence_interval() +
  add_risktable(risktable_stats = "n.risk", stats_label = list(n.risk = "Em risco")) +
  scale_x_continuous(limits = c(0, 365), breaks = c(0, 30, 90, 180, 270, 365)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  labs(x = "Dias desde o ajuizamento", y = "Incidência acumulada da decisão de tutela",
       color = "Ajuizamento", fill = "Ajuizamento") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(dir_res, "cif_tutela.png"), grafico_tutela, width = 7, height = 5.5, dpi = 300)

grafico_todos <- ajuste_cif |>
  tidy(times = 0:365) |>
  ggplot(aes(time, estimate, color = strata)) +
  geom_step(linewidth = 0.7) +
  facet_wrap(vars(outcome), scales = "free_y") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Dias desde o ajuizamento", y = "Incidência acumulada", color = "Ajuizamento") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(dir_res, "cif_desfechos.png"), grafico_todos, width = 8, height = 5.5, dpi = 300)

# Modelos ------------------------------------------------------------------

# Cox causa-específica: taxa instantânea de decisão de tutela, estratificada
# por tribunal (cada tribunal com seu risco basal)
cox_cs <- coxph(
  Surv(dias, desfecho == "decisão de tutela") ~ periodo + strata(tribunal_modelo),
  data = eventos
)

# Fine-Gray: efeito sobre a incidência acumulada, com tribunal como covariável
fine_gray <- crr(
  Surv(dias, desfecho) ~ periodo + tribunal_modelo,
  data = eventos,
  failcode = "decisão de tutela"
)

bind_rows(
  broom::tidy(cox_cs, exponentiate = TRUE, conf.int = TRUE) |> mutate(modelo = "Cox causa-específico"),
  tidy(fine_gray, exponentiate = TRUE, conf.int = TRUE) |> mutate(modelo = "Fine-Gray")
) |>
  filter(term == "periododepois") |>
  select(modelo, term, estimate, conf.low, conf.high, p.value) |>
  write_csv(file.path(dir_res, "modelos.csv"))

# Proporcionalidade dos riscos no Cox
cox.zph(cox_cs)$table |>
  as_tibble(rownames = "termo") |>
  write_csv(file.path(dir_res, "cox_zph.csv"))

# Efeito por tribunal (só tribunais com volume suficiente nos dois períodos)
tribunais_grandes <- eventos |>
  count(tribunal, periodo) |>
  filter(n >= 100) |>
  count(tribunal) |>
  filter(n == 2) |>
  pull(tribunal)

map(tribunais_grandes, \(t) {
  coxph(Surv(dias, desfecho == "decisão de tutela") ~ periodo,
        data = filter(eventos, tribunal == t)) |>
    broom::tidy(exponentiate = TRUE, conf.int = TRUE) |>
    mutate(tribunal = t)
}) |>
  list_rbind() |>
  select(tribunal, estimate, conf.low, conf.high, p.value) |>
  write_csv(file.path(dir_res, "cox_por_tribunal.csv"))

writeLines(
  c(sprintf("processos: %d", nrow(eventos)),
    sprintf("tribunais: %d", n_distinct(eventos$tribunal)),
    sprintf("gerado em: %s", Sys.Date())),
  file.path(dir_res, "metadados.txt")
)

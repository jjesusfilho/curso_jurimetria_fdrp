# Coleta no DataJud em fatias retomáveis
#
# A consulta não filtra classe (a classe informada é a atual) nem data de
# ajuizamento (o índice interpreta mal o formato compacto); os dois filtros
# são aplicados depois, na limpeza.

library(datajud)

consulta_medicamentos <- function(assunto = 12495, grau = "G1") {
  list(bool = list(filter = list(
    list(term = list(assuntos.codigo = assunto)),
    list(match = list(grau = grau))
  )))
}

# Baixa uma fatia (tribunal x prefixo do número CNJ). Se passar de max_size,
# desce um dígito no prefixo. Cada fatia completa vira um .rds; fatias já
# gravadas são puladas, de modo que retomar é rodar de novo.
coletar_fatia <- function(tribunal, consulta, prefixo = "", dir_saida,
                          max_size = 2000, log = message) {
  arquivo <- file.path(dir_saida, paste0("p", prefixo, ".rds"))
  if (file.exists(arquivo)) return(invisible("existente"))

  q <- add_cnj_prefix(consulta, prefixo)
  resultado <- tryCatch(
    search_slice(tribunal, q, max_size = max_size),
    datajud_slice_too_large = function(e) e
  )

  if (inherits(resultado, "datajud_slice_too_large")) {
    log(sprintf("%s p%s: %d documentos, dividindo", tribunal, prefixo, resultado$total))
    for (p in paste0(prefixo, 0:9)) {
      coletar_fatia(tribunal, consulta, p, dir_saida, max_size, log)
    }
    return(invisible("dividida"))
  }

  total <- attr(resultado, "total")
  obtidos <- if (total > 0) dplyr::n_distinct(resultado$id) else 0L
  if (obtidos < total) {
    stop(sprintf("%s p%s: %d ids distintos de %d", tribunal, prefixo, obtidos, total))
  }

  attr(resultado, "coletado_em") <- Sys.time()
  saveRDS(resultado, arquivo)
  log(sprintf("%s p%s: %d documentos", tribunal, prefixo, total))
  invisible("gravada")
}

coletar_tribunal <- function(tribunal, consulta = consulta_medicamentos(),
                             dir_base = "data/brutos", log = message) {
  dir_saida <- file.path(dir_base, tribunal)
  dir.create(dir_saida, recursive = TRUE, showWarnings = FALSE)
  feito <- file.path(dir_saida, "_completo")
  if (file.exists(feito)) return(invisible(TRUE))

  ok <- tryCatch({
    coletar_fatia(tribunal, consulta, "", dir_saida, log = log)
    TRUE
  }, error = function(e) {
    log(sprintf("%s: ERRO %s (rode de novo para retomar)", tribunal, conditionMessage(e)))
    FALSE
  })

  if (ok) file.create(feito)
  invisible(ok)
}

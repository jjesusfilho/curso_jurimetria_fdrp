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
                             dir_base = "data/brutos", max_size = 2000, log = message) {
  dir_saida <- file.path(dir_base, tribunal)
  dir.create(dir_saida, recursive = TRUE, showWarnings = FALSE)
  feito <- file.path(dir_saida, "_completo")
  if (file.exists(feito)) return(invisible(TRUE))

  ok <- tryCatch({
    coletar_fatia(tribunal, consulta, "", dir_saida, max_size = max_size, log = log)
    TRUE
  }, error = function(e) {
    log(sprintf("%s: ERRO %s (rode de novo para retomar)", tribunal, conditionMessage(e)))
    FALSE
  })

  if (ok) file.create(feito)
  invisible(ok)
}

# Alternativa para índices em que o filtro por prefixo do número estoura o
# gateway (TJMG, TJRS): filtra a classe na consulta e divide por faixas do
# código IBGE do município do órgão julgador, campo numérico de filtro rápido.
# Só serve quando a análise usa uma única classe.
com_filtro <- function(consulta, filtro) {
  list(bool = list(filter = c(consulta$bool$filter, list(filtro))))
}

coletar_faixa_ibge <- function(tribunal, consulta, inicio, fim, dir_saida,
                               max_size = 200, log = message) {
  arquivo <- file.path(dir_saida, sprintf("ibge_%d_%d.rds", inicio, fim))
  if (file.exists(arquivo)) return(invisible("existente"))

  q <- com_filtro(consulta, list(range = list(
    orgaoJulgador.codigoMunicipioIBGE = list(gte = inicio, lt = fim)
  )))
  resultado <- tryCatch(
    search_slice(tribunal, q, max_size = max_size),
    datajud_slice_too_large = function(e) e
  )

  if (inherits(resultado, "datajud_slice_too_large")) {
    if (fim - inicio <= 1) {
      # Um só município acima do limite: baixa assim mesmo
      resultado <- search_slice(tribunal, q, max_size = resultado$total)
    } else {
      meio <- inicio + (fim - inicio) %/% 2
      log(sprintf("%s ibge %d-%d: %d documentos, dividindo", tribunal, inicio, fim, resultado$total))
      coletar_faixa_ibge(tribunal, consulta, inicio, meio, dir_saida, max_size, log)
      coletar_faixa_ibge(tribunal, consulta, meio, fim, dir_saida, max_size, log)
      return(invisible("dividida"))
    }
  }

  attr(resultado, "coletado_em") <- Sys.time()
  saveRDS(resultado, arquivo)
  log(sprintf("%s ibge %d-%d: %d documentos", tribunal, inicio, fim, attr(resultado, "total")))
  invisible("gravada")
}

coletar_tribunal_por_ibge <- function(tribunal, classe = 7L, dir_base = "data/brutos",
                                      max_size = 200, log = message) {
  dir_saida <- file.path(dir_base, tribunal)
  dir.create(dir_saida, recursive = TRUE, showWarnings = FALSE)
  feito <- file.path(dir_saida, "_completo")
  if (file.exists(feito)) return(invisible(TRUE))

  consulta <- com_filtro(consulta_medicamentos(), list(term = list(classe.codigo = classe)))

  ok <- tryCatch({
    coletar_faixa_ibge(tribunal, consulta, 1000000L, 6000000L, dir_saida, max_size, log)

    # Processos sem código IBGE ficam fora das faixas
    arquivo_sem <- file.path(dir_saida, "ibge_ausente.rds")
    if (!file.exists(arquivo_sem)) {
      q_sem <- list(bool = list(
        filter = consulta$bool$filter,
        must_not = list(list(exists = list(field = "orgaoJulgador.codigoMunicipioIBGE")))
      ))
      sem <- search_slice(tribunal, q_sem, max_size = max_size)
      saveRDS(sem, arquivo_sem)
      log(sprintf("%s sem IBGE: %d documentos", tribunal, attr(sem, "total")))
    }
    TRUE
  }, error = function(e) {
    log(sprintf("%s: ERRO %s (rode de novo para retomar)", tribunal, conditionMessage(e)))
    FALSE
  })

  if (ok) file.create(feito)
  invisible(ok)
}

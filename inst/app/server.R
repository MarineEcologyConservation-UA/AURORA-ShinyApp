server <- function(input, output, session) {

  example_map <- list(
    "Example (event.csv)" = system.file("extdata", "event.csv", package = "shinyRv02"),
    "Example (occurrence.csv)" = system.file("extdata", "occurrence.csv", package = "shinyRv02"),
    "Example (aurora.xlsx)" = system.file(
      "extdata",
      "aurora_good_quality_data_densitydata_id_corrected.xlsx",
      package = "shinyRv02"
    )
  )

  ingest <- mod_ingest_server("ingest", example_map = example_map)

  dwc_map <- mod_dwc_mapping_server(
    "dwc_map",
    df_in = ingest$tidy
  )

  # --- Build DwC-A -----------------------------------------------------------
  dwc_terms_path <- system.file("extdata", "dwc_terms.csv", package = "shinyRv02")
  dwc_terms <- read.csv(dwc_terms_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

  dwca <- mod_build_dwca_server(
    "dwca",
    df_in = dwc_map$cleaned,
    dwc_terms = dwc_terms
  )

  # --- QC & Diagnostics ------------------------------------------------------
  # Pré-issues: usa o que já tens no mapping. No teu módulo, o nome é `issues`.
  # Se `dwc_map$issues` não existir ainda como reactive exportado, coloca NULL.
  # --- QC & Diagnostics ------------------------------------------------------
  shiny::observe({
    cat("\n--- DEBUG DWCA ---\n")
    cat("dwca exists:", !is.null(dwca), "\n")
    cat("dwca$event is reactive:", shiny::is.reactive(dwca$event), "\n")
    cat("dwca$occurrence is reactive:", shiny::is.reactive(dwca$occurrence), "\n")
    cat("dwca$emof is reactive:", shiny::is.reactive(dwca$emof), "\n")

    # tentar “puxar” os dados (vai bloquear se ainda não houver result())
    ev <- try(dwca$event(), silent = TRUE)
    oc <- try(dwca$occurrence(), silent = TRUE)
    em <- try(dwca$emof(), silent = TRUE)

    cat("event():", if (inherits(ev, "try-error")) "ERROR/req blocked" else paste(dim(ev), collapse = " x "), "\n")
    cat("occurrence():", if (inherits(oc, "try-error")) "ERROR/req blocked" else paste(dim(oc), collapse = " x "), "\n")
    cat("emof():", if (inherits(em, "try-error")) "ERROR/req blocked" else paste(dim(em), collapse = " x "), "\n")
  })

  qc <- mod_qc_server(
    "qc",
    event_in = dwca$event,
    occ_in = dwca$occurrence,
    emof_in = dwca$emof,
    pre_issues_in = dwc_map$issues %||% NULL
  )

  invisible(list(ingest = ingest, dwc_map = dwc_map, dwca = dwca, qc = qc))
}

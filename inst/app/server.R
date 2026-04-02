server <- function(input, output, session) {

  example_map <- list(
    "Example (event.csv)" = system.file("extdata", "event.csv", package = "shinyRv02"),
    "Example (occurrence.csv)" = system.file("extdata", "occurrence.csv", package = "shinyRv02"),
    "Example (aurora.csv)" = system.file(
      "extdata",
      "aurora_good_quality_data_densitydata_id_corrected.csv",
      package = "shinyRv02"
    )
  )

  `%||%` <- function(x, y) if (is.null(x)) y else x

  safe_flag <- function(expr) {
    out <- try(expr, silent = TRUE)

    if (inherits(out, "try-error")) {
      return(FALSE)
    }

    isTRUE(out)
  }

  ingest <- mod_ingest_server("ingest", example_map = example_map)

  dwc_map <- mod_dwc_mapping_server(
    "dwc_map",
    df_in = ingest$tidy
  )

  tax_match <- mod_taxonomy_match_server(
    "tax_match",
    df_in = dwc_map$cleaned
  )

  # --- Build DwC-A -----------------------------------------------------------
  dwc_terms_path <- system.file("extdata", "dwc_terms.csv", package = "shinyRv02")
  dwc_terms <- read.csv(
    dwc_terms_path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )

  dwca <- mod_build_dwca_server(
    "dwca",
    df_in = tax_match$df_out,
    dwc_terms = dwc_terms
  )

  metadata <- mod_metadata_server("metadata")

  # --- Sequential tab locking ------------------------------------------------
  shiny::observe({
    ingest_done   <- safe_flag(ingest$ready())
    mapping_done  <- ingest_done  && safe_flag(dwc_map$ready())
    taxonomy_done <- mapping_done && safe_flag(tax_match$ready())
    build_done    <- taxonomy_done && safe_flag(dwca$ready())

    allowed_tabs <- c("home", "ingest")

    if (ingest_done) {
      allowed_tabs <- c(allowed_tabs, "field_mapping")
    }
    if (mapping_done) {
      allowed_tabs <- c(allowed_tabs, "taxonomy")
    }
    if (taxonomy_done) {
      allowed_tabs <- c(allowed_tabs, "darwin_tables")
    }
    if (build_done) {
      allowed_tabs <- c(allowed_tabs, "metadata", "qc")
    }

    session$sendCustomMessage(
      "toggleMainTabs",
      list(allowed = allowed_tabs)
    )

    current_tab <- input$main_nav %||% "home"

    if (!current_tab %in% allowed_tabs) {
      fallback_tab <- tail(allowed_tabs, 1)

      bslib::nav_select(
        id = "main_nav",
        selected = fallback_tab,
        session = session
      )
    }
  })

  # --- QC & Diagnostics ------------------------------------------------------
  shiny::observe({
    cat("\n--- DEBUG WORKFLOW ---\n")
    cat("ingest$ready():",   safe_flag(ingest$ready()), "\n")
    cat("dwc_map$ready():",  safe_flag(dwc_map$ready()), "\n")
    cat("tax_match$ready():", safe_flag(tax_match$ready()), "\n")
    cat("dwca$ready():",     safe_flag(dwca$ready()), "\n")

    cat("\n--- DEBUG DWCA ---\n")
    cat("dwca exists:", !is.null(dwca), "\n")
    cat("dwca$event is reactive:", shiny::is.reactive(dwca$event), "\n")
    cat("dwca$occurrence is reactive:", shiny::is.reactive(dwca$occurrence), "\n")
    cat("dwca$emof is reactive:", shiny::is.reactive(dwca$emof), "\n")

    ev <- try(dwca$event(), silent = TRUE)
    oc <- try(dwca$occurrence(), silent = TRUE)
    em <- try(dwca$emof(), silent = TRUE)

    cat(
      "event():",
      if (inherits(ev, "try-error")) "ERROR/req blocked" else paste(dim(ev), collapse = " x "),
      "\n"
    )
    cat(
      "occurrence():",
      if (inherits(oc, "try-error")) "ERROR/req blocked" else paste(dim(oc), collapse = " x "),
      "\n"
    )
    cat(
      "emof():",
      if (inherits(em, "try-error")) "ERROR/req blocked" else paste(dim(em), collapse = " x "),
      "\n"
    )
  })

  qc <- mod_qc_server(
    "qc",
    event_in = dwca$event,
    occ_in = dwca$occurrence,
    emof_in = dwca$emof,
    pre_issues_in = dwc_map$issues %||% NULL
  )

  invisible(list(
    ingest = ingest,
    dwc_map = dwc_map,
    dwca = dwca,
    qc = qc,
    tax_match = tax_match,
    metadata = metadata
  ))
}
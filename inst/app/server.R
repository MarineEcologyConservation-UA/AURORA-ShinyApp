server <- function(input, output, session) {

  example_map <- list(
    "Example (dataset_example.csv)" = system.file("extdata", "dataset_example.csv", package = "shinyRv02")
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

  id_cleaning <- mod_identification_cleaning_server(
    "id_cleaning",
    df_in = dwc_map$cleaned,
    mapping_in = dwc_map$mapping
  )

  tax_match <- mod_taxonomy_match_server(
    "tax_match",
    df_in = id_cleaning$df_out
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
    dwc_terms = dwc_terms,
    target_database_in = dwc_map$target_database,
    pre_issues_in = dwc_map$issues
  )

  # --- eMoF Editor -----------------------------------------------------------
  emof_editor <- mod_emof_data_editor_server(
    "emof_editor",
    emof_in = dwca$emof
  )

  # --- Metadata --------------------------------------------------------------
  metadata <- mod_metadata_server("metadata")

  # --- Sequential tab locking ------------------------------------------------
  shiny::observe({
    ingest_done   <- safe_flag(ingest$ready())
    mapping_done  <- ingest_done  && safe_flag(dwc_map$ready())
    id_clean_done <- mapping_done && safe_flag(id_cleaning$ready())
    taxonomy_done <- id_clean_done && safe_flag(tax_match$ready())

    dwca_ready_flag <- safe_flag(dwca$ready())

    build_done <- taxonomy_done && dwca_ready_flag

    emof_editor_done <- build_done && safe_flag(emof_editor$ready())

    allowed_tabs <- c("home", "ingest", "about")

    if (ingest_done) {
      allowed_tabs <- c(allowed_tabs, "field_mapping")
    }
    if (mapping_done) {
      allowed_tabs <- c(allowed_tabs, "id_cleaning")
    }
    if (id_clean_done) {
      allowed_tabs <- c(allowed_tabs, "taxonomy")
    }
    if (taxonomy_done) {
      allowed_tabs <- c(allowed_tabs, "darwin_tables")
    }
    if (build_done) {
      allowed_tabs <- c(allowed_tabs, "emof_editor")
    }
    if (emof_editor_done) {
      allowed_tabs <- c(allowed_tabs, "qc", "metadata")
    }

    session$sendCustomMessage(
      "toggleMainTabs",
      list(allowed = allowed_tabs)
    )

    current_tab <- input$main_nav %||% "home"

    if (!current_tab %in% allowed_tabs) {
      fallback_tab <- "home"

      if (emof_editor_done) {
        fallback_tab <- "qc"
      } else if (build_done) {
        fallback_tab <- "emof_editor"
      } else if (taxonomy_done) {
        fallback_tab <- "darwin_tables"
      } else if (id_clean_done) {
        fallback_tab <- "id_cleaning"
      } else if (mapping_done) {
        fallback_tab <- "field_mapping"
      } else if (ingest_done) {
        fallback_tab <- "ingest"
      }

      bslib::nav_select(
        id = "main_nav",
        selected = fallback_tab,
        session = session
      )
    }
  })

  # --- QC & Diagnostics ------------------------------------------------------
  qc <- mod_qc_server(
    "qc",
    event_in = dwca$event,
    occ_in = dwca$occurrence,
    emof_in = emof_editor$emof,
    pre_issues_in = dwc_map$issues %||% NULL
  )

  invisible(list(
    ingest = ingest,
    dwc_map = dwc_map,
    id_cleaning = id_cleaning,
    tax_match = tax_match,
    dwca = dwca,
    emof_editor = emof_editor,
    qc = qc,
    metadata = metadata
  ))
}
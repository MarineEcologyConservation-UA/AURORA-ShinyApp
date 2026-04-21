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

  `%||%` <- function(x, y) if (is.null(x)) y else x

  safe_flag <- function(expr) {
    out <- try(expr, silent = TRUE)

    if (inherits(out, "try-error")) {
      return(FALSE)
    }

    isTRUE(out)
  }

  safe_df_ready <- function(rx) {
    out <- try(rx(), silent = TRUE)
    is.data.frame(out) && !inherits(out, "try-error")
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
    target_database_in = dwc_map$target_database
  )

  metadata <- mod_metadata_server("metadata")

  # --- Sequential tab locking ------------------------------------------------
  shiny::observe({
    ingest_done   <- safe_flag(ingest$ready())
    mapping_done  <- ingest_done   && safe_flag(dwc_map$ready())
    id_clean_done <- mapping_done  && safe_flag(id_cleaning$ready())
    taxonomy_done <- id_clean_done && safe_flag(tax_match$ready())

    dwca_ready_flag   <- safe_flag(dwca$ready())
    dwca_tables_ready <- safe_df_ready(dwca$event) && safe_df_ready(dwca$occurrence)

    build_done <- taxonomy_done && (dwca_ready_flag || dwca_tables_ready)

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
      allowed_tabs <- c(allowed_tabs, "qc", "metadata")
    }

    session$sendCustomMessage(
      "toggleMainTabs",
      list(allowed = allowed_tabs)
    )

    current_tab <- input$main_nav %||% "home"

    if (!current_tab %in% allowed_tabs) {
      fallback_tab <- "home"

      if (build_done) {
        fallback_tab <- "qc"
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
    emof_in = dwca$emof,
    pre_issues_in = dwc_map$issues %||% NULL
  )

  invisible(list(
    ingest = ingest,
    dwc_map = dwc_map,
    id_cleaning = id_cleaning,
    dwca = dwca,
    qc = qc,
    tax_match = tax_match,
    metadata = metadata
  ))
}
# modules/mod_dwc_mapping.R

#' Darwin Core mapping module UI
#'
#' Shiny module UI for mapping user columns to Darwin Core terms (Item 3).
#'
#' @param id Module id.
#' @return A Shiny UI definition.
#' @export
mod_dwc_mapping_ui <- function(id) {
  ns <- shiny::NS(id)


  # --- NEW: CSS to fix selectize dropdown clipping + stacking ---
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      /* selectize dropdown must appear above cards/tables/scroll areas */
      .selectize-dropdown, .selectize-dropdown.form-control {
        z-index: 3000 !important;
      }

      /* avoid clipping in common bootstrap/bslib containers */
      .bslib-card, .card, .card-body {
        overflow: visible !important;
      }
    "))
  )

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      bslib::card(
        bslib::card_header("Actions"),
        shiny::actionButton(
          ns("auto_suggest"),
          "Auto-suggest",
          icon = shiny::icon("magic")
        ),
        shiny::br(), shiny::br(),
        shiny::actionButton(
          ns("apply_mapping"),
          "Apply mapping",
          icon = shiny::icon("check")
        ),
        shiny::br(), shiny::br(),
        shiny::uiOutput(ns("apply_notice"))
      ),

      bslib::card(
        bslib::card_header("Validation"),
        shiny::verbatimTextOutput(ns("validation"))
      ),

      bslib::card(
        bslib::card_header("Coordinate system"),
        shiny::p(
          "Informe o CRS (EPSG) do ficheiro de entrada.",
          "O sistema alvo será sempre EPSG:[4326] (WGS84 lon/lat)."
        ),
        shiny::numericInput(
          ns("coord_epsg_in"),
          label = "Input CRS EPSG:[ ]",
          value = 4326,
          min = 1
        ),
        shiny::checkboxInput(
          ns("coord_cols_are_messy"),
          label = "Forçar parsing (parzer) mesmo se parecer numérico",
          value = TRUE
        ),
        shiny::tags$hr(),
        shiny::tags$b("Target CRS EPSG:[4326]"),
        shiny::p("As coordenadas serão guardadas em decimal degrees.")
      ),

      bslib::card(
        bslib::card_header("Data cleaning"),
        shiny::verbatimTextOutput(ns("cleaning_summary")),
        shiny::downloadButton(ns("download_issues_csv"), "Download issues (CSV)")
      )
    ),

    bslib::navset_card_tab(
      id = ns("main_tabs"),
      title = "Field mapping to Darwin Core",

      bslib::nav_panel(
        "Mapping",
        value = "mapping",
        shiny::h4("Map each column to a Darwin Core term"),
        shiny::uiOutput(ns("mapping_ui"))
      ),

      bslib::nav_panel(
        "Preview (mapped)",
        value = "preview_mapped",
        shiny::p("Preview após aplicar o mapping (primeiras linhas)."),
        DT::DTOutput(ns("preview_mapped_tbl"))
      ),

      bslib::nav_panel(
        "Preview (cleaned)",
        value = "preview_cleaned",
        shiny::p("Preview após cleaning (primeiras linhas)."),
        DT::DTOutput(ns("preview_cleaned_tbl"))
      ),

      bslib::nav_panel(
        "Cleaning issues",
        value = "issues",
        DT::DTOutput(ns("issues_tbl"))
      )
    )
  )
}

# ---- internal helpers ------------------------------------------------------

`%||%` <- rlang::`%||%`

.dwc_term_meta_from_corella <- function() {
  if (!requireNamespace("corella", quietly = TRUE)) {
    stop("Package 'corella' is required for DwC term list.")
  }

  x <- corella::darwin_core_terms
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  x <- x[!is.na(x$term) & x$term != "", , drop = FALSE]
  x <- x[!duplicated(x$term), , drop = FALSE]
  x
}

.dwc_terms_from_corella <- function(term_meta) {
  terms <- unique(stats::na.omit(term_meta$term))
  terms <- sort(terms)
  c("", terms)
}

.dwc_required_terms <- function() {
  c(
    "scientificName",
    "eventDate",
    "decimalLatitude",
    "decimalLongitude",
    "basisOfRecord",
    "occurrenceID"
  )
}

.clean_col_for_match <- function(x) {
  x <- tolower(x %||% "")
  x <- gsub("\\([^)]*\\)", " ", x)
  x <- gsub("\\[[^]]*\\]", " ", x)
  x <- gsub("\\b(dd|deg|degree|degrees)\\b", " ", x)
  x <- gsub("\\b(m|meter|meters|metre|metres)\\b", " ", x)
  x <- gsub("[/_\\-]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

.normalize_key <- function(x) {
  x <- .clean_col_for_match(x)
  gsub("[^a-z0-9]+", "", x)
}

.dwc_alias_rules <- function() {
  list(
    list(pattern = "\\b(latitude|^lat$|\\blat\\b)\\b", term = "decimalLatitude"),
    list(
      pattern = "\\b(longitude|^lon$|\\blon\\b|\\blng\\b|\\blong\\b)\\b",
      term = "decimalLongitude"
    ),
    list(
      pattern = "\\b(event date|sampling date|sample date|date sampled|^date$)\\b",
      term = "eventDate"
    ),
    list(
      pattern = "\\b(scientific name|scientific_name|taxon name|taxon_name|species)\\b",
      term = "scientificName"
    )
  )
}

.apply_alias_rules <- function(col_name, dwc_terms) {
  col_clean <- .clean_col_for_match(col_name)
  rules <- .dwc_alias_rules()

  for (r in rules) {
    if (grepl(r$pattern, col_clean, perl = TRUE)) {
      if (r$term %in% dwc_terms) return(r$term)
    }
  }
  ""
}

.suggest_term <- function(col_name, dwc_terms) {
  if (is.null(col_name) || is.na(col_name) || col_name == "") return("")
  if (col_name %in% dwc_terms) return(col_name)

  ali <- .apply_alias_rules(col_name, dwc_terms)
  if (nzchar(ali)) return(ali)

  key <- .normalize_key(col_name)
  if (key == "") return("")

  terms2 <- dwc_terms[dwc_terms != ""]
  terms_key <- vapply(terms2, .normalize_key, character(1))

  if (!requireNamespace("stringdist", quietly = TRUE)) return("")

  d <- stringdist::stringdist(key, terms_key, method = "jw", p = 0.1)
  i <- which.min(d)
  if (!length(i) || !is.finite(d[i])) return("")
  if (d[i] <= 0.22) terms2[i] else ""
}

.validate_mapping_basic <- function(map_df) {
  req <- .dwc_required_terms()
  mapped <- map_df$dwc_term

  missing_req <- setdiff(req, mapped)

  dup <- mapped[mapped != ""]
  dup <- dup[duplicated(dup)]
  dup <- unique(dup)

  msgs <- character(0)

  if (length(missing_req) > 0) {
    msgs <- c(
      msgs,
      paste0(
        "Missing required/strongly recommended terms: ",
        paste(missing_req, collapse = ", ")
      )
    )
  }

  if (length(dup) > 0) {
    msgs <- c(
      msgs,
      paste0(
        "Duplicate mappings not allowed: ",
        paste(dup, collapse = ", ")
      )
    )
  }

  if (length(msgs) == 0) msgs <- "OK: mapping passes basic validation."
  msgs
}

.mapping_equal <- function(a, b) {
  if (is.null(a) || is.null(b)) return(FALSE)
  if (!all(c("user_column", "dwc_term") %in% names(a))) return(FALSE)
  if (!all(c("user_column", "dwc_term") %in% names(b))) return(FALSE)

  a2 <- a[, c("user_column", "dwc_term"), drop = FALSE]
  b2 <- b[, c("user_column", "dwc_term"), drop = FALSE]
  identical(a2, b2)
}

.apply_mapping <- function(df, map_df) {
  out <- df

  final_names <- map_df$dwc_term
  blank <- is.na(final_names) | final_names == ""
  final_names[blank] <- map_df$user_column[blank]

  out <- out[, map_df$user_column, drop = FALSE]
  names(out) <- final_names
  out
}

.term_tooltip_html <- function(term, meta_row) {
  if (is.null(term) || is.na(term) || term == "") {
    return("Select a term to see definition and examples.")
  }

  def <- meta_row$definition %||% ""
  ex <- meta_row$examples %||% ""
  url <- meta_row$url %||% ""

  paste0(
    "<b>", term, "</b><br/>",
    if (nzchar(def)) paste0("<b>Definition:</b> ", def, "<br/>") else "",
    if (nzchar(ex)) paste0("<b>Examples:</b> ", ex, "<br/>") else "",
    if (nzchar(url)) paste0("<b>Ref:</b> ", url) else ""
  )
}

.corella_summary <- function(x, label) {
  if (is.null(x)) return(character(0))
  if (inherits(x, "error")) return(c(paste0(label, " failed:"), x$message))

  c(
    paste0(label, ": OK"),
    paste0("class: ", paste(class(x), collapse = "/")),
    paste0("length: ", length(x))
  )
}

#' Darwin Core mapping module server
#'
#' @param id Module id.
#' @param df_in Reactive returning a data.frame (use ingest$tidy).
#' @return A list of reactives: mapping, mapped, cleaned, issues, clean_summary, validation.
#' @export
mod_dwc_mapping_server <- function(id, df_in) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(
      mapping = NULL,
      mapped = NULL,
      cleaned = NULL,
      issues = NULL,
      clean_summary = NULL,
      corella_checks = NULL,
      corella_suggest = NULL
    )

    if (!requireNamespace("bslib", quietly = TRUE)) {
      stop("Package 'bslib' is required (app is now Bootstrap 5).")
    }

    term_meta <- .dwc_term_meta_from_corella()
    dwc_terms <- .dwc_terms_from_corella(term_meta)

    col_key_map <- shiny::reactive({
      df <- df_in()
      shiny::req(df)

      cols <- names(df)
      keys <- make.names(cols, unique = TRUE)
      stats::setNames(cols, keys)
    })

    # --- mapping table from inputs (OK to depend on input; do NOT use this inside mapping_ui) ---
    mapping_tbl <- shiny::reactive({
      df <- df_in()
      shiny::req(df)

      key_map <- col_key_map()
      keys <- names(key_map)

      dwc_selected <- vapply(
        keys,
        function(k) input[[paste0("map__", k)]] %||% "",
        character(1)
      )

      map <- data.frame(
        user_column = unname(key_map),
        dwc_term = unname(dwc_selected),
        stringsAsFactors = FALSE
      )

      map$final_column <- map$dwc_term
      blank <- is.na(map$final_column) | map$final_column == ""
      map$final_column[blank] <- map$user_column[blank]
      map
    })

    # --- tooltip outputs per row: updates when the corresponding select changes ---
    shiny::observeEvent(df_in(), {
      df <- df_in()
      shiny::req(df)

      key_map <- col_key_map()
      keys <- names(key_map)

      for (k in keys) {
        local({
          kk <- k
          tip_id <- paste0("tip__", kk)
          input_id <- paste0("map__", kk)

          output[[tip_id]] <- shiny::renderUI({
            term <- input[[input_id]] %||% ""

            meta_row <- term_meta[term_meta$term == term, , drop = FALSE]
            html <- if (nrow(meta_row) == 0) {
              if (term == "") "Select a term to see definition and examples."
              else "No metadata found for this term in corella."
            } else {
              .term_tooltip_html(term, meta_row[1, ])
            }

            htmltools::HTML(html)
          })
        })
      }
    }, ignoreInit = FALSE)

    output$apply_notice <- shiny::renderUI({
      df <- df_in()
      shiny::req(df)

      current <- mapping_tbl()

      if (is.null(rv$mapping)) {
        shiny::tags$div(
          class = "alert alert-warning",
          style = "margin-top: 10px;",
          "Changes not applied yet. Click 'Apply mapping' to confirm."
        )
      } else if (!.mapping_equal(current, rv$mapping)) {
        shiny::tags$div(
          class = "alert alert-warning",
          style = "margin-top: 10px;",
          "You changed the mapping. Click 'Apply mapping' to apply changes."
        )
      } else {
        shiny::tags$div(
          class = "alert alert-success",
          style = "margin-top: 10px;",
          "Mapping applied."
        )
      }
    })

    # --- UI is built ONLY from df_in() + rv$mapping (NOT from input$map__*) ---
    output$mapping_ui <- shiny::renderUI({
      df <- df_in()
      shiny::req(df)

      key_map <- col_key_map()
      keys <- names(key_map)

      border_col <- "#cbd5e1"

      shiny::tags$div(
        # --- CHANGED: remove internal scroll to avoid "scroll inside scroll"
        style = "padding-right: 8px;",
        shiny::tags$div(
          style = paste0(
            "display:grid;",
            "grid-template-columns: 1fr 2fr;",
            "gap: 0;",
            "width: 100%;",
            "border: 1px solid ", border_col, ";",
            "border-radius: 10px;",
            # mantém para arredondar, mas dropdown vai para o body (não fica recortado)
            "overflow: hidden;",
            "background: #fff;"
          ),

          shiny::tags$div(
            style = paste0(
              "padding: 10px 12px;",
              "font-weight: 700;",
              "border-right: 1px solid ", border_col, ";",
              "border-bottom: 1px solid ", border_col, ";",
              "background: #f8fafc;",
              "position: sticky; top: 0; z-index: 2;"
            ),
            "Original"
          ),
          shiny::tags$div(
            style = paste0(
              "padding: 10px 12px;",
              "font-weight: 700;",
              "border-bottom: 1px solid ", border_col, ";",
              "background: #f8fafc;",
              "position: sticky; top: 0; z-index: 2;"
            ),
            "DwC"
          ),

          lapply(keys, function(k) {
            col <- key_map[[k]]

            init_term <- ""
            if (!is.null(rv$mapping) &&
              is.data.frame(rv$mapping) &&
              all(c("user_column", "dwc_term") %in% names(rv$mapping))) {
              i <- match(col, rv$mapping$user_column)
              if (!is.na(i)) init_term <- rv$mapping$dwc_term[i] %||% ""
            }

            tip_trigger <- shiny::tags$span(
              shiny::icon("info-circle"),
              style = "cursor: help; opacity: 0.85; margin-left: 10px;"
            )

            shiny::tagList(
              shiny::tags$div(
                style = paste0(
                  "padding: 12px;",
                  "font-weight: 600;",
                  "border-right: 1px solid ", border_col, ";",
                  "border-bottom: 1px solid ", border_col, ";"
                ),
                col
              ),

              shiny::tags$div(
                style = paste0(
                  "padding: 12px;",
                  "border-bottom: 1px solid ", border_col, ";"
                ),
                shiny::tags$div(
                  style = "display:flex; align-items:center;",
                  shiny::tags$div(
                    style = "flex:1; max-width: 680px;",
                    shiny::selectizeInput(
                      inputId = session$ns(paste0("map__", k)),
                      label = NULL,
                      choices = dwc_terms,
                      selected = init_term,
                      options = list(
                        placeholder = "Select a Darwin Core term (or leave blank)",
                        dropdownParent = "body"
                      )
                    )
                  ),
                  bslib::tooltip(
                    trigger = tip_trigger,
                    shiny::uiOutput(session$ns(paste0("tip__", k))),
                    placement = "right"
                  )
                )
              )
            )
          })
        )
      )
    })

    shiny::observeEvent(input$auto_suggest, {
      df <- df_in()
      shiny::req(df)

      key_map <- col_key_map()
      keys <- names(key_map)

      for (k in keys) {
        col <- key_map[[k]]
        sug <- .suggest_term(col, dwc_terms)

        shiny::updateSelectizeInput(
          session,
          paste0("map__", k),
          selected = sug
        )
      }
    })

    shiny::observeEvent(input$apply_mapping, {
      df <- df_in()
      shiny::req(df)

      map_df <- mapping_tbl()
      rv$mapping <- map_df
      rv$mapped <- .apply_mapping(df, map_df)

      if (!exists("clean_dwc_pipeline", mode = "function")) {
        rv$cleaned <- rv$mapped
        rv$issues <- data.frame()
        rv$clean_summary <- data.frame()
      } else {
        epsg_in <- input$coord_epsg_in %||% 4326
        force_parse <- isTRUE(input$coord_cols_are_messy)

        clean_res <- tryCatch(
          clean_dwc_pipeline(
            rv$mapped,
            coord_epsg_in = epsg_in,
            target_epsg = 4326,
            force_parzer = force_parse
          ),
          error = function(e) e
        )

        if (inherits(clean_res, "error")) {
          rv$cleaned <- rv$mapped
          rv$issues <- data.frame(
            row = NA_integer_,
            field = "pipeline",
            rule = "clean_dwc_pipeline_error",
            severity = "ERROR",
            message = clean_res$message,
            stringsAsFactors = FALSE
          )
          rv$clean_summary <- data.frame(
            severity = "ERROR",
            n = 1L,
            stringsAsFactors = FALSE
          )
        } else {
          rv$cleaned <- clean_res$data
          rv$issues <- clean_res$issues
          rv$clean_summary <- clean_res$summary
        }
      }

      if (requireNamespace("corella", quietly = TRUE)) {
        rv$corella_checks <- tryCatch(
          corella::check_dataset(rv$mapped),
          error = function(e) e
        )

        rv$corella_suggest <- tryCatch(
          corella::suggest_workflow(rv$mapped),
          error = function(e) e
        )
      }
      shiny::isolate(cat("tab atual (antes):", input$main_tabs, "\n"))

      session$onFlushed(function() {
        bslib::nav_select(
          id = "main_tabs",
          selected = "preview_cleaned",
          session = session
        )

        shiny::isolate(cat("tab atual (depois):", input$main_tabs, "\n"))
      }, once = TRUE)

    })

    output$validation <- shiny::renderText({
      basic <- .validate_mapping_basic(mapping_tbl())

      if (is.null(rv$mapped)) {
        return(paste(basic, collapse = "\n"))
      }

      extra <- character(0)
      if (!is.null(rv$corella_checks)) {
        extra <- c(extra, "", .corella_summary(rv$corella_checks, "corella::check_dataset()"))
      }
      if (!is.null(rv$corella_suggest)) {
        extra <- c(extra, "", .corella_summary(rv$corella_suggest, "corella::suggest_workflow()"))
      }

      paste(c(basic, extra), collapse = "\n")
    })

    output$cleaning_summary <- shiny::renderText({
      if (is.null(rv$mapped)) {
        return("Cleaning: not run yet (apply mapping first).")
      }

      s <- rv$clean_summary
      if (is.null(s) || !is.data.frame(s) || nrow(s) == 0) {
        return("Cleaning: no issues detected (or summary unavailable).")
      }

      lines <- apply(s, 1, function(r) paste0(r[[1]], ": ", r[[2]]))
      paste(c("Cleaning summary:", lines), collapse = "\n")
    })

    output$preview_mapped_tbl <- DT::renderDT({
      shiny::req(rv$mapped)
      DT::datatable(
        utils::head(rv$mapped, 50),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
      )
    })

    output$preview_cleaned_tbl <- DT::renderDT({
      shiny::req(rv$cleaned)
      DT::datatable(
        utils::head(rv$cleaned, 50),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
      )
    })

    output$issues_tbl <- DT::renderDT({
      iss <- rv$issues
      if (is.null(iss) || !is.data.frame(iss) || nrow(iss) == 0) {
        iss <- data.frame(
          row = integer(),
          field = character(),
          rule = character(),
          severity = character(),
          message = character(),
          stringsAsFactors = FALSE
        )
      }

      DT::datatable(
        iss,
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          scrollY = "320px",
          pageLength = 10,
          order = list(list(0, "asc"))
        )
      )
    })

    output$download_issues_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("dwc_cleaning_issues_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        iss <- rv$issues
        if (is.null(iss) || !is.data.frame(iss)) iss <- data.frame()
        utils::write.csv(iss, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    list(
      mapping = shiny::reactive(rv$mapping),
      mapped = shiny::reactive(rv$mapped),
      cleaned = shiny::reactive(rv$cleaned),
      issues = shiny::reactive(rv$issues),
      clean_summary = shiny::reactive(rv$clean_summary),
      validation = shiny::reactive(.validate_mapping_basic(mapping_tbl()))
    )
  })
}

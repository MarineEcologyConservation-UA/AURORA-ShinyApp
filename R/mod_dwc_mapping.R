# modules/mod_dwc_mapping.R

#' Darwin Core mapping module UI
#'
#' Shiny module UI for mapping user columns to Darwin Core terms.
#'
#' @param id Module id.
#' @return A Shiny UI definition.
#' @export
mod_dwc_mapping_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .dwc-map-page {
        padding: 1rem 1.25rem 2rem 1.25rem;
        min-width: 0;
      }

      .dwc-map-page h2 {
        margin-bottom: 0.5rem;
      }

      .dwc-map-page .card {
        border-radius: 0.9rem;
      }

      .dwc-map-page .card-header {
        font-weight: 600;
      }

      .dwc-map-page .btn {
        padding: 0.6rem 1rem;
        font-size: 0.98rem;
      }

      .dwc-map-page .form-control,
      .dwc-map-page .form-select,
      .dwc-map-page .selectize-input {
        font-size: 0.98rem;
      }

      .dwc-map-page .nav-tabs .nav-link {
        font-size: 1rem;
        padding: 0.85rem 1rem;
      }

      .dwc-map-page .alert {
        margin-bottom: 0.75rem;
      }

      .dwc-map-sidebar .btn,
      .dwc-map-sidebar .download-btn {
        width: 100%;
      }

      .dwc-map-sidebar .card + .card {
        margin-top: 1rem;
      }

      .dwc-map-section-gap {
        height: 1rem;
      }

      .dwc-map-section-gap-sm {
        height: 0.75rem;
      }

      .dwc-map-muted {
        color: #6c757d;
      }

      .dwc-map-mapping-wrap {
        max-height: 600px;
        min-height: 420px;
        overflow-y: auto;
        overflow-x: hidden;
        padding-right: 8px;
      }

      .dwc-map-grid {
        display: grid;
        grid-template-columns: minmax(220px, 1fr) minmax(360px, 2fr);
        gap: 0;
        width: 100%;
        border: 1px solid #cbd5e1;
        border-radius: 10px;
        overflow: hidden;
        background: #fff;
      }

      .dwc-map-grid-head {
        padding: 10px 12px;
        font-weight: 700;
        background: #f8fafc;
        border-bottom: 1px solid #cbd5e1;
        position: sticky;
        top: 0;
        z-index: 2;
      }

      .dwc-map-grid-head-left {
        border-right: 1px solid #cbd5e1;
      }

      .dwc-map-grid-cell-left {
        padding: 12px;
        font-weight: 600;
        border-right: 1px solid #cbd5e1;
        border-bottom: 1px solid #cbd5e1;
        word-break: break-word;
      }

      .dwc-map-grid-cell-right {
        padding: 12px;
        border-bottom: 1px solid #cbd5e1;
      }

      .dwc-map-grid-inputrow {
        display: flex;
        align-items: center;
        gap: 10px;
      }

      .dwc-map-grid-inputcol {
        flex: 1;
        min-width: 0;
        max-width: 680px;
      }

      .dwc-map-tip-trigger {
        cursor: help;
        opacity: 0.85;
        flex: 0 0 auto;
      }

      .dwc-map-status p {
        margin-bottom: 0.45rem;
      }

      .dwc-map-validation-card .card-body,
      .dwc-map-tab-card .card-body {
        overflow: visible !important;
      }
    ")),

    bslib::layout_sidebar(
      fillable = FALSE,

      sidebar = bslib::sidebar(
        width = "280px",
        open = "desktop",
        class = "dwc-map-sidebar",

        bslib::card(
          fill = FALSE,
          bslib::card_header("Actions"),
          shiny::actionButton(
            ns("auto_suggest"),
            "Auto-suggest",
            icon = shiny::icon("magic"),
            class = "btn-primary"
          ),
          shiny::div(class = "dwc-map-section-gap"),
          shiny::downloadButton(
            ns("download_issues_csv"),
            "Download issues (CSV)",
            class = "btn-outline-secondary download-btn"
          )
        ),

        bslib::card(
          fill = FALSE,
          bslib::card_header("Workflow status"),
          shiny::div(
            class = "dwc-map-status",
            shiny::uiOutput(ns("workflow_status"))
          )
        )
      ),

      shiny::div(
        class = "dwc-map-page",

        shiny::div(
          class = "mb-4",
          shiny::h2("Field mapping to Darwin Core"),
          shiny::p(
            class = "dwc-map-muted",
            "Configure formatting options, map columns to Darwin Core terms, ",
            "create derived fields, and review the final cleaned dataset."
          ),
          shiny::tags$p(
            shiny::tags$a(
              href = "https://dwc.tdwg.org/terms/",
              target = "_blank",
              "Darwin Core Quick Reference Guide"
            )
          )
        ),

        bslib::card(
          fill = FALSE,
          class = "mb-4 dwc-map-validation-card",
          bslib::card_header("Validation"),
          shiny::uiOutput(ns("validation_ui"))
        ),

        bslib::card(
          fill = FALSE,
          class = "dwc-map-tab-card",

          bslib::navset_tab(
            id = ns("main_tabs"),

            bslib::nav_panel(
              "Formatting",
              value = "formatting",

              shiny::div(
                class = "p-3",

                bslib::layout_columns(
                  col_widths = c(6, 6),

                  bslib::card(
                    fill = FALSE,
                    bslib::card_header("Coordinate system"),
                    shiny::p(
                      "Set the input CRS (EPSG). The target CRS will always be EPSG:[4326] (WGS84 lon/lat)."
                    ),
                    shiny::numericInput(
                      ns("coord_epsg_in"),
                      label = "Input CRS EPSG:[ ]",
                      value = 4326,
                      min = 1,
                      width = "100%"
                    ),
                    shiny::checkboxInput(
                      ns("coord_cols_are_messy"),
                      label = "Force parsing (parzer) even if coordinates look numeric",
                      value = TRUE
                    ),
                    shiny::checkboxInput(
                      ns("preserve_original_coords"),
                      label = "Preserve original coordinates in verbatimLatitude / verbatimLongitude",
                      value = TRUE
                    ),
                    shiny::checkboxInput(
                      ns("create_geodetic_datum"),
                      label = "Create/fill geodeticDatum",
                      value = TRUE
                    ),
                    shiny::textInput(
                      ns("geodetic_datum_value"),
                      label = "geodeticDatum value",
                      value = "WGS84",
                      width = "100%"
                    ),
                    shiny::tags$hr(),
                    shiny::tags$b("Target CRS EPSG:[4326]"),
                    shiny::p("Coordinates will be stored in decimal degrees.")
                  ),

                  bslib::card(
                    fill = FALSE,
                    bslib::card_header("Date formatting"),
                    shiny::checkboxInput(
                      ns("standardize_event_date"),
                      label = "Standardize eventDate to ISO-8601",
                      value = TRUE
                    ),
                    shiny::p(
                      class = "text-muted",
                      "eventDate will be normalized during the cleaning step."
                    )
                  )
                ),

                shiny::div(class = "dwc-map-section-gap"),

                shiny::actionButton(
                  ns("formatting_done"),
                  "Continue to Mapping",
                  icon = shiny::icon("arrow-right"),
                  class = "btn-primary"
                )
              )
            ),

            bslib::nav_panel(
              "Mapping",
              value = "mapping",

              shiny::div(
                class = "p-3",
                shiny::h4("Map each column to a Darwin Core term"),
                shiny::uiOutput(ns("mapping_ui")),
                shiny::div(class = "dwc-map-section-gap"),
                shiny::actionButton(
                  ns("apply_mapping"),
                  "Apply mapping",
                  icon = shiny::icon("check"),
                  class = "btn-primary"
                ),
                shiny::div(class = "dwc-map-section-gap"),
                shiny::uiOutput(ns("apply_notice"))
              )
            ),

            bslib::nav_panel(
              "Create fields",
              value = "create_fields",

              shiny::div(
                class = "p-3",

                bslib::layout_columns(
                  col_widths = c(6, 6),

                  bslib::card(
                    fill = FALSE,
                    bslib::card_header("Create IDs"),
                    shiny::selectInput(
                      ns("id_target"),
                      "Target field",
                      choices = c("eventID", "parentEventID", "occurrenceID"),
                      selected = "eventID",
                      width = "100%"
                    ),
                    shiny::selectizeInput(
                      ns("id_source_cols"),
                      "Source columns",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE,
                      width = "100%",
                      options = list(plugins = list("remove_button"))
                    ),
                    shiny::textInput(
                      ns("id_sep"),
                      "Separator",
                      value = ":",
                      width = "100%"
                    ),
                    shiny::checkboxInput(
                      ns("id_overwrite"),
                      "Overwrite target if it already exists",
                      value = FALSE
                    ),
                    shiny::actionButton(
                      ns("create_id_btn"),
                      "Create / update ID field",
                      icon = shiny::icon("plus")
                    )
                  ),

                  bslib::card(
                    fill = FALSE,
                    bslib::card_header("Create remarks"),
                    shiny::selectInput(
                      ns("remarks_target"),
                      "Remarks field",
                      choices = c(
                        "eventRemarks",
                        "occurrenceRemarks",
                        "organismRemarks",
                        "materialEntityRemarks",
                        "locationRemarks",
                        "georeferenceRemarks",
                        "identificationRemarks",
                        "taxonRemarks",
                        "measurementRemarks",
                        "relationshipRemarks"
                      ),
                      selected = "occurrenceRemarks",
                      width = "100%"
                    ),
                    shiny::selectizeInput(
                      ns("remarks_source_cols"),
                      "Source columns",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE,
                      width = "100%",
                      options = list(plugins = list("remove_button"))
                    ),
                    shiny::textInput(
                      ns("remarks_sep"),
                      "Separator",
                      value = " | ",
                      width = "100%"
                    ),
                    shiny::checkboxInput(
                      ns("remarks_overwrite"),
                      "Overwrite target if it already exists",
                      value = FALSE
                    ),
                    shiny::actionButton(
                      ns("create_remarks_btn"),
                      "Create / update remarks field",
                      icon = shiny::icon("plus")
                    )
                  )
                ),

                shiny::div(class = "dwc-map-section-gap"),

                bslib::card(
                  fill = FALSE,
                  bslib::card_header("Companion fields"),
                  shiny::uiOutput(ns("dependency_ui"))
                ),

                shiny::div(class = "dwc-map-section-gap"),

                shiny::actionButton(
                  ns("apply_create_fields"),
                  "Apply created fields",
                  icon = shiny::icon("check"),
                  class = "btn-primary"
                ),
                shiny::div(class = "dwc-map-section-gap-sm"),
                shiny::actionButton(
                  ns("complete_mapping"),
                  "Complete field mapping",
                  icon = shiny::icon("lock-open"),
                  class = "btn-success"
                )
              )
            ),

            bslib::nav_panel(
              "Preview",
              value = "preview",

              shiny::div(
                class = "p-3",
                shiny::p("Preview of the cleaned dataset after formatting, mapping, and create-fields."),
                DT::DTOutput(ns("preview_tbl"))
              )
            ),

            bslib::nav_panel(
              "Issues",
              value = "issues",

              shiny::div(
                class = "p-3",
                DT::DTOutput(ns("issues_tbl"))
              )
            )
          )
        )
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

# termos mínimos para desbloquear os módulos seguintes
.dwc_gate_terms <- function() {
  c("eventDate", "occurrenceID", "basisOfRecord", "scientificName")
}

# termos mostrados na validação, além do gate
.dwc_validation_extra_terms <- function() {
  c("eventID")
}

# termos "fortemente recomendados" definidos localmente na app
.dwc_strongly_recommended_terms <- function() {
  c("decimalLatitude", "decimalLongitude", "geodeticDatum")
}

# dependências genéricas entre termos
# um termo "trigger_term" implica ou recomenda fortemente "required_term"
# suggested_value é apenas sugestão opcional para criação rápida
.dwc_term_dependencies <- function() {
  data.frame(
    trigger_term = c(
      "sampleSizeValue",
      "organismQuantity"
    ),
    required_term = c(
      "sampleSizeUnit",
      "organismQuantityType"
    ),
    relation = c(
      "required_with",
      "required_with"
    ),
    suggested_value = c(
      "square meters",
      "individuals"
    ),
    stringsAsFactors = FALSE
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

.validation_state <- function(map_df, final_df = NULL) {
  gate_terms <- .dwc_gate_terms()
  extra_terms <- .dwc_validation_extra_terms()
  strong_terms <- .dwc_strongly_recommended_terms()

  mapped <- map_df$dwc_term %||% character(0)
  mapped <- mapped[!is.na(mapped) & mapped != ""]

  missing_gate <- setdiff(gate_terms, mapped)
  missing_extra <- setdiff(extra_terms, mapped)
  missing_strong <- setdiff(strong_terms, mapped)

  dup <- mapped[duplicated(mapped)]
  dup <- unique(dup)

  dep_msgs <- character(0)
  if (!is.null(final_df) && is.data.frame(final_df)) {
    deps <- .detect_missing_dependencies(final_df)
    if (nrow(deps) > 0) {
      dep_msgs <- paste0(
        deps$trigger_term, " is present but ", deps$required_term, " is missing."
      )
    }
  }

  list(
    missing_gate = missing_gate,
    missing_extra = missing_extra,
    missing_strong = missing_strong,
    duplicate_terms = dup,
    dependency_messages = dep_msgs
  )
}

.validation_ui_block <- function(v) {
  make_alert <- function(type, title, items = NULL, text = NULL) {
    shiny::tags$div(
      class = paste("alert", paste0("alert-", type), "mb-2"),
      style = "padding: 0.75rem 1rem;",
      shiny::tags$b(title),
      if (!is.null(text)) shiny::tags$div(text),
      if (!is.null(items) && length(items) > 0) {
        shiny::tags$ul(
          style = "margin-top: 0.35rem; margin-bottom: 0;",
          lapply(items, shiny::tags$li)
        )
      }
    )
  }

  tag_list <- list()

  if (length(v$missing_gate) > 0) {
    tag_list <- c(tag_list, list(
      make_alert(
        "danger",
        "Missing minimum terms required to unlock next modules:",
        v$missing_gate
      )
    ))
  } else {
    tag_list <- c(tag_list, list(
      make_alert("success", "Minimum required terms are present.")
    ))
  }

  if (length(v$missing_extra) > 0) {
    tag_list <- c(tag_list, list(
      make_alert(
        "warning",
        "Additional validation term(s) missing:",
        v$missing_extra
      )
    ))
  }

  if (length(v$missing_strong) > 0) {
    tag_list <- c(tag_list, list(
      make_alert(
        "warning",
        "Strongly recommended term(s) missing:",
        v$missing_strong
      )
    ))
  }

  if (length(v$duplicate_terms) > 0) {
    tag_list <- c(tag_list, list(
      make_alert(
        "danger",
        "Duplicate mappings are not allowed:",
        v$duplicate_terms
      )
    ))
  }

  if (length(v$dependency_messages) > 0) {
    tag_list <- c(tag_list, list(
      make_alert(
        "warning",
        "Companion field warnings:",
        v$dependency_messages
      )
    ))
  }

  if (length(tag_list) == 0) {
    return(make_alert("light", "No validation messages."))
  }

  shiny::tagList(tag_list)
}

.detect_missing_dependencies <- function(df) {
  deps <- .dwc_term_dependencies()
  if (is.null(df) || !is.data.frame(df) || nrow(deps) == 0) {
    return(deps[0, , drop = FALSE])
  }

  nms <- names(df)

  keep <- vapply(
    seq_len(nrow(deps)),
    function(i) {
      deps$trigger_term[i] %in% nms && !(deps$required_term[i] %in% nms)
    },
    logical(1)
  )

  deps[keep, , drop = FALSE]
}

.build_companion_choice_label <- function(trigger_term, required_term) {
  paste0(trigger_term, " -> ", required_term)
}

.add_constant_if_missing <- function(df, field, value, overwrite = FALSE) {
  if (field %in% names(df) && !isTRUE(overwrite)) {
    return(df)
  }
  df[[field]] <- value
  df
}

.build_concat_field <- function(df, target, source_cols, sep = ":", overwrite = FALSE) {
  if (length(source_cols) == 0) return(df)
  if (target %in% names(df) && !isTRUE(overwrite)) return(df)

  vals <- lapply(source_cols, function(nm) as.character(df[[nm]]))
  mat <- as.data.frame(vals, stringsAsFactors = FALSE)

  out_val <- apply(mat, 1, function(row) {
    row <- trimws(as.character(row))
    row <- row[!is.na(row) & row != ""]
    if (length(row) == 0) return(NA_character_)
    paste(row, collapse = sep)
  })

  df[[target]] <- out_val
  df
}

#' Darwin Core mapping module server
#'
#' @param id Module id.
#' @param df_in Reactive returning a data.frame.
#' @return A list of reactives: mapping, mapped, cleaned, issues,
#'   clean_summary, validation, ready.
#' @export
mod_dwc_mapping_server <- function(id, df_in) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(
      formatting_done = FALSE,
      mapping = NULL,
      mapped = NULL,
      created = NULL,
      cleaned = NULL,
      issues = NULL,
      clean_summary = NULL,
      corella_checks = NULL,
      corella_suggest = NULL,
      ready = FALSE
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

    current_preclean_df <- shiny::reactive({
      if (!is.null(rv$created)) return(rv$created)
      if (!is.null(rv$mapped)) return(rv$mapped)
      NULL
    })

    current_validation <- shiny::reactive({
      .validation_state(mapping_tbl(), current_preclean_df())
    })

    run_clean_pipeline <- function(df_to_clean) {
      if (!exists("clean_dwc_pipeline", mode = "function")) {
        rv$cleaned <- df_to_clean
        rv$issues <- data.frame()
        rv$clean_summary <- data.frame()
        return(invisible(NULL))
      }

      epsg_in <- input$coord_epsg_in %||% 4326
      force_parse <- isTRUE(input$coord_cols_are_messy)
      preserve_original_coords_flag = isTRUE(input$preserve_original_coords)
      create_geodetic_datum_flag = isTRUE(input$create_geodetic_datum)
      geodetic_datum_value_flag = input$geodetic_datum_value %||% "WGS84"
      standardize_event_date_flag = isTRUE(input$standardize_event_date)

      clean_res <- tryCatch(
        clean_dwc_pipeline(
          df_to_clean,
          coord_epsg_in = epsg_in,
          target_epsg = 4326,
          force_parzer = force_parse,
          preserve_original_coords = preserve_original_coords_flag,
          create_geodetic_datum = create_geodetic_datum_flag,
          geodetic_datum_value = geodetic_datum_value_flag,
          standardize_event_date = standardize_event_date_flag
        ),
        error = function(e) e
      )

      if (inherits(clean_res, "error")) {
        rv$cleaned <- df_to_clean
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

    shiny::observeEvent(df_in(), {
      rv$formatting_done <- FALSE
      rv$mapping <- NULL
      rv$mapped <- NULL
      rv$created <- NULL
      rv$cleaned <- NULL
      rv$issues <- NULL
      rv$clean_summary <- NULL
      rv$corella_checks <- NULL
      rv$corella_suggest <- NULL
      rv$ready <- FALSE
    }, ignoreInit = FALSE)

    shiny::observeEvent(df_in(), {
      df <- df_in()
      shiny::req(df)

      cols <- names(df)

      shiny::updateSelectizeInput(
        session,
        "id_source_cols",
        choices = cols,
        selected = character(0),
        server = TRUE
      )

      shiny::updateSelectizeInput(
        session,
        "remarks_source_cols",
        choices = cols,
        selected = character(0),
        server = TRUE
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(current_preclean_df(), {
      df <- current_preclean_df()
      if (is.null(df)) return()

      cols <- names(df)

      shiny::updateSelectizeInput(
        session,
        "id_source_cols",
        choices = cols,
        server = TRUE
      )

      shiny::updateSelectizeInput(
        session,
        "remarks_source_cols",
        choices = cols,
        server = TRUE
      )
    }, ignoreInit = TRUE)

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
              if (term == "") {
                "Select a term to see definition and examples."
              } else {
                "No metadata found for this term in corella."
              }
            } else {
              .term_tooltip_html(term, meta_row[1, ])
            }

            htmltools::HTML(html)
          })
        })
      }
    }, ignoreInit = FALSE)

    output$workflow_status <- shiny::renderUI({
      shiny::tagList(
        shiny::tags$p(
          shiny::tags$b("Formatting completed: "),
          if (isTRUE(rv$formatting_done)) "Yes" else "No"
        ),
        shiny::tags$p(
          shiny::tags$b("Mapping applied: "),
          if (!is.null(rv$mapped)) "Yes" else "No"
        ),
        shiny::tags$p(
          shiny::tags$b("Create fields applied: "),
          if (!is.null(rv$created)) "Yes" else "No"
        ),
        shiny::tags$p(
          shiny::tags$b("Field mapping complete: "),
          if (isTRUE(rv$ready)) "Yes" else "No"
        )
      )
    })

    output$validation_ui <- shiny::renderUI({
      .validation_ui_block(current_validation())
    })

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

    output$mapping_ui <- shiny::renderUI({
      df <- df_in()
      shiny::req(df)

      key_map <- col_key_map()
      keys <- names(key_map)

      shiny::tags$div(
        class = "dwc-map-mapping-wrap",
        shiny::tags$div(
          class = "dwc-map-grid",

          shiny::tags$div(
            class = "dwc-map-grid-head dwc-map-grid-head-left",
            "Original"
          ),
          shiny::tags$div(
            class = "dwc-map-grid-head",
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
              class = "dwc-map-tip-trigger"
            )

            shiny::tagList(
              shiny::tags$div(
                class = "dwc-map-grid-cell-left",
                col
              ),

              shiny::tags$div(
                class = "dwc-map-grid-cell-right",
                shiny::tags$div(
                  class = "dwc-map-grid-inputrow",
                  shiny::tags$div(
                    class = "dwc-map-grid-inputcol",
                    shiny::selectizeInput(
                      inputId = session$ns(paste0("map__", k)),
                      label = NULL,
                      choices = dwc_terms,
                      selected = init_term,
                      width = "100%",
                      options = list(
                        placeholder = "Select a Darwin Core term (or leave blank)"
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

    output$dependency_ui <- shiny::renderUI({
      df <- current_preclean_df()

      if (is.null(df)) {
        return(
          shiny::tags$div(
            class = "alert alert-light",
            "Apply mapping first to detect companion fields."
          )
        )
      }

      deps <- .detect_missing_dependencies(df)

      if (nrow(deps) == 0) {
        return(
          shiny::tags$div(
            class = "alert alert-light",
            "No companion fields are currently suggested."
          )
        )
      }

      choice_values <- paste0(deps$trigger_term, "||", deps$required_term)
      choice_labels <- mapply(
        .build_companion_choice_label,
        deps$trigger_term,
        deps$required_term,
        USE.NAMES = FALSE
      )

      default_suggestion <- deps$suggested_value[1] %||% ""

      shiny::tagList(
        shiny::p(
          "Some mapped or created terms imply the presence of a companion field. ",
          "Select one dependency below and create the missing field with a constant value."
        ),
        shiny::selectInput(
          session$ns("dependency_choice"),
          "Dependency",
          choices = stats::setNames(choice_values, choice_labels),
          selected = choice_values[1]
        ),
        shiny::textInput(
          session$ns("dependency_value"),
          "Value to fill the missing companion field",
          value = default_suggestion
        ),
        shiny::actionButton(
          session$ns("create_dependency_btn"),
          "Create companion field",
          icon = shiny::icon("plus")
        )
      )
    })

    shiny::observeEvent(input$dependency_choice, {
      df <- current_preclean_df()
      if (is.null(df)) return()

      deps <- .detect_missing_dependencies(df)
      if (nrow(deps) == 0) return()

      key <- input$dependency_choice %||% ""
      parts <- strsplit(key, "\\|\\|")[[1]]

      if (length(parts) != 2) return()

      i <- which(
        deps$trigger_term == parts[1] &
          deps$required_term == parts[2]
      )

      if (length(i) == 1) {
        shiny::updateTextInput(
          session,
          "dependency_value",
          value = deps$suggested_value[i] %||% ""
        )
      }
    }, ignoreInit = TRUE)

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

    shiny::observeEvent(input$formatting_done, {
      rv$formatting_done <- TRUE
      bslib::nav_select(
        id = "main_tabs",
        selected = "mapping",
        session = session
      )
    })

    shiny::observeEvent(input$apply_mapping, {
      df <- df_in()
      shiny::req(df)

      map_df <- mapping_tbl()
      rv$mapping <- map_df
      rv$mapped <- .apply_mapping(df, map_df)
      rv$created <- NULL
      rv$ready <- FALSE

      run_clean_pipeline(rv$mapped)

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

      bslib::nav_select(
        id = "main_tabs",
        selected = "create_fields",
        session = session
      )
    })

    shiny::observeEvent(input$create_id_btn, {
      df <- current_preclean_df()
      shiny::req(df)
      shiny::req(input$id_target)
      shiny::req(input$id_source_cols)

      rv$created <- .build_concat_field(
        df = df,
        target = input$id_target,
        source_cols = input$id_source_cols,
        sep = input$id_sep %||% ":",
        overwrite = isTRUE(input$id_overwrite)
      )
      rv$ready <- FALSE
    })

    shiny::observeEvent(input$create_remarks_btn, {
      df <- current_preclean_df()
      shiny::req(df)
      shiny::req(input$remarks_target)
      shiny::req(input$remarks_source_cols)

      rv$created <- .build_concat_field(
        df = df,
        target = input$remarks_target,
        source_cols = input$remarks_source_cols,
        sep = input$remarks_sep %||% " | ",
        overwrite = isTRUE(input$remarks_overwrite)
      )
      rv$ready <- FALSE
    })

    shiny::observeEvent(input$create_dependency_btn, {
      df <- current_preclean_df()
      shiny::req(df)

      deps <- .detect_missing_dependencies(df)
      shiny::req(nrow(deps) > 0)

      key <- input$dependency_choice %||% ""
      parts <- strsplit(key, "\\|\\|")[[1]]

      shiny::req(length(parts) == 2)

      dep_row <- deps[
        deps$trigger_term == parts[1] &
          deps$required_term == parts[2],
        ,
        drop = FALSE
      ]

      shiny::req(nrow(dep_row) == 1)

      rv$created <- .add_constant_if_missing(
        df = df,
        field = dep_row$required_term[1],
        value = input$dependency_value %||% dep_row$suggested_value[1] %||% "",
        overwrite = FALSE
      )
      rv$ready <- FALSE
    })

    shiny::observeEvent(input$apply_create_fields, {
      df_final <- current_preclean_df()
      shiny::req(df_final)

      run_clean_pipeline(df_final)
      rv$ready <- FALSE

      bslib::nav_select(
        id = "main_tabs",
        selected = "preview",
        session = session
      )
    })

    shiny::observeEvent(input$complete_mapping, {
      shiny::req(rv$formatting_done)
      shiny::req(!is.null(rv$mapped))

      missing_gate <- current_validation()$missing_gate

      if (length(missing_gate) > 0) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Cannot complete field mapping yet",
            shiny::p("The following minimum terms are still missing:"),
            shiny::tags$ul(lapply(missing_gate, shiny::tags$li)),
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          )
        )
        rv$ready <- FALSE
        return()
      }

      if (is.null(rv$cleaned)) {
        df_final <- current_preclean_df()
        shiny::req(df_final)
        run_clean_pipeline(df_final)
      }

      rv$ready <- TRUE

      shiny::showModal(
        shiny::modalDialog(
          title = "Field mapping completed",
          shiny::p("Minimum terms are present. The next modules can now be unlocked."),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
    })

    output$preview_tbl <- DT::renderDT({
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
      validation = current_validation,
      ready = shiny::reactive(isTRUE(rv$ready))
    )
  })
}
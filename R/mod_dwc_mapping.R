# modules/mod_dwc_mapping.R

#' Darwin Core mapping module UI
#'
#' Shiny module UI for mapping user columns to DwC terms.
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
        overflow: visible;
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
        overflow: visible;
      }

      .dwc-map-grid-inputrow {
        display: flex;
        align-items: center;
        gap: 10px;
        overflow: visible;
      }

      .dwc-map-grid-inputcol {
        flex: 1;
        min-width: 0;
        max-width: 680px;
        overflow: visible;
      }

      .dwc-map-tip-trigger {
        cursor: help;
        opacity: 0.85;
        flex: 0 0 auto;
      }

      .dwc-map-status p {
        margin-bottom: 0.45rem;
      }

      .dwc-map-validation-card,
      .dwc-map-tab-card,
      .dwc-map-validation-card .card-body,
      .dwc-map-tab-card .card-body,
      .dwc-map-tab-card .tab-content,
      .dwc-map-tab-card .tab-pane {
        overflow: visible !important;
      }

      .dwc-map-inline-help {
        margin-top: -0.35rem;
        margin-bottom: 0.6rem;
        color: #6c757d;
        font-size: 0.92rem;
      }

      .dwc-map-preview-wrap .dataTables_wrapper {
        width: 100%;
      }

      .dwc-map-bor-note {
        color: #6c757d;
        font-size: 0.92rem;
        margin-top: 0.5rem;
      }

      .dwc-map-date-help {
        margin-top: 0.5rem;
        color: #6c757d;
        font-size: 0.92rem;
      }

      .dwc-map-repeat-note {
        background: #f8fafc;
        border: 1px solid #e2e8f0;
        border-radius: 0.75rem;
        padding: 0.85rem 1rem;
        margin-bottom: 1rem;
        color: #475569;
      }

      .dwc-map-repeat-note p:last-child {
        margin-bottom: 0;
      }

      .dwc-map-mapping-actions {
        margin-bottom: 1rem;
      }

      .selectize-dropdown {
        z-index: 5000 !important;
      }
    ")),

    bslib::layout_sidebar(
      fillable = FALSE,

      sidebar = bslib::sidebar(
        width = "320px",
        open = "desktop",
        class = "dwc-map-sidebar",

        bslib::card(
          fill = FALSE,
          bslib::card_header("Target data repository"),
          shiny::selectInput(
            ns("target_database"),
            "Data repository",
            choices = c("GBIF", "OBIS", "EMODnet"),
            selected = "GBIF",
            width = "100%"
          ),
          shiny::div(
            class = "dwc-map-inline-help",
            "Required and strongly recommended terms below follow the selected database."
          )
        ),

        bslib::card(
          fill = FALSE,
          bslib::card_header("Actions"),
          shiny::downloadButton(
            ns("download_issues_csv"),
            "Download Warning/Issues (CSV)",
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
            "Map columns to Darwin Core terms, create derived fields, configure formatting options, and review the cleaned dataset."
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
              "Mapping",
              value = "mapping",

              shiny::div(
                class = "p-3",
                shiny::h4("Map each column to a Darwin Core term"),

                shiny::div(
                  class = "dwc-map-mapping-actions",
                  shiny::actionButton(
                    ns("auto_suggest"),
                    "Auto-suggest",
                    icon = shiny::icon("magic"),
                    class = "btn-primary"
                  )
                ),

                shiny::uiOutput(ns("mapping_export_note")),
                shiny::uiOutput(ns("scientific_name_note")),
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

                shiny::div(
                  class = "dwc-map-repeat-note",
                  shiny::tags$p(
                    shiny::tags$b("Note: "),
                    "Create IDs and Create remarks can be used more than once."
                  )
                ),

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
                      options = list(
                        plugins = list("remove_button"),
                        dropdownParent = "body"
                      )
                    ),
                    shiny::textInput(
                      ns("id_sep"),
                      "Separator",
                      value = ":",
                      width = "100%"
                    ),
                    shiny::uiOutput(ns("id_mode_ui")),
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
                      options = list(
                        plugins = list("remove_button"),
                        dropdownParent = "body"
                      )
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

                bslib::layout_columns(
                  col_widths = c(6, 6),

                  bslib::card(
                    fill = FALSE,
                    bslib::card_header("Create / fill basisOfRecord"),
                    shiny::selectInput(
                      ns("basis_of_record_value"),
                      "basisOfRecord value",
                      choices = c(
                        "MaterialEntity",
                        "PreservedSpecimen",
                        "FossilSpecimen",
                        "LivingSpecimen",
                        "MaterialSample",
                        "Event",
                        "HumanObservation",
                        "MachineObservation",
                        "Taxon",
                        "Occurrence",
                        "MaterialCitation"
                      ),
                      selected = "HumanObservation",
                      width = "100%"
                    ),
                    shiny::checkboxInput(
                      ns("basis_of_record_overwrite"),
                      "Overwrite existing basisOfRecord values",
                      value = FALSE
                    ),
                    shiny::actionButton(
                      ns("create_basis_of_record_btn"),
                      "Create / fill basisOfRecord",
                      icon = shiny::icon("plus"),
                      class = "btn-primary"
                    ),
                    shiny::div(
                      class = "dwc-map-bor-note",
                      "Use this only when all records in the dataset share the same basisOfRecord. Otherwise, the field should be filled correctly before import."
                    )
                  ),

                  bslib::card(
                    fill = FALSE,
                    bslib::card_header(
                      shiny::tagList(
                        "organismQuantity and sampleSizeValue Associated Fields ",
                        bslib::tooltip(
                          shiny::tags$span(
                            shiny::icon("info-circle"),
                            style = "cursor: help;"
                          ),
                          "These are fields suggested because another mapped or created field usually requires an associated term."
                        )
                      )
                    ),
                    shiny::uiOutput(ns("dependency_ui"))
                  )
                ),

                shiny::div(class = "dwc-map-section-gap"),

                shiny::actionButton(
                  ns("continue_to_formatting"),
                  "Continue to formatting",
                  icon = shiny::icon("arrow-right"),
                  class = "btn-success"
                )
              )
            ),

            bslib::nav_panel(
              "Formatting",
              value = "formatting",

              shiny::div(
                class = "p-3",

                bslib::layout_columns(
                  col_widths = c(6, 6),

                  bslib::card(
                    fill = FALSE,
                    bslib::card_header("Original coordinate system"),
                    shiny::p(
                      "Identify the original CRS (EPSG) of the dataset. Coordinates will be transformed into EPSG:4326 (WGS 84)."
                    ),
                    shiny::numericInput(
                      ns("coord_epsg_in"),
                      label = "Input CRS EPSG:",
                      value = 4326,
                      min = 1,
                      width = "100%"
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
                    shiny::radioButtons(
                      ns("event_date_order"),
                      label = "Interpret ambiguous numeric dates as",
                      choices = c(
                        "dd/mm" = "dmy",
                        "mm/dd" = "mdy"
                      ),
                      selected = "dmy",
                      inline = FALSE
                    ),
                    shiny::div(
                      class = "dwc-map-date-help",
                      "This option is only used for ambiguous numeric dates such as 01/02/2024. ",
                      "If you choose dd/mm, that value is interpreted as 1 February 2024. ",
                      "If you choose mm/dd, it is interpreted as 2 January 2024. ",
                      "Dates already in ISO format, such as 2024-02-01 or 2019-10-01T22:39:12, are not affected."
                    )
                  )
                ),

                shiny::div(class = "dwc-map-section-gap"),

                shiny::actionButton(
                  ns("apply_formatting"),
                  "Apply formatting and preview",
                  icon = shiny::icon("check"),
                  class = "btn-primary"
                )
              )
            ),

            bslib::nav_panel(
              "Preview",
              value = "preview",

              shiny::div(
                class = "p-3 dwc-map-preview-wrap",
                shiny::p("Preview of the cleaned dataset after mapping, create-fields, and formatting."),
                DT::DTOutput(ns("preview_tbl"))
              )
            ),

            bslib::nav_panel(
              "Warnings/Issues",
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

.dwc_database_profiles <- function() {
  list(
    GBIF = list(
      label = "GBIF",
      required = c(
        "basisOfRecord",
        "eventDate",
        "eventID",
        "occurrenceID",
        "samplingProtocol",
        "scientificName"
      ),
      strongly_recommended = c(
        "decimalLatitude",
        "decimalLongitude",
        "kingdom",
        "taxonRank",
        "sampleSizeUnit",
        "sampleSizeValue",
        "samplingEffort",
        "locationID",
        "organismQuantity",
        "organismQuantityType",
        "parentEventID",
        "footprintWKT",
        "footprintSRS",
        "individualCount"
      ),
      note = "Test"
    ),
    OBIS = list(
      label = "OBIS",
      required = c(
        "basisOfRecord",
        "decimalLatitude",
        "decimalLongitude",
        "eventDate",
        "eventID",
        "occurrenceID",
        "occurrenceStatus",
        "scientificName"
      ),
      strongly_recommended = c(
        "measurementType",
        "measurementTypeID",
        "measurementUnit",
        "measurementValue",
        "scientificNameID",
        "kingdom",
        "taxonRank",
        "sampleSizeUnit",
        "sampleSizeValue",
        "samplingEffort",
        "locationID",
        "organismQuantity",
        "organismQuantityType",
        "countryCode",
        "geodeticDatum",
        "coordinateUncertaintyInMeters",
        "maximumDepthInMeters",
        "minimumDepthInMeters",
        "measurementUnitID",
        "measurementValueID",
        "type",
        "eventType",
        "month",
        "year",
        "continent",
        "coordinatePrecision",
        "license",
        "genus",
        "order"
      ),
      note = "Test"
    ),
    EMODnet = list(
      label = "EMODnet",
      required = c(
        "basisOfRecord",
        "datasetName",
        "decimalLatitude",
        "decimalLongitude",
        "eventDate",
        "eventID",
        "institutionCode",
        "measurementType",
        "measurementTypeID",
        "measurementUnit",
        "measurementValue",
        "occurrenceID",
        "occurrenceStatus",
        "scientificName",
        "scientificNameID"
      ),
      strongly_recommended = c(
        "kingdom",
        "taxonRank",
        "parentEventID",
        "footprintWKT",
        "coordinateUncertaintyInMeters",
        "maximumDepthInMeters",
        "minimumDepthInMeters",
        "measurementUnitID",
        "measurementValueID",
        "type",
        "identificationQualifier",
        "measurementAccuracy",
        "measurementID",
        "measurementRemarks",
        "catalogNumber",
        "collectionCode",
        "modified",
        "scientificNameAuthorship"
      ),
      note = "Test"
    )
  )
}

.get_database_profile <- function(database_name = "GBIF") {
  profiles <- .dwc_database_profiles()
  if (!database_name %in% names(profiles)) database_name <- "GBIF"
  profiles[[database_name]]
}

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

.validation_state <- function(map_df, final_df = NULL, target_database = "GBIF") {
  prof <- .get_database_profile(target_database)

  required_terms <- prof$required %||% character(0)
  strong_terms <- prof$strongly_recommended %||% character(0)

  mapped <- map_df$dwc_term %||% character(0)
  mapped <- mapped[!is.na(mapped) & mapped != ""]

  if (!is.null(final_df) && is.data.frame(final_df)) {
    final_names <- names(final_df)
    all_present <- unique(c(mapped, final_names))
  } else {
    all_present <- unique(mapped)
  }

  missing_required <- setdiff(required_terms, all_present)
  missing_strong <- setdiff(strong_terms, all_present)

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
    target_database = target_database,
    target_database_label = prof$label %||% target_database,
    profile_note = prof$note %||% "",
    required_terms = required_terms,
    strongly_recommended_terms = strong_terms,
    missing_required = missing_required,
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

  if (length(v$missing_required) > 0) {
    tag_list <- c(tag_list, list(
      make_alert(
        "danger",
        "Missing required terms:",
        v$missing_required
      )
    ))
  } else {
    tag_list <- c(tag_list, list(
      make_alert("success", "All required terms for the selected database are present.")
    ))
  }

  if (length(v$missing_strong) > 0) {
    tag_list <- c(tag_list, list(
      make_alert(
        "warning",
        "Strongly recommended terms still missing:",
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

.fill_basis_of_record <- function(df, value, overwrite = FALSE) {
  if (!("basisOfRecord" %in% names(df))) {
    df$basisOfRecord <- NA_character_
  }

  idx_fill <- is.na(df$basisOfRecord) | trimws(as.character(df$basisOfRecord)) == ""
  if (isTRUE(overwrite)) {
    idx_fill <- rep(TRUE, nrow(df))
  }

  if (any(idx_fill)) {
    df$basisOfRecord[idx_fill] <- value
  }

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

.build_sequential_occurrence_id <- function(df,
                                            target = "occurrenceID",
                                            source_cols,
                                            sep = "_",
                                            overwrite = FALSE,
                                            suffix_prefix = "occ") {
  if (length(source_cols) == 0) return(df)
  if (target %in% names(df) && !isTRUE(overwrite)) return(df)

  vals <- lapply(source_cols, function(nm) as.character(df[[nm]]))
  mat <- as.data.frame(vals, stringsAsFactors = FALSE)

  base_id <- apply(mat, 1, function(row) {
    row <- trimws(as.character(row))
    row[row %in% c("", "NA")] <- NA_character_
    row <- row[!is.na(row)]
    if (length(row) == 0) return(NA_character_)
    paste(row, collapse = sep)
  })

  seq_idx <- ave(
    seq_along(base_id),
    base_id,
    FUN = function(x) seq_along(x)
  )

  out <- ifelse(
    is.na(base_id),
    NA_character_,
    paste0(base_id, sep, suffix_prefix, "_", seq_idx)
  )

  df[[target]] <- out
  df
}

.show_missing_terms_modal <- function(session, validation, step_label = "this step") {
  missing_required <- validation$missing_required %||% character(0)
  duplicate_terms <- validation$duplicate_terms %||% character(0)

  if (length(missing_required) == 0 && length(duplicate_terms) == 0) {
    return(invisible(NULL))
  }

  body_parts <- list(
    shiny::tags$div(
      style = "color:#b91c1c; font-weight:700; margin-bottom:0.75rem;",
      "Warning"
    ),
    shiny::tags$p(
      paste0(
        "Before or after ",
        step_label,
        ", please review the minimum required terms for the selected database."
      )
    )
  )

  if (length(missing_required) > 0) {
    body_parts <- c(
      body_parts,
      list(
        shiny::tags$p("Required terms still missing:"),
        shiny::tags$ul(lapply(missing_required, shiny::tags$li))
      )
    )
  }

  if (length(duplicate_terms) > 0) {
    body_parts <- c(
      body_parts,
      list(
        shiny::tags$p("Duplicate Darwin Core mappings found:"),
        shiny::tags$ul(lapply(duplicate_terms, shiny::tags$li))
      )
    )
  }

  shiny::showModal(
    shiny::modalDialog(
      title = shiny::tags$span(style = "color:#b91c1c;", "Validation warning"),
      easyClose = TRUE,
      footer = shiny::modalButton("Close"),
      body_parts
    )
  )
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

    validation_df <- shiny::reactive({
      if (!is.null(rv$cleaned)) return(rv$cleaned)
      if (!is.null(rv$created)) return(rv$created)
      if (!is.null(rv$mapped)) return(rv$mapped)
      NULL
    })

    current_validation <- shiny::reactive({
      .validation_state(
        map_df = mapping_tbl(),
        final_df = validation_df(),
        target_database = input$target_database %||% "GBIF"
      )
    })

    run_clean_pipeline <- function(df_to_clean) {
      if (!exists("clean_dwc_pipeline", mode = "function")) {
        rv$cleaned <- df_to_clean
        rv$issues <- data.frame()
        rv$clean_summary <- data.frame()
        return(invisible(NULL))
      }

      epsg_in <- input$coord_epsg_in %||% 4326
      force_parse <- TRUE
      preserve_original_coords_flag <- isTRUE(input$preserve_original_coords)
      create_geodetic_datum_flag <- isTRUE(input$create_geodetic_datum)
      geodetic_datum_value_flag <- "WGS84"
      standardize_event_date_flag <- isTRUE(input$standardize_event_date)
      ambiguous_date_order_flag <- input$event_date_order %||% "dmy"
      swap_auto_fix_flag <- TRUE

      clean_res <- tryCatch(
        clean_dwc_pipeline(
          df_to_clean,
          coord_epsg_in = epsg_in,
          target_epsg = 4326,
          force_parzer = force_parse,
          swap_auto_fix = swap_auto_fix_flag,
          preserve_original_coords = preserve_original_coords_flag,
          create_geodetic_datum = create_geodetic_datum_flag,
          geodetic_datum_value = geodetic_datum_value_flag,
          standardize_event_date = standardize_event_date_flag,
          ambiguous_date_order = ambiguous_date_order_flag
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

    output$id_mode_ui <- shiny::renderUI({
      if (!identical(input$id_target %||% "", "occurrenceID")) {
        return(NULL)
      }

      shiny::tagList(
        shiny::checkboxInput(
          session$ns("id_sequential"),
          "Sequential",
          value = FALSE
        ),
        shiny::div(
          class = "dwc-map-inline-help",
          "When checked, occurrenceID will be generated as <base><separator>occ_<n> using the selected source columns."
        )
      )
    })

    output$workflow_status <- shiny::renderUI({
      shiny::tagList(
        shiny::tags$p(
          shiny::tags$b("Target data repository: "),
          input$target_database %||% "GBIF"
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
          shiny::tags$b("Formatting applied: "),
          if (isTRUE(rv$formatting_done)) "Yes" else "No"
        ),
        shiny::tags$p(
          shiny::tags$b("Field mapping complete: "),
          if (!is.null(rv$cleaned) && isTRUE(rv$formatting_done) && isTRUE(rv$ready)) "Yes" else "No"
        )
      )
    })

    output$validation_ui <- shiny::renderUI({
      .validation_ui_block(current_validation())
    })

    output$mapping_export_note <- shiny::renderUI({
      shiny::tags$div(
        class = "alert alert-info",
        shiny::tags$b("Note: "),
        "Only columns mapped to Darwin Core terms will be exported to the Darwin Core final tables."
      )
    })

    output$scientific_name_note <- shiny::renderUI({
      shiny::tags$div(
        class = "alert alert-warning",
        shiny::tags$b("Note about scientificName mapping: "),
        "If you map a source column to ",
        shiny::tags$code("scientificName"),
        ", its content may later be replaced or standardized during Taxonomy Match. ",
        "The original identification should be mapped to ",
        shiny::tags$code("verbatimIdentification"),
        "."
      )
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

    output$dependency_ui <- shiny::renderUI({
      df <- current_preclean_df()

      if (is.null(df)) {
        return(
          shiny::tags$div(
            class = "alert alert-light",
            "Apply mapping first to detect associated fields."
          )
        )
      }

      deps <- .detect_missing_dependencies(df)

      if (nrow(deps) == 0) {
        return(
          shiny::tags$div(
            class = "alert alert-light",
            "No associated fields are currently suggested."
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

    shiny::observeEvent(input$apply_mapping, {
      df <- df_in()
      shiny::req(df)

      map_df <- mapping_tbl()
      rv$mapping <- map_df
      rv$mapped <- .apply_mapping(df, map_df)
      rv$created <- NULL
      rv$cleaned <- NULL
      rv$issues <- NULL
      rv$clean_summary <- NULL
      rv$formatting_done <- FALSE
      rv$ready <- FALSE

      .show_missing_terms_modal(
        session = session,
        validation = current_validation(),
        step_label = "applying mapping"
      )

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

      target_field <- input$id_target %||% ""
      source_cols <- input$id_source_cols %||% character(0)
      sep <- input$id_sep %||% ":"
      overwrite <- isTRUE(input$id_overwrite)
      sequential <- isTRUE(input$id_sequential)

      if (identical(target_field, "occurrenceID") && sequential) {
        rv$created <- .build_sequential_occurrence_id(
          df = df,
          target = target_field,
          source_cols = source_cols,
          sep = sep,
          overwrite = overwrite,
          suffix_prefix = "occ"
        )
      } else {
        rv$created <- .build_concat_field(
          df = df,
          target = target_field,
          source_cols = source_cols,
          sep = sep,
          overwrite = overwrite
        )
      }

      rv$cleaned <- NULL
      rv$issues <- NULL
      rv$clean_summary <- NULL
      rv$formatting_done <- FALSE
      rv$ready <- FALSE

      shiny::showNotification(
        paste0(target_field, " created/updated."),
        type = "message",
        duration = 2.5
      )
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

      rv$cleaned <- NULL
      rv$issues <- NULL
      rv$clean_summary <- NULL
      rv$formatting_done <- FALSE
      rv$ready <- FALSE

      shiny::showNotification(
        paste0(input$remarks_target, " created/updated."),
        type = "message",
        duration = 2.5
      )
    })

    shiny::observeEvent(input$create_basis_of_record_btn, {
      df <- current_preclean_df()
      shiny::req(df)

      rv$created <- .fill_basis_of_record(
        df = df,
        value = input$basis_of_record_value %||% "HumanObservation",
        overwrite = isTRUE(input$basis_of_record_overwrite)
      )

      rv$cleaned <- NULL
      rv$issues <- NULL
      rv$clean_summary <- NULL
      rv$formatting_done <- FALSE
      rv$ready <- FALSE

      shiny::showNotification(
        "basisOfRecord created/filled.",
        type = "message",
        duration = 2.5
      )
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

      rv$cleaned <- NULL
      rv$issues <- NULL
      rv$clean_summary <- NULL
      rv$formatting_done <- FALSE
      rv$ready <- FALSE

      shiny::showNotification(
        paste0(dep_row$required_term[1], " created."),
        type = "message",
        duration = 2.5
      )
    })

    shiny::observeEvent(input$continue_to_formatting, {
      shiny::req(!is.null(rv$mapped))

      .show_missing_terms_modal(
        session = session,
        validation = current_validation(),
        step_label = "continuing to formatting"
      )

      bslib::nav_select(
        id = "main_tabs",
        selected = "formatting",
        session = session
      )
    })

    shiny::observeEvent(input$apply_formatting, {
      df_final <- current_preclean_df()
      shiny::req(df_final)

      .show_missing_terms_modal(
        session = session,
        validation = current_validation(),
        step_label = "applying formatting"
      )

      shiny::withProgress(
        message = "Applying formatting",
        detail = "Cleaning and preparing preview...",
        value = 0,
        {
          shiny::incProgress(0.25, detail = "Running cleaning pipeline...")
          run_clean_pipeline(df_final)
          shiny::incProgress(0.65, detail = "Preparing preview and issues...")
          rv$formatting_done <- TRUE
          rv$ready <- TRUE
          shiny::incProgress(1, detail = "Done.")
        }
      )

      bslib::nav_select(
        id = "main_tabs",
        selected = "preview",
        session = session
      )
    })

    output$preview_tbl <- DT::renderDT({
      shiny::req(rv$cleaned)
      DT::datatable(
        utils::head(rv$cleaned, 200),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          scrollY = "500px",
          pageLength = 25
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
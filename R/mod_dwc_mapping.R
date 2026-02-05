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
      )
    ),

    bslib::card(
      bslib::card_header("Field mapping to Darwin Core"),
      shiny::h4("Map each column to a Darwin Core term"),
      shiny::uiOutput(ns("mapping_ui")),
      shiny::hr(),
      shiny::h4("Mapping table"),
      DT::DTOutput(ns("mapping_tbl"))
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

  # ensure uniqueness by term ## verificar!!
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

.normalize_key <- function(x) {
  x <- tolower(x)
  gsub("[^a-z0-9]+", "", x)
}

.clean_col_for_match <- function(x) {
  x <- tolower(x %||% "")

  # remove conteúdo entre parênteses e colchetes (unidades, notas)
  x <- gsub("\\([^)]*\\)", " ", x)
  x <- gsub("\\[[^]]*\\]", " ", x)

  # remove tokens comuns de unidades/formato
  x <- gsub("\\b(dd|deg|degree|degrees)\\b", " ", x)
  x <- gsub("\\b(m|meter|meters|metre|metres)\\b", " ", x)

  # normaliza separadores
  x <- gsub("[/_\\-]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

.normalize_key <- function(x) {
  x <- .clean_col_for_match(x)
  gsub("[^a-z0-9]+", "", x)
}

# Tabela de aliases: regex -> termo DwC
# Pode expandir isto com casos marinhos/ROV sem mudar o resto do código.
.dwc_alias_rules <- function() {
  list(
    # coordenadas
    list(pattern = "\\b(latitude|^lat$|\\blat\\b)\\b", term = "decimalLatitude"),
    list(pattern = "\\b(longitude|^lon$|\\blon\\b|\\blng\\b|\\blong\\b)\\b",
         term = "decimalLongitude"),
    list(pattern = "\\b(verbatim latitude|raw latitude|original latitude)\\b",
         term = "verbatimLatitude"),
    list(pattern = "\\b(verbatim longitude|raw longitude|original longitude)\\b",
         term = "verbatimLongitude"),
    list(pattern = "\\b(epsg|crs|coordinate system|coordsystem)\\b",
         term = "verbatimCoordinateSystem"),

    # datas
    list(pattern = "\\b(event date|sampling date|sample date|date sampled|^date$)\\b",
         term = "eventDate"),
    list(pattern = "\\b(datetime|date time|timestamp)\\b", term = "eventDate"),
    list(pattern = "\\b(event time|^time$)\\b", term = "eventTime"),

    # IDs comuns
    list(pattern = "\\b(occurrence id|occ_id|occ id|record id|recordid|^id$)\\b",
         term = "occurrenceID"),
    list(pattern = "\\b(event id|eventid|sample id|sampleid|haul id|tow id)\\b",
         term = "eventID"),
    list(pattern = "\\b(location id|locationid|station id|stationid|station)\\b",
         term = "locationID"),

    # taxonomia
    list(pattern = "\\b(scientific name|scientific_name|taxon name|taxon_name|species)\\b",
         term = "scientificName"),
    list(pattern = "\\b(aphia id|aphiaid|worms id|lsid|scientificnameid)\\b",
         term = "scientificNameID"),

    # país
    list(pattern = "\\b(country code|countrycode|iso2|iso country)\\b",
         term = "countryCode"),
    list(pattern = "\\bcountry\\b", term = "country"),

    # profundidade (genérico)
    list(pattern = "\\b(min depth|depth min|minimum depth)\\b",
         term = "minimumDepthInMeters"),
    list(pattern = "\\b(max depth|depth max|maximum depth)\\b",
         term = "maximumDepthInMeters"),
    list(pattern = "\\bdepth\\b", term = "verbatimDepth"),

    # remarks
    list(pattern = "\\b(remarks|notes|comments|observation|obs)\\b",
         term = "occurrenceRemarks")
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

  # 1) match exato
  if (col_name %in% dwc_terms) return(col_name)

  # 2) aliases (semântica simples)
  ali <- .apply_alias_rules(col_name, dwc_terms)
  if (nzchar(ali)) return(ali)

  # 3) fuzzy fallback
  key <- .normalize_key(col_name)
  if (key == "") return("")

  terms2 <- dwc_terms[dwc_terms != ""]
  terms_key <- vapply(terms2, .normalize_key, character(1))

  if (!requireNamespace("stringdist", quietly = TRUE)) {
    return("")
  }

  d <- stringdist::stringdist(key, terms_key, method = "jw", p = 0.1)
  i <- which.min(d)
  if (!length(i) || !is.finite(d[i])) return("")

  # threshold um pouco mais permissivo do que 0.15
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

  if (length(msgs) == 0) {
    msgs <- "OK: mapping passes basic validation."
  }

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

  if (inherits(x, "error")) {
    return(c(paste0(label, " failed:"), x$message))
  }

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
#' @return A list of reactives: mapping, mapped, validation.
#' @export
mod_dwc_mapping_server <- function(id, df_in) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      mapping = NULL,
      mapped = NULL,
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

    # aviso Apply (warning/success)
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

      selected <- vapply(
        keys,
        function(k) input[[paste0("map__", k)]] %||% "",
        character(1)
      )

      border_col <- "#cbd5e1"

      # container com scroll interno (ajuste max-height se quiser)
      shiny::tags$div(
        style = paste0(
          "max-height: 520px;",
          "overflow-y: auto;",
          "padding-right: 6px;"  # espaço para scrollbar não colar no conteúdo
        ),

        shiny::tags$div(
          style = paste0(
            "display:grid;",
            "grid-template-columns: 1fr 2fr;",
            "gap: 0;",
            "width: 100%;",
            "border: 1px solid ", border_col, ";",
            "border-radius: 10px;",
            "overflow: hidden;",
            "background: #fff;"
          ),

          # Header (sticky dentro do container)
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

          # Rows
          lapply(keys, function(k) {
            col <- key_map[[k]]
            term <- selected[[k]] %||% ""

            meta_row <- term_meta[term_meta$term == term, , drop = FALSE]
            html <- if (nrow(meta_row) == 0) {
              if (term == "") "Select a term to see definition and examples."
              else "No metadata found for this term in corella."
            } else {
              .term_tooltip_html(term, meta_row[1, ])
            }

            tip_trigger <- shiny::tags$span(
              shiny::icon("info-circle"),
              style = "cursor: help; opacity: 0.85; margin-left: 10px;"
            )

            shiny::tagList(
              # left cell
              shiny::tags$div(
                style = paste0(
                  "padding: 12px;",
                  "font-weight: 600;",
                  "border-right: 1px solid ", border_col, ";",
                  "border-bottom: 1px solid ", border_col, ";"
                ),
                col
              ),

              # right cell
              shiny::tags$div(
                style = paste0(
                  "padding: 12px;",
                  "border-bottom: 1px solid ", border_col, ";"
                ),
                shiny::tags$div(
                  style = "display:flex; align-items:center;",
                  shiny::tags$div(
                    style = "flex:1; max-width: 560px;",
                    shiny::selectizeInput(
                      inputId = ns(paste0("map__", k)),
                      label = NULL,
                      choices = dwc_terms,
                      selected = term,
                      options = list(
                        placeholder = "Select a Darwin Core term (or leave blank)"
                      )
                    )
                  ),
                  bslib::tooltip(
                    trigger = tip_trigger,
                    htmltools::HTML(html),
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
    })

    output$validation <- shiny::renderText({
      basic <- .validate_mapping_basic(mapping_tbl())

      if (is.null(rv$mapped)) {
        return(paste(basic, collapse = "\n"))
      }

      extra <- character(0)
      if (!is.null(rv$corella_checks)) {
        extra <- c(extra, "", .corella_summary(rv$corella_checks,
                                              "corella::check_dataset()"))
      }
      if (!is.null(rv$corella_suggest)) {
        extra <- c(extra, "", .corella_summary(rv$corella_suggest,
                                              "corella::suggest_workflow()"))
      }

      paste(c(basic, extra), collapse = "\n")
    })

    output$mapping_tbl <- DT::renderDT({
      DT::datatable(
        mapping_tbl(),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    list(
      mapping = shiny::reactive(rv$mapping),
      mapped = shiny::reactive(rv$mapped),
      validation = shiny::reactive(.validate_mapping_basic(mapping_tbl()))
    )
  })
}

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

  # ensure uniqueness by term
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

.suggest_term <- function(col_name, dwc_terms) {
  if (is.null(col_name) || is.na(col_name) || col_name == "") return("")

  if (col_name %in% dwc_terms) return(col_name)

  key <- .normalize_key(col_name)
  if (key == "") return("")

  terms2 <- dwc_terms[dwc_terms != ""]
  terms_key <- .normalize_key(terms2)

  if (!requireNamespace("stringdist", quietly = TRUE)) {
    return("")
  }

  d <- stringdist::stringdist(key, terms_key, method = "jw", p = 0.1)
  i <- which.min(d)
  if (!length(i) || !is.finite(d[i])) return("")

  if (d[i] <= 0.15) terms2[i] else ""
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

      # garante re-render e preserva seleções
      selected <- vapply(
        keys,
        function(k) input[[paste0("map__", k)]] %||% "",
        character(1)
      )

      shiny::tagList(
        lapply(keys, function(k) {
          col <- key_map[[k]]

          term <- selected[[k]] %||% ""
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

          # trigger do tooltip
          tip_trigger <- shiny::tags$span(
            shiny::icon("info-circle"),
            style = "cursor: help; opacity: 0.85;"
          )

          bslib::layout_columns(
            col_widths = c(4, 8),

            shiny::tags$div(
              style = "padding-top: 7px; font-weight: 600;",
              col
            ),

            shiny::tags$div(
              style = "display:flex; gap:10px; align-items:center;",
              shiny::tags$div(
                style = "max-width: 520px; flex: 1;",
                shiny::selectizeInput(
                  inputId = ns(paste0("map__", k)),
                  label = NULL,
                  choices = dwc_terms,
                  selected = selected[[k]] %||% "",
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
        })
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

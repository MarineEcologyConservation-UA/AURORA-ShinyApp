# modules/mod_ingest.R

#' Ingestion module UI
#'
#' Shiny module UI for ingesting data from upload or examples.
#'
#' @param id Module id.
#' @return A Shiny UI definition.
#' @export
mod_ingest_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(4, 8),

    # ---- Left: controls ----
    bslib::card(
      bslib::card_header("Data ingestion"),

      shiny::radioButtons(
        ns("source"),
        "Source",
        choices = c("Upload file" = "upload", "Example dataset" = "example"),
        selected = "example"
      ),

      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'upload'", ns("source")),
        shiny::fileInput(
          ns("file"),
          "Upload (CSV/TSV/XLSX)",
          accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")
        )
      ),

      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'example'", ns("source")),
        shiny::selectInput(
          ns("example_pick"),
          "Choose example dataset",
          choices = NULL
        )
      ),

      shiny::hr(),

      shiny::selectInput(
        ns("encoding"),
        "Encoding",
        choices = c(
          "UTF-8" = "UTF-8",
          "UTF-8-BOM" = "UTF-8",
          "Latin1 (ISO-8859-1)" = "Latin1",
          "Windows-1252" = "Windows-1252"
        ),
        selected = "UTF-8"
      ),

      shiny::selectInput(
        ns("delim"),
        "Delimiter (CSV/TSV)",
        choices = c("," = ",", ";" = ";", "TAB" = "\t", "|" = "|"),
        selected = ","
      ),

      shiny::checkboxInput(ns("has_header"), "Header row", value = TRUE),

      shiny::actionButton(ns("load"), "Load", icon = shiny::icon("play")),
      shiny::br(),
      shiny::br(),
      shiny::verbatimTextOutput(ns("load_info"))
    ),

    # ---- Right: preview ----
    bslib::card(
      bslib::card_header("Preview"),

      bslib::navset_card_tab(
        bslib::nav_panel("Raw data", DT::DTOutput(ns("raw_tbl"))),
        bslib::nav_panel("Tidy data", DT::DTOutput(ns("tidy_tbl"))),
        bslib::nav_panel("Column summary", DT::DTOutput(ns("col_stats")))
      )
    )
  )
}

# -------- helper: normalize strings to UTF-8 (once) --------
.normalize_utf8_once <- function(df, encoding_used) {
  if (is.null(df)) return(NULL)

  is_chr <- vapply(df, is.character, logical(1))
  if (!any(is_chr)) return(df)

  enc <- encoding_used
  if (is.null(enc) || is.na(enc) || enc == "") enc <- "Latin1"

  df[is_chr] <- lapply(df[is_chr], function(x) {
    x <- as.character(x)

    ok_utf8 <- !any(is.na(iconv(x, from = "UTF-8", to = "UTF-8")))
    if (ok_utf8) return(x)

    x2 <- iconv(x, from = enc, to = "UTF-8", sub = "byte")

    if (anyNA(x2)) {
      x2[is.na(x2)] <- iconv(
        x[is.na(x2)],
        from = "Windows-1252",
        to = "UTF-8",
        sub = "byte"
      )
    }

    x2[is.na(x2)] <- ""
    x2
  })

  df
}

#' Ingestion module server
#'
#' @param id Module id.
#' @param example_map Named list mapping example labels to file paths.
#' @return A list of reactives: raw, tidy, label.
#' @export
mod_ingest_server <- function(id, example_map) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(
      raw = NULL,
      tidy = NULL,
      src_label = NULL,
      encoding_used = NULL
    )

    shiny::observe({
      shiny::updateSelectInput(
        session,
        "example_pick",
        choices = names(example_map),
        selected = names(example_map)[1]
      )
    })

    get_path <- shiny::reactive({
      if (input$source == "upload") {
        shiny::req(input$file$datapath)
        list(path = input$file$datapath, label = input$file$name)
      } else {
        shiny::req(input$example_pick)
        list(path = example_map[[input$example_pick]], label = input$example_pick)
      }
    })

    shiny::observeEvent(input$load, {
      p <- get_path()

      df <- tryCatch({
        read_any_table(
          path = p$path,
          delim = input$delim,
          has_header = input$has_header,
          encoding = input$encoding
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Read failed:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        NULL
      })

      if (is.null(df)) {
        rv$raw <- NULL
        rv$tidy <- NULL
        rv$src_label <- p$label
        rv$encoding_used <- NULL
        return()
      }

      used_enc <- attr(df, "encoding_used")
      rv$encoding_used <- used_enc

      df_norm <- .normalize_utf8_once(df, used_enc)

      rv$raw <- df_norm

      rv$tidy <- tryCatch({
        tidy_minimal(df_norm, encoding_used = NULL)
      }, error = function(e) {
        shiny::showNotification(
          paste("Tidy step failed:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        NULL
      })

      rv$src_label <- p$label
    })

    output$load_info <- shiny::renderText({
      if (is.null(rv$raw)) return("No dataset loaded yet.")

      tidy_rows <- if (is.null(rv$tidy)) 0 else nrow(rv$tidy)
      tidy_cols <- if (is.null(rv$tidy)) 0 else ncol(rv$tidy)

      paste0(
        "Loaded: ", rv$src_label, "\n",
        "Encoding used: ",
        ifelse(is.null(rv$encoding_used), "NA", rv$encoding_used), "\n\n",
        "Raw: ", nrow(rv$raw), " rows x ", ncol(rv$raw), " cols\n",
        "Tidy: ", tidy_rows, " rows x ", tidy_cols, " cols"
      )
    })

    output$raw_tbl <- DT::renderDT({
      shiny::req(rv$raw)
      DT::datatable(
        utils::head(rv$raw, 200),
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    output$tidy_tbl <- DT::renderDT({
      shiny::req(rv$tidy)
      DT::datatable(
        utils::head(rv$tidy, 200),
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    output$col_stats <- DT::renderDT({
      shiny::req(rv$raw)

      df <- rv$raw
      stats <- data.frame(
        column = names(df),
        type = vapply(df, function(x) class(x)[1], character(1)),
        missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
        unique = vapply(df, function(x) length(unique(x)), integer(1)),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        stats,
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    list(
      raw = shiny::reactive(rv$raw),
      tidy = shiny::reactive(rv$tidy),
      label = shiny::reactive(rv$src_label)
    )
  })
}

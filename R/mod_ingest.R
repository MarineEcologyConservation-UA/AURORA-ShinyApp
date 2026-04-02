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

      shiny::hr(),

      shiny::actionButton(
        ns("complete_ingest"),
        "Complete ingest",
        class = "btn-success"
      ),

      shiny::br(),
      shiny::br(),
      shiny::verbatimTextOutput(ns("load_info"))
    ),

    # ---- Right: preview ----
    bslib::card(
      bslib::card_header("Preview"),

      bslib::navset_card_tab(
        bslib::nav_panel("Raw data", DT::DTOutput(ns("raw_tbl"))),
        bslib::nav_panel("Tidy / Pivot", shiny::uiOutput(ns("pivot_ui"))),
        bslib::nav_panel("Column summary", DT::DTOutput(ns("col_stats")))
      )
    )
  )
}

# -------- helper: normalize strings to UTF-8 (once) --------
.normalize_utf8_once <- function(df, encoding_used) {
  if (is.null(df)) {
    return(NULL)
  }

  is_chr <- vapply(df, is.character, logical(1))
  if (!any(is_chr)) {
    return(df)
  }

  enc <- encoding_used
  if (is.null(enc) || is.na(enc) || enc == "") {
    enc <- "Latin1"
  }

  df[is_chr] <- lapply(df[is_chr], function(x) {
    x <- as.character(x)
    x2 <- iconv(x, from = enc, to = "UTF-8", sub = "byte")
    x2[is.na(x2)] <- ""
    x2
  })

  df
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Ingestion module server
#'
#' @param id Module id.
#' @param example_map Named list mapping example labels to file paths.
#' @return A list of reactives: raw, tidy, label, ready.
#' @export
mod_ingest_server <- function(id, example_map) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(
      raw = NULL,
      tidy = NULL,
      src_label = NULL,
      encoding_used = NULL,
      ingest_complete = FALSE
    )

    # --------------------------------------------------
    # PIVOT STATE
    # --------------------------------------------------
    pv <- shiny::reactiveValues(
      baseline = NULL,
      data = NULL,
      history = list(),
      history_labels = character(),
      is_pivoted = FALSE
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
        rv$ingest_complete <- FALSE

        pv$baseline <- NULL
        pv$data <- NULL
        pv$history <- list()
        pv$history_labels <- character()
        pv$is_pivoted <- FALSE
        return()
      }

      rv$encoding_used <- attr(df, "encoding_used")
      df_norm <- .normalize_utf8_once(df, rv$encoding_used)

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
      rv$ingest_complete <- FALSE

      pv$baseline <- rv$tidy
      pv$data <- rv$tidy
      pv$history <- list()
      pv$history_labels <- character()
      pv$is_pivoted <- FALSE
    })

    shiny::observeEvent(input$complete_ingest, {
      shiny::req(pv$data)
      shiny::validate(
        shiny::need(nrow(pv$data) > 0, "The current dataset is empty."),
        shiny::need(ncol(pv$data) > 0, "The current dataset has no columns.")
      )

      rv$ingest_complete <- TRUE

      shiny::showNotification(
        "Ingest completed. Field Mapping and downstream tabs are now available.",
        type = "message",
        duration = 4
      )
    })

    output$load_info <- shiny::renderText({
      if (is.null(rv$raw)) {
        return("No dataset loaded yet.")
      }

      current_rows <- if (is.null(pv$data)) 0 else nrow(pv$data)
      current_cols <- if (is.null(pv$data)) 0 else ncol(pv$data)

      paste0(
        "Loaded: ", rv$src_label, "\n",
        "Encoding used: ",
        ifelse(is.null(rv$encoding_used), "NA", rv$encoding_used), "\n\n",
        "Raw: ", nrow(rv$raw), " rows x ", ncol(rv$raw), " cols\n",
        "Current dataset: ", current_rows, " rows x ", current_cols, " cols\n",
        "Pivot applied: ", ifelse(isTRUE(pv$is_pivoted), "Yes", "No"), "\n",
        "Ingest complete: ", ifelse(isTRUE(rv$ingest_complete), "Yes", "No")
      )
    })

    # --------------------------------------------------
    # RAW PREVIEW
    # --------------------------------------------------
    output$raw_tbl <- DT::renderDT({
      shiny::req(rv$raw)

      DT::datatable(
        utils::head(rv$raw, 200),
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    # --------------------------------------------------
    # COLUMN SUMMARY
    # --------------------------------------------------
    output$col_stats <- DT::renderDT({
      shiny::req(rv$raw)

      df <- if (isTRUE(pv$is_pivoted) && !is.null(pv$data)) {
        pv$data
      } else {
        rv$raw
      }

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

    # --------------------------------------------------
    # PIVOT UI
    # --------------------------------------------------
    output$pivot_ui <- shiny::renderUI({
      shiny::req(pv$data)

      df <- pv$data
      cols <- names(df)
      num_cols <- cols[vapply(df, is.numeric, logical(1))]

      shiny::tagList(
        shiny::tags$style(shiny::HTML(sprintf("
          #%s .pivot-check-wrap {
            max-height: 260px;
            overflow-y: auto;
            border: 1px solid #d1d5db;
            border-radius: 0.5rem;
            padding: 0.75rem 0.9rem;
            background: #ffffff;
          }
          #%s .pivot-check-wrap .shiny-options-group {
            display: block;
            margin-bottom: 0;
          }
          #%s .pivot-check-wrap .checkbox {
            margin-top: 0.15rem;
            margin-bottom: 0.45rem;
          }
          #%s .pivot-check-wrap label {
            font-weight: 400;
            margin-bottom: 0;
          }
        ",
          session$ns("pivot_block"),
          session$ns("pivot_block"),
          session$ns("pivot_block"),
          session$ns("pivot_block")
        ))),

        shiny::fluidRow(
          shiny::column(
            4,
            shiny::div(
              id = session$ns("pivot_block"),
              shiny::div(
                class = "mb-3",
                shiny::strong("Current dataset used for downstream steps"),
                shiny::tags$br(),
                shiny::tags$small(
                  if (isTRUE(pv$is_pivoted)) {
                    "Pivoted dataset"
                  } else {
                    "Minimal tidy dataset (no pivot applied yet)"
                  }
                )
              ),

              shiny::tags$label(
                `for` = session$ns("pivot_value_cols"),
                "Columns to pivot"
              ),

              shiny::div(
                class = "pivot-check-wrap",
                shiny::checkboxGroupInput(
                  session$ns("pivot_value_cols"),
                  label = NULL,
                  choices = cols,
                  selected = intersect(num_cols, cols)
                )
              ),

              shiny::br(),
              shiny::textInput(session$ns("pivot_names_to"), "names_to", "variable"),
              shiny::textInput(session$ns("pivot_values_to"), "values_to", "value"),
              shiny::checkboxInput(session$ns("pivot_drop_na"), "Drop NA", TRUE),
              shiny::checkboxInput(session$ns("pivot_drop_zero"), "Drop zero", TRUE),
              shiny::br(),
              shiny::actionButton(session$ns("pivot_apply"), "Pivot"),
              shiny::actionButton(session$ns("pivot_undo"), "Undo"),
              shiny::actionButton(session$ns("pivot_reset"), "Reset")
            )
          ),
          shiny::column(
            8,
            shiny::verbatimTextOutput(session$ns("pivot_info")),
            shiny::h5("Current dataset preview"),
            DT::DTOutput(session$ns("pivot_after"))
          )
        )
      )
    })

    # --------------------------------------------------
    # PIVOT INFO
    # --------------------------------------------------
    output$pivot_info <- shiny::renderText({
      shiny::req(pv$data, rv$raw)

      paste0(
        "Raw dataset: ", nrow(rv$raw), " rows x ", ncol(rv$raw), " cols\n",
        "Current dataset: ", nrow(pv$data), " rows x ", ncol(pv$data), " cols\n",
        "Pivot applied: ", ifelse(isTRUE(pv$is_pivoted), "Yes", "No"), "\n",
        "Undo steps available: ", length(pv$history), "\n",
        "Ingest complete: ", ifelse(isTRUE(rv$ingest_complete), "Yes", "No")
      )
    })

    # --------------------------------------------------
    # PIVOT APPLY
    # --------------------------------------------------
    shiny::observeEvent(input$pivot_apply, {
      shiny::req(pv$data)

      df_before <- pv$data
      pivot_cols <- input$pivot_value_cols %||% character()
      id_cols <- setdiff(names(df_before), pivot_cols)

      shiny::validate(
        shiny::need(length(pivot_cols) > 0, "Select at least one column to pivot.")
      )

      df_after <- tryCatch({
        apply_pivot_longer(
          df = df_before,
          id_cols = id_cols,
          pivot_cols = pivot_cols,
          names_to = input$pivot_names_to,
          values_to = input$pivot_values_to,
          drop_na = isTRUE(input$pivot_drop_na),
          drop_zero = isTRUE(input$pivot_drop_zero),
          trim_names = TRUE
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Pivot failed:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        NULL
      })

      if (is.null(df_after)) {
        return()
      }

      pv$history <- c(list(df_before), pv$history)
      pv$data <- df_after
      pv$is_pivoted <- TRUE
      rv$ingest_complete <- FALSE
    })

    # --------------------------------------------------
    # UNDO / RESET
    # --------------------------------------------------
    shiny::observeEvent(input$pivot_undo, {
      if (length(pv$history) == 0) {
        shiny::showNotification(
          "Nothing to undo.",
          type = "message",
          duration = 3
        )
        return()
      }

      pv$data <- pv$history[[1]]
      pv$history <- pv$history[-1]
      pv$is_pivoted <- length(pv$history) > 0
      rv$ingest_complete <- FALSE
    })

    shiny::observeEvent(input$pivot_reset, {
      shiny::req(pv$baseline)

      pv$data <- pv$baseline
      pv$history <- list()
      pv$history_labels <- character()
      pv$is_pivoted <- FALSE
      rv$ingest_complete <- FALSE
    })

    output$pivot_after <- DT::renderDT({
      shiny::req(pv$data)

      DT::datatable(
        utils::head(pv$data, 200),
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    # --------------------------------------------------
    # Return
    # --------------------------------------------------
    list(
      raw = shiny::reactive(rv$raw),
      tidy = shiny::reactive({
        shiny::req(rv$ingest_complete)
        pv$data
      }),
      current = shiny::reactive(pv$data),
      ready = shiny::reactive(isTRUE(rv$ingest_complete)),
      label = shiny::reactive(rv$src_label)
    )
  })
}
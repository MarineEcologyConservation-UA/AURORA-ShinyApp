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
          "Upload (CSV)",
          accept = c(".csv")
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
        "Delimiter (CSV)",
        choices = c("," = ",", ";" = ";", "TAB" = "\t", "|" = "|"),
        selected = ","
      ),

      shiny::checkboxInput(ns("has_header"), "Header row", value = TRUE),

      shiny::actionButton(ns("load"), "Load", icon = shiny::icon("play")),

      shiny::hr(),

      shiny::div(
        style = "margin-bottom: 0.75rem;",
        shiny::tags$strong("Note"),
        shiny::tags$br(),
        shiny::tags$small(
  "If your dataset is already in a tidy format, you can proceed directly by clicking ",
  shiny::tags$em("Complete Ingestion"),
  ".",

  shiny::tags$br(),
  shiny::tags$br(),

  shiny::tags$b("What is a tidy format?"),
  shiny::tags$ul(
    shiny::tags$li("Each occurrence must correspond to a single row."),
    shiny::tags$li("Do not store a single variable across multiple columns (e.g., species names split into separate columns).")
  )
)
      ),

      shiny::actionButton(
        ns("complete_ingest"),
        "Complete Ingestion",
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

# -------- helper: init drop log --------
.init_drop_log <- function() {
  data.frame(
    module = character(),
    step = character(),
    reason = character(),
    .aurora_origin_row = integer(),
    .aurora_origin_id = character(),
    details = character(),
    stringsAsFactors = FALSE
  )
}

# -------- helper: append dropped rows to log --------
.append_drop_log <- function(log_df, dropped_df, module, step, reason, details = NA_character_) {
  if (is.null(log_df) || !is.data.frame(log_df)) {
    log_df <- .init_drop_log()
  }

  if (is.null(dropped_df) || !is.data.frame(dropped_df) || nrow(dropped_df) == 0) {
    return(log_df)
  }

  req_cols <- c(".aurora_origin_row", ".aurora_origin_id")
  miss <- setdiff(req_cols, names(dropped_df))
  if (length(miss) > 0) {
    stop("Dropped rows are missing origin columns: ", paste(miss, collapse = ", "))
  }

  new_rows <- data.frame(
    module = rep(module, nrow(dropped_df)),
    step = rep(step, nrow(dropped_df)),
    reason = rep(reason, nrow(dropped_df)),
    .aurora_origin_row = dropped_df$.aurora_origin_row,
    .aurora_origin_id = dropped_df$.aurora_origin_id,
    details = rep(as.character(details), nrow(dropped_df)),
    stringsAsFactors = FALSE
  )

  dplyr::bind_rows(log_df, new_rows)
}

#' Ingestion module server
#'
#' @param id Module id.
#' @param example_map Named list mapping example labels to file paths.
#' @return A list of reactives: raw, tidy, current, ready, label, dropped_log.
#' @export
mod_ingest_server <- function(id, example_map) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(
      raw = NULL,
      tidy = NULL,
      src_label = NULL,
      encoding_used = NULL,
      ingest_complete = FALSE,
      dropped_log = .init_drop_log()
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
        rv$dropped_log <- .init_drop_log()

        pv$baseline <- NULL
        pv$data <- NULL
        pv$history <- list()
        pv$history_labels <- character()
        pv$is_pivoted <- FALSE
        return()
      }

      rv$encoding_used <- attr(df, "encoding_used")
      df_norm <- .normalize_utf8_once(df, rv$encoding_used)

      # add fixed origin trace columns
      df_norm <- aurora_add_origin_cols(df_norm)

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
      rv$dropped_log <- .init_drop_log()

      pv$baseline <- rv$tidy
      pv$data <- rv$tidy
      pv$history <- list()
      pv$history_labels <- character()
      pv$is_pivoted <- FALSE
    })

    shiny::observeEvent(input$complete_ingest, {
      shiny::req(pv$data)

      if (nrow(pv$data) == 0) {
        shiny::showNotification(
          "The current dataset is empty.",
          type = "error",
          duration = 4
        )
        return()
      }

      if (ncol(pv$data) == 0) {
        shiny::showNotification(
          "The current dataset has no columns.",
          type = "error",
          duration = 4
        )
        return()
      }

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
        "Undo steps available: ", length(pv$history), "\n",
        "Ingest complete: ", ifelse(isTRUE(rv$ingest_complete), "Yes", "No"), "\n",
        "Dropped rows logged: ", nrow(rv$dropped_log)
      )
    })

    # --------------------------------------------------
    # RAW PREVIEW
    # --------------------------------------------------
    output$raw_tbl <- DT::renderDT({
      shiny::req(rv$raw)

      DT::datatable(
        utils::head(aurora_drop_internal_cols(rv$raw), 200),
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

      df <- aurora_drop_internal_cols(df)

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
      cols <- aurora_user_cols(df)
      num_cols <- cols[vapply(df[, cols, drop = FALSE], is.numeric, logical(1))]

      block_id <- session$ns("pivot_block")

      shiny::tagList(
        shiny::tags$style(shiny::HTML(sprintf("
          #%s .pivot-check-wrap {
            max-height: 280px;
            overflow-y: auto;
            overflow-x: hidden;
            width: 100%%;
            border: 1px solid #d1d5db;
            border-radius: 0.5rem;
            padding: 0.85rem 1rem;
            background: #ffffff;
            margin-bottom: 1rem;
          }

          #%s .pivot-check-wrap .shiny-options-group {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
            column-gap: 1rem;
            row-gap: 0.2rem;
            margin-bottom: 0;
            width: 100%%;
          }

          #%s .pivot-check-wrap .checkbox {
            margin-top: 0.1rem;
            margin-bottom: 0.2rem;
          }

          #%s .pivot-check-wrap label {
            font-weight: 400;
            margin-bottom: 0;
            word-break: break-word;
          }

          #%s .pivot-top-note {
            margin-bottom: 0.85rem;
          }

          #%s .pivot-controls-row {
            display: flex;
            flex-wrap: wrap;
            gap: 0.75rem;
            align-items: flex-end;
            border: 1px solid #e5e7eb;
            border-radius: 0.6rem;
            padding: 0.9rem 1rem;
            background: #fafafa;
            margin-bottom: 1rem;
          }

          #%s .pivot-field {
            min-width: 170px;
            flex: 1 1 170px;
          }

          #%s .pivot-field-sm {
            min-width: 140px;
            flex: 0 1 160px;
            padding-top: 1.9rem;
          }

          .pivot-actions {
            display: flex;
            flex-wrap: wrap;
            gap: 0.45rem;
            align-items: flex-end;
            padding-top: 1.6rem;
            margin-left: auto;
            justify-content: flex-end;
          }

          #%s .pivot-actions .btn {
            margin-right: 0;
            margin-bottom: 0;
          }

          #%s .pivot-preview-card {
            border: 1px solid #e5e7eb;
            border-radius: 0.6rem;
            padding: 0.9rem 1rem;
            background: #ffffff;
          }

          #%s .pivot-preview-card .form-group {
            margin-bottom: 0.75rem;
          }

          #%s .pivot-preview-card .dataTables_wrapper {
            width: 100%%;
          }
        ",
          block_id,
          block_id,
          block_id,
          block_id,
          block_id,
          block_id,
          block_id,
          block_id,
          block_id,
          block_id,
          block_id,
          block_id
        ))),

        shiny::div(
          id = block_id,

          shiny::div(
            class = "pivot-top-note",
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
            shiny::strong("Columns to pivot")
          ),

          shiny::div(
            class = "pivot-check-wrap",
            shiny::checkboxGroupInput(
              session$ns("pivot_value_cols"),
              label = NULL,
              choices = cols,
              selected = intersect(num_cols, cols),
              width = "100%"
            )
          ),

          shiny::div(
            class = "pivot-controls-row",

            shiny::div(
              class = "pivot-field",
              shiny::textInput(
                session$ns("pivot_names_to"),
                "Name for the new column (e.g., species) of former headers",
                "variable"
              )
            ),

            shiny::div(
              class = "pivot-field",
              shiny::textInput(
                session$ns("pivot_values_to"),
                "New column name (e.g., density) for previous cell values",
                "value"
              )
            ),

            shiny::div(
              class = "pivot-field-sm",
              shiny::checkboxInput(session$ns("pivot_drop_na"), "Drop NA", TRUE)
            ),

            shiny::div(
              class = "pivot-field-sm",
              shiny::checkboxInput(session$ns("pivot_drop_zero"), "Drop zero", TRUE)
            ),

            shiny::div(
              class = "pivot-actions",
              shiny::actionButton(session$ns("pivot_apply"), "Pivot"),
              shiny::actionButton(session$ns("pivot_undo"), "Undo"),
              shiny::actionButton(session$ns("pivot_reset"), "Reset")
            )
          ),

          shiny::div(
            class = "pivot-preview-card",
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
        "Ingest complete: ", ifelse(isTRUE(rv$ingest_complete), "Yes", "No"), "\n",
        "Dropped rows logged: ", nrow(rv$dropped_log)
      )
    })

    # --------------------------------------------------
    # PIVOT APPLY
    # --------------------------------------------------
    shiny::observeEvent(input$pivot_apply, {
      shiny::req(pv$data)

      df_before <- pv$data
      pivot_cols <- input$pivot_value_cols %||% character()
      id_cols <- setdiff(aurora_user_cols(df_before), pivot_cols)

      if (length(pivot_cols) == 0) {
        shiny::showNotification(
          "Select at least one column to pivot.",
          type = "warning",
          duration = 4
        )
        return()
      }

      res <- tryCatch({
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

      if (is.null(res)) {
        return()
      }

      if (nrow(res$dropped_na) > 0) {
        rv$dropped_log <- .append_drop_log(
          log_df = rv$dropped_log,
          dropped_df = res$dropped_na,
          module = "ingest",
          step = "pivot",
          reason = "drop_na_after_pivot",
          details = paste0("Dropped NA rows from column '", res$meta$values_to, "'.")
        )
      }

      if (nrow(res$dropped_zero) > 0) {
        rv$dropped_log <- .append_drop_log(
          log_df = rv$dropped_log,
          dropped_df = res$dropped_zero,
          module = "ingest",
          step = "pivot",
          reason = "drop_zero_after_pivot",
          details = paste0("Dropped zero rows from column '", res$meta$values_to, "'.")
        )
      }

      pv$history <- c(list(df_before), pv$history)
      pv$data <- res$data
      pv$is_pivoted <- !identical(pv$data, pv$baseline)
      rv$ingest_complete <- FALSE

      shiny::showNotification(
        "Pivot applied to the current dataset.",
        type = "message",
        duration = 3
      )
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
      if (length(pv$history) > 1) {
        pv$history <- pv$history[-1]
      } else {
        pv$history <- list()
      }

      pv$is_pivoted <- !is.null(pv$baseline) && !identical(pv$data, pv$baseline)
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
        utils::head(aurora_drop_internal_cols(pv$data), 200),
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
      label = shiny::reactive(rv$src_label),
      dropped_log = shiny::reactive(rv$dropped_log)
    )
  })
}
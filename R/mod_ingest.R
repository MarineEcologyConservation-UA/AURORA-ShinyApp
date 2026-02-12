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
      shiny::checkboxInput(ns("is_tidy"), "Tidy format", value = TRUE),

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
        bslib::nav_panel("Tidy / Pivot", shiny::uiOutput(ns("pivot_ui"))),
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

    # --------------------------------------------------
    # PIVOT STATE
    # --------------------------------------------------
    pv <- shiny::reactiveValues(
      baseline = NULL,
      data = NULL,
      history = list(),
      history_labels = character()
    )

    # --------------------------------------------------
    # Popup quando ativar Tidy format
    # --------------------------------------------------
    shiny::observeEvent(input$is_tidy, {
    if (isFALSE(input$is_tidy)) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Tidy format ativado",
          "Se precisares transformar dados wide → long, utiliza a aba 'Tidy / Pivot'.",
          easyClose = FALSE,   # não fecha clicando fora
          footer = shiny::modalButton("OK")
        )
      )
    }
  }, ignoreInit = TRUE)

    shiny::observeEvent(rv$tidy, {
      shiny::req(rv$tidy)
      pv$baseline <- rv$tidy
      pv$data <- rv$tidy
      pv$history <- list()
      pv$history_labels <- character()
    }, ignoreInit = TRUE)

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
          encoding = input$encoding,
          is_tidy = input$is_tidy
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
    })

    output$load_info <- shiny::renderText({
      if (is.null(rv$raw)) return("No dataset loaded yet.")

      tidy_rows <- if (is.null(pv$data)) 0 else nrow(pv$data)
      tidy_cols <- if (is.null(pv$data)) 0 else ncol(pv$data)

      paste0(
        "Loaded: ", rv$src_label, "\n",
        "Encoding used: ",
        ifelse(is.null(rv$encoding_used), "NA", rv$encoding_used), "\n\n",
        "Raw: ", nrow(rv$raw), " rows x ", ncol(rv$raw), " cols\n",
        "Tidy: ", tidy_rows, " rows x ", tidy_cols, " cols"
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
    # TIDY PREVIEW (dataset final, após pivot)
    # --------------------------------------------------
    output$tidy_tbl <- DT::renderDT({
      shiny::req(pv$data)
      DT::datatable(
        utils::head(pv$data, 200),
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    # --------------------------------------------------
    # COLUMN SUMMARY (sobre raw)
    # --------------------------------------------------
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

    # --------------------------------------------------
    # PIVOT UI
    # --------------------------------------------------
    output$pivot_ui <- shiny::renderUI({
      shiny::req(pv$data)

      df <- pv$data
      cols <- names(df)
      num_cols <- cols[vapply(df, is.numeric, logical(1))]

      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectizeInput(
            session$ns("pivot_id_cols"),
            "ID columns",
            choices = cols,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::selectizeInput(
            session$ns("pivot_value_cols"),
            "Columns to pivot",
            choices = cols,
            selected = num_cols,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::textInput(session$ns("pivot_names_to"), "names_to", "variable"),
          shiny::textInput(session$ns("pivot_values_to"), "values_to", "value"),
          shiny::checkboxInput(session$ns("pivot_drop_na"), "Drop NA", TRUE),
          shiny::checkboxInput(session$ns("pivot_drop_zero"), "Drop zero", FALSE),
          shiny::checkboxInput(session$ns("pivot_trim_names"), "Trim names_to", TRUE),
          shiny::br(),
          shiny::actionButton(session$ns("pivot_apply"), "Pivotar"),
          shiny::actionButton(session$ns("pivot_undo"), "Desfazer"),
          shiny::actionButton(session$ns("pivot_reset"), "Resetar")
        ),
        shiny::column(
          8,
          shiny::verbatimTextOutput(session$ns("pivot_info")),
          #shiny::h5("Preview (antes)"),
          #DT::DTOutput(session$ns("pivot_before")),
          shiny::h5("Preview (depois)"),
          DT::DTOutput(session$ns("pivot_after"))
        )
      )
    })

    # --------------------------------------------------
    # PIVOT INFO (prova que alterou)
    # --------------------------------------------------
    output$pivot_info <- shiny::renderText({
      shiny::req(pv$data)

      before <- if (length(pv$history) > 0) pv$history[[1]] else pv$data
      after <- pv$data

      paste0(
        "Antes: ", nrow(before), " linhas x ", ncol(before), " colunas\n",
        "Depois: ", nrow(after), " linhas x ", ncol(after), " colunas\n",
        "Passos disponíveis para desfazer: ", length(pv$history)
      )
    })

    # --------------------------------------------------
    # PIVOT APPLY (seguro com tryCatch; só empilha se funcionar)
    # --------------------------------------------------
    shiny::observeEvent(input$pivot_apply, {
      shiny::req(pv$data)

      df_before <- pv$data

      df_after <- tryCatch({
        apply_pivot_longer(
          df = df_before,
          id_cols = input$pivot_id_cols %||% character(),
          pivot_cols = input$pivot_value_cols %||% character(),
          names_to = input$pivot_names_to,
          values_to = input$pivot_values_to,
          drop_na = isTRUE(input$pivot_drop_na),
          drop_zero = isTRUE(input$pivot_drop_zero),
          trim_names = isTRUE(input$pivot_trim_names)
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Pivot failed:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        NULL
      })

      if (is.null(df_after)) return()

      pv$history <- c(list(df_before), pv$history)
      pv$data <- df_after
    })

    # --------------------------------------------------
    # UNDO / RESET
    # --------------------------------------------------
    shiny::observeEvent(input$pivot_undo, {
      if (length(pv$history) == 0) {
        shiny::showNotification("Nada para desfazer.", type = "message", duration = 3)
        return()
      }
      pv$data <- pv$history[[1]]
      pv$history <- pv$history[-1]
    })

    shiny::observeEvent(input$pivot_reset, {
      shiny::req(pv$baseline)
      pv$data <- pv$baseline
      pv$history <- list()
      pv$history_labels <- character()
    })

    # --------------------------------------------------
    # PIVOT PREVIEWS (limitados, mas com total mostrado em pivot_info)
    # --------------------------------------------------

    output$pivot_after <- DT::renderDT({
      shiny::req(pv$data)
      DT::datatable(
        utils::head(pv$data, 50),
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    # --------------------------------------------------
    # Return (mantém API: ingest$raw(), ingest$tidy(), ingest$label())
    # --------------------------------------------------
    list(
      raw = shiny::reactive(rv$raw),
      tidy = shiny::reactive(pv$data),
      label = shiny::reactive(rv$src_label)
    )
  })
}

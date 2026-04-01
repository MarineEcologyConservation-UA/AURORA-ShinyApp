# =========================================================
# Shiny Module: Build DwC-A
# =========================================================

#' @export
mod_build_dwca_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        /* Espaço e alinhamento para o 'footer' abaixo de cada DT */
        .dwca-tab-footer {
          margin-top: 12px;
          margin-bottom: 6px;
          display: flex;
          gap: 10px;
          justify-content: flex-start;
          align-items: center;
        }

        /* Garante que o DT não 'cola' no footer */
        .dwca-dt-wrap {
          padding-bottom: 8px;
        }
      "))
    ),

    shiny::h3("Build Darwin Core Archive"),

    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          ns("event_mode"),
          "EventID rule",
          choices = c("use", "concat", "auto_row")
        ),

        shiny::uiOutput(ns("event_column_ui")),
        shiny::uiOutput(ns("event_concat_ui")),

        shiny::checkboxInput(
          ns("enable_parent"),
          "Create parentEventID",
          FALSE
        ),
        shiny::uiOutput(ns("parent_cols_ui"))
      ),

      shiny::column(
        4,
        shiny::selectInput(
          ns("occ_mode"),
          "OccurrenceID rule",
          choices = c("event_seq", "use")
        ),

        shiny::uiOutput(ns("occ_use_ui")),

        shiny::hr(),

        shiny::h4("eMoF columns"),

        shiny::selectizeInput(
          ns("emof_cols_event"),
          "Columns to move to eMoF (event level)",
          choices = NULL,
          multiple = TRUE
        ),

        shiny::selectizeInput(
          ns("emof_cols_occ"),
          "Columns to move to eMoF (occurrence level)",
          choices = NULL,
          multiple = TRUE
        ),

        shiny::hr(),

        shiny::checkboxInput(
          ns("enable_remarks"),
          "Create remarks column (join with '|')",
          FALSE
        ),
        shiny::uiOutput(ns("remarks_ui"))
      ),

      shiny::column(
        4,
        shiny::actionButton(
          ns("build"),
          "Build DwC-A",
          class = "btn-primary"
        )
      )
    ),

    shiny::hr(),

    shiny::h4("Quality report"),
    shiny::verbatimTextOutput(ns("qc")),

    shiny::hr(),

    shiny::tabsetPanel(
      shiny::tabPanel(
        "Event",
        shiny::div(
          class = "dwca-dt-wrap",
          DT::DTOutput(ns("event_preview"))
        ),
        shiny::div(
          class = "dwca-tab-footer",
          shiny::downloadButton(ns("download_event"), "Download Event CSV")
        )
      ),

      shiny::tabPanel(
        "Occurrence",
        shiny::div(
          class = "dwca-dt-wrap",
          DT::DTOutput(ns("occ_preview"))
        ),
        shiny::div(
          class = "dwca-tab-footer",
          shiny::downloadButton(ns("download_occ"), "Download Occurrence CSV")
        )
      ),

      shiny::tabPanel(
        "eMoF",
        shiny::div(
          class = "dwca-dt-wrap",
          DT::DTOutput(ns("emof_preview"))
        ),
        shiny::div(
          class = "dwca-tab-footer",
          shiny::downloadButton(ns("download_emof"), "Download eMoF CSV")
        )
      )
    )
  )
}

#' @export
mod_build_dwca_server <- function(id, df_in, dwc_terms) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- shiny::reactiveValues(
      ready = FALSE
    )

    shiny::observe({
      shiny::req(df_in())
      cols <- names(df_in())

      shiny::updateSelectizeInput(session, "emof_cols_event", choices = cols)
      shiny::updateSelectizeInput(session, "emof_cols_occ", choices = cols)
      shiny::updateSelectizeInput(session, "remarks_cols", choices = cols)
    })

    output$event_column_ui <- shiny::renderUI({
      shiny::req(df_in())
      if (input$event_mode == "use") {
        shiny::selectInput(
          ns("event_column"),
          "Select eventID column",
          choices = names(df_in())
        )
      }
    })

    output$event_concat_ui <- shiny::renderUI({
      shiny::req(df_in())
      if (input$event_mode == "concat") {
        shiny::selectizeInput(
          ns("event_concat_cols"),
          "Columns to build eventID (concat with ':')",
          choices = names(df_in()),
          multiple = TRUE
        )
      }
    })

    output$parent_cols_ui <- shiny::renderUI({
      shiny::req(df_in())
      if (isTRUE(input$enable_parent)) {
        shiny::selectizeInput(
          ns("parent_cols"),
          "Columns for parentEventID (concat with ':')",
          choices = names(df_in()),
          multiple = TRUE
        )
      }
    })

    output$occ_use_ui <- shiny::renderUI({
      shiny::req(df_in())
      if (input$occ_mode == "use") {
        shiny::selectInput(
          ns("occ_column"),
          "Select occurrenceID column",
          choices = names(df_in()),
          selected = if ("occurrenceID" %in% names(df_in())) "occurrenceID" else NULL
        )
      }
    })

    output$remarks_ui <- shiny::renderUI({
      shiny::req(df_in())
      if (!isTRUE(input$enable_remarks)) return(NULL)

      shiny::tagList(
        shiny::selectizeInput(
          ns("remarks_cols"),
          "Columns to join into remarks",
          choices = names(df_in()),
          multiple = TRUE
        ),
        shiny::textInput(
          ns("remarks_target"),
          "Target remarks column name",
          value = "occurrenceRemarks"
        )
      )
    })

    emof_levels <- shiny::reactive({
      ev <- input$emof_cols_event
      oc <- input$emof_cols_occ
      if (is.null(ev)) ev <- character(0)
      if (is.null(oc)) oc <- character(0)

      ev <- unique(as.character(ev))
      oc <- unique(as.character(oc))

      levs <- c(
        setNames(as.list(rep("event", length(ev))), ev),
        setNames(as.list(rep("occurrence", length(oc))), oc)
      )

      if (length(levs) == 0) return(NULL)
      levs
    })

    # reset ready if upstream data changes
    shiny::observeEvent(df_in(), {
      rv$ready <- FALSE
    }, ignoreInit = FALSE)

    # reset ready if user changes any build-relevant option
    shiny::observeEvent(input$event_mode, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$event_column, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$event_concat_cols, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$enable_parent, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$parent_cols, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$occ_mode, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$occ_column, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$emof_cols_event, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$emof_cols_occ, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$enable_remarks, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$remarks_cols, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$remarks_target, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    result <- shiny::eventReactive(input$build, {
      shiny::req(df_in())

      if (input$event_mode == "concat") {
        if (is.null(input$event_concat_cols) || length(input$event_concat_cols) == 0) {
          stop("Select at least 1 column to build eventID by concat.")
        }
      }
      if (isTRUE(input$enable_parent)) {
        if (is.null(input$parent_cols) || length(input$parent_cols) == 0) {
          stop("Select at least 1 column to build parentEventID.")
        }
      }
      if (input$occ_mode == "use") {
        if (is.null(input$occ_column) || !nzchar(input$occ_column)) {
          stop("Select the occurrenceID column (or use event_seq).")
        }
      }

      if (isTRUE(input$enable_remarks)) {
        if (is.null(input$remarks_cols) || length(input$remarks_cols) == 0) {
          stop("Select at least 1 column for remarks (or disable remarks).")
        }
        if (is.null(input$remarks_target) || !nzchar(input$remarks_target)) {
          stop("Provide a target column name for remarks.")
        }
      }

      ev <- input$emof_cols_event
      oc <- input$emof_cols_occ
      if (is.null(ev)) ev <- character(0)
      if (is.null(oc)) oc <- character(0)

      both <- intersect(ev, oc)
      if (length(both) > 0) {
        stop(paste0(
          "These columns were selected in BOTH eMoF lists (choose only one level): ",
          paste(both, collapse = ", ")
        ))
      }

      emof_cols_all <- unique(c(ev, oc))
      levs <- emof_levels()

      build_dwca_tables(
        df = df_in(),
        dwc_terms = dwc_terms,

        id_spec = list(
          event_mode = input$event_mode,
          event_column = input$event_column,
          event_concat = input$event_concat_cols,
          occ_mode = input$occ_mode,
          occ_column = input$occ_column
        ),

        parent_spec = list(
          enabled = isTRUE(input$enable_parent),
          columns = input$parent_cols
        ),

        emof_spec = list(
          columns = emof_cols_all,
          levels = levs
        ),

        remarks_spec = if (isTRUE(input$enable_remarks)) {
          list(columns = input$remarks_cols, target = input$remarks_target)
        } else {
          NULL
        }
      )
    })

    # mark ready only after a successful build
    shiny::observeEvent(result(), {
      shiny::req(result())
      rv$ready <- TRUE
    }, ignoreInit = TRUE)

    output$qc <- shiny::renderPrint({
      shiny::req(result())
      result()$qc
    })

    output$event_preview <- DT::renderDT({
      shiny::req(result())
      DT::datatable(
        utils::head(result()$event, 50),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    output$occ_preview <- DT::renderDT({
      shiny::req(result())
      DT::datatable(
        utils::head(result()$occurrence, 50),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    output$emof_preview <- DT::renderDT({
      shiny::req(result())
      x <- result()$emof
      if (is.null(x)) x <- data.frame()
      DT::datatable(
        utils::head(x, 50),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    output$download_event <- shiny::downloadHandler(
      filename = function() "event.csv",
      content = function(file) {
        utils::write.csv(result()$event, file, row.names = FALSE, na = "")
      }
    )

    output$download_occ <- shiny::downloadHandler(
      filename = function() "occurrence.csv",
      content = function(file) {
        utils::write.csv(result()$occurrence, file, row.names = FALSE, na = "")
      }
    )

    output$download_emof <- shiny::downloadHandler(
      filename = function() "emof.csv",
      content = function(file) {
        x <- result()$emof
        if (is.null(x)) x <- data.frame()
        utils::write.csv(x, file, row.names = FALSE, na = "")
      }
    )

    # -----------------------------------------------------
    # Return reactives so other pages (QC/Export) can consume
    # -----------------------------------------------------
    list(
      result = result,
      event = shiny::reactive({
        shiny::req(result())
        result()$event
      }),
      occurrence = shiny::reactive({
        shiny::req(result())
        result()$occurrence
      }),
      emof = shiny::reactive({
        shiny::req(result())
        x <- result()$emof
        if (is.null(x)) x <- data.frame()
        x
      }),
      qc_report = shiny::reactive({
        shiny::req(result())
        result()$qc
      }),
      ready = shiny::reactive(isTRUE(rv$ready))
    )
  })
}
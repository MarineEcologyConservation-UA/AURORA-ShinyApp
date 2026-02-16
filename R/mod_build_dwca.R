# =========================================================
# Shiny Module: Build DwC-A
# =========================================================

mod_build_dwca_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(

    shiny::h3("Build Darwin Core Archive"),

    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          ns("event_mode"),
          "EventID rule",
          choices = c("use", "concat", "auto_row")
        ),

        # use -> escolhe coluna
        shiny::uiOutput(ns("event_column_ui")),

        # concat -> escolhe colunas
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

        shiny::selectizeInput(
          ns("emof_cols"),
          "Columns to move to eMoF",
          choices = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          ns("emof_level"),
          "eMoF level",
          choices = c("occurrence", "event")
        )
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
      shiny::tabPanel("Event",
        DT::DTOutput(ns("event_preview"))
      ),
      shiny::tabPanel("Occurrence",
        DT::DTOutput(ns("occ_preview"))
      ),
      shiny::tabPanel("eMoF",
        DT::DTOutput(ns("emof_preview"))
      )
    ),

    shiny::hr(),

    shiny::downloadButton(ns("download_event"), "Download Event CSV"),
    shiny::downloadButton(ns("download_occ"), "Download Occurrence CSV"),
    shiny::downloadButton(ns("download_emof"), "Download eMoF CSV")
  )
}

mod_build_dwca_server <- function(id, df_in, dwc_terms) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    sshiny::observe({
      shiny::req(df_in())
      shiny::updateSelectizeInput(
        session,
        "emof_cols",
        choices = names(df_in())
      )
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

    result <- shiny::eventReactive(input$build, {
      shiny::req(df_in())

      # validações mínimas para evitar "falhas silenciosas"
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
          columns = input$emof_cols,
          level = input$emof_level
        )
      )
    })

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
  })
}

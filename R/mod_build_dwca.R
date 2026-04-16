# =========================================================
# Shiny Module: Build DwC-A
# =========================================================

#' @export
mod_build_dwca_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .dwca-page {
          padding: 1rem 1.25rem 2rem 1.25rem;
        }

        .dwca-title-wrap {
          margin-bottom: 1.25rem;
        }

        .dwca-title {
          margin-bottom: 0.35rem;
          font-weight: 700;
        }

        .dwca-subtitle {
          color: #6c757d;
          margin-bottom: 0;
        }

        .dwca-top-card,
        .dwca-preview-card,
        .dwca-qc-card {
          border: 1px solid #e5e7eb;
          border-radius: 16px;
          box-shadow: 0 4px 14px rgba(15, 23, 42, 0.04);
          background: #ffffff;
        }

        .dwca-top-card .card-header,
        .dwca-preview-card .card-header,
        .dwca-qc-card .card-header {
          background: #f8fafc;
          border-bottom: 1px solid #e5e7eb;
          font-weight: 700;
          border-top-left-radius: 16px;
          border-top-right-radius: 16px;
        }

        .dwca-section-note {
          color: #6c757d;
          font-size: 0.95rem;
          margin-bottom: 0;
        }

        .dwca-build-actions {
          display: flex;
          justify-content: flex-end;
          align-items: flex-start;
          height: 100%;
        }

        .dwca-build-btn {
          min-width: 160px;
          padding: 0.7rem 1.1rem;
          font-weight: 600;
          border-radius: 12px;
        }

        .dwca-divider {
          margin: 1rem 0 1.25rem 0;
          border-top: 1px solid #e5e7eb;
        }

        .dwca-tab-footer {
          margin-top: 12px;
          margin-bottom: 6px;
          display: flex;
          gap: 10px;
          justify-content: flex-start;
          align-items: center;
        }

        .dwca-dt-wrap {
          padding-bottom: 8px;
        }

        .dwca-qc-pre {
          background: #f8fafc;
          border: 1px solid #e5e7eb;
          border-radius: 12px;
          padding: 0.85rem 1rem;
          min-height: 120px;
        }

        .dwca-preview-card .nav-tabs .nav-link {
          font-weight: 500;
        }

        .dwca-preview-card .tab-content {
          padding-top: 0.5rem;
        }

        .selectize-dropdown {
          z-index: 5000 !important;
        }

        @media (max-width: 991.98px) {
          .dwca-build-actions {
            justify-content: flex-start;
            margin-top: 1rem;
          }
        }
      "))
    ),

    shiny::div(
      class = "dwca-page",

      shiny::div(
        class = "dwca-title-wrap",
        shiny::h3(class = "dwca-title", "Build Darwin Core Tables"),
        shiny::p(
          class = "dwca-subtitle",
          "Generate the Darwin Core tables and choose which measurement-related columns should be exported to eMoF."
        )
      ),

      bslib::card(
        class = "dwca-top-card mb-4",
        bslib::card_header("Build settings"),
        bslib::card_body(
          shiny::fluidRow(
            shiny::column(
              8,
              shiny::p(
                class = "dwca-section-note",
                "Select columns to move to eMoF and build the Darwin Core tables."
              )
            ),
            shiny::column(
              4,
              shiny::div(
                class = "dwca-build-actions",
                shiny::actionButton(
                  ns("build"),
                  "Build DwC-A",
                  class = "btn-primary dwca-build-btn"
                )
              )
            )
          ),

          shiny::div(class = "dwca-divider"),

          shiny::fluidRow(
            shiny::column(
              6,
              shiny::h4("eMoF columns"),
              shiny::selectizeInput(
                ns("emof_cols_event"),
                "Columns to move to eMoF (event level)",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  plugins = list("remove_button"),
                  dropdownParent = "body"
                )
              )
            ),
            shiny::column(
              6,
              shiny::h4("\u00A0"),
              shiny::selectizeInput(
                ns("emof_cols_occ"),
                "Columns to move to eMoF (occurrence level)",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  plugins = list("remove_button"),
                  dropdownParent = "body"
                )
              )
            )
          )
        )
      ),

      bslib::card(
        class = "dwca-qc-card mb-4",
        bslib::card_header("Quality report"),
        bslib::card_body(
          shiny::div(
            class = "dwca-qc-pre",
            shiny::verbatimTextOutput(ns("qc"))
          )
        )
      ),

      bslib::card(
        class = "dwca-preview-card",
        bslib::card_header("Table previews"),
        bslib::card_body(
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
      )
    )
  )
}

#' @export
mod_build_dwca_server <- function(id, df_in, dwc_terms) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(
      ready = FALSE
    )

    shiny::observe({
      shiny::req(df_in())
      cols <- names(df_in())

      shiny::updateSelectizeInput(
        session, "emof_cols_event",
        choices = cols,
        server = TRUE
      )

      shiny::updateSelectizeInput(
        session, "emof_cols_occ",
        choices = cols,
        server = TRUE
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

    shiny::observeEvent(df_in(), {
      rv$ready <- FALSE
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$emof_cols_event, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$emof_cols_occ, {
      rv$ready <- FALSE
    }, ignoreInit = TRUE)

    result <- shiny::eventReactive(input$build, {
      shiny::req(df_in())

      df <- df_in()

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
        df = df,
        dwc_terms = dwc_terms,
        emof_spec = list(
          columns = emof_cols_all,
          levels = levs
        )
      )
    })

    shiny::observeEvent(result(), {
      shiny::req(result())
      rv$ready <- TRUE

      shiny::showNotification(
        "Darwin Core Tables built successfully. QC & Diagnostics and Metadata are now available.",
        type = "message",
        duration = 5
      )
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
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
      )
    })

    output$occ_preview <- DT::renderDT({
      shiny::req(result())
      DT::datatable(
        utils::head(result()$occurrence, 50),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
      )
    })

    output$emof_preview <- DT::renderDT({
      shiny::req(result())
      x <- result()$emof
      if (is.null(x)) x <- data.frame()
      DT::datatable(
        utils::head(x, 50),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
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
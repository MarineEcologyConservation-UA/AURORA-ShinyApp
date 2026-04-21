# =========================================================
# Shiny Module: Build DwC-A
# File: R/mod_build_dwca.R
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
        .dwca-qc-card,
        .dwca-section-card {
          border: 1px solid #e5e7eb;
          border-radius: 16px;
          box-shadow: 0 4px 14px rgba(15, 23, 42, 0.04);
          background: #ffffff;
        }

        .dwca-top-card .card-header,
        .dwca-preview-card .card-header,
        .dwca-qc-card .card-header,
        .dwca-section-card .card-header {
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

        .dwca-help-block {
          color: #6c757d;
          font-size: 0.92rem;
          margin-bottom: 0.75rem;
        }

        .dwca-build-actions-bottom {
          display: flex;
          justify-content: flex-end;
          margin-top: 1rem;
        }

        .dwca-build-btn {
          min-width: 160px;
          padding: 0.7rem 1.1rem;
          font-weight: 600;
          border-radius: 12px;
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
          white-space: pre-wrap;
        }

        .dwca-preview-card .nav-tabs .nav-link {
          font-weight: 500;
        }

        .dwca-preview-card .tab-content {
          padding-top: 0.5rem;
        }

        .dwca-main-grid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 1rem;
          align-items: start;
        }

        .dwca-subgrid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 1rem;
          align-items: start;
        }

        .dwca-term-group-title {
          font-size: 1rem;
          font-weight: 700;
          margin-bottom: 0.6rem;
        }

        .dwca-scroll-box {
          border: 1px solid #e5e7eb;
          border-radius: 12px;
          background: #ffffff;
          padding: 0.55rem 0.65rem;
          height: 255px;
          overflow-y: auto;
        }

        .dwca-scroll-box-empty {
          color: #6c757d;
          font-size: 0.92rem;
        }

        .dwca-term-check-row {
          border: 1px solid #e5e7eb;
          border-radius: 10px;
          background: #ffffff;
          padding: 0.5rem 0.7rem;
          margin-bottom: 0.45rem;
        }

        .dwca-term-check-row.is-strong {
          background: #f0f9ff;
          border-color: #bae6fd;
        }

        .dwca-term-check-row.is-recommended {
          background: #fffdf5;
          border-color: #fef3c7;
        }

        .dwca-term-check-row.is-optional {
          background: #ffffff;
          border-color: #e5e7eb;
        }

        .dwca-term-check-row:last-child {
          margin-bottom: 0;
        }

        .dwca-term-check-row .checkbox {
          margin: 0;
        }

        .dwca-term-check-row .checkbox label {
          display: flex;
          align-items: flex-start;
          gap: 0.45rem;
          margin: 0;
          width: 100%;
          font-weight: 600;
          line-height: 1.2;
          word-break: break-word;
        }

        .dwca-term-check-row .checkbox input[type='checkbox'] {
          margin-top: 0.1rem;
          flex: 0 0 auto;
        }

        .dwca-badge {
          display: inline-block;
          margin-left: 0.55rem;
          padding: 0.15rem 0.5rem;
          border-radius: 999px;
          font-size: 0.78rem;
          line-height: 1.2;
          vertical-align: middle;
          font-weight: 500;
        }

        .dwca-badge-strong {
          background: #dbeafe;
          color: #1d4ed8;
        }

        .dwca-badge-recommended {
          background: #fef3c7;
          color: #92400e;
        }

        .dwca-badge-optional {
          background: #f3f4f6;
          color: #4b5563;
        }

        .dwca-build-error {
          margin-bottom: 1rem;
        }

        .dwca-card-spacer {
          height: 1rem;
        }

        .dwca-resource-type-wrap .radio {
          margin-top: 0;
          margin-bottom: 0.35rem;
        }

        .dwca-resource-type-wrap .radio:last-child {
          margin-bottom: 0;
        }

        @media (max-width: 991.98px) {
          .dwca-main-grid,
          .dwca-subgrid {
            grid-template-columns: 1fr;
          }

          .dwca-build-actions-bottom {
            justify-content: flex-start;
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
          "Choose the archive resource type, review the allowed fields for that architecture, and build the Darwin Core tables."
        )
      ),

      bslib::card(
        class = "dwca-top-card mb-4",
        bslib::card_header("Build settings"),
        bslib::card_body(
          shiny::uiOutput(ns("build_info_ui"))
        )
      ),

      shiny::uiOutput(ns("table_cards_ui")),

      shiny::div(style = "height: 1rem;"),

      bslib::card(
        class = "dwca-section-card mb-4",
        bslib::card_header("eMoF table"),
        bslib::card_body(
          shiny::uiOutput(ns("build_error_ui")),
          shiny::uiOutput(ns("emof_terms_ui")),
          shiny::div(
            class = "dwca-build-actions-bottom",
            shiny::actionButton(
              ns("build"),
              "Build DwC-A",
              class = "btn-primary dwca-build-btn"
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
            shiny::textOutput(ns("qc"))
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
mod_build_dwca_server <- function(id, df_in, dwc_terms, target_database_in) {
  shiny::moduleServer(id, function(input, output, session) {

    `%||%` <- function(x, y) if (is.null(x)) y else x

    rv <- shiny::reactiveValues(
      ready = FALSE,
      result = NULL,
      build_error = NULL
    )

    normalize_repo_name <- function(x) {
      x <- as.character(x %||% "GBIF")
      x <- trimws(x)
      valid <- c("GBIF", "OBIS", "EMODnet")
      if (!x %in% valid) x <- "GBIF"
      x
    }

    normalize_resource_type <- function(x) {
      x <- as.character(x %||% "sampling_event")
      x <- trimws(tolower(x))
      if (!x %in% c("sampling_event", "occurrence_core")) {
        x <- "sampling_event"
      }
      x
    }

    normalize_table_name <- function(x) {
      x <- as.character(x %||% "")
      x <- trimws(x)

      out <- rep(NA_character_, length(x))
      out[x == "Event"] <- "Event"
      out[x == "Occurrence"] <- "Occurrence"
      out[x == "Occurrence.Core"] <- "Occurrence.Core"
      out[x == "eMoF"] <- "eMoF"
      out
    }

    safe_id_piece <- function(x) {
      x <- as.character(x %||% "")
      x <- gsub("[^A-Za-z0-9]+", "_", x)
      x <- gsub("^_+|_+$", "", x)
      if (!nzchar(x)) x <- "term"
      x
    }

    normalize_status <- function(x) {
      x <- as.character(x %||% "")
      x[is.na(x)] <- ""
      x <- trimws(tolower(x))
      x[x %in% c("strongly_recommended", "strongly-recommended")] <- "strongly recommended"
      x[x %in% c("optional", "neutral", "", "na")] <- "optional"
      x
    }

    badge_html <- function(status) {
      st <- normalize_status(status)
      if (st == "strongly recommended") {
        return("<span class='dwca-badge dwca-badge-strong'>strongly recommended</span>")
      }
      if (st == "recommended") {
        return("<span class='dwca-badge dwca-badge-recommended'>recommended</span>")
      }
      if (st == "optional") {
        return("<span class='dwca-badge dwca-badge-optional'>optional</span>")
      }
      ""
    }

    status_rank <- function(status) {
      st <- normalize_status(status)
      ifelse(
        st == "required", 1L,
        ifelse(
          st == "strongly recommended", 2L,
          ifelse(st == "recommended", 3L, 4L)
        )
      )
    }

    repo_col <- shiny::reactive({
      normalize_repo_name(target_database_in())
    })

    resource_type_rx <- shiny::reactive({
      normalize_resource_type(input$resource_type)
    })

    resource_config <- shiny::reactive({
      rt <- resource_type_rx()

      if (identical(rt, "sampling_event")) {
        return(list(
          resource_type = "sampling_event",
          event_table_key = "Event",
          occurrence_table_key = "Occurrence",
          show_event = TRUE,
          show_occurrence = TRUE,
          show_emof_event = TRUE,
          show_emof_occurrence = TRUE,
          occurrence_card_title = "Occurrence table",
          architecture_label = "Sampling event (Event core)"
        ))
      }

      list(
        resource_type = "occurrence_core",
        event_table_key = NULL,
        occurrence_table_key = "Occurrence.Core",
        show_event = FALSE,
        show_occurrence = TRUE,
        show_emof_event = FALSE,
        show_emof_occurrence = TRUE,
        occurrence_card_title = "Occurrence.Core table",
        architecture_label = "Occurrence (Occurrence core)"
      )
    })

    dwc_terms_norm <- shiny::reactive({
      shiny::req(is.data.frame(dwc_terms))
      shiny::req(all(c("Term", "Table") %in% names(dwc_terms)))

      out <- dwc_terms
      out$Term <- as.character(out$Term)
      out$Table <- as.character(out$Table)
      out$.table_norm <- normalize_table_name(out$Table)

      repo_nm <- repo_col()
      if (!repo_nm %in% names(out)) {
        stop(paste0("Column '", repo_nm, "' was not found in dwc_terms."))
      }

      out$.repo_status <- normalize_status(out[[repo_nm]])
      out <- out[!is.na(out$.table_norm) & out$.table_norm != "", , drop = FALSE]
      out
    })

    present_terms <- shiny::reactive({
      df <- df_in()
      shiny::req(is.data.frame(df))
      names(df)
    })

    table_term_specs <- shiny::reactive({
      df <- df_in()
      shiny::req(is.data.frame(df))

      dt <- dwc_terms_norm()
      terms_present <- present_terms()

      dt <- dt[dt$Term %in% terms_present, , drop = FALSE]

      make_tbl <- function(tbl_name) {
        if (is.null(tbl_name) || !nzchar(tbl_name)) {
          return(data.frame(
            term = character(),
            status = character(),
            selected_default = logical(),
            locked = logical(),
            stringsAsFactors = FALSE
          ))
        }

        x <- dt[dt$.table_norm == tbl_name, c("Term", ".repo_status"), drop = FALSE]

        if (nrow(x) == 0) {
          return(data.frame(
            term = character(),
            status = character(),
            selected_default = logical(),
            locked = logical(),
            stringsAsFactors = FALSE
          ))
        }

        x <- x[!duplicated(x$Term), , drop = FALSE]
        names(x) <- c("term", "status")

        x$term <- as.character(x$term)
        x$status <- normalize_status(x$status)
        x$locked <- x$status == "required"
        x$selected_default <- x$status == "strongly recommended"

        x <- x[order(status_rank(x$status), x$term), , drop = FALSE]
        rownames(x) <- NULL
        x
      }

      cfg <- resource_config()

      list(
        Event = make_tbl(cfg$event_table_key),
        Occurrence = make_tbl(cfg$occurrence_table_key)
      )
    })

    emof_candidate_specs <- shiny::reactive({
      df <- df_in()
      shiny::req(is.data.frame(df))

      cols <- names(df)

      structural_exclude <- c(
        "eventID",
        "occurrenceID",
        "parentEventID"
      )

      cols <- setdiff(cols, structural_exclude)
      cols <- cols[!grepl("^\\.aurora", cols)]
      cols <- sort(unique(cols))

      dt <- dwc_terms_norm()
      repo_map <- dt[dt$.table_norm == "eMoF", c("Term", ".repo_status"), drop = FALSE]
      repo_map <- repo_map[!duplicated(repo_map$Term), , drop = FALSE]

      status_map <- stats::setNames(repo_map$.repo_status, repo_map$Term)

      status <- unname(status_map[cols])
      status[is.na(status)] <- ""
      status <- normalize_status(status)

      out <- data.frame(
        term = cols,
        status = status,
        selected_default = status == "strongly recommended",
        stringsAsFactors = FALSE
      )

      out <- out[order(status_rank(out$status), out$term), , drop = FALSE]
      rownames(out) <- NULL
      out
    })

    checkbox_value_or_default <- function(input_id, default = FALSE) {
      val <- input[[input_id]]
      if (is.null(val)) {
        return(isTRUE(default))
      }
      isTRUE(val)
    }

    reset_table_checkboxes <- function(spec_df, prefix) {
      if (nrow(spec_df) == 0) return(invisible(NULL))

      session$onFlushed(function() {
        for (i in seq_len(nrow(spec_df))) {
          if (isTRUE(spec_df$locked[i])) next
          cid <- paste0(prefix, safe_id_piece(spec_df$term[i]))
          shiny::updateCheckboxInput(
            session,
            inputId = cid,
            value = isTRUE(spec_df$selected_default[i])
          )
        }
      }, once = TRUE)
    }

    render_scroll_choices <- function(spec_df, prefix) {
      if (!"locked" %in% names(spec_df)) {
        spec_df$locked <- FALSE
      }

      sel_df <- spec_df[!as.logical(spec_df$locked), , drop = FALSE]

      if (nrow(sel_df) == 0) {
        return(
          shiny::tags$div(
            class = "dwca-scroll-box",
            shiny::tags$div(
              class = "dwca-scroll-box-empty",
              "No selectable fields are available here for the active rules."
            )
          )
        )
      }

      items <- lapply(seq_len(nrow(sel_df)), function(i) {
        term <- sel_df$term[i]
        status <- sel_df$status[i]
        selected_default <- isTRUE(sel_df$selected_default[i])
        cid <- paste0(prefix, safe_id_piece(term))

        box_class <- if (status == "strongly recommended") {
          "dwca-term-check-row is-strong"
        } else if (status == "recommended") {
          "dwca-term-check-row is-recommended"
        } else {
          "dwca-term-check-row is-optional"
        }

        shiny::tags$div(
          class = box_class,
          shiny::checkboxInput(
            session$ns(cid),
            label = shiny::HTML(paste0(term, badge_html(status))),
            value = selected_default,
            width = "100%"
          )
        )
      })

      shiny::tags$div(
        class = "dwca-scroll-box",
        items
      )
    }

    render_table_card_body <- function(spec_df, prefix, help_text) {
      shiny::tagList(
        shiny::tags$div(class = "dwca-help-block", help_text),
        render_scroll_choices(spec_df, prefix)
      )
    }

    output$build_info_ui <- shiny::renderUI({
      cfg <- resource_config()

      shiny::tagList(
        shiny::div(
          class = "dwca-resource-type-wrap",
          shiny::radioButtons(
            session$ns("resource_type"),
            "Resource type",
            choices = c(
              "Sampling event (Event core)" = "sampling_event",
              "Occurrence (Occurrence core)" = "occurrence_core"
            ),
            selected = cfg$resource_type,
            inline = FALSE
          )
        ),
        shiny::p(
          class = "dwca-section-note",
          paste0(
            "Repository rules currently applied: ",
            repo_col(),
            ". Active architecture: ",
            cfg$architecture_label,
            ". Required fields are included automatically and are not shown here. Strongly recommended fields start selected. Recommended and optional fields start unselected."
          )
        )
      )
    })

    output$table_cards_ui <- shiny::renderUI({
      cfg <- resource_config()
      specs <- table_term_specs()

      cards <- list()

      if (isTRUE(cfg$show_event)) {
        cards[[length(cards) + 1]] <- bslib::card(
          class = "dwca-section-card",
          bslib::card_header("Event table"),
          bslib::card_body(
            render_table_card_body(
              spec_df = specs$Event,
              prefix = "event_term__",
              help_text = "Required fields are included automatically and are not shown here. Only non-required fields are selectable below."
            )
          )
        )
      }

      if (isTRUE(cfg$show_occurrence)) {
        cards[[length(cards) + 1]] <- bslib::card(
          class = "dwca-section-card",
          bslib::card_header(cfg$occurrence_card_title),
          bslib::card_body(
            render_table_card_body(
              spec_df = specs$Occurrence,
              prefix = "occ_term__",
              help_text = "Required fields are included automatically and are not shown here. Only non-required fields are selectable below."
            )
          )
        )
      }

      if (length(cards) == 0) {
        return(NULL)
      }

      if (length(cards) == 1) {
        return(cards[[1]])
      }

      shiny::tags$div(class = "dwca-main-grid", cards)
    })

    output$build_error_ui <- shiny::renderUI({
      msg <- rv$build_error
      if (is.null(msg) || !nzchar(msg)) return(NULL)

      shiny::tags$div(
        class = "alert alert-danger dwca-build-error",
        shiny::tags$strong("Build error: "),
        msg
      )
    })

    output$emof_terms_ui <- shiny::renderUI({
      cfg <- resource_config()
      spec <- emof_candidate_specs()

      make_emof_panel <- function(level_prefix, title) {
        shiny::tags$div(
          shiny::tags$div(class = "dwca-term-group-title", title),
          shiny::tags$div(
            class = "dwca-help-block",
            "Select the columns that should become eMoF records at this level."
          ),
          render_scroll_choices(spec, level_prefix)
        )
      }

      if (nrow(spec) == 0) {
        return(
          shiny::tags$div(
            class = "dwca-help-block",
            "No eligible columns are available for eMoF in the current dataset."
          )
        )
      }

      if (isTRUE(cfg$show_emof_event) && isTRUE(cfg$show_emof_occurrence)) {
        return(
          shiny::tagList(
            shiny::tags$div(
              class = "dwca-help-block",
              "The available eMoF levels depend on the selected resource type. Do not select the same field in both lists."
            ),
            shiny::tags$div(
              class = "dwca-subgrid",
              make_emof_panel("emof_event__", "eMoF - event"),
              make_emof_panel("emof_occ__", "eMoF - occurrence")
            )
          )
        )
      }

      if (isTRUE(cfg$show_emof_occurrence)) {
        return(
          shiny::tagList(
            shiny::tags$div(
              class = "dwca-help-block",
              "For Occurrence core, only occurrence-level eMoF is available."
            ),
            make_emof_panel("emof_occ__", "eMoF - occurrence")
          )
        )
      }

      shiny::tags$div(
        class = "dwca-help-block",
        "eMoF is not available for the active architecture."
      )
    })

    selected_table_terms <- shiny::reactive({
      specs <- table_term_specs()
      cfg <- resource_config()

      event_keep <- character(0)
      if (isTRUE(cfg$show_event) && nrow(specs$Event) > 0) {
        event_keep <- vapply(seq_len(nrow(specs$Event)), function(i) {
          term <- specs$Event$term[i]
          if (isTRUE(specs$Event$locked[i])) {
            return(term)
          }
          cid <- paste0("event_term__", safe_id_piece(term))
          keep <- checkbox_value_or_default(cid, default = specs$Event$selected_default[i])
          if (keep) term else NA_character_
        }, character(1))
        event_keep <- event_keep[!is.na(event_keep)]
      }

      occ_keep <- character(0)
      if (isTRUE(cfg$show_occurrence) && nrow(specs$Occurrence) > 0) {
        occ_keep <- vapply(seq_len(nrow(specs$Occurrence)), function(i) {
          term <- specs$Occurrence$term[i]
          if (isTRUE(specs$Occurrence$locked[i])) {
            return(term)
          }
          cid <- paste0("occ_term__", safe_id_piece(term))
          keep <- checkbox_value_or_default(cid, default = specs$Occurrence$selected_default[i])
          if (keep) term else NA_character_
        }, character(1))
        occ_keep <- occ_keep[!is.na(occ_keep)]
      }

      list(
        event = unique(event_keep),
        occurrence = unique(occ_keep)
      )
    })

    emof_selected <- shiny::reactive({
      cfg <- resource_config()
      spec <- emof_candidate_specs()

      event_cols <- character(0)
      occ_cols <- character(0)

      if (nrow(spec) > 0 && isTRUE(cfg$show_emof_event)) {
        event_cols <- vapply(seq_len(nrow(spec)), function(i) {
          term <- spec$term[i]
          cid <- paste0("emof_event__", safe_id_piece(term))
          keep <- checkbox_value_or_default(cid, default = FALSE)
          if (keep) term else NA_character_
        }, character(1))
        event_cols <- event_cols[!is.na(event_cols)]
      }

      if (nrow(spec) > 0 && isTRUE(cfg$show_emof_occurrence)) {
        occ_cols <- vapply(seq_len(nrow(spec)), function(i) {
          term <- spec$term[i]
          cid <- paste0("emof_occ__", safe_id_piece(term))
          keep <- checkbox_value_or_default(cid, default = FALSE)
          if (keep) term else NA_character_
        }, character(1))
        occ_cols <- occ_cols[!is.na(occ_cols)]
      }

      list(
        event = unique(event_cols),
        occurrence = unique(occ_cols)
      )
    })

    emof_levels <- shiny::reactive({
      sel <- emof_selected()

      ev <- sel$event %||% character(0)
      oc <- sel$occurrence %||% character(0)

      levs <- c(
        setNames(as.list(rep("event", length(ev))), ev),
        setNames(as.list(rep("occurrence", length(oc))), oc)
      )

      if (length(levs) == 0) return(NULL)
      levs
    })

    shiny::observeEvent(list(df_in(), target_database_in(), input$resource_type), {
      rv$ready <- FALSE
      rv$result <- NULL
      rv$build_error <- NULL
    }, ignoreInit = FALSE)

    shiny::observeEvent(list(table_term_specs(), input$resource_type), {
      specs <- table_term_specs()
      cfg <- resource_config()

      if (isTRUE(cfg$show_event)) {
        reset_table_checkboxes(specs$Event, "event_term__")
      }

      if (isTRUE(cfg$show_occurrence)) {
        reset_table_checkboxes(specs$Occurrence, "occ_term__")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(list(emof_candidate_specs(), input$resource_type), {
      spec <- emof_candidate_specs()
      cfg <- resource_config()

      session$onFlushed(function() {
        for (i in seq_len(nrow(spec))) {
          term <- spec$term[i]

          if (isTRUE(cfg$show_emof_event)) {
            shiny::updateCheckboxInput(
              session,
              inputId = paste0("emof_event__", safe_id_piece(term)),
              value = FALSE
            )
          }

          if (isTRUE(cfg$show_emof_occurrence)) {
            shiny::updateCheckboxInput(
              session,
              inputId = paste0("emof_occ__", safe_id_piece(term)),
              value = FALSE
            )
          }
        }
      }, once = TRUE)
    }, ignoreInit = TRUE)

    build_once <- function() {
      df <- df_in()
      shiny::req(is.data.frame(df))

      cfg <- resource_config()
      emof_sel <- emof_selected()

      both <- intersect(emof_sel$event, emof_sel$occurrence)

      if (length(both) > 0) {
        stop(
          paste0(
            "These columns were selected in BOTH eMoF lists. Choose only one level for each field: ",
            paste(both, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      build_dwca_tables(
        df = df,
        dwc_terms = dwc_terms,
        target_database = repo_col(),
        resource_type = cfg$resource_type,
        selected_terms = selected_table_terms(),
        emof_spec = list(
          columns = unique(c(emof_sel$event, emof_sel$occurrence)),
          levels = emof_levels()
        )
      )
    }

    shiny::observeEvent(input$build, {
      rv$build_error <- NULL
      rv$ready <- FALSE
      rv$result <- NULL

      res <- tryCatch(
        {
          build_once()
        },
        error = function(e) {
          e
        }
      )

      if (inherits(res, "error")) {
        rv$build_error <- conditionMessage(res)

        shiny::showNotification(
          rv$build_error,
          type = "error",
          duration = 8
        )
        return(invisible(NULL))
      }

      rv$result <- res
      rv$ready <- TRUE

      shiny::showNotification(
        "Darwin Core Tables built successfully. QC & Diagnostics and Metadata are now available.",
        type = "message",
        duration = 5
      )
    }, ignoreInit = TRUE)

    output$qc <- shiny::renderText({
      if (is.null(rv$result)) {
        return("Build the Darwin Core tables to see the quality report.")
      }
      paste(rv$result$qc, collapse = "\n")
    })

    output$event_preview <- DT::renderDT({
      shiny::req(rv$result)
      x <- rv$result$event
      if (is.null(x)) x <- data.frame()
      x <- aurora_drop_internal_cols(x)

      DT::datatable(
        utils::head(x, 50),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
      )
    })

    output$occ_preview <- DT::renderDT({
      shiny::req(rv$result)
      x <- rv$result$occurrence
      if (is.null(x)) x <- data.frame()
      x <- aurora_drop_internal_cols(x)

      DT::datatable(
        utils::head(x, 50),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
      )
    })

    output$emof_preview <- DT::renderDT({
      shiny::req(rv$result)
      x <- rv$result$emof
      if (is.null(x)) x <- data.frame()
      x <- aurora_drop_internal_cols(x)

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
        shiny::req(rv$result)
        x <- rv$result$event
        if (is.null(x)) x <- data.frame()
        utils::write.csv(
          aurora_drop_internal_cols(x),
          file,
          row.names = FALSE,
          na = ""
        )
      }
    )

    output$download_occ <- shiny::downloadHandler(
      filename = function() "occurrence.csv",
      content = function(file) {
        shiny::req(rv$result)
        x <- rv$result$occurrence
        if (is.null(x)) x <- data.frame()
        utils::write.csv(
          aurora_drop_internal_cols(x),
          file,
          row.names = FALSE,
          na = ""
        )
      }
    )

    output$download_emof <- shiny::downloadHandler(
      filename = function() "emof.csv",
      content = function(file) {
        shiny::req(rv$result)
        x <- rv$result$emof
        if (is.null(x)) x <- data.frame()
        x <- aurora_drop_internal_cols(x)
        utils::write.csv(x, file, row.names = FALSE, na = "")
      }
    )

    result_rx <- shiny::reactive({
      shiny::req(rv$result)
      rv$result
    })

    list(
      result = result_rx,
      event = shiny::reactive({
        shiny::req(rv$result)
        x <- rv$result$event
        if (is.null(x)) x <- data.frame()
        x
      }),
      occurrence = shiny::reactive({
        shiny::req(rv$result)
        x <- rv$result$occurrence
        if (is.null(x)) x <- data.frame()
        x
      }),
      emof = shiny::reactive({
        shiny::req(rv$result)
        x <- rv$result$emof
        if (is.null(x)) x <- data.frame()
        x
      }),
      qc_report = shiny::reactive({
        shiny::req(rv$result)
        rv$result$qc
      }),
      ready = shiny::reactive(isTRUE(rv$ready))
    )
  })
}
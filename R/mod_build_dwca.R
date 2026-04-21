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

        .dwca-select-grid {
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

        .dwca-term-list {
          display: flex;
          flex-direction: column;
          gap: 0.55rem;
        }

        .dwca-term-box {
          border: 1px solid #e5e7eb;
          border-radius: 12px;
          padding: 0.65rem 0.9rem;
          background: #ffffff;
        }

        .dwca-term-box.is-required {
          background: #f8fafc;
          border-color: #cbd5e1;
        }

        .dwca-term-box.is-strong {
          background: #f0f9ff;
          border-color: #bae6fd;
        }

        .dwca-term-box.is-recommended {
          background: #ffffff;
          border-color: #e5e7eb;
        }

        .dwca-term-box.is-neutral {
          background: #ffffff;
          border-color: #e5e7eb;
        }

        .dwca-term-static {
          display: flex;
          align-items: center;
          gap: 0.6rem;
        }

        .dwca-term-static input[type='checkbox'] {
          margin: 0;
        }

        .dwca-term-name {
          font-weight: 600;
        }

        .dwca-badge {
          display: inline-block;
          margin-left: 0.55rem;
          padding: 0.15rem 0.5rem;
          border-radius: 999px;
          font-size: 0.78rem;
          line-height: 1.2;
          vertical-align: middle;
        }

        .dwca-badge-required {
          background: #e2e8f0;
          color: #334155;
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

        .dwca-term-box .checkbox {
          margin: 0;
        }

        .dwca-term-box .checkbox label {
          display: flex;
          align-items: center;
          gap: 0.45rem;
          margin: 0;
          font-weight: 600;
          width: 100%;
        }

        .dwca-term-box .checkbox input[type='checkbox'] {
          margin-top: 0;
        }

        .dwca-help-block {
          color: #6c757d;
          font-size: 0.92rem;
          margin-bottom: 0.75rem;
        }

        .dwca-subgrid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 1rem;
        }

        .dwca-emof-grid-wrap {
          max-height: 380px;
          overflow-y: auto;
          padding-right: 0.25rem;
        }

        .dwca-emof-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
          gap: 0.5rem 0.75rem;
          align-items: start;
        }

        .dwca-emof-item {
          border: 1px solid #e5e7eb;
          border-radius: 10px;
          padding: 0.35rem 0.55rem;
          background: #ffffff;
          min-height: 40px;
        }

        .dwca-emof-item.is-strong {
          background: #f0f9ff;
          border-color: #bae6fd;
        }

        .dwca-emof-item.is-recommended {
          background: #ffffff;
          border-color: #e5e7eb;
        }

        .dwca-emof-item.is-neutral {
          background: #ffffff;
          border-color: #e5e7eb;
        }

        .dwca-emof-item .checkbox {
          margin: 0;
        }

        .dwca-emof-item .checkbox label {
          display: flex;
          align-items: flex-start;
          gap: 0.35rem;
          margin: 0;
          font-weight: 500;
          width: 100%;
          line-height: 1.15;
          font-size: 0.9rem;
          word-break: break-word;
        }

        .dwca-emof-item .checkbox input[type='checkbox'] {
          margin-top: 0.1rem;
          transform: scale(0.95);
          flex: 0 0 auto;
        }

        .dwca-build-error {
          margin-bottom: 1rem;
        }

        @media (max-width: 991.98px) {
          .dwca-select-grid,
          .dwca-subgrid {
            grid-template-columns: 1fr;
          }

          .dwca-build-actions-bottom {
            justify-content: flex-start;
          }

          .dwca-emof-grid {
            grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
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
          "Generate the Darwin Core tables and review which terms and measurement-like columns should remain in Event, Occurrence, and eMoF."
        )
      ),

      bslib::card(
        class = "dwca-top-card mb-4",
        bslib::card_header("Build settings"),
        bslib::card_body(
          shiny::uiOutput(ns("build_info_ui"))
        )
      ),

      shiny::div(
        class = "dwca-select-grid",

        bslib::card(
          class = "dwca-section-card",
          bslib::card_header("Event table"),
          bslib::card_body(
            shiny::uiOutput(ns("event_terms_ui"))
          )
        ),

        bslib::card(
          class = "dwca-section-card",
          bslib::card_header("Occurrence table"),
          bslib::card_body(
            shiny::uiOutput(ns("occurrence_terms_ui"))
          )
        )
      ),

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

    normalize_table_name <- function(x) {
      x <- as.character(x %||% "")
      x <- trimws(x)

      out <- rep(NA_character_, length(x))
      out[x == "Event"] <- "Event"
      out[x %in% c("Occurrence", "Occurrence.Core")] <- "Occurrence"
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

    badge_html <- function(status) {
      st <- tolower(trimws(as.character(status %||% "")))
      if (st == "required") {
        return("<span class='dwca-badge dwca-badge-required'>required</span>")
      }
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

    repo_col <- shiny::reactive({
      normalize_repo_name(target_database_in())
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

      out$.repo_status <- as.character(out[[repo_nm]])
      out$.repo_status[is.na(out$.repo_status)] <- ""
      out$.repo_status <- trimws(tolower(out$.repo_status))
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
      dt <- dt[!is.na(dt$.table_norm) & dt$.table_norm != "", , drop = FALSE]

      make_tbl <- function(tbl_name) {
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
        x$status <- as.character(x$status)
        x$status[is.na(x$status)] <- ""
        x$status <- trimws(tolower(x$status))

        x$status[x$status %in% c("strongly_recommended", "strongly-recommended")] <- "strongly recommended"
        x$status[x$status %in% c("optional", "neutral", "", "na")] <- "optional"

        x$locked <- x$status == "required"
        x$selected_default <- x$status %in% c("required", "strongly recommended")

        status_order <- ifelse(
          x$status == "required", 1L,
          ifelse(
            x$status == "strongly recommended", 2L,
            ifelse(
              x$status == "recommended", 3L,
              4L
            )
          )
        )

        x <- x[order(status_order, x$term), , drop = FALSE]
        rownames(x) <- NULL
        x
      }

      list(
        Event = make_tbl("Event"),
        Occurrence = make_tbl("Occurrence"),
        eMoF = make_tbl("eMoF")
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
      status <- trimws(tolower(as.character(status)))
      status[status %in% c("strongly_recommended", "strongly-recommended")] <- "strongly recommended"
      status[status %in% c("optional", "neutral", "", "na")] <- "optional"

      out <- data.frame(
        term = cols,
        status = status,
        selected_default = status == "strongly recommended",
        stringsAsFactors = FALSE
      )

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

    output$build_info_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::p(
          class = "dwca-section-note",
          paste0(
            "Repository rules currently applied: ",
            repo_col(),
            ". Required terms are always included. Strongly recommended terms start selected. Recommended terms and optional terms start unselected."
          )
        ),
        shiny::div(style = "height: 0.5rem;"),
        shiny::p(
          class = "dwca-help-block",
          "For eMoF, you can choose any eligible column present in the dataset except structural IDs and internal tracking fields."
        )
      )
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

    output$event_terms_ui <- shiny::renderUI({
      spec <- table_term_specs()$Event

      if (nrow(spec) == 0) {
        return(
          shiny::tags$div(
            class = "dwca-help-block",
            "No Event terms from the current dataset match the selected repository rules."
          )
        )
      }

      tag_list <- list(
        shiny::tags$div(
          class = "dwca-help-block",
          "Required terms are locked. Strongly recommended terms start selected. Recommended terms and optional terms start unselected."
        ),
        shiny::tags$div(class = "dwca-term-list")
      )

      boxes <- lapply(seq_len(nrow(spec)), function(i) {
        term <- spec$term[i]
        status <- spec$status[i]
        locked <- isTRUE(spec$locked[i])
        selected_default <- isTRUE(spec$selected_default[i])

        box_class <- if (status == "required") {
          "dwca-term-box is-required"
        } else if (status == "strongly recommended") {
          "dwca-term-box is-strong"
        } else if (status == "recommended") {
          "dwca-term-box is-recommended"
        } else {
          "dwca-term-box is-neutral"
        }

        if (locked) {
          shiny::tags$div(
            class = box_class,
            shiny::tags$div(
              class = "dwca-term-static",
              shiny::tags$input(type = "checkbox", checked = NA, disabled = NA),
              shiny::tags$span(
                class = "dwca-term-name",
                shiny::HTML(paste0(term, badge_html(status)))
              )
            )
          )
        } else {
          cid <- paste0("event_term__", safe_id_piece(term))
          shiny::tags$div(
            class = box_class,
            shiny::checkboxInput(
              session$ns(cid),
              label = shiny::HTML(paste0(term, badge_html(status))),
              value = selected_default,
              width = "100%"
            )
          )
        }
      })

      tag_list[[2]] <- shiny::tags$div(class = "dwca-term-list", boxes)
      shiny::tagList(tag_list)
    })

    output$occurrence_terms_ui <- shiny::renderUI({
      spec <- table_term_specs()$Occurrence

      if (nrow(spec) == 0) {
        return(
          shiny::tags$div(
            class = "dwca-help-block",
            "No Occurrence terms from the current dataset match the selected repository rules."
          )
        )
      }

      tag_list <- list(
        shiny::tags$div(
          class = "dwca-help-block",
          "Required terms are locked. Strongly recommended terms start selected. Recommended terms and optional terms start unselected."
        ),
        shiny::tags$div(class = "dwca-term-list")
      )

      boxes <- lapply(seq_len(nrow(spec)), function(i) {
        term <- spec$term[i]
        status <- spec$status[i]
        locked <- isTRUE(spec$locked[i])
        selected_default <- isTRUE(spec$selected_default[i])

        box_class <- if (status == "required") {
          "dwca-term-box is-required"
        } else if (status == "strongly recommended") {
          "dwca-term-box is-strong"
        } else if (status == "recommended") {
          "dwca-term-box is-recommended"
        } else {
          "dwca-term-box is-neutral"
        }

        if (locked) {
          shiny::tags$div(
            class = box_class,
            shiny::tags$div(
              class = "dwca-term-static",
              shiny::tags$input(type = "checkbox", checked = NA, disabled = NA),
              shiny::tags$span(
                class = "dwca-term-name",
                shiny::HTML(paste0(term, badge_html(status)))
              )
            )
          )
        } else {
          cid <- paste0("occ_term__", safe_id_piece(term))
          shiny::tags$div(
            class = box_class,
            shiny::checkboxInput(
              session$ns(cid),
              label = shiny::HTML(paste0(term, badge_html(status))),
              value = selected_default,
              width = "100%"
            )
          )
        }
      })

      tag_list[[2]] <- shiny::tags$div(class = "dwca-term-list", boxes)
      shiny::tagList(tag_list)
    })

    output$emof_terms_ui <- shiny::renderUI({
      spec <- emof_candidate_specs()

      if (nrow(spec) == 0) {
        return(
          shiny::tags$div(
            class = "dwca-help-block",
            "No eligible columns are available for eMoF in the current dataset."
          )
        )
      }

      make_emof_col <- function(level_prefix, title) {
        items <- lapply(seq_len(nrow(spec)), function(i) {
          term <- spec$term[i]
          status <- spec$status[i]
          selected_default <- isTRUE(spec$selected_default[i])
          cid <- paste0(level_prefix, safe_id_piece(term))

          box_class <- if (status == "strongly recommended") {
            "dwca-emof-item is-strong"
          } else if (status == "recommended") {
            "dwca-emof-item is-recommended"
          } else {
            "dwca-emof-item is-neutral"
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
          shiny::tags$div(class = "dwca-term-group-title", title),
          shiny::tags$div(
            class = "dwca-emof-grid-wrap",
            shiny::tags$div(class = "dwca-emof-grid", items)
          )
        )
      }

      shiny::tagList(
        shiny::tags$div(
          class = "dwca-help-block",
          "Choose which eligible dataset columns should be transformed into eMoF records. Structural IDs are added automatically when needed. Do not select the same column in both event and occurrence levels."
        ),
        shiny::tags$div(
          class = "dwca-subgrid",
          make_emof_col("emof_event__", "Event-level eMoF"),
          make_emof_col("emof_occ__", "Occurrence-level eMoF")
        )
      )
    })

    selected_table_terms <- shiny::reactive({
      specs <- table_term_specs()

      event_spec <- specs$Event
      occ_spec <- specs$Occurrence

      event_keep <- character(0)
      if (nrow(event_spec) > 0) {
        event_keep <- vapply(seq_len(nrow(event_spec)), function(i) {
          term <- event_spec$term[i]
          if (isTRUE(event_spec$locked[i])) {
            return(term)
          }
          cid <- paste0("event_term__", safe_id_piece(term))
          keep <- checkbox_value_or_default(cid, default = event_spec$selected_default[i])
          if (keep) term else NA_character_
        }, character(1))
        event_keep <- event_keep[!is.na(event_keep)]
      }

      occ_keep <- character(0)
      if (nrow(occ_spec) > 0) {
        occ_keep <- vapply(seq_len(nrow(occ_spec)), function(i) {
          term <- occ_spec$term[i]
          if (isTRUE(occ_spec$locked[i])) {
            return(term)
          }
          cid <- paste0("occ_term__", safe_id_piece(term))
          keep <- checkbox_value_or_default(cid, default = occ_spec$selected_default[i])
          if (keep) term else NA_character_
        }, character(1))
        occ_keep <- occ_keep[!is.na(occ_keep)]
      }

      list(
        event = unique(c("eventID", "parentEventID", event_keep)),
        occurrence = unique(c("eventID", "occurrenceID", occ_keep))
      )
    })

    emof_selected <- shiny::reactive({
      spec <- emof_candidate_specs()

      event_cols <- character(0)
      occ_cols <- character(0)

      if (nrow(spec) > 0) {
        event_cols <- vapply(seq_len(nrow(spec)), function(i) {
          term <- spec$term[i]
          cid <- paste0("emof_event__", safe_id_piece(term))
          keep <- checkbox_value_or_default(cid, default = FALSE)
          if (keep) term else NA_character_
        }, character(1))
        event_cols <- event_cols[!is.na(event_cols)]

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

    shiny::observeEvent(list(df_in(), target_database_in()), {
      rv$ready <- FALSE
      rv$result <- NULL
      rv$build_error <- NULL
    }, ignoreInit = FALSE)

    shiny::observeEvent(table_term_specs(), {
      specs <- table_term_specs()
      reset_table_checkboxes(specs$Event, "event_term__")
      reset_table_checkboxes(specs$Occurrence, "occ_term__")
    }, ignoreInit = TRUE)

    shiny::observeEvent(emof_candidate_specs(), {
      spec <- emof_candidate_specs()

      session$onFlushed(function() {
        for (i in seq_len(nrow(spec))) {
          term <- spec$term[i]
          def <- isTRUE(spec$selected_default[i])

          shiny::updateCheckboxInput(
            session,
            inputId = paste0("emof_event__", safe_id_piece(term)),
            value = def
          )

          shiny::updateCheckboxInput(
            session,
            inputId = paste0("emof_occ__", safe_id_piece(term)),
            value = FALSE
          )
        }
      }, once = TRUE)
    }, ignoreInit = TRUE)

    build_once <- function() {
      df <- df_in()
      shiny::req(is.data.frame(df))

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
      DT::datatable(
        utils::head(aurora_drop_internal_cols(rv$result$event), 50),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
      )
    })

    output$occ_preview <- DT::renderDT({
      shiny::req(rv$result)
      DT::datatable(
        utils::head(aurora_drop_internal_cols(rv$result$occurrence), 50),
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
        utils::write.csv(
          aurora_drop_internal_cols(rv$result$event),
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
        utils::write.csv(
          aurora_drop_internal_cols(rv$result$occurrence),
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
        rv$result$event
      }),
      occurrence = shiny::reactive({
        shiny::req(rv$result)
        rv$result$occurrence
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

# =========================================================
# Build DwC-A tables from flat mapped dataframe
# =========================================================

#' @export
build_dwca_tables <- function(df,
                              dwc_terms,
                              target_database = "GBIF",
                              selected_terms = NULL,
                              emof_spec = NULL) {

  `%||%` <- function(x, y) if (is.null(x)) y else x
  .data <- rlang::.data
  qc_messages <- character(0)

  if (is.null(df) || !is.data.frame(df)) {
    stop("df must be a data.frame.")
  }
  if (nrow(df) == 0) {
    stop("df has 0 rows.")
  }
  if (is.null(dwc_terms) || !is.data.frame(dwc_terms)) {
    stop("dwc_terms must be a data.frame.")
  }
  if (!all(c("Table", "Term") %in% names(dwc_terms))) {
    stop("dwc_terms must contain columns: Table, Term.")
  }
  if (!target_database %in% names(dwc_terms)) {
    stop(paste0("dwc_terms must contain repository column: ", target_database))
  }

  df_work <- df

  .first_non_empty <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) return(NA_character_)
    x[[1]]
  }

  .normalize_table_name <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    out <- rep(NA_character_, length(x))
    out[x == "Event"] <- "Event"
    out[x %in% c("Occurrence", "Occurrence.Core")] <- "Occurrence"
    out[x == "eMoF"] <- "eMoF"
    out
  }

  aurora_trace_cols <- intersect(
    aurora_internal_cols(),
    names(df_work)
  )

  repo_status <- as.character(dwc_terms[[target_database]])
  repo_status[is.na(repo_status)] <- ""
  repo_status <- trimws(tolower(repo_status))

  dwc_terms2 <- dwc_terms
  dwc_terms2$.table_norm <- .normalize_table_name(dwc_terms2$Table)
  dwc_terms2$.repo_status <- repo_status

  dwc_terms2 <- dwc_terms2[
    !is.na(dwc_terms2$.table_norm) &
      dwc_terms2$.table_norm != "",
    ,
    drop = FALSE
  ]

  if (!"eventID" %in% names(df_work)) {
    qc_messages <- c(qc_messages, "ERROR: Input dataframe is missing eventID.")
  } else {
    df_work$eventID <- as.character(df_work$eventID)
    df_work$eventID[is.na(df_work$eventID)] <- ""
    if (any(trimws(df_work$eventID) == "")) {
      qc_messages <- c(qc_messages, "ERROR: Some rows have blank eventID in input dataframe.")
    }
  }

  if ("occurrenceID" %in% names(df_work)) {
    df_work$occurrenceID <- as.character(df_work$occurrenceID)
    df_work$occurrenceID[is.na(df_work$occurrenceID)] <- ""
    if (any(trimws(df_work$occurrenceID) == "")) {
      qc_messages <- c(qc_messages, "ERROR: Some rows have blank occurrenceID in input dataframe.")
    }
  } else {
    qc_messages <- c(qc_messages, "WARNING: Input dataframe does not contain occurrenceID.")
  }

  if ("parentEventID" %in% names(df_work)) {
    df_work$parentEventID <- as.character(df_work$parentEventID)
    df_work$parentEventID[trimws(df_work$parentEventID) == ""] <- NA_character_
  }

  get_repo_terms <- function(tbl) {
    x <- dwc_terms2 |>
      dplyr::filter(.data$.table_norm == tbl) |>
      dplyr::pull(.data$Term) |>
      unique()

    x <- intersect(x, names(df_work))
    sort(unique(x))
  }

  repo_event_terms <- get_repo_terms("Event")
  repo_occ_terms <- get_repo_terms("Occurrence")

  if (!is.null(selected_terms) && is.list(selected_terms)) {
    event_terms <- unique(c(
      "eventID",
      "parentEventID",
      selected_terms$event %||% character(0),
      aurora_trace_cols
    ))
    occ_terms <- unique(c(
      "eventID",
      "occurrenceID",
      selected_terms$occurrence %||% character(0),
      aurora_trace_cols
    ))
  } else {
    event_terms <- unique(c("eventID", "parentEventID", repo_event_terms, aurora_trace_cols))
    occ_terms <- unique(c("eventID", "occurrenceID", repo_occ_terms, aurora_trace_cols))
  }

  event_terms <- intersect(event_terms, names(df_work))
  occ_terms <- intersect(occ_terms, names(df_work))

  if ("eventID" %in% names(df_work)) {
    event_table <- df_work |>
      dplyr::select(dplyr::any_of(event_terms)) |>
      dplyr::distinct(.data$eventID, .keep_all = TRUE)
  } else {
    event_table <- data.frame()
  }

  if ("occurrenceID" %in% names(df_work)) {
    occurrence_table <- df_work |>
      dplyr::select(dplyr::any_of(occ_terms)) |>
      dplyr::distinct(.data$occurrenceID, .keep_all = TRUE)
  } else {
    occurrence_table <- data.frame()
  }

  emof_table <- NULL

  if (!is.null(emof_spec) &&
      !is.null(emof_spec$columns) &&
      length(emof_spec$columns) > 0) {

    miss <- setdiff(emof_spec$columns, names(df_work))
    if (length(miss) > 0) {
      qc_messages <- c(
        qc_messages,
        paste0(
          "WARNING: eMoF columns missing and ignored: ",
          paste(miss, collapse = ", ")
        )
      )
    }

    cols_ok <- intersect(emof_spec$columns, names(df_work))

    if (length(cols_ok) > 0) {

      levels <- emof_spec$levels
      if (is.null(levels) || length(levels) == 0) {
        stop("emof_spec$levels must be a named list mapping column -> level.")
      }

      bad <- cols_ok[!cols_ok %in% names(levels)]
      if (length(bad) > 0) {
        stop(paste0(
          "Missing eMoF levels for columns: ",
          paste(bad, collapse = ", ")
        ))
      }

      event_cols <- cols_ok[
        vapply(cols_ok, function(x) identical(levels[[x]], "event"), logical(1))
      ]
      occurrence_cols <- cols_ok[
        vapply(cols_ok, function(x) identical(levels[[x]], "occurrence"), logical(1))
      ]

      emof_parts <- list()

      if (length(event_cols) > 0) {

        if (!"eventID" %in% names(df_work)) {
          qc_messages <- c(
            qc_messages,
            "ERROR: Cannot build event-level eMoF because eventID is missing."
          )
        } else {

          for (cc in event_cols) {
            conflicts <- df_work |>
              dplyr::mutate(.tmp_val = as.character(.data[[cc]])) |>
              dplyr::mutate(.tmp_val = trimws(.data$.tmp_val)) |>
              dplyr::filter(!is.na(.data$.tmp_val) & .data$.tmp_val != "") |>
              dplyr::group_by(.data$eventID) |>
              dplyr::summarise(
                n_values = dplyr::n_distinct(.data$.tmp_val),
                .groups = "drop"
              ) |>
              dplyr::filter(.data$n_values > 1)

            if (nrow(conflicts) > 0) {
              qc_messages <- c(
                qc_messages,
                paste0(
                  "WARNING: event-level eMoF column '", cc,
                  "' has conflicting values within the same eventID; keeping the first non-empty value per event."
                )
              )
            }
          }

          event_emof_wide <- df_work |>
            dplyr::select(
              .data$eventID,
              dplyr::any_of(aurora_trace_cols),
              dplyr::all_of(event_cols)
            ) |>
            dplyr::group_by(.data$eventID) |>
            dplyr::summarise(
              dplyr::across(
                dplyr::all_of(c(aurora_trace_cols, event_cols)),
                .first_non_empty
              ),
              .groups = "drop"
            )

          event_emof <- event_emof_wide |>
            tidyr::pivot_longer(
              cols = dplyr::all_of(event_cols),
              names_to = "measurementType",
              values_to = "measurementValue",
              values_transform = list(measurementValue = as.character)
            ) |>
            dplyr::filter(!is.na(.data$measurementValue) & trimws(.data$measurementValue) != "") |>
            dplyr::mutate(occurrenceID = "")

          emof_parts[["event"]] <- event_emof
        }
      }

      if (length(occurrence_cols) > 0) {

        if (!"occurrenceID" %in% names(df_work)) {
          qc_messages <- c(
            qc_messages,
            "ERROR: Cannot build occurrence-level eMoF because occurrenceID is missing."
          )
        } else {

          if (!"eventID" %in% names(df_work)) {
            qc_messages <- c(
              qc_messages,
              "WARNING: occurrence-level eMoF is being built without eventID in input dataframe."
            )
            df_work$eventID <- ""
          }

          occurrence_emof <- df_work |>
            dplyr::select(
              .data$eventID,
              .data$occurrenceID,
              dplyr::any_of(aurora_trace_cols),
              dplyr::all_of(occurrence_cols)
            ) |>
            dplyr::distinct() |>
            tidyr::pivot_longer(
              cols = dplyr::all_of(occurrence_cols),
              names_to = "measurementType",
              values_to = "measurementValue",
              values_transform = list(measurementValue = as.character)
            ) |>
            dplyr::filter(!is.na(.data$measurementValue) & trimws(.data$measurementValue) != "")

          emof_parts[["occurrence"]] <- occurrence_emof
        }
      }

      if (length(emof_parts) > 0) {
        emof_table <- dplyr::bind_rows(emof_parts)

        for (nm in c(
          "measurementTypeID",
          "measurementValueID",
          "measurementUnit",
          "measurementUnitID"
        )) {
          if (!nm %in% names(emof_table)) emof_table[[nm]] <- ""
        }

        emof_table <- emof_table |>
          dplyr::select(
            .data$eventID, .data$occurrenceID,
            dplyr::any_of(aurora_trace_cols),
            .data$measurementType, .data$measurementTypeID,
            .data$measurementValue, .data$measurementValueID,
            .data$measurementUnit, .data$measurementUnitID
          ) |>
          dplyr::distinct()

        dup_event_measurements <- emof_table |>
          dplyr::filter(.data$occurrenceID == "") |>
          dplyr::count(.data$eventID, .data$measurementType, name = "n") |>
          dplyr::filter(.data$n > 1)

        if (nrow(dup_event_measurements) > 0) {
          qc_messages <- c(
            qc_messages,
            "WARNING: duplicate event-level eMoF records were detected and collapsed."
          )

          emof_table_event <- emof_table |>
            dplyr::filter(.data$occurrenceID == "") |>
            dplyr::distinct(.data$eventID, .data$measurementType, .keep_all = TRUE)

          emof_table_occ <- emof_table |>
            dplyr::filter(.data$occurrenceID != "")

          emof_table <- dplyr::bind_rows(emof_table_event, emof_table_occ) |>
            dplyr::distinct()
        }
      }
    }
  }

  if (!is.data.frame(event_table) || nrow(event_table) == 0) {
    qc_messages <- c(qc_messages, "ERROR: event table is empty.")
  } else {
    if (!"eventID" %in% names(event_table)) {
      qc_messages <- c(qc_messages, "ERROR: event table missing eventID.")
    } else {
      event_ids <- trimws(as.character(event_table$eventID))
      if (any(is.na(event_ids) | event_ids == "")) {
        qc_messages <- c(qc_messages, "ERROR: event table contains blank eventID.")
      }
      if (anyDuplicated(event_ids) > 0) {
        qc_messages <- c(qc_messages, "ERROR: Duplicate eventID detected in event table.")
      }
    }

    if ("parentEventID" %in% names(event_table)) {
      pid <- trimws(as.character(event_table$parentEventID))
      pid[is.na(pid) | pid == ""] <- NA_character_
      if (any(!is.na(pid) & !(pid %in% as.character(event_table$eventID)))) {
        qc_messages <- c(
          qc_messages,
          "WARNING: Some parentEventID values do not match any eventID in event table."
        )
      }
    }
  }

  if (!is.data.frame(occurrence_table) || nrow(occurrence_table) == 0) {
    qc_messages <- c(qc_messages, "WARNING: occurrence table is empty.")
  } else {
    if (!"occurrenceID" %in% names(occurrence_table)) {
      qc_messages <- c(qc_messages, "ERROR: occurrence table missing occurrenceID.")
    } else {
      occurrence_ids <- trimws(as.character(occurrence_table$occurrenceID))
      if (any(is.na(occurrence_ids) | occurrence_ids == "")) {
        qc_messages <- c(qc_messages, "ERROR: occurrence table contains blank occurrenceID.")
      }
      if (anyDuplicated(occurrence_ids) > 0) {
        qc_messages <- c(qc_messages, "ERROR: Duplicate occurrenceID detected in occurrence table.")
      }
    }

    if ("eventID" %in% names(occurrence_table) && "eventID" %in% names(event_table)) {
      occ_event_ids <- trimws(as.character(occurrence_table$eventID))
      ev_ids <- trimws(as.character(event_table$eventID))
      missing_links <- !is.na(occ_event_ids) & occ_event_ids != "" & !(occ_event_ids %in% ev_ids)
      if (any(missing_links)) {
        qc_messages <- c(
          qc_messages,
          "WARNING: Some occurrence eventID values do not match any eventID in event table."
        )
      }
    }
  }

  if (!is.null(emof_table)) {
    if (!"eventID" %in% names(emof_table)) {
      qc_messages <- c(qc_messages, "ERROR: eMoF table missing eventID.")
    }

    if (all(c("eventID", "occurrenceID", "measurementType") %in% names(emof_table))) {
      dup_event_measurements <- emof_table |>
        dplyr::filter(.data$occurrenceID == "") |>
        dplyr::count(.data$eventID, .data$measurementType, name = "n") |>
        dplyr::filter(.data$n > 1)

      if (nrow(dup_event_measurements) > 0) {
        qc_messages <- c(
          qc_messages,
          "ERROR: Duplicate measurementType linked to the same eventID still exists in eMoF."
        )
      }
    }
  }

  if (length(qc_messages) == 0) {
    qc_messages <- "OK: build completed with no QC messages."
  } else {
    qc_messages <- unique(qc_messages)
  }

  list(
    event = event_table,
    occurrence = occurrence_table,
    emof = emof_table,
    qc = qc_messages
  )
}
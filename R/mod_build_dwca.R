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
          justify-content: space-between;
          align-items: center;
          gap: 0.75rem;
          margin-top: 1rem;
          flex-wrap: wrap;
        }

        .dwca-build-actions-right {
          display: flex;
          justify-content: flex-end;
          align-items: center;
          gap: 0.75rem;
          flex-wrap: wrap;
        }

        .dwca-build-btn {
          min-width: 160px;
          padding: 0.7rem 1.1rem;
          font-weight: 600;
          border-radius: 12px;
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
          margin-bottom: 0;
        }

        .dwca-resource-type-wrap .radio {
          margin-top: 0;
          margin-bottom: 0.35rem;
        }

        .dwca-resource-type-wrap .radio:last-child {
          margin-bottom: 0;
        }

        .dwca-stepper {
          display: flex;
          gap: 0.65rem;
          flex-wrap: wrap;
          margin-top: 0.75rem;
        }

        .dwca-step-pill {
          display: inline-flex;
          align-items: center;
          gap: 0.45rem;
          border-radius: 999px;
          padding: 0.45rem 0.8rem;
          font-size: 0.9rem;
          font-weight: 600;
          background: #f3f4f6;
          color: #4b5563;
          border: 1px solid #e5e7eb;
        }

        .dwca-step-pill.is-active {
          background: #e0f2fe;
          color: #0369a1;
          border-color: #bae6fd;
        }

        .dwca-step-pill.is-done {
          background: #ecfdf5;
          color: #047857;
          border-color: #a7f3d0;
        }

        .dwca-search-box {
          margin-bottom: 0.75rem;
        }

        .dwca-search-box .form-control {
          border-radius: 10px;
        }

        .dwca-warning-box {
          border: 1px solid #f59e0b;
          background: #fffbeb;
          border-radius: 12px;
          padding: 0.85rem 1rem;
        }

        .dwca-warning-box .alert,
        .dwca-warning-box p:last-child,
        .dwca-warning-box ul:last-child {
          margin-bottom: 0;
        }

        .dwca-selection-summary {
          color: #6b7280;
          font-size: 0.92rem;
          margin-top: 0.75rem;
        }

        @media (max-width: 991.98px) {
          .dwca-subgrid {
            grid-template-columns: 1fr;
          }

          .dwca-build-actions-bottom,
          .dwca-build-actions-right {
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
          "Choose the archive resource type and review the allowed fields step by step before building the Darwin Core tables."
        )
      ),

      bslib::card(
        class = "dwca-top-card mb-4",
        bslib::card_header("Build settings"),
        bslib::card_body(
          shiny::uiOutput(ns("build_info_ui"))
        )
      ),

      bslib::card(
        class = "dwca-section-card mb-4",
        bslib::card_header("Selection warnings / status"),
        bslib::card_body(
          shiny::uiOutput(ns("selection_warning_ui"))
        )
      ),

      bslib::card(
        class = "dwca-section-card mb-4",
        bslib::card_header(shiny::uiOutput(ns("current_step_title_ui"))),
        bslib::card_body(
          shiny::uiOutput(ns("step_body_ui")),
          shiny::div(
            class = "dwca-build-actions-bottom",
            shiny::uiOutput(ns("wizard_left_actions_ui")),
            shiny::div(
              class = "dwca-build-actions-right",
              shiny::uiOutput(ns("wizard_right_actions_ui"))
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
        class = "dwca-preview-card mb-4",
        bslib::card_header("Table previews"),
        bslib::card_body(
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Event",
              shiny::div(
                class = "dwca-dt-wrap",
                DT::DTOutput(ns("event_preview"))
              )
            ),

            shiny::tabPanel(
              "Occurrence",
              shiny::div(
                class = "dwca-dt-wrap",
                DT::DTOutput(ns("occ_preview"))
              )
            ),

            shiny::tabPanel(
              "eMoF",
              shiny::div(
                class = "dwca-dt-wrap",
                DT::DTOutput(ns("emof_preview"))
              )
            )
          )
        )
      ),

      bslib::card(
        class = "dwca-section-card",
        bslib::card_header("Pre-issues from Field Mapping"),
        bslib::card_body(
          shiny::div(
            class = "dwca-dt-wrap",
            DT::DTOutput(ns("pre_issues_preview"))
          )
        )
      )
    )
  )
}

#' @export
mod_build_dwca_server <- function(id, df_in, dwc_terms, target_database_in, pre_issues_in = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    `%||%` <- function(x, y) if (is.null(x)) y else x

    rv <- shiny::reactiveValues(
      ready = FALSE,
      result = NULL,
      build_error = NULL,
      current_step = NULL
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

    step_index <- function(step_name, steps) {
      idx <- match(step_name, steps)
      if (length(idx) == 0 || is.na(idx)) {
        return(NULL)
      }
      idx
    }

    first_step_from_steps <- function(steps) {
      if (length(steps) == 0) return(NULL)
      steps[[1]]
    }

    last_step_from_steps <- function(steps) {
      if (length(steps) == 0) return(NULL)
      steps[[length(steps)]]
    }

    set_step_to_first <- function(steps) {
      rv$current_step <- first_step_from_steps(steps)
    }

    clear_built_result <- function() {
      rv$ready <- FALSE
      rv$result <- NULL
      rv$build_error <- NULL
    }

    get_pre_issues <- shiny::reactive({
      if (is.null(pre_issues_in)) {
        return(data.frame())
      }

      x <- tryCatch(pre_issues_in(), error = function(e) NULL)

      if (is.null(x) || !is.data.frame(x)) {
        return(data.frame())
      }

      x
    })

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
          architecture_label = "Sampling event (Event core)",
          steps = c("event", "occurrence", "emof")
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
        architecture_label = "Occurrence (Occurrence core)",
        steps = c("occurrence", "emof")
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

    checkbox_value_or_default <- function(input_id, default = FALSE) {
      val <- input[[input_id]]
      if (is.null(val)) {
        return(isTRUE(default))
      }
      isTRUE(val)
    }

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

    emof_candidate_specs <- shiny::reactive({
      df <- df_in()
      shiny::req(is.data.frame(df))

      chosen_core_terms <- unique(c(
        selected_table_terms()$event %||% character(0),
        selected_table_terms()$occurrence %||% character(0)
      ))

      cols <- names(df)

      structural_exclude <- c(
        "eventID",
        "occurrenceID",
        "parentEventID"
      )

      cols <- setdiff(cols, structural_exclude)
      cols <- cols[!grepl("^\\.aurora", cols)]
      cols <- setdiff(cols, chosen_core_terms)
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
        selected_default = FALSE,
        stringsAsFactors = FALSE
      )

      out <- out[order(status_rank(out$status), out$term), , drop = FALSE]
      rownames(out) <- NULL
      out
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

    reset_table_checkboxes <- function(spec_df, prefix) {
      if (is.null(spec_df) || !is.data.frame(spec_df) || nrow(spec_df) == 0) {
        return(invisible(NULL))
      }

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

    reset_emof_checkboxes <- function(spec_df, cfg_snapshot) {
      if (is.null(spec_df) || !is.data.frame(spec_df) || nrow(spec_df) == 0) {
        return(invisible(NULL))
      }

      show_emof_event <- isTRUE(cfg_snapshot$show_emof_event)
      show_emof_occurrence <- isTRUE(cfg_snapshot$show_emof_occurrence)

      session$onFlushed(function() {
        for (i in seq_len(nrow(spec_df))) {
          term <- spec_df$term[i]

          if (show_emof_event) {
            shiny::updateCheckboxInput(
              session,
              inputId = paste0("emof_event__", safe_id_piece(term)),
              value = FALSE
            )
          }

          if (show_emof_occurrence) {
            shiny::updateCheckboxInput(
              session,
              inputId = paste0("emof_occ__", safe_id_piece(term)),
              value = FALSE
            )
          }
        }
      }, once = TRUE)
    }

    render_scroll_choices <- function(spec_df, prefix, search_text = "") {
      if (!"locked" %in% names(spec_df)) {
        spec_df$locked <- FALSE
      }

      sel_df <- spec_df[!as.logical(spec_df$locked), , drop = FALSE]

      query <- trimws(tolower(as.character(search_text %||% "")))
      if (nzchar(query) && nrow(sel_df) > 0) {
        keep <- grepl(query, tolower(sel_df$term), fixed = TRUE)
        sel_df <- sel_df[keep, , drop = FALSE]
      }

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

    current_step_title <- shiny::reactive({
      cfg <- resource_config()
      steps <- cfg$steps
      step <- rv$current_step %||% first_step_from_steps(steps)

      idx <- step_index(step, steps)
      if (is.null(idx)) {
        step <- first_step_from_steps(steps)
      }

      if (identical(step, "event")) return("Step 1 — Event table")
      if (identical(step, "occurrence") && identical(cfg$resource_type, "sampling_event")) {
        return("Step 2 — Occurrence table")
      }
      if (identical(step, "occurrence") && identical(cfg$resource_type, "occurrence_core")) {
        return("Step 1 — Occurrence.Core table")
      }
      if (identical(step, "emof") && identical(cfg$resource_type, "sampling_event")) {
        return("Step 3 — eMoF tables")
      }
      if (identical(step, "emof") && identical(cfg$resource_type, "occurrence_core")) {
        return("Step 2 — eMoF table")
      }

      "Build Darwin Core Tables"
    })

    output$current_step_title_ui <- shiny::renderUI({
      shiny::span(current_step_title())
    })

    output$build_info_ui <- shiny::renderUI({
      cfg <- resource_config()
      steps <- cfg$steps

      current_step_safe <- rv$current_step %||% first_step_from_steps(steps)
      current_idx <- step_index(current_step_safe, steps)

      if (is.null(current_idx)) {
        current_idx <- 1L
      }

      pill_label <- function(step_name) {
        if (identical(step_name, "event")) return("Event")
        if (identical(step_name, "occurrence") && identical(cfg$resource_type, "occurrence_core")) return("Occurrence.Core")
        if (identical(step_name, "occurrence")) return("Occurrence")
        if (identical(step_name, "emof")) return("eMoF")
        step_name
      }

      pills <- lapply(seq_along(steps), function(i) {
        st <- steps[[i]]
        cls <- "dwca-step-pill"

        if (i < current_idx) {
          cls <- paste(cls, "is-done")
        }
        if (i == current_idx) {
          cls <- paste(cls, "is-active")
        }

        shiny::tags$div(
          class = cls,
          shiny::tags$span(pill_label(st))
        )
      })

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
        ),
        shiny::div(
          class = "dwca-stepper",
          pills
        )
      )
    })

    output$selection_warning_ui <- shiny::renderUI({
      cfg <- resource_config()
      steps <- cfg$steps
      sel <- selected_table_terms()
      emof_sel <- emof_selected()
      emof_spec <- emof_candidate_specs()

      current_step_safe <- rv$current_step %||% first_step_from_steps(steps)
      if (is.null(step_index(current_step_safe, steps))) {
        current_step_safe <- first_step_from_steps(steps)
      }

      msg_parts <- list()

      if (!is.null(rv$build_error) && nzchar(rv$build_error)) {
        msg_parts[[length(msg_parts) + 1]] <- shiny::tags$div(
          class = "alert alert-danger dwca-build-error",
          shiny::tags$strong("Build error: "),
          rv$build_error
        )
      }

      both <- intersect(emof_sel$event, emof_sel$occurrence)
      if (length(both) > 0) {
        msg_parts[[length(msg_parts) + 1]] <- shiny::tags$div(
          class = "alert alert-danger",
          shiny::tags$strong("eMoF conflict: "),
          paste0(
            "These columns were selected in both eMoF levels: ",
            paste(both, collapse = ", "),
            ". Choose only one level for each field."
          )
        )
      }

      if (identical(current_step_safe, "emof") && nrow(emof_spec) == 0) {
        msg_parts[[length(msg_parts) + 1]] <- shiny::tags$div(
          class = "alert alert-warning",
          "No eligible columns remain for eMoF after excluding structural fields and the columns already selected for Event / Occurrence."
        )
      }

      summary_text <- paste0(
        "Current selections — Event: ",
        length(sel$event %||% character(0)),
        " field(s); Occurrence: ",
        length(sel$occurrence %||% character(0)),
        " field(s); eMoF event: ",
        length(emof_sel$event %||% character(0)),
        " field(s); eMoF occurrence: ",
        length(emof_sel$occurrence %||% character(0)),
        " field(s)."
      )

      msg_parts[[length(msg_parts) + 1]] <- shiny::tags$div(
        class = "dwca-selection-summary",
        summary_text
      )

      shiny::tags$div(
        class = "dwca-warning-box",
        msg_parts
      )
    })

    output$emof_event_terms_ui <- shiny::renderUI({
      spec <- emof_candidate_specs()
      render_scroll_choices(
        spec_df = spec,
        prefix = "emof_event__",
        search_text = input$emof_event_search %||% ""
      )
    })

    output$emof_occ_terms_ui <- shiny::renderUI({
      spec <- emof_candidate_specs()
      render_scroll_choices(
        spec_df = spec,
        prefix = "emof_occ__",
        search_text = input$emof_occ_search %||% ""
      )
    })

    output$step_body_ui <- shiny::renderUI({
      cfg <- resource_config()
      steps <- cfg$steps
      specs <- table_term_specs()

      step <- rv$current_step %||% first_step_from_steps(steps)
      if (is.null(step_index(step, steps))) {
        step <- first_step_from_steps(steps)
      }

      make_emof_panel <- function(title, search_id, terms_output_id) {
        shiny::tags$div(
          shiny::tags$div(class = "dwca-term-group-title", title),
          shiny::div(
            class = "dwca-help-block",
            "Select the columns that should become eMoF records at this level."
          ),
          shiny::div(
            class = "dwca-search-box",
            shiny::textInput(
              session$ns(search_id),
              "Search terms",
              value = "",
              placeholder = "Type to filter terms"
            )
          ),
          shiny::uiOutput(session$ns(terms_output_id))
        )
      }

      if (identical(step, "event")) {
        return(
          render_table_card_body(
            spec_df = specs$Event,
            prefix = "event_term__",
            help_text = "Required fields are included automatically and are not shown here. Only non-required fields are selectable below."
          )
        )
      }

      if (identical(step, "occurrence")) {
        return(
          render_table_card_body(
            spec_df = specs$Occurrence,
            prefix = "occ_term__",
            help_text = "Required fields are included automatically and are not shown here. Only non-required fields are selectable below."
          )
        )
      }

      if (identical(step, "emof")) {
        spec <- emof_candidate_specs()

        if (nrow(spec) == 0) {
          return(
            shiny::tags$div(
              class = "dwca-help-block",
              "No eligible columns are available for eMoF in the current dataset after excluding structural fields and the fields already selected for the core tables."
            )
          )
        }

        if (isTRUE(cfg$show_emof_event) && isTRUE(cfg$show_emof_occurrence)) {
          return(
            shiny::tagList(
              shiny::tags$div(
                class = "dwca-help-block",
                "Only fields not already selected in Event or Occurrence are shown here. Choose each remaining field in at most one eMoF level."
              ),
              shiny::tags$div(
                class = "dwca-subgrid",
                make_emof_panel("eMoF - event", "emof_event_search", "emof_event_terms_ui"),
                make_emof_panel("eMoF - occurrence", "emof_occ_search", "emof_occ_terms_ui")
              )
            )
          )
        }

        if (isTRUE(cfg$show_emof_occurrence)) {
          return(
            shiny::tagList(
              shiny::tags$div(
                class = "dwca-help-block",
                "For Occurrence core, only occurrence-level eMoF is available. Only fields not already selected in the core table are shown here."
              ),
              make_emof_panel("eMoF - occurrence", "emof_occ_search", "emof_occ_terms_ui")
            )
          )
        }
      }

      shiny::tags$div(
        class = "dwca-help-block",
        "No step content available."
      )
    })

    output$wizard_left_actions_ui <- shiny::renderUI({
      cfg <- resource_config()
      steps <- cfg$steps

      step <- rv$current_step %||% first_step_from_steps(steps)
      idx <- step_index(step, steps)

      if (is.null(idx)) {
        idx <- 1L
      }

      out <- list()

      if (idx > 1) {
        out[[length(out) + 1]] <- shiny::actionButton(
          session$ns("go_back"),
          "Back",
          class = "btn-outline-secondary dwca-build-btn"
        )
      }

      if (!is.null(rv$result)) {
        out[[length(out) + 1]] <- shiny::actionButton(
          session$ns("restart_wizard"),
          "Start over",
          class = "btn-outline-danger dwca-build-btn"
        )
      }

      do.call(shiny::tagList, out)
    })

    output$wizard_right_actions_ui <- shiny::renderUI({
      cfg <- resource_config()
      steps <- cfg$steps

      step <- rv$current_step %||% first_step_from_steps(steps)
      idx <- step_index(step, steps)

      if (is.null(idx)) {
        step <- first_step_from_steps(steps)
      }

      if (!identical(step, last_step_from_steps(steps))) {
        return(
          shiny::actionButton(
            session$ns("go_next"),
            "Next",
            class = "btn-primary dwca-build-btn"
          )
        )
      }

      shiny::actionButton(
        session$ns("build"),
        "Build Darwin Core tables",
        class = "btn-primary dwca-build-btn"
      )
    })

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

    shiny::observeEvent(list(df_in(), target_database_in(), input$resource_type), {
      cfg_snapshot <- resource_config()
      clear_built_result()
      set_step_to_first(cfg_snapshot$steps)
    }, ignoreInit = FALSE)

    shiny::observeEvent(list(table_term_specs(), input$resource_type), {
      specs_snapshot <- table_term_specs()
      cfg_snapshot <- resource_config()

      if (isTRUE(cfg_snapshot$show_event)) {
        reset_table_checkboxes(specs_snapshot$Event, "event_term__")
      }

      if (isTRUE(cfg_snapshot$show_occurrence)) {
        reset_table_checkboxes(specs_snapshot$Occurrence, "occ_term__")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(list(emof_candidate_specs(), input$resource_type), {
      spec_snapshot <- emof_candidate_specs()
      cfg_snapshot <- resource_config()
      reset_emof_checkboxes(spec_snapshot, cfg_snapshot)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$go_back, {
      cfg_snapshot <- resource_config()
      steps_snapshot <- cfg_snapshot$steps

      cur <- rv$current_step %||% first_step_from_steps(steps_snapshot)
      idx <- step_index(cur, steps_snapshot)

      if (is.null(idx)) {
        rv$current_step <- first_step_from_steps(steps_snapshot)
        return(invisible(NULL))
      }

      if (idx > 1) {
        rv$current_step <- steps_snapshot[[idx - 1]]
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$go_next, {
      cfg_snapshot <- resource_config()
      steps_snapshot <- cfg_snapshot$steps
      specs_snapshot <- table_term_specs()
      emof_spec_snapshot <- emof_candidate_specs()

      cur <- rv$current_step %||% first_step_from_steps(steps_snapshot)
      idx <- step_index(cur, steps_snapshot)

      if (is.null(idx)) {
        rv$current_step <- first_step_from_steps(steps_snapshot)
        return(invisible(NULL))
      }

      if (idx >= length(steps_snapshot)) {
        return(invisible(NULL))
      }

      clear_built_result()

      if (identical(cur, "event")) {
        if (isTRUE(cfg_snapshot$show_occurrence)) {
          reset_table_checkboxes(specs_snapshot$Occurrence, "occ_term__")
        }
        reset_emof_checkboxes(emof_spec_snapshot, cfg_snapshot)
      }

      if (identical(cur, "occurrence")) {
        reset_emof_checkboxes(emof_spec_snapshot, cfg_snapshot)
      }

      rv$current_step <- steps_snapshot[[idx + 1]]
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$restart_wizard, {
      cfg_snapshot <- resource_config()
      specs_snapshot <- table_term_specs()
      emof_spec_snapshot <- emof_candidate_specs()

      clear_built_result()
      set_step_to_first(cfg_snapshot$steps)

      if (isTRUE(cfg_snapshot$show_event)) {
        reset_table_checkboxes(specs_snapshot$Event, "event_term__")
      }

      if (isTRUE(cfg_snapshot$show_occurrence)) {
        reset_table_checkboxes(specs_snapshot$Occurrence, "occ_term__")
      }

      reset_emof_checkboxes(emof_spec_snapshot, cfg_snapshot)
    }, ignoreInit = TRUE)

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

    output$pre_issues_preview <- DT::renderDT({
      x <- get_pre_issues()
      if (!is.data.frame(x) || nrow(x) == 0) {
        x <- data.frame(
          message = "No pre-issues available.",
          stringsAsFactors = FALSE
        )
      }

      DT::datatable(
        x,
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 10
        )
      )
    })

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
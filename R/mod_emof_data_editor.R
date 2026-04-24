# =========================================================
# eMoF Data Editor Module
# - Receives the eMoF table produced upstream (for example, by Build DwC-A)
# - No upload / build / download buttons
# - Builds the editable grouped table automatically when emof_in() is available
# - Non-numeric measurementValue rows are grouped by the full editor signature
# - Numeric measurementValue rows are grouped value-agnostically and the
#   measurementValue cell is locked as "Multiple values: Editing disabled"
# - Edits stay in a working draft until the user clicks Apply
# - Skip keeps the upstream built eMoF unchanged and unlocks downstream tabs
# - Internal AURORA traceability columns are preserved in the returned data
# File: R/mod_emof_data_editor.R
# =========================================================

# -------- helpers --------

.emof_editor_required_cols <- function() {
  c(
    "measurementType",
    "measurementTypeID",
    "measurementValue",
    "measurementValueID",
    "measurementUnit",
    "measurementUnitID"
  )
}

.emof_editor_blank_chr <- function(x) {
  if (is.null(x)) return(character())
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

.emof_editor_is_numeric_like <- function(x) {
  x <- .emof_editor_blank_chr(x)
  x_trim <- trimws(x)
  suppressWarnings(x_trim != "" & !is.na(as.numeric(x_trim)))
}

.emof_editor_empty_full <- function() {
  data.frame(
    measurementType = character(),
    measurementTypeID = character(),
    measurementValue = character(),
    measurementValueID = character(),
    measurementUnit = character(),
    measurementUnitID = character(),
    .emof_editor_row_id = integer(),
    stringsAsFactors = FALSE
  )
}

.emof_editor_empty_display <- function() {
  data.frame(
    measurementType = character(),
    measurementTypeID = character(),
    measurementValue = character(),
    measurementValueID = character(),
    measurementUnit = character(),
    measurementUnitID = character(),
    .emof_editor_row_id = character(),
    numeric_flag = logical(),
    stringsAsFactors = FALSE
  )
}

.emof_editor_init_df <- function(df) {
  stopifnot(is.data.frame(df))

  req_cols <- .emof_editor_required_cols()
  n <- nrow(df)

  for (col in req_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- rep("", n)
    }

    df[[col]] <- .emof_editor_blank_chr(df[[col]])

    if (n == 0) {
      df[[col]] <- character(0)
    }
  }

  df
}

.emof_editor_prepare_display <- function(df_full) {
  stopifnot(is.data.frame(df_full))
  stopifnot(".emof_editor_row_id" %in% names(df_full))

  req_cols <- .emof_editor_required_cols()

  df_full <- .emof_editor_init_df(df_full)

  if (nrow(df_full) == 0) {
    return(.emof_editor_empty_display())
  }

  is_num <- .emof_editor_is_numeric_like(df_full$measurementValue)

  df_non_num <- df_full[!is_num, , drop = FALSE]
  df_num <- df_full[is_num, , drop = FALSE]

  disp_non_num <- NULL
  disp_num <- NULL

  if (nrow(df_non_num) > 0) {
    disp_non_num <- stats::aggregate(
      .emof_editor_row_id ~ measurementType +
        measurementTypeID +
        measurementValue +
        measurementValueID +
        measurementUnit +
        measurementUnitID,
      data = df_non_num[, c(req_cols, ".emof_editor_row_id"), drop = FALSE],
      FUN = function(x) paste(x, collapse = ",")
    )
    disp_non_num$numeric_flag <- FALSE
  }

  if (nrow(df_num) > 0) {
    disp_num <- stats::aggregate(
      .emof_editor_row_id ~ measurementType +
        measurementTypeID +
        measurementValueID +
        measurementUnit +
        measurementUnitID,
      data = df_num[, c(
        "measurementType",
        "measurementTypeID",
        "measurementValueID",
        "measurementUnit",
        "measurementUnitID",
        ".emof_editor_row_id"
      ), drop = FALSE],
      FUN = function(x) paste(x, collapse = ",")
    )

    disp_num$measurementValue <- "Multiple values: Editing disabled"
    disp_num$numeric_flag <- TRUE
  }

  final_disp <- do.call(
    rbind,
    Filter(Negate(is.null), list(disp_non_num, disp_num))
  )

  if (is.null(final_disp) || nrow(final_disp) == 0) {
    final_disp <- .emof_editor_empty_display()
  }

  final_disp <- final_disp[, c(req_cols, ".emof_editor_row_id", "numeric_flag"), drop = FALSE]
  rownames(final_disp) <- NULL
  final_disp
}

.emof_editor_split_ids <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(x)
  if (!nzchar(x)) return(integer())
  as.integer(stats::na.omit(as.numeric(strsplit(x, ",", fixed = TRUE)[[1]])))
}


#' eMoF Data Editor UI
#'
#' @param id Module id.
#' @export
mod_emof_data_editor_ui <- function(id) {
  ns <- shiny::NS(id)

  root_sel <- paste0("#", ns("root"))

  css <- paste0(
    root_sel, " .emofed-page {
      padding: 1rem 1.25rem 2rem 1.25rem;
    }

    ", root_sel, " .emofed-muted {
      color: #5f6b68;
    }

    ", root_sel, " .emofed-info {
      background-color: #f8f9fa;
      padding: 15px;
      border-left: 5px solid #303d34;
      margin-bottom: 16px;
      border-radius: .5rem;
    }

    ", root_sel, " .emofed-warning {
      color: #303d34;
      font-weight: 400;
      margin-bottom: 14px;
      padding: 10px 12px;
      border: 1px solid #d1d1d1;
      border-radius: .5rem;
      background-color: #fffdf5;
    }

    ", root_sel, " .emofed-small {
      font-size: .92rem;
      color: #64706d;
    }

    ", root_sel, " .emofed-side .card-body,
    ", root_sel, " .emofed-main .card-body {
      padding: .9rem;
    }

    ", root_sel, " .emofed-side {
      max-width: 100%;
    }

    ", root_sel, " .emofed-side p {
      margin-bottom: .3rem;
      line-height: 1.35;
    }

    ", root_sel, " .emofed-side hr {
      margin: .75rem 0;
    }

    ", root_sel, " .emofed-btn-apply,
    ", root_sel, " .emofed-btn-apply:focus,
    ", root_sel, " .emofed-btn-apply:hover {
      background: #1B998B !important;
      border-color: #1B998B !important;
      color: #ffffff !important;
    }

    ", root_sel, " .emofed-btn-skip,
    ", root_sel, " .emofed-btn-skip:focus,
    ", root_sel, " .emofed-btn-skip:hover {
      background: #E9C46A !important;
      border-color: #D4A73A !important;
      color: #2f2f2f !important;
    }

    ", root_sel, " .emofed-btn-reset,
    ", root_sel, " .emofed-btn-reset:focus,
    ", root_sel, " .emofed-btn-reset:hover {
      background: #ffffff !important;
      border: 1px solid #5f6b68 !important;
      color: #303d34 !important;
    }

    ", root_sel, " .emofed-status-ok {
      color: #1B998B;
      font-weight: 600;
    }

    ", root_sel, " .emofed-status-warn {
      color: #8a6d3b;
      font-weight: 600;
    }

    ", root_sel, " .emofed-status-pending {
      color: #5C7AEA;
      font-weight: 600;
    }

    ", root_sel, " .emofed-side-actions {
      display: flex;
      gap: .6rem;
      flex-wrap: wrap;
      margin-top: .9rem;
    }

    ", root_sel, " .emofed-side-actions .btn {
      margin-right: 0;
    }

    ", root_sel, " .emofed-main .dataTables_wrapper {
      width: 100%;
      margin-bottom: 0;
    }
  ")

  shiny::tagList(
    shiny::tags$style(shiny::HTML(css)),

    shiny::tags$div(
      id = ns("root"),
      class = "emofed-page",

      bslib::card(
        bslib::card_header("eMoF Data Editor"),
        bslib::card_body(
          shiny::tags$div(
            class = "emofed-info",
            shiny::tags$p(
              "This editor helps standardize the core measurement vocabulary fields in the ",
              shiny::tags$strong("Extended Measurement or Fact (eMoF)"),
              " table after the Darwin Core tables have already been built."
            ),
            shiny::tags$ul(
              shiny::tags$li("Repeated non-numeric measurement rows are grouped into a single editable row."),
              shiny::tags$li("Rows with numeric measurementValue are grouped separately and their value cell is locked to avoid accidental bulk edits."),
              shiny::tags$li("Edits remain as a draft until you click Apply changes to eMoF.")
            )
          ),

          shiny::tags$div(
            class = "emofed-warning",
            shiny::tags$strong("Note: "),
            "Consult the controlled vocabulary at ",
            shiny::tags$a(
              href = "https://vocab.nerc.ac.uk/search_nvs/",
              target = "_blank",
              rel = "noopener noreferrer",
              "BODC-NERC controlled vocabulary."
            )
          ),

          bslib::layout_columns(
            col_widths = c(3, 9),
            gap = "1rem",

            bslib::card(
              class = "emofed-side",
              bslib::card_header("Status"),
              bslib::card_body(
                shiny::uiOutput(ns("status_ui")),
                shiny::uiOutput(ns("apply_btn_ui")),
                shiny::hr(),
                shiny::div(
                  class = "d-flex gap-2 flex-wrap",
                  shiny::actionButton(
                    ns("reset_table"),
                    "Reset to built eMoF",
                    class = "emofed-btn-reset"
                  ),
                  shiny::uiOutput(ns("skip_btn_ui"), inline = TRUE)
                )
              )
            ),

            bslib::card(
              class = "emofed-main",
              bslib::card_header("Editable lookup table"),
              bslib::card_body(
                shiny::uiOutput(ns("table_note")),
                DT::DTOutput(ns("editable_table"))
              )
            )
          )
        )
      )
    )
  )
}


#' eMoF Data Editor server
#'
#' @param id Module id.
#' @param emof_in Reactive returning the eMoF table produced upstream.
#' @return A list with reactives: emof, lookup, ready.
#' @export
mod_emof_data_editor_server <- function(id, emof_in) {
  shiny::moduleServer(id, function(input, output, session) {

    `%||%` <- function(x, y) if (is.null(x)) y else x

    rv <- shiny::reactiveValues(
      source_data = NULL,
      working_data = NULL,
      applied_data = NULL,
      display_data = NULL,
      ready = FALSE,
      skipped = FALSE,
      has_input = FALSE,
      has_rows = FALSE,
      table_seed = 0L
    )

    table_proxy <- DT::dataTableProxy(session$ns("editable_table"))

    clear_state <- function() {
      rv$source_data <- NULL
      rv$working_data <- NULL
      rv$applied_data <- NULL
      rv$display_data <- NULL
      rv$ready <- FALSE
      rv$skipped <- FALSE
      rv$has_input <- FALSE
      rv$has_rows <- FALSE
      rv$table_seed <- rv$table_seed + 1L
    }

    build_from_input <- function(df) {
      validate_df <- is.data.frame(df)
      shiny::req(validate_df)

      df <- .emof_editor_init_df(df)

      if (!".emof_editor_row_id" %in% names(df)) {
        if (nrow(df) > 0) {
          df$.emof_editor_row_id <- seq_len(nrow(df))
        } else {
          df$.emof_editor_row_id <- integer(0)
        }
      }

      rv$has_input <- TRUE
      rv$has_rows <- nrow(df) > 0

      rv$source_data <- df
      rv$working_data <- df
      rv$applied_data <- NULL
      rv$display_data <- .emof_editor_prepare_display(df)
      rv$ready <- FALSE
      rv$skipped <- FALSE
      rv$table_seed <- rv$table_seed + 1L
    }

    shiny::observeEvent(emof_in(), {
      df <- emof_in()

      if (is.null(df) || !is.data.frame(df)) {
        clear_state()
        return()
      }

      build_from_input(df)
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$reset_table, {
      shiny::req(rv$has_input)
      shiny::req(is.data.frame(rv$source_data))

      build_from_input(rv$source_data)

      shiny::showNotification(
        "eMoF editor reset to the table produced by Build DwC-A.",
        type = "message"
      )
    })

    shiny::observeEvent(input$apply_changes, {
      shiny::req(rv$has_input)
      shiny::req(rv$has_rows)
      shiny::req(is.data.frame(rv$working_data))

      rv$applied_data <- rv$working_data
      rv$ready <- TRUE
      rv$skipped <- FALSE

      shiny::showNotification(
        "eMoF edits applied. QC and Metadata are now unlocked.",
        type = "message"
      )
    })

    shiny::observeEvent(input$skip_step, {
      shiny::req(rv$has_input)
      shiny::req(is.data.frame(rv$source_data))

      rv$applied_data <- rv$source_data
      rv$ready <- TRUE
      rv$skipped <- TRUE

      shiny::showNotification(
        "eMoF editing skipped. The upstream built eMoF was kept unchanged.",
        type = "warning"
      )
    })

    output$apply_btn_ui <- shiny::renderUI({
      if (!isTRUE(rv$has_rows)) {
        return(NULL)
      }

      shiny::div(
        class = "emofed-side-actions",
        shiny::actionButton(
          session$ns("apply_changes"),
          "Apply changes to eMoF",
          class = "emofed-btn-apply"
        )
      )
    })

    output$skip_btn_ui <- shiny::renderUI({
      enabled <- isTRUE(rv$has_input)

      shiny::actionButton(
        session$ns("skip_step"),
        "Skip this step",
        class = "emofed-btn-skip",
        disabled = if (!enabled) "disabled" else NULL
      )
    })

    output$status_ui <- shiny::renderUI({
      if (!isTRUE(rv$has_input)) {
        return(shiny::tags$div(
          class = "emofed-small",
          "Waiting for the eMoF table generated upstream."
        ))
      }

      if (!isTRUE(rv$has_rows)) {
        return(shiny::tagList(
          shiny::tags$p(
            class = "emofed-status-warn",
            "No eMoF rows were built upstream."
          ),
          shiny::tags$p(
            class = "emofed-small",
            "You can click Skip this step to continue to QC and Metadata."
          )
        ))
      }

      df_full <- rv$working_data %||% .emof_editor_empty_full()
      df_disp <- rv$display_data %||% .emof_editor_empty_display()

      num_locked <- if ("numeric_flag" %in% names(df_disp)) {
        sum(df_disp$numeric_flag %in% TRUE, na.rm = TRUE)
      } else {
        0
      }

      decision_ui <- NULL
      if (isTRUE(rv$ready) && isTRUE(rv$skipped)) {
        decision_ui <- shiny::tags$p(
          class = "emofed-status-warn",
          "Step skipped: the original built eMoF will be used."
        )
      } else if (isTRUE(rv$ready)) {
        decision_ui <- shiny::tags$p(
          class = "emofed-status-ok",
          "Changes applied: the edited eMoF will be used downstream."
        )
      } else {
        decision_ui <- shiny::tags$p(
          class = "emofed-status-pending",
          "Draft ready: click Apply or Skip to unlock downstream tabs."
        )
      }

      shiny::tagList(
        decision_ui,
        shiny::tags$p(
          class = "emofed-small",
          sprintf("Built eMoF rows loaded: %s", format(nrow(df_full), big.mark = ","))
        ),
        shiny::tags$p(
          class = "emofed-small",
          sprintf("Grouped editor rows: %s", format(nrow(df_disp), big.mark = ","))
        ),
        shiny::tags$p(
          class = "emofed-small",
          sprintf("Numeric grouped rows with locked measurementValue: %s", format(num_locked, big.mark = ","))
        )
      )
    })

    output$table_note <- shiny::renderUI({
      if (!isTRUE(rv$has_input)) {
        return(shiny::tags$p(
          class = "emofed-small",
          "The grouped editor table will appear when an eMoF table is available upstream."
        ))
      }

      if (!isTRUE(rv$has_rows)) {
        return(shiny::tags$p(
          class = "emofed-small",
          "No editable eMoF rows are available because the upstream build did not create any eMoF records."
        ))
      }

      shiny::tags$p(
        class = "emofed-small",
        "Edit a cell to update the working draft for all original eMoF rows represented by that grouped row. ",
        "For rows marked internally as numeric groups, ",
        shiny::tags$code("measurementValue"),
        " is intentionally locked. ",
        "If you try to edit one of those rows, the value will be restored immediately. ",
        "Changes are only committed downstream after clicking ",
        shiny::tags$strong("Apply changes to eMoF"),
        "."
      )
    })

    output$editable_table <- DT::renderDT({
      rv$table_seed

      df_disp <- shiny::isolate(rv$display_data %||% .emof_editor_empty_display())

      hide_idx <- which(names(df_disp) %in% c(".emof_editor_row_id", "numeric_flag")) - 1L

      dt <- DT::datatable(
        df_disp,
        rownames = FALSE,
        selection = "none",
        editable = list(
          target = "cell",
          disable = list(columns = hide_idx)
        ),
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          autoWidth = TRUE,
          ordering = FALSE,
          columnDefs = list(
            list(visible = FALSE, targets = hide_idx)
          )
        )
      )

      DT::formatStyle(
        dt,
        "measurementValue",
        "numeric_flag",
        backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#f9f9f9", "white")),
        color = DT::styleEqual(c(TRUE, FALSE), c("#999999", "black"))
      )
    })

    shiny::observeEvent(input$editable_table_cell_edit, {
      shiny::req(rv$has_rows)
      shiny::req(is.data.frame(rv$display_data))
      shiny::req(is.data.frame(rv$working_data))

      info <- input$editable_table_cell_edit
      df_disp <- rv$display_data
      df_full <- rv$working_data

      if (nrow(df_disp) == 0 || nrow(df_full) == 0) {
        return()
      }

      row_i <- info$row
      col_i <- info$col + 1L

      shiny::req(row_i >= 1, row_i <= nrow(df_disp))
      shiny::req(col_i >= 1, col_i <= ncol(df_disp))

      col_name <- names(df_disp)[col_i]

      if (identical(col_name, "measurementValue") && isTRUE(df_disp$numeric_flag[row_i])) {
        DT::replaceData(
          proxy = table_proxy,
          data = df_disp,
          resetPaging = FALSE,
          rownames = FALSE
        )

        shiny::showNotification(
          "Cannot edit 'Multiple values'. Please edit the original numeric values upstream if needed.",
          type = "error"
        )
        return()
      }

      ids_to_update <- .emof_editor_split_ids(df_disp[row_i, ".emof_editor_row_id"])

      if (!length(ids_to_update)) {
        DT::replaceData(
          proxy = table_proxy,
          data = df_disp,
          resetPaging = FALSE,
          rownames = FALSE
        )

        shiny::showNotification(
          "Could not resolve the grouped row back to the original eMoF records.",
          type = "error"
        )
        return()
      }

      old_value <- as.character(df_disp[row_i, col_name])
      new_value <- as.character(DT::coerceValue(info$value, old_value))

      df_full[df_full$.emof_editor_row_id %in% ids_to_update, col_name] <- new_value
      df_full <- .emof_editor_init_df(df_full)

      df_disp[row_i, col_name] <- new_value

      rv$working_data <- df_full
      rv$display_data <- df_disp
      rv$ready <- FALSE
      rv$skipped <- FALSE
      rv$applied_data <- NULL

      DT::replaceData(
        proxy = table_proxy,
        data = df_disp,
        resetPaging = FALSE,
        rownames = FALSE
      )
    })

    emof_rx <- shiny::reactive({
      shiny::req(isTRUE(rv$ready))
      shiny::req(is.data.frame(rv$applied_data))

      out <- rv$applied_data

      if (".emof_editor_row_id" %in% names(out)) {
        out$.emof_editor_row_id <- NULL
      }

      out
    })

    lookup_rx <- shiny::reactive({
      rv$display_data %||% .emof_editor_empty_display()
    })

    list(
      emof = emof_rx,
      lookup = lookup_rx,
      ready = shiny::reactive(isTRUE(rv$ready))
    )
  })
}
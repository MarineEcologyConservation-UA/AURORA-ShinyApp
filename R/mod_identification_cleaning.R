# =========================================================
# Identification Data Cleaning Module
# - Uses mapped/cleaned DwC data from Field Mapping
# - Builds one editable row per unique source identification value
# - Source priority: verbatimIdentification -> scientificName
# - Optional editable fields come from ALL DwC terms available in the
#   Field Mapping dropdown, EXCEPT those already selected in mapping
# - Keeps DT pagination/page length/state after cell edits
# File: R/mod_identification_cleaning.R
# =========================================================

#' Identification Data Cleaning UI
#'
#' @param id Module id.
#' @export
mod_identification_cleaning_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$style(shiny::HTML(sprintf("
#%s .idc-page { padding: 1rem 1.25rem 2rem 1.25rem; }
#%s .idc-muted { color: #5f6b68; }
#%s .idc-info {
  background-color: #f8f9fa;
  padding: 15px;
  border-left: 5px solid #303d34;
  margin-bottom: 16px;
  border-radius: .5rem;
}
#%s .idc-warning {
  color: #303d34;
  font-weight: 400;
  margin-bottom: 14px;
  padding: 10px 12px;
  border: 1px solid #d1d1d1;
  border-radius: .5rem;
  background-color: #fffdf5;
}
#%s .idc-yellow { background-color: #fff4ce !important; font-weight: 600; }
#%s .idc-small { font-size: .92rem; color: #64706d; }
#%s .idc-side .card-body { padding: 1rem; }
#%s .idc-main .card-body { padding: 1rem; }
",
      ns("root"), ns("root"), ns("root"), ns("root"),
      ns("root"), ns("root"), ns("root"), ns("root")
    ))),

    shiny::tags$div(
      id = ns("root"),
      class = "idc-page",

      bslib::card(
        bslib::card_header("Identification Data Cleaning"),
        bslib::card_body(
          shiny::tags$div(
            class = "idc-info",
            shiny::tags$p(
              "According to the ", shiny::tags$strong("Darwin Core"),
              " standard, when scientific names include open nomenclature qualifiers such as 'cf.', 'aff.', or informal placeholders (e.g., 'sp.'), these qualifiers should not be included in the ",
              shiny::tags$code("scientificName"),
              " field, but rather in the ",
              shiny::tags$code("identificationQualifier"),
              " term. The ",
              shiny::tags$code("scientificName"),
              " should contain the lowest possible taxon rank that can be confidently determined, while the ",
              shiny::tags$code("identificationQualifier"),
              " field captures the determiner's uncertainty or relationship to a known taxon."
            ),
            shiny::tags$p(
              "For example, for ", shiny::tags$em("Gadus cf. morhua"), ":"
            ),
            shiny::tags$ul(
              shiny::tags$li(shiny::tags$code("scientificName"), ": 'Gadus'"),
              shiny::tags$li(shiny::tags$code("taxonRank"), ": 'genus'"),
              shiny::tags$li(shiny::tags$code("identificationQualifier"), ": 'cf. morhua'")
            ),
            shiny::tags$p(
              "To ensure data traceability, the original identification is retained in the ",
              shiny::tags$code("verbatimIdentification"),
              " field as a historical record."
            )
          ),

          shiny::tags$div(
            class = "idc-warning",
            shiny::tags$strong("Warning: "),
            "The ", shiny::tags$strong("scientificName"), " column (yellow) requires manual editing and review.",
            shiny::tags$br(),
            "Optional fields shown below are Darwin Core terms that were not selected during Field Mapping. They can be reviewed and filled here from values already present in the dataset. ",
            shiny::tags$strong("Please review all entries for accuracy.")
          ),

          bslib::layout_columns(
            col_widths = c(4, 8),
            gap = "1rem",

            bslib::card(
              class = "idc-side",
              bslib::card_header("Settings"),
              bslib::card_body(
                shiny::uiOutput(ns("source_info")),
                shiny::uiOutput(ns("other_cols_ui")),
                shiny::div(
                  class = "d-flex gap-2 flex-wrap mt-3",
                  shiny::actionButton(
                    ns("build_table"),
                    "Build Editable Table",
                    class = "btn-primary"
                  ),
                  shiny::uiOutput(ns("apply_btn_ui"), inline = TRUE)
                ),
                shiny::hr(),
                shiny::uiOutput(ns("status_ui")),
                shiny::hr(),
                shiny::downloadButton(
                  ns("download_lookup"),
                  "Download decisions (CSV)",
                  class = "btn-outline-secondary"
                )
              )
            ),

            bslib::card(
              class = "idc-main",
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

#' Identification Data Cleaning server
#'
#' @param id Module id.
#' @param df_in Reactive returning the cleaned output from Field Mapping.
#' @param mapping_in Reactive returning the Field Mapping table.
#' @return A list with reactives: df_out, lookup, ready, source_column.
#' @export
mod_identification_cleaning_server <- function(id, df_in, mapping_in) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    `%||%` <- function(x, y) if (is.null(x)) y else x

    rv <- shiny::reactiveValues(
      full_data = NULL,
      display_data = NULL,
      col_map = NULL,
      source_col = NULL,
      applied_df = NULL,
      ready = FALSE,
      fallback_used = FALSE,
      skipped = FALSE,
      table_seed = 0L
    )

    normalize_text <- function(x) {
      x <- as.character(x %||% "")
      x[is.na(x)] <- ""
      x <- gsub("[[:space:]]+", " ", trimws(x))
      x
    }

    first_non_empty <- function(x) {
      x <- normalize_text(x)
      x <- x[nzchar(x)]
      if (length(x) == 0) "" else x[1]
    }

    get_all_dwc_terms <- function() {
      if (!requireNamespace("corella", quietly = TRUE)) {
        return(character(0))
      }

      x <- tryCatch(corella::darwin_core_terms, error = function(e) NULL)

      if (is.null(x)) {
        return(character(0))
      }

      x <- as.data.frame(x, stringsAsFactors = FALSE)

      if (!("term" %in% names(x))) {
        return(character(0))
      }

      x <- x[!is.na(x$term) & x$term != "", , drop = FALSE]
      x <- x[!duplicated(x$term), , drop = FALSE]

      sort(unique(x$term))
    }

    parse_identification_scientific_name <- function(txt) {
      raw <- normalize_text(txt)
      if (!nzchar(raw)) return("")

      tokens <- unlist(strsplit(raw, "\\s+"))
      low_tokens <- tolower(tokens)

      qualifier_set <- c(
        "cf", "cf.", "aff", "aff.", "nr", "nr.",
        "sp", "sp.", "spp", "spp.", "indet", "indet."
      )

      q_idx <- which(low_tokens %in% qualifier_set)
      q_idx <- if (length(q_idx) == 0) 0 else q_idx[1]

      sci <- raw

      if (q_idx > 0) {
        sci_tokens <- if (q_idx > 1) tokens[seq_len(q_idx - 1)] else character(0)

        if (length(sci_tokens) == 0) {
          sci <- ""
        } else {
          q_word <- tolower(tokens[q_idx])

          if (q_word %in% c("cf", "cf.", "aff", "aff.", "nr", "nr.", "sp", "sp.", "spp", "spp.")) {
            sci_tokens <- sci_tokens[1]
          }

          sci <- paste(sci_tokens, collapse = " ")
        }
      } else if (grepl("\\?$", raw)) {
        sci <- trimws(sub("\\?+$", "", raw))
      }

      normalize_text(sci)
    }

    source_column <- shiny::reactive({
      df <- df_in()
      shiny::req(df)

      cols <- names(df)

      has_vi <- "verbatimIdentification" %in% cols &&
        any(nzchar(normalize_text(df$verbatimIdentification)))

      has_sn <- "scientificName" %in% cols &&
        any(nzchar(normalize_text(df$scientificName)))

      if (has_vi) return("verbatimIdentification")
      if (has_sn) return("scientificName")
      NULL
    })

    output$source_info <- shiny::renderUI({
      df <- df_in()
      shiny::req(df)

      src <- source_column()

      if (is.null(src)) {
        return(
          shiny::tags$div(
            class = "alert alert-warning",
            "Neither verbatimIdentification nor scientificName is available with values in the current dataset."
          )
        )
      }

      has_vi <- "verbatimIdentification" %in% names(df) &&
        any(nzchar(normalize_text(df$verbatimIdentification)))

      fallback <- identical(src, "scientificName") && !has_vi

      shiny::tagList(
        shiny::tags$p(
          shiny::tags$b("Source column used for grouping: "),
          shiny::tags$code(src)
        ),
        if (fallback) {
          shiny::tags$div(
            class = "alert alert-info py-2 mb-2",
            "verbatimIdentification was not available with values, so the module is using scientificName as the source column."
          )
        },
        shiny::tags$p(
          class = "idc-small",
          "The table below shows one editable row per unique value of the source identification column."
        )
      )
    })

    output$other_cols_ui <- shiny::renderUI({
      df <- df_in()
      map_df <- mapping_in()
      shiny::req(df, map_df)

      src <- source_column()
      if (is.null(src)) return(NULL)

      shiny::req(is.data.frame(map_df))
      shiny::req(all(c("user_column", "dwc_term") %in% names(map_df)))

      dwc_term_chr <- as.character(map_df$dwc_term %||% "")
      dwc_term_chr[is.na(dwc_term_chr)] <- ""
      dwc_term_chr <- trimws(dwc_term_chr)

      mapped_terms <- unique(dwc_term_chr[nzchar(dwc_term_chr)])

      all_dwc_terms <- get_all_dwc_terms()

      if (length(all_dwc_terms) == 0) {
        all_dwc_terms <- sort(unique(c(
          mapped_terms,
          "identificationQualifier", "taxonRank", "lifeStage", "sex",
          "scientificName", "verbatimIdentification", "eventID", "occurrenceID",
          "basisOfRecord", "eventDate", "decimalLatitude", "decimalLongitude"
        )))
      }

      excluded_base_terms <- c(
        "scientificName",
        "verbatimIdentification"
      )

      available <- setdiff(all_dwc_terms, unique(c(mapped_terms, excluded_base_terms)))
      available <- sort(unique(available))

      default_selected <- intersect(
        c("identificationQualifier", "taxonRank", "lifeStage", "sex"),
        available
      )

      shiny::selectizeInput(
        ns("other_cols"),
        label = "Optional fields to include/edit",
        choices = available,
        selected = default_selected,
        multiple = TRUE,
        options = list(
          placeholder = "Select Darwin Core terms not selected during Field Mapping"
        )
      )
    })

    shiny::observeEvent(list(df_in(), mapping_in()), {
      rv$full_data <- NULL
      rv$display_data <- NULL
      rv$col_map <- NULL
      rv$source_col <- NULL
      rv$applied_df <- NULL
      rv$ready <- FALSE
      rv$fallback_used <- FALSE
      rv$skipped <- FALSE
      rv$table_seed <- 0L
    }, ignoreInit = FALSE)

    build_lookup <- function(df, selected_cols) {
      src <- source_column()
      if (is.null(src)) return(NULL)

      out <- df
      out$.row_id <- seq_len(nrow(out))

      for (col in c("scientificName", "verbatimIdentification")) {
        if (!col %in% names(out)) out[[col]] <- ""
        out[[col]] <- normalize_text(out[[col]])
      }

      for (col in selected_cols) {
        if (!col %in% names(out)) out[[col]] <- ""
        out[[col]] <- normalize_text(out[[col]])
      }

      if (!any(nzchar(out$verbatimIdentification)) && any(nzchar(out$scientificName))) {
        out$verbatimIdentification <- out$scientificName
      } else {
        blank_vi <- !nzchar(out$verbatimIdentification)
        out$verbatimIdentification[blank_vi] <- out$scientificName[blank_vi]
      }

      key_vec <- normalize_text(out[[src]])
      blank_key <- !nzchar(key_vec)

      if (any(blank_key)) {
        alt <- if (identical(src, "verbatimIdentification")) {
          normalize_text(out$scientificName)
        } else {
          normalize_text(out$verbatimIdentification)
        }
        key_vec[blank_key] <- alt[blank_key]
      }

      out$.source_key <- key_vec
      out <- out[nzchar(out$.source_key), , drop = FALSE]

      if (nrow(out) == 0) return(NULL)

      split_idx <- split(seq_len(nrow(out)), out$.source_key)

      show_sn <- "scientificName" %in% names(df) &&
        any(nzchar(normalize_text(df$scientificName)))

      show_vi <- "verbatimIdentification" %in% names(df) &&
        any(nzchar(normalize_text(df$verbatimIdentification)))

      rows <- lapply(names(split_idx), function(key) {
        idx <- split_idx[[key]]
        block <- out[idx, , drop = FALSE]

        src_value <- key

        sci_current <- first_non_empty(block$scientificName)
        if (!nzchar(sci_current)) sci_current <- src_value

        verb_current <- first_non_empty(block$verbatimIdentification)
        if (!nzchar(verb_current)) verb_current <- src_value

        parsed_scientific_name <- parse_identification_scientific_name(src_value)

        row <- list(
          sourceValue = src_value
        )

        if (show_sn) row$scientificName_current <- sci_current
        if (show_vi) row$verbatimIdentification <- verb_current

        row$scientificName <- if (nzchar(parsed_scientific_name)) {
          parsed_scientific_name
        } else {
          sci_current
        }

        for (col in selected_cols) {
          row[[col]] <- first_non_empty(block[[col]])
        }

        row$.row_ids <- paste(block$.row_id, collapse = ",")
        as.data.frame(row, stringsAsFactors = FALSE)
      })

      disp <- do.call(rbind, rows)
      disp[] <- lapply(disp, normalize_text)

      map <- c(scientificName = "scientificName")
      for (col in selected_cols) {
        map[[col]] <- col
      }

      list(
        full_data = out,
        display_data = disp,
        col_map = map,
        source_col = src
      )
    }

    table_proxy <- DT::dataTableProxy(outputId = ns("editable_table"))

    shiny::observeEvent(input$build_table, {
      df <- df_in()
      shiny::req(df)

      built <- build_lookup(df, input$other_cols %||% character(0))
      shiny::req(!is.null(built))

      rv$full_data <- built$full_data
      rv$display_data <- built$display_data
      rv$col_map <- built$col_map
      rv$source_col <- built$source_col
      rv$applied_df <- NULL
      rv$ready <- FALSE
      rv$fallback_used <- identical(built$source_col, "scientificName")
      rv$skipped <- FALSE
      rv$table_seed <- rv$table_seed + 1L
    })

    output$table_note <- shiny::renderUI({
      req(rv$display_data)

      shiny::tags$p(
        class = "idc-small",
        paste0(
          nrow(rv$display_data),
          " unique identification entr",
          if (nrow(rv$display_data) == 1) "y" else "ies",
          " available for review."
        )
      )
    })

    output$editable_table <- DT::renderDT({
      req(rv$table_seed > 0L)

      df_disp <- isolate(rv$display_data)
      req(df_disp)

      hide_idx <- which(names(df_disp) %in% c("sourceValue", ".row_ids")) - 1L
      editable_names <- c("scientificName", isolate(input$other_cols %||% character(0)))
      editable_cols <- which(names(df_disp) %in% editable_names) - 1L
      yellow_col_name <- "scientificName"

      dt <- DT::datatable(
        df_disp,
        rownames = FALSE,
        selection = "none",
        editable = list(
          target = "cell",
          disable = list(
            columns = setdiff(seq_along(names(df_disp)) - 1L, editable_cols)
          )
        ),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          stateSave = TRUE,
          stateDuration = -1,
          columnDefs = list(
            list(visible = FALSE, targets = hide_idx)
          )
        )
      )

      if (yellow_col_name %in% names(df_disp)) {
        dt <- DT::formatStyle(
          dt,
          columns = yellow_col_name,
          backgroundColor = "#fff4ce",
          fontWeight = "600"
        )
      }

      dt
    }, server = FALSE)

    shiny::observeEvent(input$editable_table_cell_edit, {
      req(rv$display_data, rv$full_data, rv$col_map)

      info <- input$editable_table_cell_edit
      df_disp <- rv$display_data
      df_full <- rv$full_data

      if (is.null(info$row) || is.null(info$col)) {
        return(invisible(NULL))
      }

      col_name <- names(df_disp)[info$col + 1L]

      if (!col_name %in% names(rv$col_map)) {
        return(invisible(NULL))
      }

      target_col <- rv$col_map[[col_name]]
      ids <- as.integer(strsplit(df_disp$.row_ids[info$row], ",", fixed = TRUE)[[1]])
      value <- normalize_text(info$value)

      df_disp[info$row, col_name] <- value
      df_full[df_full$.row_id %in% ids, target_col] <- value

      rv$display_data <- df_disp
      rv$full_data <- df_full
      rv$ready <- FALSE

      DT::replaceData(
        proxy = table_proxy,
        data = rv$display_data,
        resetPaging = FALSE,
        rownames = FALSE
      )
    })

    output$apply_btn_ui <- shiny::renderUI({
      req(rv$display_data)

      shiny::tagList(
        shiny::actionButton(
          ns("apply_changes"),
          "Apply changes to dataset",
          class = "btn-success"
        ),
        shiny::actionButton(
          ns("skip_step"),
          "Skip this step",
          class = "btn-outline-warning"
        )
      )
    })

    shiny::observeEvent(input$apply_changes, {
      req(rv$full_data)

      out <- rv$full_data
      if (".row_id" %in% names(out)) out$.row_id <- NULL
      if (".source_key" %in% names(out)) out$.source_key <- NULL

      rv$applied_df <- out
      rv$ready <- TRUE
      rv$skipped <- FALSE

      shiny::showNotification(
        "Identification cleaning changes applied to the dataset.",
        type = "message"
      )
    })

    shiny::observeEvent(input$skip_step, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Important Notice",
          shiny::tags$p(
            "If you choose to skip this step, you must ensure that your scientificName field is fully compliant with the Darwin Core Standards:"
          ),
          shiny::tags$p(
            shiny::tags$a(
              href = "http://rs.tdwg.org/dwc/terms/scientificName",
              target = "_blank",
              "http://rs.tdwg.org/dwc/terms/scientificName"
            )
          ),
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("confirm_skip"), "Skip this step", class = "btn-warning")
          )
        )
      )
    })

    shiny::observeEvent(input$confirm_skip, {
      shiny::removeModal()
      rv$applied_df <- NULL
      rv$ready <- TRUE
      rv$skipped <- TRUE

      shiny::showNotification(
        "Identification cleaning was skipped. Please make sure scientificName is already Darwin Core compliant.",
        type = "warning",
        duration = 8
      )
    })

    output$status_ui <- shiny::renderUI({
      df <- rv$display_data

      if (is.null(df)) {
        return(
          shiny::tags$div(
            class = "idc-small",
            "Build the editable table to start reviewing identifications."
          )
        )
      }

      edited_vals <- if ("scientificName" %in% names(df)) {
        sum(nzchar(normalize_text(df$scientificName)))
      } else {
        0L
      }

      shiny::tagList(
        shiny::tags$p(shiny::tags$b("Source column: "), shiny::tags$code(rv$source_col %||% "")),
        shiny::tags$p(shiny::tags$b("Unique entries: "), nrow(df)),
        shiny::tags$p(shiny::tags$b("scientificName values present in review table: "), edited_vals),
        shiny::tags$p(shiny::tags$b("Applied to dataset: "), if (!is.null(rv$applied_df) && !isTRUE(rv$skipped)) "Yes" else "No"),
        shiny::tags$p(shiny::tags$b("Step skipped: "), if (isTRUE(rv$skipped)) "Yes" else "No")
      )
    })

    output$download_lookup <- shiny::downloadHandler(
      filename = function() {
        paste0("identification_cleaning_lookup_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        req(rv$display_data)
        out <- rv$display_data
        if ("sourceValue" %in% names(out)) out$sourceValue <- NULL
        if (".row_ids" %in% names(out)) out$.row_ids <- NULL
        utils::write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    df_out <- shiny::reactive({
      if (!is.null(rv$applied_df)) return(rv$applied_df)

      df <- df_in()
      shiny::req(df)
      df
    })

    list(
      df_out = df_out,
      lookup = shiny::reactive(rv$display_data),
      ready = shiny::reactive(isTRUE(rv$ready)),
      source_column = shiny::reactive(rv$source_col %||% source_column())
    )
  })
}
# =========================================================
# Identification Data Cleaning Module
# - Uses mapped/cleaned DwC data from Field Mapping
# - Builds one editable row per unique source identification value
# - Source priority: verbatimIdentification -> scientificName
# - Optional editable fields come from ALL DwC terms available in the
#   Field Mapping dropdown, EXCEPT those already selected in mapping
# - Optional fields are deselected by default
# - scientificName is NOT changed automatically
# - A suggested cleaned name is shown separately
# - The user must either copy the suggestion or type the reviewed name
# - Apply is blocked until all Reviewed scientificName cells are filled
# - Skip keeps the original df_in() unchanged
# - Uses incremental table updates via DT proxy
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
#%s .idc-small { font-size: .92rem; color: #64706d; }
#%s .idc-side .card-body { padding: 1rem; }
#%s .idc-main .card-body { padding: 1rem; }
",
      ns("root"), ns("root"), ns("root"), ns("root"),
      ns("root"), ns("root"), ns("root")
    ))),

    shiny::tags$script(
      shiny::HTML("
    (function() {
      function getScrollBody(id) {
        var root = document.getElementById(id);
        if (!root) return null;
        return root.querySelector('.dataTables_scrollBody');
      }

      function rememberScroll(id) {
        var body = getScrollBody(id);
        if (!body) return;
        body.dataset.lastScrollTop = body.scrollTop || 0;
        body.dataset.lastScrollLeft = body.scrollLeft || 0;
      }

      function restoreScroll(id) {
        var body = getScrollBody(id);
        if (!body) return;
        var top = parseInt(body.dataset.lastScrollTop || '0', 10);
        var left = parseInt(body.dataset.lastScrollLeft || '0', 10);
        body.scrollTop = isNaN(top) ? 0 : top;
        body.scrollLeft = isNaN(left) ? 0 : left;
      }

      document.addEventListener('scroll', function(e) {
        var body = e.target;
        if (!body || !body.classList || !body.classList.contains('dataTables_scrollBody')) return;
        var wrapper = body.closest('.datatables');
        if (!wrapper) return;
        var id = wrapper.id;
        if (!id) return;
        body.dataset.lastScrollTop = body.scrollTop || 0;
        body.dataset.lastScrollLeft = body.scrollLeft || 0;
      }, true);

      Shiny.addCustomMessageHandler('idc-remember-scroll', function(x) {
        rememberScroll(x.id);
      });

      Shiny.addCustomMessageHandler('idc-restore-scroll', function(x) {
        setTimeout(function() { restoreScroll(x.id); }, 0);
      });
    })();
    ")
    ),
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
            "The ", shiny::tags$strong("Reviewed scientificName"), " column (red) must be completed manually or by accepting the suggestion for every row before applying changes.",
            shiny::tags$br(),
            "Optional fields shown below are Darwin Core terms that were not selected during Field Mapping. They can be reviewed and edited here if needed. ",
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

    suggest_scientific_name <- function(txt) {
      raw <- normalize_text(txt)
      if (!nzchar(raw)) return("")

      out <- tolower(raw)
      out <- gsub("\\([^)]*\\)", " ", out)
      out <- gsub("\\[[^]]*\\]", " ", out)
      out <- gsub("\\b(juvenile|juveniles|adult|adults|larva|larvae|egg|eggs|subadult|zygote|seedling|flowering|fruiting)\\b", " ", out, perl = TRUE)
      out <- gsub("\\b(male|female|unknown sex|hermaphrodite)\\b", " ", out, perl = TRUE)
      out <- gsub("\\b(alive|dead)\\b", " ", out, perl = TRUE)
      out <- gsub("\\b(indet\\.?|msp\\.?|sp\\.?|spp\\.?|cf\\.?|aff\\.?|nr\\.?|morphotype)\\b", " ", out, perl = TRUE)
      out <- gsub("\\b\\d+\\b", " ", out, perl = TRUE)
      out <- gsub("[,;:]+", " ", out)
      out <- gsub("\\?+", " ", out)
      out <- gsub("\\s+", " ", out)
      out <- trimws(out)

      toks <- unlist(strsplit(out, "\\s+"))
      toks <- toks[nzchar(toks)]
      toks <- toks[grepl("[a-z]", toks, perl = TRUE)]

      if (length(toks) == 0) {
        return("")
      }

      if (length(toks) >= 2) {
        toks <- toks[1:2]
      }

      toks[1] <- paste0(toupper(substr(toks[1], 1, 1)), substr(toks[1], 2, nchar(toks[1])))
      if (length(toks) >= 2) {
        toks[2] <- tolower(toks[2])
      }

      normalize_text(paste(toks, collapse = " "))
    }

    suggest_identification_qualifier <- function(x) {
      x <- normalize_text(x)
      if (!nzchar(x)) return("")

      low <- tolower(x)

      m <- regexec("\\b(cf\\.?|aff\\.?|nr\\.?|sp\\.?|spp\\.?|msp\\.?)(.*)$", low, perl = TRUE)
      hit <- regmatches(low, m)[[1]]

      if (length(hit) > 0) {
        return(normalize_text(paste0(hit[2], hit[3])))
      }

      if (grepl("\\?$", low)) {
        return("?")
      }

      ""
    }

    suggest_lifestage <- function(x) {
      x <- tolower(normalize_text(x))
      if (!nzchar(x)) return("")

      terms <- c("zygote", "larva", "juvenile", "subadult", "adult", "seedling", "flowering", "fruiting")
      hit <- terms[vapply(terms, function(tt) grepl(paste0("\\b", tt, "\\b"), x, perl = TRUE), logical(1))]
      if (length(hit) == 0) "" else hit[1]
    }

    suggest_sex <- function(x) {
      x <- tolower(normalize_text(x))
      if (!nzchar(x)) return("")

      terms <- c("male", "female", "hermaphrodite")
      hit <- terms[vapply(terms, function(tt) grepl(paste0("\\b", tt, "\\b"), x, perl = TRUE), logical(1))]
      if (length(hit) == 0) "" else hit[1]
    }

    suggest_optional_value <- function(col, block, source_value) {
      existing <- first_non_empty(block[[col]])
      if (nzchar(existing)) {
        return(existing)
      }

      combined <- normalize_text(paste(
        first_non_empty(block$scientificName),
        first_non_empty(block$verbatimIdentification),
        source_value
      ))

      if (identical(col, "identificationQualifier")) {
        return(suggest_identification_qualifier(combined))
      }

      if (identical(col, "lifeStage")) {
        return(suggest_lifestage(combined))
      }

      if (identical(col, "sex")) {
        return(suggest_sex(combined))
      }

      ""
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

      shiny::selectizeInput(
        ns("other_cols"),
        label = "Optional fields to include/edit",
        choices = available,
        selected = character(0),
        multiple = TRUE,
        options = list(
          placeholder = "Select Darwin Core terms not selected during Field Mapping"
        )
      )
    })

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

      if (!(".reviewedScientificName" %in% names(out))) {
        out$.reviewedScientificName <- ""
      } else {
        out$.reviewedScientificName <- normalize_text(out$.reviewedScientificName)
      }

      if (!(".suggestedScientificName" %in% names(out))) {
        out$.suggestedScientificName <- ""
      } else {
        out$.suggestedScientificName <- normalize_text(out$.suggestedScientificName)
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

      rows <- lapply(names(split_idx), function(key) {
        idx <- split_idx[[key]]
        block <- out[idx, , drop = FALSE]

        src_value <- key
        sci_current <- first_non_empty(block$scientificName)
        verb_current <- first_non_empty(block$verbatimIdentification)
        suggested <- suggest_scientific_name(src_value)
        reviewed <- first_non_empty(block$.reviewedScientificName)

        row <- list(
          sourceValue = src_value,
          currentScientificName = sci_current,
          verbatimIdentification = verb_current,
          suggestedScientificName = suggested,
          useSuggestion = "",
          reviewedScientificName = reviewed
        )

        for (col in selected_cols) {
          row[[col]] <- suggest_optional_value(col, block, src_value)
        }

        row$.row_ids <- paste(block$.row_id, collapse = ",")
        as.data.frame(row, stringsAsFactors = FALSE)
      })

      disp <- do.call(rbind, rows)
      disp[] <- lapply(disp, normalize_text)

      for (i in seq_len(nrow(disp))) {
        ids <- as.integer(strsplit(disp$.row_ids[i], ",", fixed = TRUE)[[1]])
        ids <- ids[!is.na(ids)]

        if (length(ids) == 0) next

        out[out$.row_id %in% ids, ".suggestedScientificName"] <- disp$suggestedScientificName[i]

        disp$useSuggestion[i] <- as.character(
          shiny::tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary",
            onclick = sprintf(
              "Shiny.setInputValue('%s', {row: %d, nonce: Math.random()}, {priority: 'event'})",
              ns("use_suggestion"),
              i
            ),
            "Use"
          )
        )
      }

      map <- c(reviewedScientificName = ".reviewedScientificName")
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

    refresh_table_incremental <- function() {
      session$sendCustomMessage(
        "idc-remember-scroll",
        list(id = ns("editable_table"))
      )

      DT::replaceData(
        proxy = table_proxy,
        data = rv$display_data,
        resetPaging = FALSE,
        rownames = FALSE
      )

      session$onFlushed(function() {
        session$sendCustomMessage(
          "idc-restore-scroll",
          list(id = ns("editable_table"))
        )
      }, once = TRUE)
    }

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

    table_proxy <- DT::dataTableProxy("editable_table", session = session)

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
      shiny::req(rv$display_data)

      missing_n <- sum(!nzchar(normalize_text(rv$display_data$reviewedScientificName)))

      shiny::tags$p(
        class = "idc-small",
        paste0(
          nrow(rv$display_data),
          " unique identification entr",
          if (nrow(rv$display_data) == 1) "y" else "ies",
          " available for review. ",
          missing_n,
          " reviewed scientificName cell",
          if (missing_n == 1) " is" else "s are",
          " still missing."
        )
      )
    })

    output$editable_table <- DT::renderDT({
      shiny::req(rv$table_seed > 0L)

      df_disp <- rv$display_data
      shiny::req(df_disp)

      hide_idx <- which(names(df_disp) %in% c("sourceValue", ".row_ids")) - 1L
      editable_names <- c("reviewedScientificName", input$other_cols %||% character(0))
      editable_cols <- which(names(df_disp) %in% editable_names) - 1L

      dt <- DT::datatable(
        df_disp,
        rownames = FALSE,
        selection = "none",
        escape = FALSE,
        editable = list(
          target = "cell",
          disable = list(
            columns = setdiff(seq_along(names(df_disp)) - 1L, editable_cols)
          )
        ),
        options = list(
          paging = FALSE,
          searching = TRUE,
          info = FALSE,
          scrollX = TRUE,
          scrollY = "520px",
          scrollCollapse = TRUE,
          autoWidth = TRUE,
          order = list(list(which(names(df_disp) == "reviewedScientificName") - 1L, "asc")),
          columnDefs = list(
            list(visible = FALSE, targets = hide_idx)
          ),
          preDrawCallback = DT::JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
          drawCallback = DT::JS("function() { Shiny.bindAll(this.api().table().node()); }")
        )
      )

      dt <- DT::formatStyle(
        dt,
        columns = "currentScientificName",
        backgroundColor = "#f8f9fa"
      )

      dt <- DT::formatStyle(
        dt,
        columns = "suggestedScientificName",
        backgroundColor = "#eef6ff"
      )

      dt <- DT::formatStyle(
        dt,
        columns = "reviewedScientificName",
        backgroundColor = "#fde7e9",
        fontWeight = "600"
      )

      dt
    }, server = FALSE)

    shiny::observeEvent(input$use_suggestion, {
      shiny::req(rv$display_data, rv$full_data)

      row_i <- as.integer(input$use_suggestion$row %||% NA_integer_)
      if (is.na(row_i) || row_i < 1 || row_i > nrow(rv$display_data)) {
        return(invisible(NULL))
      }

      df_disp <- rv$display_data
      df_full <- rv$full_data

      suggested <- normalize_text(df_disp$suggestedScientificName[row_i])
      ids <- as.integer(strsplit(df_disp$.row_ids[row_i], ",", fixed = TRUE)[[1]])
      ids <- ids[!is.na(ids)]

      df_disp$reviewedScientificName[row_i] <- suggested

      if (length(ids) > 0) {
        df_full[df_full$.row_id %in% ids, ".reviewedScientificName"] <- suggested
      }

      rv$display_data <- df_disp
      rv$full_data <- df_full
      rv$ready <- FALSE

      refresh_table_incremental()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$editable_table_cell_edit, {
      shiny::req(rv$display_data, rv$full_data, rv$col_map)

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
      ids <- ids[!is.na(ids)]
      value <- normalize_text(info$value)

      df_disp[info$row, col_name] <- value

      if (length(ids) > 0) {
        df_full[df_full$.row_id %in% ids, target_col] <- value
      }

      rv$display_data <- df_disp
      rv$full_data <- df_full
      rv$ready <- FALSE

      refresh_table_incremental()
    }, ignoreInit = TRUE)

    output$apply_btn_ui <- shiny::renderUI({
      shiny::req(rv$display_data)

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
      shiny::req(rv$full_data, rv$display_data)

      reviewed <- normalize_text(rv$display_data$reviewedScientificName)
      missing_idx <- which(!nzchar(reviewed))

      if (length(missing_idx) > 0) {
        missing_vals <- unique(normalize_text(rv$display_data$sourceValue[missing_idx]))
        missing_vals <- missing_vals[nzchar(missing_vals)]

        shiny::showModal(
          shiny::modalDialog(
            title = shiny::tags$span(style = "color:#b91c1c;", "Warning"),
            shiny::tags$p(
              "All cells in ",
              shiny::tags$strong("Reviewed scientificName"),
              " must be completed before applying changes."
            ),
            if (length(missing_vals) > 0) {
              shiny::tagList(
                shiny::tags$p("Pending rows:"),
                shiny::tags$ul(
                  lapply(missing_vals, shiny::tags$li)
                )
              )
            },
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          )
        )
        return(invisible(NULL))
      }

      out <- rv$full_data
      reviewed_full <- normalize_text(out$.reviewedScientificName)

      replace_idx <- nzchar(reviewed_full)
      out$scientificName[replace_idx] <- reviewed_full[replace_idx]

      drop_cols <- intersect(
        c(".row_id", ".source_key", ".reviewedScientificName", ".suggestedScientificName"),
        names(out)
      )
      if (length(drop_cols) > 0) {
        out[drop_cols] <- NULL
      }

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

      reviewed_n <- sum(nzchar(normalize_text(df$reviewedScientificName)))
      missing_n <- nrow(df) - reviewed_n

      shiny::tagList(
        shiny::tags$p(shiny::tags$b("Source column: "), shiny::tags$code(rv$source_col %||% "")),
        shiny::tags$p(shiny::tags$b("Unique entries: "), nrow(df)),
        shiny::tags$p(shiny::tags$b("Reviewed scientificName completed: "), reviewed_n, " / ", nrow(df)),
        shiny::tags$p(shiny::tags$b("Missing reviewed scientificName: "), missing_n),
        shiny::tags$p(shiny::tags$b("Applied to dataset: "), if (!is.null(rv$applied_df) && !isTRUE(rv$skipped)) "Yes" else "No"),
        shiny::tags$p(shiny::tags$b("Step skipped: "), if (isTRUE(rv$skipped)) "Yes" else "No")
      )
    })

    output$download_lookup <- shiny::downloadHandler(
      filename = function() {
        paste0("identification_cleaning_lookup_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        shiny::req(rv$display_data)
        out <- rv$display_data
        drop_cols <- intersect(c("sourceValue", ".row_ids", "useSuggestion"), names(out))
        if (length(drop_cols) > 0) {
          out[drop_cols] <- NULL
        }
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
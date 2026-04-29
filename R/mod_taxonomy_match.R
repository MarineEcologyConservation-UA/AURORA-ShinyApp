# =========================================================
# Taxonomy Match Module (WoRMS / GBIF)
# - GBIF remains available
# - WoRMS uses ONLY worrms::wm_records_taxamatch()
# - GBIF uses ONLY rgbif::name_backbone(verbose = TRUE)
# - input column configurable (default: scientificName at this stage)
# - normalisedQuery explicit and always created
# - one lookup row per unique inputName
# - selectable output columns to keep in final dataset
# - notes block moved to the top
# - dropped rows are logged for later QC merge
# File: R/mod_taxonomy_match.R
# =========================================================

# -------- helper: init drop log --------
.init_drop_log <- function() {
  data.frame(
    module = character(),
    step = character(),
    reason = character(),
    .aurora_origin_row = integer(),
    .aurora_origin_id = character(),
    details = character(),
    stringsAsFactors = FALSE
  )
}

# -------- helper: append dropped rows to log --------
.append_drop_log <- function(log_df, dropped_df, module, step, reason, details = NA_character_) {
  if (is.null(log_df) || !is.data.frame(log_df)) {
    log_df <- .init_drop_log()
  }

  if (is.null(dropped_df) || !is.data.frame(dropped_df) || nrow(dropped_df) == 0) {
    return(log_df)
  }

  req_cols <- c(".aurora_origin_row", ".aurora_origin_id")
  miss <- setdiff(req_cols, names(dropped_df))
  if (length(miss) > 0) {
    stop("Dropped rows are missing origin columns: ", paste(miss, collapse = ", "))
  }

  new_rows <- data.frame(
    module = rep(module, nrow(dropped_df)),
    step = rep(step, nrow(dropped_df)),
    reason = rep(reason, nrow(dropped_df)),
    .aurora_origin_row = dropped_df$.aurora_origin_row,
    .aurora_origin_id = as.character(dropped_df$.aurora_origin_id),
    details = rep(as.character(details), nrow(dropped_df)),
    stringsAsFactors = FALSE
  )

  dplyr::bind_rows(log_df, new_rows)
}

#' Taxonomy match UI
#' @param id module id
#' @export
mod_taxonomy_match_ui <- function(id) {
  ns <- shiny::NS(id)

  root_sel <- paste0("#", ns("root"))

  css <- paste0(
    "
", root_sel, " .tax-top .card-body { padding: 1rem; }
", root_sel, " .tax-top .card { height: 100%; }
", root_sel, " .tax-minh { min-height: 300px; }
", root_sel, " .tax-actions .btn { margin-right: .5rem; }

", root_sel, " .tax-actions-right {
  display: flex;
  justify-content: flex-end;
  align-items: center;
  gap: .5rem;
  flex-wrap: wrap;
  margin-top: 1rem;
  width: 100%;
}

", root_sel, " .tax-actions-right .btn { margin-right: 0; }
", root_sel, " .tax-scroll-note { color: #6b7280; font-size: .9rem; }
", root_sel, " .tax-muted { color: #6b7280; }
", root_sel, " .tax-small { font-size: .92rem; }
", root_sel, " .tax-cols-wrap { max-height: 260px; overflow-y: auto; padding-right: .5rem; }
", root_sel, " .tax-card-fill { height: 100%; }

", root_sel, " .tax-summary-list {
  max-height: 120px;
  overflow-y: auto;
  margin-top: .5rem;
  padding-left: 1rem;
}

", root_sel, " .tax-warning-box {
  border: 1px solid #dc3545;
  background: #fff5f5;
  border-radius: .5rem;
  padding: .75rem 1rem;
  margin-bottom: .75rem;
}

", root_sel, " .tax-warning-title {
  color: #b42318;
  font-weight: 700;
  margin-bottom: .35rem;
}

", root_sel, " .tax-warning-list {
  margin: .25rem 0 0 1rem;
  max-height: 180px;
  overflow-y: auto;
}

", root_sel, " .tax-lookup-card .card-body {
  overflow: visible !important;
}

", root_sel, " .tax-lookup-wrap {
  width: 100%;
  overflow: visible !important;
}

", root_sel, " .tax-lookup-wrap .dataTables_wrapper {
  width: 100%;
  margin-bottom: 0;
  overflow: visible !important;
}

", root_sel, " .tax-lookup-wrap .dataTables_scroll {
  margin-bottom: 0;
  overflow: visible !important;
}

", root_sel, " .tax-lookup-wrap .dataTables_scrollHead {
  overflow: hidden !important;
}

", root_sel, " .tax-lookup-wrap .dataTables_scrollBody {
  border-bottom: 0 !important;
  overflow-x: auto !important;
  overflow-y: auto !important;
}

", root_sel, " .tax-lookup-wrap table.dataTable {
  margin-top: 0 !important;
  margin-bottom: 0 !important;
}
"
  )

  shiny::tagList(
    shiny::tags$style(shiny::HTML(css)),

    shiny::tags$div(
      id = ns("root"),

      bslib::card(
        bslib::card_header("Taxonomy match"),
        bslib::card_body(

          bslib::card(
            bslib::card_header("Notes"),
            bslib::card_body(
              shiny::tags$ul(
                shiny::tags$li(
                  shiny::tags$b("Taxonomic Matching"),
                  " — Matches are generated from the scientificName field."
                ),
                shiny::tags$li(
                  shiny::tags$b("Database Authorities"),
                  " — Marine taxa are validated against the World Register of Marine Species (worrms package, wm_records_taxamatch()). Terrestrial taxa are matched against the GBIF Backbone Taxonomy via the rgbif package."
                ),
                shiny::tags$li(
                  shiny::tags$b("Ambiguity Resolution"),
                  " — The results table contains one row per unique scientificName. When a match is uncertain, a manual selection is required to confirm the correct taxon."
                ),
                shiny::tags$li(
                  shiny::tags$b("Inclusion Rules"),
                  " — Only records with a confirmed match are included in the final dataset and passed to subsequent processing steps. Unresolved names are excluded from the output but retained in the validation log for review."
                ),
                shiny::tags$li(
                  shiny::tags$b("Workflow Restart"),
                  " — To process unresolved names, corrections must be made in the Identification Data Cleaning tab and the taxonomic matching step must be rerun."
                )
              )
            )
          ),

          shiny::br(),

          shiny::tags$div(
            class = "tax-top",
            bslib::layout_columns(
              col_widths = c(sm = 12, md = 6, lg = 4),
              gap = "1rem",

              bslib::card(
                class = "tax-minh tax-card-fill",
                bslib::card_header("Settings"),
                bslib::card_body(
                  shiny::uiOutput(ns("input_col_ui")),

                  shiny::radioButtons(
                    inputId = ns("tax_db"),
                    label = "Database",
                    choices = c(
                      "WoRMS (marine)" = "worms",
                      "GBIF Backbone (terrestrial)" = "gbif"
                    ),
                    selected = "worms",
                    inline = FALSE
                  ),

                  shiny::tags$div(
                    class = "tax-actions d-flex gap-2 flex-wrap",
                    shiny::actionButton(ns("run_match"), "Run match", class = "btn-primary")
                  ),

                  shiny::hr(),
                  shiny::uiOutput(ns("pkg_status"))
                )
              ),

              bslib::card(
                class = "tax-minh tax-card-fill",
                bslib::card_header("Columns to keep in final dataset"),
                bslib::card_body(
                  shiny::tags$div(
                    class = "tax-cols-wrap",
                    shiny::uiOutput(ns("keep_cols_ui"))
                  ),
                  shiny::tags$div(
                    class = "tax-scroll-note mt-2",
                    "Only Darwin Core output terms are available for selection. The ",
                    shiny::tags$strong("inputName"),
                    " and ",
                    shiny::tags$strong("taxonMatchStatus"),
                    " fields are strictly technical and are excluded from the final dataset. When using WoRMS, the ",
                    shiny::tags$strong("scientificNameID"),
                    " is populated with the identifier returned by the matching source (i.e., the LSID). Conversely, when the GBIF backbone is used, the ",
                    shiny::tags$strong("scientificNameID"),
                    " field is currently ignored (see: ",
                    shiny::tags$a(
                      href = "https://github.com/gbif/pipelines/issues/217",
                      target = "_blank",
                      rel = "noopener noreferrer",
                      "GBIF Issue #217"
                    ),
                    ")."
                  )
                )
              ),

              bslib::card(
                class = "tax-minh tax-card-fill",
                bslib::card_header("Summary"),
                bslib::card_body(
                  shiny::uiOutput(ns("summary_box"))
                )
              )
            )
          )
        )
      ),

      bslib::card(
        class = "tax-lookup-card",
        bslib::card_header("Name lookup table"),
        bslib::card_body(
          shiny::tags$div(
            class = "tax-lookup-wrap",
            DT::DTOutput(ns("lookup_tbl"))
          ),
          shiny::tags$div(
            class = "tax-actions-right",
            shiny::downloadButton(ns("download_lookup"), "Export lookup (CSV)"),
            shiny::uiOutput(ns("apply_btn_ui"), inline = TRUE)
          )
        )
      ),

      bslib::card(
        bslib::card_header("Issues (taxonomy)"),
        bslib::card_body(
          DT::DTOutput(ns("issues_tbl"))
        )
      )
    )
  )
}

#' Taxonomy match server
#' @param id module id
#' @param df_in reactive df after DwC mapping
#' @return list(df_out, lookup, issues, ready, dropped_log)
#' @export
mod_taxonomy_match_server <- function(id, df_in) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    `%||%` <- function(x, y) if (is.null(x)) y else x

    auto_apply_unique <- TRUE
    replace_scientificName <- FALSE

    empty_issues <- function() {
      data.frame(
        row = integer(),
        field = character(),
        rule = character(),
        severity = character(),
        message = character(),
        stringsAsFactors = FALSE
      )
    }

    lookup_base_cols <- c(
      "inputName",
      "normalisedQuery",
      "taxonMatchStatus",
      "selectedID",
      "nameAccordingTo"
    )

    worms_output_cols <- c(
      "AphiaID", "url", "scientificname", "authority", "status", "unacceptreason",
      "taxonRankID", "rank", "valid_AphiaID", "valid_name", "valid_authority",
      "parentNameUsageID", "originalNameUsageID", "kingdom", "phylum", "class",
      "order", "family", "genus", "citation", "lsid", "isMarine", "isBrackish",
      "isFreshwater", "isTerrestrial", "isExtinct", "match_type", "modified"
    )

    gbif_output_cols <- c(
      "usageKey", "acceptedUsageKey", "scientificName", "canonicalName", "rank",
      "status", "taxonomicStatus", "authorship", "confidence", "matchType",
      "synonym", "acceptedScientificName", "kingdom", "phylum", "class",
      "order", "family", "genus", "species", "is_alternative"
    )

    empty_lookup <- function(db = "worms") {
      out_cols <- if (identical(db, "worms")) worms_output_cols else gbif_output_cols

      as.data.frame(
        setNames(
          replicate(length(c(lookup_base_cols, out_cols)), character(), simplify = FALSE),
          c(lookup_base_cols, out_cols)
        ),
        stringsAsFactors = FALSE
      )
    }

    technical_cols <- c(
      "inputName",
      "normalisedQuery",
      "taxonMatchStatus",
      "selectedID",
      "nameAccordingTo"
    )

    final_match_cols_default <- c(
      "scientificName",
      "scientificNameID",
      "taxonID",
      "acceptedNameUsage",
      "acceptedNameUsageID",
      "taxonomicStatus",
      "taxonRank",
      "kingdom",
      "phylum",
      "family",
      "genus",
      "scientificNameAuthorship"
    )

    worms_keep_choices <- c(
      "scientificName",
      "scientificNameID",
      "taxonID",
      "acceptedNameUsage",
      "acceptedNameUsageID",
      "taxonomicStatus",
      "taxonRank",
      "scientificNameAuthorship",
      "parentNameUsageID",
      "originalNameUsageID",
      "kingdom",
      "phylum",
      "class",
      "order",
      "family",
      "genus"
    )

    gbif_keep_choices <- c(
      "scientificName",
      "acceptedNameUsage",
      "acceptedNameUsageID",
      "taxonID",
      "taxonomicStatus",
      "taxonRank",
      "scientificNameAuthorship",
      "kingdom",
      "phylum",
      "class",
      "order",
      "family",
      "genus",
      "specificEpithet"
    )

    worms_lookup_display_map <- c(
      "inputName" = "inputName",
      "taxonMatchStatus" = "taxonMatchStatus",
      "selectedID" = "selectedID",
      "AphiaID" = "taxonID",
      "scientificname" = "scientificName",
      "authority" = "scientificNameAuthorship",
      "status" = "taxonomicStatus",
      "rank" = "taxonRank",
      "valid_name" = "acceptedNameUsage",
      "valid_AphiaID" = "acceptedNameUsageID",
      "kingdom" = "kingdom",
      "phylum" = "phylum",
      "class" = "class",
      "order" = "order",
      "family" = "family",
      "genus" = "genus",
      "lsid" = "scientificNameID"
    )

    gbif_lookup_display_map <- c(
      "inputName" = "inputName",
      "taxonMatchStatus" = "taxonMatchStatus",
      "selectedID" = "selectedID",
      "usageKey" = "taxonID",
      "scientificName" = "scientificName",
      "canonicalName" = "canonicalName",
      "authorship" = "scientificNameAuthorship",
      "status" = "taxonomicStatus",
      "rank" = "taxonRank",
      "confidence" = "confidence",
      "matchType" = "matchType",
      "acceptedUsageKey" = "acceptedNameUsageID",
      "acceptedScientificName" = "acceptedNameUsage",
      "kingdom" = "kingdom",
      "phylum" = "phylum",
      "class" = "class",
      "order" = "order",
      "family" = "family",
      "genus" = "genus",
      "species" = "species"
    )

    rv <- shiny::reactiveValues(
      lookup = NULL,
      lookup_display = NULL,
      applied_df = NULL,
      issues = empty_issues(),
      cache = new.env(parent = emptyenv()),
      candidates_map = list(),
      suppress_choice_popup = FALSE,
      pending_choice = NULL,
      ready = FALSE,
      pending_apply = FALSE,
      dropped_log = .init_drop_log()
    )

    choice_observers <- list()

    add_issue <- function(idx, field, rule, severity, message) {
      rv$issues <- rbind(
        rv$issues,
        data.frame(
          row = if (length(idx) == 0 || is.null(idx) || is.na(idx)) {
            NA_integer_
          } else {
            as.integer(idx)
          },
          field = as.character(field),
          rule = as.character(rule),
          severity = as.character(severity),
          message = as.character(message),
          stringsAsFactors = FALSE
        )
      )
    }

    destroy_choice_observers <- function() {
      if (length(choice_observers) > 0) {
        for (obs in choice_observers) {
          if (!is.null(obs)) {
            try(obs$destroy(), silent = TRUE)
          }
        }
      }

      choice_observers <<- list()
      invisible(TRUE)
    }

    shiny::observeEvent(df_in(), {
      destroy_choice_observers()
      rv$lookup <- NULL
      rv$lookup_display <- NULL
      rv$applied_df <- NULL
      rv$issues <- empty_issues()
      rv$candidates_map <- list()
      rv$suppress_choice_popup <- FALSE
      rv$pending_choice <- NULL
      rv$ready <- FALSE
      rv$pending_apply <- FALSE
      rv$dropped_log <- .init_drop_log()
      rv$cache <- new.env(parent = emptyenv())
    }, ignoreInit = FALSE)

    has_needed_pkgs <- shiny::reactive({
      if (identical(input$tax_db, "worms")) {
        requireNamespace("worrms", quietly = TRUE)
      } else {
        requireNamespace("rgbif", quietly = TRUE)
      }
    })

    output$pkg_status <- shiny::renderUI({
      if (identical(input$tax_db, "worms")) {
        if (has_needed_pkgs()) {
          shiny::tags$div("Package available: worrms.")
        } else {
          shiny::tags$div("Package 'worrms' is required for WoRMS matching.")
        }
      } else {
        if (has_needed_pkgs()) {
          shiny::tags$div("Package available: rgbif.")
        } else {
          shiny::tags$div("Package 'rgbif' is required for GBIF matching.")
        }
      }
    })

    normalize_ws <- function(x) {
      x <- as.character(x)
      x[is.na(x)] <- ""
      x <- trimws(x)
      x <- gsub("\\s+", " ", x)
      x
    }

    is_blank <- function(x) {
      if (length(x) == 0 || is.null(x)) return(TRUE)

      x_chr <- as.character(x)

      if (length(x_chr) == 0) return(TRUE)
      if (all(is.na(x_chr))) return(TRUE)

      all(!nzchar(trimws(x_chr[!is.na(x_chr)])))
    }

    to_chr1 <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)

      x <- as.character(x[1])

      if (is.na(x) || identical(x, "NULL")) return(NA_character_)

      x
    }

    to_num1 <- function(x, default = NA_real_) {
      if (is.null(x) || length(x) == 0) return(default)

      out <- suppressWarnings(as.numeric(x[1]))

      if (is.na(out)) default else out
    }

    lower_norm <- function(x) {
      x <- normalize_ws(x)
      tolower(x)
    }

    normalize_query_inline <- function(x) {
      original <- as.character(x %||% "")
      query <- original

      query <- gsub("\\([^)]*\\)", " ", query, perl = TRUE)
      query <- gsub("\\[[^]]*\\]", " ", query, perl = TRUE)

      query <- gsub("\\bunknown\\s+sex\\b", " ", query, ignore.case = TRUE, perl = TRUE)

      query <- gsub(
        "\\b(juvenile|juveniles|adult|adults|larva|larvae|egg|eggs|male|female|alive|dead|indet\\.?|msp\\.?|sp\\.?|cf\\.?|aff\\.?|morphotype)\\b",
        " ",
        query,
        ignore.case = TRUE,
        perl = TRUE
      )

      query <- gsub("\\b[0-9]+\\b", " ", query, perl = TRUE)
      query <- gsub("[,;:]", " ", query, perl = TRUE)
      query <- gsub("\\s+", " ", query, perl = TRUE)
      query <- trimws(query)

      if (nzchar(query)) {
        toks <- unlist(strsplit(query, "\\s+", perl = TRUE), use.names = FALSE)
        if (length(toks) > 2) {
          query <- paste(toks[1:2], collapse = " ")
        }
      }

      changed <- !identical(normalize_ws(original), normalize_ws(query))
      rule <- if (changed) "normalised_query_cleanup" else "normalised_query_unchanged"

      list(
        query = query,
        changed = changed,
        rule = rule
      )
    }

    current_input_col <- shiny::reactive({
      df <- df_in()
      shiny::req(is.data.frame(df))

      cols <- names(df)

      if ("scientificName" %in% cols) return("scientificName")
      if (length(cols) > 0) return(cols[1])

      NULL
    })

    output$input_col_ui <- shiny::renderUI({
      df <- df_in()
      shiny::req(is.data.frame(df))

      cols <- names(df)

      if (!("scientificName" %in% cols)) {
        return(
          shiny::tags$div(
            class = "alert alert-warning",
            "The current dataset does not contain a scientificName column. Taxonomy matching requires scientificName at this stage."
          )
        )
      }

      shiny::tagList(
        shiny::selectInput(
          inputId = ns("input_col"),
          label = "Input column for matching",
          choices = c("scientificName"),
          selected = "scientificName"
        ),
        shiny::div(
          class = "text-muted small mt-1",
          "This step always uses scientificName, because it should already have been reviewed in Identification Data Cleaning."
        )
      )
    })

    output$apply_btn_ui <- shiny::renderUI({
      enabled <- !is.null(rv$lookup) &&
        is.data.frame(rv$lookup) &&
        nrow(rv$lookup) > 0

      shiny::actionButton(
        ns("apply_match"),
        "Apply to dataset",
        class = "btn-success",
        disabled = if (!enabled) "disabled" else NULL
      )
    })

    output$keep_cols_ui <- shiny::renderUI({
      choices <- if (identical(input$tax_db, "worms")) {
        worms_keep_choices
      } else {
        gbif_keep_choices
      }

      selected <- if (identical(input$tax_db, "worms")) {
        unique(c(
          "scientificName",
          "scientificNameID",
          intersect(final_match_cols_default, unname(choices))
        ))
      } else {
        unique(c(
          "scientificName",
          intersect(final_match_cols_default, unname(choices))
        ))
      }

      shiny::tagList(
        shiny::checkboxGroupInput(
          inputId = ns("keep_output_cols"),
          label = NULL,
          choices = choices,
          selected = selected
        ),
        shiny::div(
          class = "text-muted small",
          if (identical(input$tax_db, "worms")) {
            shiny::tagList(
              shiny::tags$strong("scientificName"),
              " and ",
              shiny::tags$strong("scientificNameID"),
              " are always kept."
            )
          } else {
            shiny::tagList(
              shiny::tags$strong("scientificName"),
              " is always kept."
            )
          }
        )
      )
    })

    shiny::observeEvent(input$keep_output_cols, {
      mandatory <- if (identical(input$tax_db, "worms")) {
        c("scientificName", "scientificNameID")
      } else {
        c("scientificName")
      }

      current_sel <- input$keep_output_cols %||% character(0)
      sel <- unique(c(current_sel, mandatory))

      if (!identical(sort(sel), sort(current_sel))) {
        shiny::updateCheckboxGroupInput(
          session,
          "keep_output_cols",
          selected = sel
        )
      }
    }, ignoreInit = TRUE)

    base_names <- shiny::reactive({
      df <- df_in()
      shiny::req(is.data.frame(df))

      src <- current_input_col()
      shiny::req(!is.null(src), src %in% names(df))

      x <- normalize_ws(df[[src]])
      x <- x[x != ""]
      sort(unique(x))
    })

    safe_col <- function(df, nm, default = NA_character_) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || !(nm %in% names(df))) {
        return(rep(default, if (is.null(df) || !is.data.frame(df)) 0 else nrow(df)))
      }

      as.character(df[[nm]])
    }

    logicalish_to_chr <- function(x) {
      out <- as.character(x)
      out[is.na(x)] <- NA_character_
      out
    }

    tax_status_group <- function(x) {
      x <- toupper(normalize_ws(x))
      out <- x
      out[grepl("^MATCHED", x)] <- "MATCHED"
      out
    }

    cache_key <- function(db, query, auto_apply_unique) {
      paste(db, as.character(auto_apply_unique), query, sep = "||")
    }

    cache_get <- function(key) {
      if (exists(key, envir = rv$cache, inherits = FALSE)) {
        get(key, envir = rv$cache, inherits = FALSE)
      } else {
        NULL
      }
    }

    cache_set <- function(key, value) {
      assign(key, value, envir = rv$cache)
      invisible(TRUE)
    }

    normalize_worms_candidates <- function(x) {
      df <- NULL

      if (is.null(x)) {
        return(data.frame(stringsAsFactors = FALSE))
      }

      if (is.data.frame(x)) {
        df <- x
      } else if (is.list(x) && length(x) >= 1) {
        df <- x[[1]]
      }

      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(data.frame(stringsAsFactors = FALSE))
      }

      out <- data.frame(
        AphiaID = safe_col(df, "AphiaID"),
        url = safe_col(df, "url"),
        scientificname = safe_col(df, "scientificname"),
        authority = safe_col(df, "authority"),
        status = safe_col(df, "status"),
        unacceptreason = safe_col(df, "unacceptreason"),
        taxonRankID = safe_col(df, "taxonRankID"),
        rank = safe_col(df, "rank"),
        valid_AphiaID = safe_col(df, "valid_AphiaID"),
        valid_name = safe_col(df, "valid_name"),
        valid_authority = safe_col(df, "valid_authority"),
        parentNameUsageID = safe_col(df, "parentNameUsageID"),
        originalNameUsageID = safe_col(df, "originalNameUsageID"),
        kingdom = safe_col(df, "kingdom"),
        phylum = safe_col(df, "phylum"),
        class = safe_col(df, "class"),
        order = safe_col(df, "order"),
        family = safe_col(df, "family"),
        genus = safe_col(df, "genus"),
        citation = safe_col(df, "citation"),
        lsid = safe_col(df, "lsid"),
        isMarine = if ("isMarine" %in% names(df)) logicalish_to_chr(df$isMarine) else rep(NA_character_, nrow(df)),
        isBrackish = if ("isBrackish" %in% names(df)) logicalish_to_chr(df$isBrackish) else rep(NA_character_, nrow(df)),
        isFreshwater = if ("isFreshwater" %in% names(df)) logicalish_to_chr(df$isFreshwater) else rep(NA_character_, nrow(df)),
        isTerrestrial = if ("isTerrestrial" %in% names(df)) logicalish_to_chr(df$isTerrestrial) else rep(NA_character_, nrow(df)),
        isExtinct = if ("isExtinct" %in% names(df)) logicalish_to_chr(df$isExtinct) else rep(NA_character_, nrow(df)),
        match_type = safe_col(df, "match_type"),
        modified = safe_col(df, "modified"),
        stringsAsFactors = FALSE
      )

      out <- out[!is.na(out$AphiaID) & out$AphiaID != "", , drop = FALSE]
      out <- out[!duplicated(out$AphiaID), , drop = FALSE]

      out
    }

    worms_taxamatch_candidates <- function(query) {
      res <- tryCatch(
        worrms::wm_records_taxamatch(name = query),
        error = function(e) NULL
      )

      out <- normalize_worms_candidates(res)

      if (!is.data.frame(out) || nrow(out) == 0) {
        return(data.frame(stringsAsFactors = FALSE))
      }

      qn <- lower_norm(query)
      sci <- lower_norm(out$scientificname)
      vnm <- lower_norm(out$valid_name)

      score <- integer(nrow(out))
      score[sci == qn] <- score[sci == qn] + 5L
      score[vnm == qn] <- score[vnm == qn] + 4L
      score[toupper(out$status) == "ACCEPTED"] <- score[toupper(out$status) == "ACCEPTED"] + 2L
      score[toupper(out$match_type) == "EXACT"] <- score[toupper(out$match_type) == "EXACT"] + 2L

      ord <- order(-score, sci, vnm)
      out[ord, , drop = FALSE]
    }

    worms_find_candidate_row <- function(input_name, selected_id) {
      cand <- rv$candidates_map[[input_name]]

      if (is.null(cand) || !is.data.frame(cand) || nrow(cand) == 0) return(NULL)
      if (!("AphiaID" %in% names(cand))) return(NULL)

      idx <- which(as.character(cand$AphiaID) == as.character(selected_id))

      if (length(idx) == 0) return(NULL)

      cand[idx[1], , drop = FALSE]
    }

    match_one_worms <- function(input_name, normalised_query, auto_apply_unique = TRUE) {
      key <- cache_key("worms", normalised_query, auto_apply_unique)
      cached <- cache_get(key)

      if (!is.null(cached)) {
        cached$row$inputName <- input_name
        cached$row$normalisedQuery <- normalised_query
        return(cached)
      }

      blank_row <- empty_lookup("worms")[rep(1, 1), , drop = FALSE]
      blank_row$inputName <- input_name
      blank_row$normalisedQuery <- normalised_query
      blank_row$nameAccordingTo <- "WoRMS"

      if (is_blank(normalised_query)) {
        blank_row$taxonMatchStatus <- "UNRESOLVABLE"
        res <- list(row = blank_row, candidates = NULL)
        cache_set(key, res)
        return(res)
      }

      cand <- worms_taxamatch_candidates(normalised_query)

      if (!is.data.frame(cand) || nrow(cand) == 0) {
        blank_row$taxonMatchStatus <- "NOT_FOUND"
        res <- list(row = blank_row, candidates = NULL)
        cache_set(key, res)
        return(res)
      }

      top1 <- cand[1, , drop = FALSE]
      status_out <- "AMBIGUOUS"
      selected_id <- NA_character_

      if (isTRUE(auto_apply_unique) && nrow(cand) == 1) {
        status_out <- "MATCHED_AUTO"
        selected_id <- to_chr1(top1$AphiaID)
      }

      row <- blank_row
      row$taxonMatchStatus <- status_out
      row$selectedID <- selected_id

      for (nm in worms_output_cols) {
        if (nm %in% names(top1)) {
          row[[nm]] <- as.character(top1[[nm]][1])
        }
      }

      res <- list(
        row = row,
        candidates = cand
      )

      cache_set(key, res)
      res
    }

    normalize_gbif_candidates <- function(x) {
      df <- NULL

      if (is.null(x)) {
        return(data.frame(stringsAsFactors = FALSE))
      }

      if (is.data.frame(x)) {
        df <- x
      } else if (inherits(x, "tbl_df") || inherits(x, "tbl")) {
        df <- as.data.frame(x, stringsAsFactors = FALSE)
      } else if (is.list(x) && !is.null(x$data) && is.data.frame(x$data)) {
        df <- x$data
      } else if (is.list(x) && length(x) >= 1 && is.data.frame(x[[1]])) {
        df <- x[[1]]
      } else {
        df <- tryCatch(
          as.data.frame(x, stringsAsFactors = FALSE),
          error = function(e) NULL
        )
      }

      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(data.frame(stringsAsFactors = FALSE))
      }

      out <- data.frame(
        usageKey = safe_col(df, "usageKey"),
        acceptedUsageKey = safe_col(df, "acceptedUsageKey"),
        scientificName = safe_col(df, "scientificName"),
        canonicalName = safe_col(df, "canonicalName"),
        rank = safe_col(df, "rank"),
        status = {
          st <- safe_col(df, "status")
          if (all(is.na(st) | !nzchar(trimws(st)))) {
            safe_col(df, "taxonomicStatus")
          } else {
            st
          }
        },
        taxonomicStatus = safe_col(df, "taxonomicStatus"),
        authorship = safe_col(df, "authorship"),
        confidence = safe_col(df, "confidence"),
        matchType = safe_col(df, "matchType"),
        synonym = safe_col(df, "synonym"),
        acceptedScientificName = {
          out1 <- safe_col(df, "acceptedScientificName")
          if (all(is.na(out1) | !nzchar(trimws(out1)))) {
            out2 <- safe_col(df, "accepted")
            if (length(out2) > 0) out2 else out1
          } else {
            out1
          }
        },
        kingdom = safe_col(df, "kingdom"),
        phylum = safe_col(df, "phylum"),
        class = safe_col(df, "class"),
        order = safe_col(df, "order"),
        family = safe_col(df, "family"),
        genus = safe_col(df, "genus"),
        species = safe_col(df, "species"),
        is_alternative = {
          alt <- safe_col(df, "is_alternative")
          if (length(alt) == 0 || all(is.na(alt) | !nzchar(trimws(alt)))) {
            rep(NA_character_, nrow(df))
          } else {
            alt
          }
        },
        stringsAsFactors = FALSE
      )

      usable_key <- !is.na(out$usageKey) & nzchar(trimws(out$usageKey))
      usable_match <- toupper(normalize_ws(out$matchType)) != "NONE"
      out <- out[usable_key | usable_match, , drop = FALSE]

      if (!is.data.frame(out) || nrow(out) == 0) {
        return(data.frame(stringsAsFactors = FALSE))
      }

      out$usageKey <- as.character(out$usageKey)
      out$acceptedUsageKey <- as.character(out$acceptedUsageKey)

      out$scientificName[!nzchar(trimws(out$scientificName))] <-
        out$canonicalName[!nzchar(trimws(out$scientificName))]

      out$acceptedScientificName[!nzchar(trimws(out$acceptedScientificName))] <-
        out$scientificName[!nzchar(trimws(out$acceptedScientificName))]

      out <- out[!duplicated(out$usageKey), , drop = FALSE]
      out
    }

    gbif_candidates <- function(query) {
      res <- tryCatch(
        rgbif::name_backbone(name = query, verbose = TRUE),
        error = function(e) NULL
      )

      out <- normalize_gbif_candidates(res)

      if (!is.data.frame(out) || nrow(out) == 0) {
        return(data.frame(stringsAsFactors = FALSE))
      }

      sci <- lower_norm(out$scientificName)
      can <- lower_norm(out$canonicalName)
      qn <- lower_norm(query)

      score <- integer(nrow(out))
      score[sci == qn] <- score[sci == qn] + 5L
      score[can == qn] <- score[can == qn] + 4L

      mt <- toupper(normalize_ws(out$matchType))
      score[mt == "EXACT"] <- score[mt == "EXACT"] + 3L
      score[mt == "HIGHERRANK"] <- score[mt == "HIGHERRANK"] + 1L
      score[mt == "FUZZY"] <- score[mt == "FUZZY"] + 1L

      conf <- suppressWarnings(as.numeric(out$confidence))
      conf[is.na(conf)] <- 0

      alt <- tolower(normalize_ws(out$is_alternative))
      alt_penalty <- ifelse(alt %in% c("true", "t", "1", "yes"), 1L, 0L)

      ord <- order(alt_penalty, -score, -conf, sci, can)
      out[ord, , drop = FALSE]
    }

    gbif_find_candidate_row <- function(input_name, selected_id) {
      cand <- rv$candidates_map[[input_name]]

      if (is.null(cand) || !is.data.frame(cand) || nrow(cand) == 0) return(NULL)
      if (!("usageKey" %in% names(cand))) return(NULL)

      idx <- which(as.character(cand$usageKey) == as.character(selected_id))

      if (length(idx) == 0) return(NULL)

      cand[idx[1], , drop = FALSE]
    }

    match_one_gbif <- function(input_name, normalised_query, auto_apply_unique = TRUE) {
      key <- cache_key("gbif", normalised_query, auto_apply_unique)
      cached <- cache_get(key)

      if (!is.null(cached)) {
        cached$row$inputName <- input_name
        cached$row$normalisedQuery <- normalised_query
        return(cached)
      }

      blank_row <- empty_lookup("gbif")[rep(1, 1), , drop = FALSE]
      blank_row$inputName <- input_name
      blank_row$normalisedQuery <- normalised_query
      blank_row$nameAccordingTo <- "GBIF Backbone"

      if (is_blank(normalised_query)) {
        blank_row$taxonMatchStatus <- "UNRESOLVABLE"
        res <- list(row = blank_row, candidates = NULL)
        cache_set(key, res)
        return(res)
      }

      cand <- gbif_candidates(normalised_query)

      if (!is.data.frame(cand) || nrow(cand) == 0) {
        blank_row$taxonMatchStatus <- "NOT_FOUND"
        res <- list(row = blank_row, candidates = NULL)
        cache_set(key, res)
        return(res)
      }

      top1 <- cand[1, , drop = FALSE]
      top_usage <- to_chr1(top1$usageKey)
      top_conf <- to_num1(top1$confidence, default = 0)
      top_match_type <- toupper(normalize_ws(top1$matchType))

      if (is_blank(top_usage) || identical(top_match_type, "NONE")) {
        blank_row$taxonMatchStatus <- "NOT_FOUND"
        res <- list(row = blank_row, candidates = cand)
        cache_set(key, res)
        return(res)
      }

      status_out <- if (!is.na(top_conf) && top_conf >= 90) {
        "MATCHED_AUTO"
      } else {
        "AMBIGUOUS"
      }

      selected_id <- if (identical(status_out, "MATCHED_AUTO")) {
        top_usage
      } else {
        NA_character_
      }

      row <- blank_row
      row$taxonMatchStatus <- status_out
      row$selectedID <- selected_id

      for (nm in gbif_output_cols) {
        if (nm %in% names(top1)) {
          row[[nm]] <- as.character(top1[[nm]][1])
        }
      }

      res <- list(
        row = row,
        candidates = cand
      )

      cache_set(key, res)
      res
    }

    resolve_row_worms <- function(i, selected_id) {
      if (is.null(rv$lookup) || nrow(rv$lookup) < i) return(invisible(FALSE))
      if (is_blank(selected_id)) return(invisible(FALSE))

      cand_row <- worms_find_candidate_row(rv$lookup$inputName[i], selected_id)

      if (is.null(cand_row) || !is.data.frame(cand_row) || nrow(cand_row) == 0) {
        return(invisible(FALSE))
      }

      rv$lookup$selectedID[i] <- as.character(selected_id)
      rv$lookup$taxonMatchStatus[i] <- "MATCHED_MANUAL"

      for (nm in worms_output_cols) {
        if (nm %in% names(cand_row)) {
          rv$lookup[[nm]][i] <- as.character(cand_row[[nm]][1])
        }
      }

      invisible(TRUE)
    }

    resolve_row_gbif <- function(i, selected_id) {
      if (is.null(rv$lookup) || nrow(rv$lookup) < i) return(invisible(FALSE))
      if (is_blank(selected_id)) return(invisible(FALSE))

      cand_row <- gbif_find_candidate_row(rv$lookup$inputName[i], selected_id)

      if (is.null(cand_row) || !is.data.frame(cand_row) || nrow(cand_row) == 0) {
        return(invisible(FALSE))
      }

      rv$lookup$selectedID[i] <- as.character(selected_id)
      rv$lookup$taxonMatchStatus[i] <- "MATCHED_MANUAL"

      for (nm in gbif_output_cols) {
        if (nm %in% names(cand_row)) {
          rv$lookup[[nm]][i] <- as.character(cand_row[[nm]][1])
        }
      }

      invisible(TRUE)
    }

    apply_choice_to_rows <- function(row_ids, selected_id) {
      if (length(row_ids) == 0) return(invisible(FALSE))

      if (identical(input$tax_db, "worms")) {
        for (ii in row_ids) resolve_row_worms(ii, selected_id)
      } else {
        for (ii in row_ids) resolve_row_gbif(ii, selected_id)
      }

      rv$lookup_display <- build_lookup_display(rv$lookup)
      rv$applied_df <- NULL
      rv$ready <- FALSE

      invisible(TRUE)
    }

    build_lookup_display <- function(lookup) {
      if (is.null(lookup) || !is.data.frame(lookup) || nrow(lookup) == 0) {
        return(data.frame(stringsAsFactors = FALSE))
      }

      keep_display <- if (identical(input$tax_db, "worms")) {
        c(
          "inputName", "taxonMatchStatus", "selectedID",
          "AphiaID", "scientificname",
          "authority", "status", "rank", "valid_name", "valid_AphiaID",
          "kingdom", "phylum", "class", "order", "family", "genus", "lsid"
        )
      } else {
        c(
          "inputName", "taxonMatchStatus", "selectedID",
          "usageKey", "scientificName", "canonicalName",
          "authorship", "status", "rank", "confidence", "matchType",
          "acceptedUsageKey", "acceptedScientificName",
          "kingdom", "phylum", "class", "order", "family", "genus", "species"
        )
      }

      disp <- lookup[, intersect(keep_display, names(lookup)), drop = FALSE]

      if (!("selectedID" %in% names(disp))) {
        disp$selectedID <- rep(NA_character_, nrow(disp))
      }

      disp$choice <- ""

      for (i in seq_len(nrow(disp))) {
        status_i <- as.character(disp$taxonMatchStatus[i] %||% "")

        if (!identical(status_i, "AMBIGUOUS")) next

        nm <- disp$inputName[i]
        cand <- rv$candidates_map[[nm]]

        if (is.null(cand) || !is.data.frame(cand) || nrow(cand) == 0) next

        if (identical(input$tax_db, "worms")) {
          ids <- as.character(cand$AphiaID)
          sci <- safe_col(cand, "scientificname", "")
          rk <- safe_col(cand, "rank", "")
          au <- safe_col(cand, "authority", "")
          st <- safe_col(cand, "status", "")
          vn <- safe_col(cand, "valid_name", "")
          mt <- safe_col(cand, "match_type", "")

          keep_id <- !is.na(ids) & nzchar(trimws(ids))
          ids <- ids[keep_id]
          sci <- sci[keep_id]
          rk <- rk[keep_id]
          au <- au[keep_id]
          st <- st[keep_id]
          vn <- vn[keep_id]
          mt <- mt[keep_id]

          if (length(ids) == 0) next

          lab <- paste0(
            ids, " — ",
            sci,
            ifelse(rk != "", paste0(" [", rk, "]"), ""),
            ifelse(st != "", paste0(" (", st, ")"), ""),
            ifelse(vn != "", paste0(" | valid: ", vn), ""),
            ifelse(mt != "", paste0(" <", mt, ">"), ""),
            ifelse(au != "", paste0(" — ", au), "")
          )
        } else {
          ids <- as.character(cand$usageKey)
          sci <- safe_col(cand, "scientificName", "")
          can <- safe_col(cand, "canonicalName", "")
          rk <- safe_col(cand, "rank", "")
          au <- safe_col(cand, "authorship", "")
          st <- safe_col(cand, "status", "")
          mt <- safe_col(cand, "matchType", "")
          cf <- safe_col(cand, "confidence", "")
          ac <- safe_col(cand, "acceptedScientificName", "")

          keep_id <- !is.na(ids) & nzchar(trimws(ids))
          ids <- ids[keep_id]
          sci <- sci[keep_id]
          can <- can[keep_id]
          rk <- rk[keep_id]
          au <- au[keep_id]
          st <- st[keep_id]
          mt <- mt[keep_id]
          cf <- cf[keep_id]
          ac <- ac[keep_id]

          if (length(ids) == 0) next

          primary_nm <- ifelse(sci != "", sci, can)

          lab <- paste0(
            ids, " — ",
            primary_nm,
            ifelse(rk != "", paste0(" [", rk, "]"), ""),
            ifelse(st != "", paste0(" (", st, ")"), ""),
            ifelse(cf != "", paste0(" {conf=", cf, "}"), ""),
            ifelse(mt != "", paste0(" <", mt, ">"), ""),
            ifelse(ac != "" & ac != primary_nm, paste0(" | accepted: ", ac), ""),
            ifelse(au != "", paste0(" — ", au), "")
          )
        }

        choices <- c("-- select candidate --" = "", stats::setNames(ids, lab))
        input_id <- ns(paste0("cand_", i))
        selected_val <- as.character(disp$selectedID[i])

        if (is.na(selected_val) || !nzchar(trimws(selected_val))) {
          selected_val <- ""
        }

        disp$choice[i] <- as.character(
          shiny::selectInput(
            inputId = input_id,
            label = NULL,
            choices = choices,
            selected = selected_val,
            width = "100%"
          )
        )
      }

      selected_chr <- as.character(disp$selectedID)
      selected_chr[is.na(selected_chr)] <- ""

      status_chr <- as.character(disp$taxonMatchStatus)
      status_chr[is.na(status_chr)] <- ""

      disp$.choice_order <- 3L
      disp$.choice_order[
        status_chr == "AMBIGUOUS" & selected_chr == ""
      ] <- 0L
      disp$.choice_order[
        status_chr == "AMBIGUOUS" & selected_chr != ""
      ] <- 1L
      disp$.choice_order[grepl("^MATCHED", status_chr)] <- 2L

      ord <- order(disp$.choice_order, as.character(disp$inputName))
      disp <- disp[ord, , drop = FALSE]

      disp$.choice_order <- NULL
      disp$selectedID <- NULL

      rename_map <- if (identical(input$tax_db, "worms")) {
        worms_lookup_display_map
      } else {
        gbif_lookup_display_map
      }

      common_cols <- intersect(names(rename_map), names(disp))
      names(disp)[match(common_cols, names(disp))] <- unname(rename_map[common_cols])

      disp
    }

    output$summary_box <- shiny::renderUI({
      nms <- base_names()
      src <- current_input_col()

      if (length(nms) == 0) {
        return(shiny::tags$div("No names available in the selected input column."))
      }

      lk <- rv$lookup

      if (is.null(lk) || !is.data.frame(lk) || nrow(lk) == 0) {
        unres <- vapply(
          nms,
          function(x) {
            query <- normalize_query_inline(x)$query
            is_blank(query)
          },
          logical(1)
        )

        return(
          shiny::tags$div(
            shiny::tags$p(paste0("Input column: ", src)),
            shiny::tags$p(paste0("Unique names: ", length(nms))),
            shiny::tags$p(paste0("UNRESOLVABLE (pre-match): ", sum(unres))),
            shiny::tags$p("MATCHED: 0 | AMBIGUOUS: 0 | NOT_FOUND: 0"),
            shiny::tags$p("Changes applied: NO"),
            shiny::tags$p(paste0("Dropped rows logged: ", nrow(rv$dropped_log)))
          )
        )
      }

      stat_grp <- tax_status_group(lk$taxonMatchStatus)
      n_matched <- sum(stat_grp == "MATCHED", na.rm = TRUE)
      n_amb <- sum(lk$taxonMatchStatus == "AMBIGUOUS", na.rm = TRUE)
      n_nf <- sum(lk$taxonMatchStatus == "NOT_FOUND", na.rm = TRUE)
      n_unres <- sum(lk$taxonMatchStatus == "UNRESOLVABLE", na.rm = TRUE)

      nf_vals <- sort(unique(lk$inputName[lk$taxonMatchStatus == "NOT_FOUND"]))
      nf_vals <- nf_vals[!is.na(nf_vals) & nzchar(nf_vals)]

      shiny::tags$div(
        shiny::tags$p(paste0("Input column: ", src)),
        shiny::tags$p(paste0("Unique names: ", length(nms))),
        shiny::tags$p(paste0("UNRESOLVABLE: ", n_unres)),
        shiny::tags$p(paste0("MATCHED: ", n_matched, " | AMBIGUOUS: ", n_amb, " | NOT_FOUND: ", n_nf)),
        if (length(nf_vals) > 0) {
          shiny::tagList(
            shiny::tags$p("Not Found values:"),
            shiny::tags$ul(
              class = "tax-summary-list",
              lapply(nf_vals, shiny::tags$li)
            )
          )
        },
        shiny::tags$p(
          paste0(
            "Changes applied: ",
            if (!is.null(rv$applied_df) && is.data.frame(rv$applied_df)) "YES" else "NO"
          )
        ),
        shiny::tags$p(paste0("Dropped rows logged: ", nrow(rv$dropped_log)))
      )
    })

    count_observations_for_inputName <- function(nm) {
      df <- df_in()
      src <- current_input_col()

      if (!is.data.frame(df) || is.null(src) || !(src %in% names(df))) {
        return(NA_integer_)
      }

      src_vals <- normalize_ws(df[[src]])
      sum(src_vals == normalize_ws(nm), na.rm = TRUE)
    }

    open_choice_popup <- function(row_index, selected_id) {
      shiny::req(!is.null(rv$lookup), row_index >= 1, row_index <= nrow(rv$lookup))

      nm <- rv$lookup$inputName[row_index]

      df <- df_in()
      src <- current_input_col()

      obs_count <- NA_integer_
      other_obs_count <- NA_integer_

      if (is.data.frame(df) && !is.null(src) && src %in% names(df)) {
        src_vals <- normalize_ws(df[[src]])
        obs_count <- sum(src_vals == normalize_ws(nm), na.rm = TRUE)
        other_obs_count <- max(0L, obs_count - 1L)
      }

      candidate_row <- NULL
      candidate_taxon <- NA_character_
      candidate_authorship <- NA_character_
      candidate_rank <- NA_character_
      candidate_tree <- character(0)

      if (identical(input$tax_db, "worms")) {
        candidate_row <- worms_find_candidate_row(nm, selected_id)

        if (!is.null(candidate_row) && is.data.frame(candidate_row) && nrow(candidate_row) > 0) {
          candidate_taxon <- to_chr1(candidate_row$scientificname)
          candidate_authorship <- to_chr1(candidate_row$authority)
          candidate_rank <- to_chr1(candidate_row$rank)
          candidate_tree <- c(
            kingdom = to_chr1(candidate_row$kingdom),
            phylum = to_chr1(candidate_row$phylum),
            class = to_chr1(candidate_row$class),
            order = to_chr1(candidate_row$order),
            family = to_chr1(candidate_row$family),
            genus = to_chr1(candidate_row$genus)
          )
        }
      } else {
        candidate_row <- gbif_find_candidate_row(nm, selected_id)

        if (!is.null(candidate_row) && is.data.frame(candidate_row) && nrow(candidate_row) > 0) {
          candidate_taxon <- to_chr1(candidate_row$scientificName)
          candidate_authorship <- to_chr1(candidate_row$authorship)
          candidate_rank <- to_chr1(candidate_row$rank)
          candidate_tree <- c(
            kingdom = to_chr1(candidate_row$kingdom),
            phylum = to_chr1(candidate_row$phylum),
            class = to_chr1(candidate_row$class),
            order = to_chr1(candidate_row$order),
            family = to_chr1(candidate_row$family),
            genus = to_chr1(candidate_row$genus),
            species = to_chr1(candidate_row$species)
          )
        }
      }

      candidate_tree <- candidate_tree[
        !is.na(candidate_tree) & nzchar(trimws(candidate_tree))
      ]

      candidate_tree_text <- if (length(candidate_tree) > 0) {
        paste(paste(names(candidate_tree), candidate_tree, sep = ": "), collapse = " > ")
      } else {
        "Not available"
      }

      candidate_label <- paste0(
        ifelse(is_blank(candidate_taxon), "Selected taxon", candidate_taxon),
        ifelse(is_blank(candidate_authorship), "", paste0(" ", candidate_authorship)),
        ifelse(is_blank(candidate_rank), "", paste0(" [", candidate_rank, "]"))
      )

      other_rows_message <- if (!is.na(obs_count)) {
        paste0(
          "This scientificName input value ('", nm, "') occurs in ",
          obs_count,
          " observation(s) in the dataset",
          if (other_obs_count > 0) {
            paste0(" (", other_obs_count, " other entr", ifelse(other_obs_count == 1, "y", "ies"), ").")
          } else {
            "."
          }
        )
      } else {
        paste0(
          "Could not determine how many dataset observations share scientificName input value ('",
          nm,
          "')."
        )
      }

      shiny::showModal(
        shiny::modalDialog(
          title = "Confirm taxonomic choice",
          shiny::tags$p(paste0("You selected a candidate for '", nm, "'.")),
          shiny::tags$p("Please confirm the selected option below. This will mark all matching observations as MATCHED_MANUAL."),
          shiny::tags$div(
            class = "tax-warning-box",
            shiny::tags$div(class = "tax-warning-title", "Selected option"),
            shiny::tags$p(shiny::tags$strong("Taxon: "), candidate_label),
            shiny::tags$p(shiny::tags$strong("Taxonomic hierarchy: "), candidate_tree_text)
          ),
          shiny::tags$p(other_rows_message),
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("choice_apply_same"),
              "Apply to all observations sharing the same scientificName input value",
              class = "btn-success"
            )
          )
        )
      )
    }

    unresolved_apply_summary <- function() {
      df <- df_in()
      shiny::req(is.data.frame(df))

      src <- current_input_col()
      shiny::req(!is.null(src), src %in% names(df))

      lk <- rv$lookup

      if (is.null(lk) || !is.data.frame(lk) || nrow(lk) == 0) {
        return(NULL)
      }

      src_vals <- normalize_ws(df[[src]])

      unresolved_amb <- lk$taxonMatchStatus == "AMBIGUOUS" &
        (is.na(lk$selectedID) | lk$selectedID == "")

      idx_nf <- which(lk$taxonMatchStatus == "NOT_FOUND")
      idx_unres <- which(lk$taxonMatchStatus == "UNRESOLVABLE")
      idx_amb <- which(unresolved_amb)
      idx_all <- sort(unique(c(idx_nf, idx_unres, idx_amb)))

      if (length(idx_all) == 0) {
        return(NULL)
      }

      names_nf <- sort(unique(lk$inputName[idx_nf]))
      names_unres <- sort(unique(lk$inputName[idx_unres]))
      names_amb <- sort(unique(lk$inputName[idx_amb]))
      names_all <- sort(unique(lk$inputName[idx_all]))

      counts_by_name <- table(src_vals[src_vals %in% names_all])

      obs_nf <- sum(counts_by_name[names(counts_by_name) %in% names_nf], na.rm = TRUE)
      obs_unres <- sum(counts_by_name[names(counts_by_name) %in% names_unres], na.rm = TRUE)
      obs_amb <- sum(counts_by_name[names(counts_by_name) %in% names_amb], na.rm = TRUE)
      obs_all <- sum(counts_by_name, na.rm = TRUE)

      list(
        idx_all = idx_all,
        names_nf = names_nf,
        names_unres = names_unres,
        names_amb = names_amb,
        names_all = names_all,
        n_lookup_nf = length(names_nf),
        n_lookup_unres = length(names_unres),
        n_lookup_amb = length(names_amb),
        n_obs_nf = as.integer(obs_nf),
        n_obs_unres = as.integer(obs_unres),
        n_obs_amb = as.integer(obs_amb),
        n_obs_all = as.integer(obs_all)
      )
    }

    limited_name_list_ui <- function(x, limit = 20) {
      x <- x[!is.na(x) & nzchar(x)]

      if (length(x) == 0) return(NULL)

      shown <- utils::head(x, limit)

      shiny::tagList(
        shiny::tags$ul(
          class = "tax-warning-list",
          lapply(shown, shiny::tags$li)
        ),
        if (length(x) > limit) {
          shiny::tags$div(
            class = "text-muted small",
            paste0("... and ", length(x) - limit, " more.")
          )
        }
      )
    }

    open_apply_warning_modal <- function(info) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Warning before applying taxonomy results",
          shiny::tags$div(
            class = "tax-warning-box",
            shiny::tags$div(
              class = "tax-warning-title",
              "Some observations do not have a resolved taxonomy match and will be discarded."
            ),
            shiny::tags$p(
              "If you need to keep these observations, correct the original file and restart the workflow."
            ),
            shiny::tags$ul(
              shiny::tags$li(paste0("NOT_FOUND names: ", info$n_lookup_nf, " | affected observations: ", info$n_obs_nf)),
              shiny::tags$li(paste0("UNRESOLVABLE names: ", info$n_lookup_unres, " | affected observations: ", info$n_obs_unres)),
              shiny::tags$li(paste0("AMBIGUOUS names without a selected choice: ", info$n_lookup_amb, " | affected observations: ", info$n_obs_amb)),
              shiny::tags$li(paste0("Total observations to be discarded: ", info$n_obs_all))
            )
          ),
          if (length(info$names_nf) > 0) {
            shiny::tagList(
              shiny::tags$strong("Not Found values:"),
              limited_name_list_ui(info$names_nf)
            )
          },
          if (length(info$names_unres) > 0) {
            shiny::tagList(
              shiny::tags$strong("Unresolvable values:"),
              limited_name_list_ui(info$names_unres)
            )
          },
          if (length(info$names_amb) > 0) {
            shiny::tagList(
              shiny::tags$strong("Ambiguous values without manual choice:"),
              limited_name_list_ui(info$names_amb)
            )
          },
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("confirm_apply_discard"),
              "Apply and discard unresolved rows",
              class = "btn-danger"
            )
          )
        )
      )
    }

    shiny::observeEvent(input$run_match, {
      destroy_choice_observers()

      rv$issues <- empty_issues()
      rv$lookup <- NULL
      rv$lookup_display <- NULL
      rv$candidates_map <- list()
      rv$applied_df <- NULL
      rv$pending_choice <- NULL
      rv$ready <- FALSE
      rv$pending_apply <- FALSE
      rv$dropped_log <- .init_drop_log()

      df <- df_in()
      shiny::req(is.data.frame(df))

      src <- current_input_col()
      shiny::req(!is.null(src), src %in% names(df))

      if (!has_needed_pkgs()) {
        add_issue(
          NA_integer_,
          src,
          "taxonomy_missing_pkg",
          "ERROR",
          "The required package is not installed for the selected database."
        )
        return()
      }

      nms <- base_names()

      if (length(nms) == 0) {
        add_issue(
          NA_integer_,
          src,
          "taxonomy_no_names",
          "WARNING",
          "No names available for validation in the selected input column."
        )
        return()
      }

      qinfo <- lapply(nms, function(nm) {
        normalize_query_inline(nm)
      })

      normalised <- vapply(qinfo, `[[`, character(1), "query")
      unres <- !nzchar(normalised)

      lookup <- empty_lookup(input$tax_db)
      lookup <- lookup[rep(1, length(nms)), , drop = FALSE]
      lookup$inputName <- nms
      lookup$normalisedQuery <- normalised
      lookup$nameAccordingTo <- if (identical(input$tax_db, "worms")) "WoRMS" else "GBIF Backbone"
      lookup$taxonMatchStatus <- ifelse(unres, "UNRESOLVABLE", "PENDING")

      for (i in seq_along(nms)) {
        if (isTRUE(qinfo[[i]]$changed)) {
          add_issue(
            NA_integer_,
            src,
            qinfo[[i]]$rule,
            "INFO",
            paste0("normalisedQuery: '", nms[i], "' -> '", normalised[i], "'.")
          )
        }
      }

      if (any(unres)) {
        for (nm in nms[unres]) {
          add_issue(
            NA_integer_,
            src,
            "taxonomy_unresolvable",
            "INFO",
            paste0("Name could not be resolved automatically: '", nm, "'.")
          )
        }
      }

      to_do <- which(lookup$taxonMatchStatus == "PENDING")

      if (length(to_do) > 0) {
        shiny::withProgress(message = "Matching names...", value = 0, {
          for (k in seq_along(to_do)) {
            i <- to_do[k]
            nm <- lookup$inputName[i]
            q <- lookup$normalisedQuery[i]

            shiny::incProgress(1 / length(to_do), detail = q)

            res <- if (identical(input$tax_db, "worms")) {
              match_one_worms(
                input_name = nm,
                normalised_query = q,
                auto_apply_unique = auto_apply_unique
              )
            } else {
              match_one_gbif(
                input_name = nm,
                normalised_query = q,
                auto_apply_unique = auto_apply_unique
              )
            }

            row <- res$row
            lookup[i, names(row)] <- row[1, names(row)]

            if (!is.null(res$candidates) && is.data.frame(res$candidates) && nrow(res$candidates) > 0) {
              rv$candidates_map[[nm]] <- res$candidates
            }

            if (lookup$taxonMatchStatus[i] == "NOT_FOUND") {
              add_issue(
                NA_integer_,
                src,
                "taxonomy_not_found",
                "WARNING",
                paste0("Not found in the selected database: '", nm, "' (query='", q, "').")
              )
            }

            if (lookup$taxonMatchStatus[i] == "AMBIGUOUS") {
              if (identical(input$tax_db, "worms")) {
                add_issue(
                  NA_integer_,
                  src,
                  "taxonomy_ambiguous",
                  "WARNING",
                  paste0("Multiple candidates found for: '", nm, "' (query='", q, "'). Select one in the dropdown.")
                )
              } else {
                conf_i <- to_num1(lookup$confidence[i], default = NA_real_)

                add_issue(
                  NA_integer_,
                  src,
                  "taxonomy_ambiguous",
                  "WARNING",
                  paste0(
                    "GBIF Backbone returned a lower-confidence match for: '",
                    nm,
                    "' (query='",
                    q,
                    "', confidence=",
                    ifelse(is.na(conf_i), "NA", as.character(conf_i)),
                    "). Select one in the dropdown."
                  )
                )
              }
            }
          }
        })
      }

      rv$lookup <- lookup
      rv$lookup_display <- build_lookup_display(lookup)

      amb_idx <- which(rv$lookup$taxonMatchStatus == "AMBIGUOUS")

      if (length(amb_idx) > 0) {
        obs_list <- vector("list", length(amb_idx))

        for (j in seq_along(amb_idx)) {
          i <- amb_idx[j]

          obs_list[[j]] <- local({
            ii <- i
            input_name <- paste0("cand_", ii)

            shiny::observeEvent(input[[input_name]], {
              val <- input[[input_name]]

              if (is_blank(val)) return()

              if (isTRUE(rv$suppress_choice_popup)) {
                nm_same <- rv$lookup$inputName[ii]
                same_rows <- which(rv$lookup$inputName == nm_same)

                apply_choice_to_rows(same_rows, val)

                obs_count <- count_observations_for_inputName(nm_same)

                add_issue(
                  NA_integer_,
                  src,
                  "taxonomy_manual_resolution_same_inputName",
                  "INFO",
                  paste0(
                    "Choice applied to ",
                    ifelse(is.na(obs_count), length(same_rows), obs_count),
                    " observation(s) with inputName='",
                    nm_same,
                    "'."
                  )
                )
              } else {
                rv$pending_choice <- list(
                  row_index = ii,
                  selected_id = val
                )

                open_choice_popup(ii, val)
              }
            }, ignoreInit = TRUE)
          })
        }

        choice_observers <<- obs_list
      }
    })

    shiny::observeEvent(input$choice_apply_same, {
      shiny::req(rv$pending_choice)

      row_index <- rv$pending_choice$row_index
      selected_id <- rv$pending_choice$selected_id
      src <- current_input_col()

      nm <- rv$lookup$inputName[row_index]
      same_rows <- which(rv$lookup$inputName == nm)

      apply_choice_to_rows(same_rows, selected_id)
      shiny::removeModal()

      obs_count <- count_observations_for_inputName(nm)

      add_issue(
        NA_integer_,
        src,
        "taxonomy_manual_resolution_same_inputName",
        "INFO",
        paste0(
          "Choice applied to ",
          ifelse(is.na(obs_count), length(same_rows), obs_count),
          " observation(s) with inputName='",
          nm,
          "'."
        )
      )

      rv$pending_choice <- NULL
    })

    compute_out <- function() {
      df <- df_in()
      shiny::req(is.data.frame(df))

      if (is.null(rv$lookup) || !is.data.frame(rv$lookup) || nrow(rv$lookup) == 0) {
        return(list(
          kept = df,
          dropped = df[0, , drop = FALSE]
        ))
      }

      src <- current_input_col()
      shiny::req(!is.null(src), src %in% names(df))

      out <- df
      src_vals <- normalize_ws(out[[src]])

      if (!("verbatimIdentification" %in% names(out))) {
        out$verbatimIdentification <- out[[src]]
      } else {
        blank_vi <- is.na(out$verbatimIdentification) |
          !nzchar(trimws(as.character(out$verbatimIdentification)))

        out$verbatimIdentification[blank_vi] <- out[[src]][blank_vi]
      }

      if (!("scientificName" %in% names(out))) {
        out$scientificName <- NA_character_
      }

      map_idx <- match(src_vals, rv$lookup$inputName)

      lk_col <- function(col) {
        vals <- rep(NA_character_, nrow(out))

        ok <- !is.na(map_idx) &
          map_idx >= 1 &
          map_idx <= nrow(rv$lookup) &
          (col %in% names(rv$lookup))

        vals[ok] <- as.character(rv$lookup[[col]][map_idx[ok]])

        vals
      }

      status_group <- tax_status_group(lk_col("taxonMatchStatus"))
      raw_status <- lk_col("taxonMatchStatus")

      if (identical(input$tax_db, "worms")) {
        resolved_name <- lk_col("valid_name")
        matched_name <- lk_col("scientificname")
      } else {
        resolved_name <- lk_col("acceptedScientificName")
        matched_name <- lk_col("scientificName")
      }

      fill_val <- ifelse(
        !is.na(resolved_name) & nzchar(resolved_name),
        resolved_name,
        matched_name
      )

      fill_ok_blank <- status_group == "MATCHED" &
        (is.na(out$scientificName) | !nzchar(trimws(as.character(out$scientificName)))) &
        !is.na(fill_val) &
        nzchar(fill_val)

      out$scientificName[fill_ok_blank] <- fill_val[fill_ok_blank]

      if (isTRUE(replace_scientificName)) {
        fill_ok <- status_group == "MATCHED" &
          !is.na(fill_val) &
          nzchar(fill_val)

        out$scientificName[fill_ok] <- fill_val[fill_ok]
      }

      candidate_payload <- if (identical(input$tax_db, "worms")) {
        list(
          scientificName = out$scientificName,
          scientificNameID = lk_col("lsid"),
          taxonID = lk_col("AphiaID"),
          acceptedNameUsage = lk_col("valid_name"),
          acceptedNameUsageID = lk_col("valid_AphiaID"),
          taxonomicStatus = lk_col("status"),
          taxonRank = lk_col("rank"),
          scientificNameAuthorship = lk_col("authority"),
          parentNameUsageID = lk_col("parentNameUsageID"),
          originalNameUsageID = lk_col("originalNameUsageID"),
          kingdom = lk_col("kingdom"),
          phylum = lk_col("phylum"),
          class = lk_col("class"),
          order = lk_col("order"),
          family = lk_col("family"),
          genus = lk_col("genus"),
          normalisedQuery = lk_col("normalisedQuery"),
          taxonMatchStatus = lk_col("taxonMatchStatus"),
          selectedID = lk_col("selectedID"),
          nameAccordingTo = lk_col("nameAccordingTo")
        )
      } else {
        list(
          scientificName = out$scientificName,
          scientificNameID = lk_col("usageKey"),
          taxonID = lk_col("usageKey"),
          acceptedNameUsage = lk_col("acceptedScientificName"),
          acceptedNameUsageID = lk_col("acceptedUsageKey"),
          taxonomicStatus = {
            st <- lk_col("status")
            ifelse(!is.na(st) & nzchar(st), st, lk_col("taxonomicStatus"))
          },
          taxonRank = lk_col("rank"),
          scientificNameAuthorship = lk_col("authorship"),
          kingdom = lk_col("kingdom"),
          phylum = lk_col("phylum"),
          class = lk_col("class"),
          order = lk_col("order"),
          family = lk_col("family"),
          genus = lk_col("genus"),
          species = lk_col("species"),
          normalisedQuery = lk_col("normalisedQuery"),
          taxonMatchStatus = lk_col("taxonMatchStatus"),
          selectedID = lk_col("selectedID"),
          nameAccordingTo = lk_col("nameAccordingTo")
        )
      }

      selected_keep <- input$keep_output_cols %||% final_match_cols_default
      keep_payload <- names(candidate_payload)[names(candidate_payload) %in% selected_keep]

      for (nm in keep_payload) {
        if (nm == "scientificName") {
          out$scientificName <- candidate_payload[[nm]]
        } else {
          out[[nm]] <- candidate_payload[[nm]]
        }
      }

      tech_not_selected <- setdiff(technical_cols, selected_keep)
      tech_not_selected <- intersect(tech_not_selected, names(out))

      if (length(tech_not_selected) > 0) {
        out[tech_not_selected] <- NULL
      }

      keep_rows <- status_group == "MATCHED"
      keep_rows[is.na(keep_rows)] <- FALSE

      dropped <- out[!keep_rows, , drop = FALSE]
      kept <- out[keep_rows, , drop = FALSE]

      if (nrow(dropped) > 0) {
        dropped$taxonMatchStatus <- raw_status[!keep_rows]
      }

      list(
        kept = kept,
        dropped = dropped
      )
    }

    df_out <- shiny::reactive({
      if (!is.null(rv$applied_df) && is.data.frame(rv$applied_df)) {
        return(rv$applied_df)
      }

      df_in()
    })

    finalize_apply <- function() {
      res <- compute_out()
      out <- res$kept
      dropped <- res$dropped

      shiny::req(is.data.frame(out))

      rv$dropped_log <- .init_drop_log()

      if (is.data.frame(dropped) && nrow(dropped) > 0) {
        dropped_nf <- dropped[normalize_ws(dropped$taxonMatchStatus) == "NOT_FOUND", , drop = FALSE]
        dropped_unres <- dropped[normalize_ws(dropped$taxonMatchStatus) == "UNRESOLVABLE", , drop = FALSE]
        dropped_amb <- dropped[normalize_ws(dropped$taxonMatchStatus) == "AMBIGUOUS", , drop = FALSE]

        if (nrow(dropped_nf) > 0) {
          rv$dropped_log <- .append_drop_log(
            log_df = rv$dropped_log,
            dropped_df = dropped_nf,
            module = "taxonomy",
            step = "apply_match",
            reason = "taxonomy_not_found",
            details = "Row discarded because taxonomy match status was NOT_FOUND."
          )
        }

        if (nrow(dropped_unres) > 0) {
          rv$dropped_log <- .append_drop_log(
            log_df = rv$dropped_log,
            dropped_df = dropped_unres,
            module = "taxonomy",
            step = "apply_match",
            reason = "taxonomy_unresolvable",
            details = "Row discarded because taxonomy match status was UNRESOLVABLE."
          )
        }

        if (nrow(dropped_amb) > 0) {
          rv$dropped_log <- .append_drop_log(
            log_df = rv$dropped_log,
            dropped_df = dropped_amb,
            module = "taxonomy",
            step = "apply_match",
            reason = "taxonomy_ambiguous_unresolved",
            details = "Row discarded because taxonomy match status was AMBIGUOUS and no manual choice was selected."
          )
        }
      }

      rv$applied_df <- out
      rv$ready <- TRUE
      rv$pending_apply <- FALSE

      stat_grp <- tax_status_group(rv$lookup$taxonMatchStatus)
      n_matched <- sum(stat_grp == "MATCHED", na.rm = TRUE)
      n_amb <- sum(rv$lookup$taxonMatchStatus == "AMBIGUOUS", na.rm = TRUE)
      n_nf <- sum(rv$lookup$taxonMatchStatus == "NOT_FOUND", na.rm = TRUE)
      n_unres <- sum(rv$lookup$taxonMatchStatus == "UNRESOLVABLE", na.rm = TRUE)

      discarded_n <- nrow(df_in()) - nrow(out)

      add_issue(
        NA_integer_,
        current_input_col(),
        "taxonomy_applied",
        "WARNING",
        paste0(
          "Taxonomy lookup applied to dataset. ",
          "MATCHED=", n_matched,
          " | AMBIGUOUS=", n_amb,
          " | NOT_FOUND=", n_nf,
          " | UNRESOLVABLE=", n_unres,
          " | Rows kept=", nrow(out),
          " | Rows discarded=", discarded_n,
          " | Dropped rows logged=", nrow(rv$dropped_log),
          ". Rows without a resolved taxonomy match were discarded. If you need to keep them, correct the original dataset and restart the workflow."
        )
      )

      shiny::showNotification(
        paste0(
          "Taxonomy results applied to dataset. ",
          nrow(out),
          " row(s) kept and ",
          max(0, discarded_n),
          " row(s) discarded."
        ),
        type = "message",
        duration = 6
      )
    }

    shiny::observeEvent(input$apply_match, {
      shiny::req(is.data.frame(df_in()))

      if (is.null(rv$lookup) || !is.data.frame(rv$lookup) || nrow(rv$lookup) == 0) {
        add_issue(
          NA_integer_,
          current_input_col(),
          "taxonomy_apply_without_lookup",
          "WARNING",
          "Run the taxonomy match before applying it to the dataset."
        )
        return()
      }

      info <- unresolved_apply_summary()

      if (!is.null(info) && isTRUE(info$n_obs_all > 0)) {
        rv$pending_apply <- TRUE

        add_issue(
          NA_integer_,
          current_input_col(),
          "taxonomy_apply_with_unresolved_rows",
          "WARNING",
          paste0(
            info$n_obs_all,
            " observation(s) will be discarded because they are NOT_FOUND, UNRESOLVABLE, or still AMBIGUOUS without a selected candidate."
          )
        )

        open_apply_warning_modal(info)
        return()
      }

      finalize_apply()
    })

    shiny::observeEvent(input$confirm_apply_discard, {
      shiny::removeModal()
      finalize_apply()
    })

    output$download_lookup <- shiny::downloadHandler(
      filename = function() {
        paste0("taxonomy_lookup_", input$tax_db, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        lk <- rv$lookup

        if (is.null(lk) || !is.data.frame(lk)) {
          lk <- data.frame()
        }

        utils::write.csv(
          lk,
          file = file,
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )
      }
    )

    output$lookup_tbl <- DT::renderDT({
      disp <- rv$lookup_display

      if (is.null(disp) || !is.data.frame(disp)) {
        disp <- data.frame()
      }

      DT::datatable(
        disp,
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        class = "compact stripe hover",
        options = list(
          paging = FALSE,
          searching = TRUE,
          info = FALSE,
          ordering = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE,
          scrollY = "520px",
          scrollCollapse = TRUE,
          dom = "ft",
          preDrawCallback = DT::JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
          drawCallback = DT::JS("function() { Shiny.bindAll(this.api().table().node()); }")
        )
      )
    })

    output$issues_tbl <- DT::renderDT({
      DT::datatable(
        rv$issues,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = TRUE,
          info = FALSE,
          scrollX = TRUE,
          scrollY = "280px",
          scrollCollapse = TRUE
        )
      )
    })

    session$onSessionEnded(function() {
      destroy_choice_observers()
    })

    list(
      df_out = df_out,
      lookup = shiny::reactive(rv$lookup),
      issues = shiny::reactive(rv$issues),
      dropped_log = shiny::reactive(rv$dropped_log),
      ready = shiny::reactive(isTRUE(rv$ready))
    )
  })
}
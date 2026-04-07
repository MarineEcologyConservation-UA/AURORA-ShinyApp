# =========================================================
# Taxonomy Match Module (WoRMS / GBIF)
# - GBIF remains available
# - WoRMS uses ONLY worrms::wm_records_taxamatch()
# - input column configurable (default: verbatimIdentification if present)
# - normalisedQuery explicit and always created
# - one lookup row per unique inputName
# - selectable output columns to keep in final dataset
# - notes block moved to the top
# File: R/mod_taxonomy_match.R
# =========================================================

#' Taxonomy match UI
#' @param id module id
#' @export
mod_taxonomy_match_ui <- function(id) {
  ns <- shiny::NS(id)

  root_sel <- paste0("#", ns("root"))

  tooltip_content <- shiny::tags$div(
    class = "tax-tip-wrap",
    shiny::tags$div(class = "tax-tip-title", "What this does"),
    shiny::tags$div(
      class = "tax-tip-text",
      "Creates a cleaned version of the input name to improve database matching."
    ),

    shiny::tags$div(class = "tax-tip-title mt-2", "Current cleanup rules"),
    shiny::tags$ul(
      class = "tax-tip-list",
      shiny::tags$li(
        "removes text inside parentheses: ",
        shiny::tags$span(class = "tax-tip-chip", "(...)")
      ),
      shiny::tags$li(
        "removes text inside brackets: ",
        shiny::tags$span(class = "tax-tip-chip", "[...]")
      ),
      shiny::tags$li(
        "removes life-stage qualifiers: ",
        shiny::tags$span(class = "tax-tip-chip", "juvenile"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "juveniles"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "adult"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "adults"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "larva"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "larvae"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "egg"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "eggs")
      ),
      shiny::tags$li(
        "removes sex qualifiers: ",
        shiny::tags$span(class = "tax-tip-chip", "male"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "female"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "unknown sex")
      ),
      shiny::tags$li(
        "removes status words: ",
        shiny::tags$span(class = "tax-tip-chip", "alive"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "dead")
      ),
      shiny::tags$li(
        "removes informal qualifiers: ",
        shiny::tags$span(class = "tax-tip-chip", "indet"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "indet."), ", ",
        shiny::tags$span(class = "tax-tip-chip", "msp"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "msp."), ", ",
        shiny::tags$span(class = "tax-tip-chip", "sp"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "sp."), ", ",
        shiny::tags$span(class = "tax-tip-chip", "cf"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "cf."), ", ",
        shiny::tags$span(class = "tax-tip-chip", "aff"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "aff."), ", ",
        shiny::tags$span(class = "tax-tip-chip", "morphotype")
      ),
      shiny::tags$li(
        "removes standalone numbers such as ",
        shiny::tags$span(class = "tax-tip-chip", "1"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "2"), ", ",
        shiny::tags$span(class = "tax-tip-chip", "3")
      ),
      shiny::tags$li(
        "removes punctuation ",
        shiny::tags$span(class = "tax-tip-chip", ","), " ",
        shiny::tags$span(class = "tax-tip-chip", ";"), " ",
        shiny::tags$span(class = "tax-tip-chip", ":")
      ),
      shiny::tags$li("collapses extra whitespace"),
      shiny::tags$li("keeps only the first two name tokens when applicable")
    ),

    shiny::tags$div(class = "tax-tip-title mt-2", "Examples"),
    shiny::tags$div(
      class = "tax-tip-example",
      shiny::tags$span(class = "tax-tip-chip", "Polynoidae msp 1"),
      " \u2192 ",
      shiny::tags$span(class = "tax-tip-chip", "Polynoidae")
    ),
    shiny::tags$div(
      class = "tax-tip-example",
      shiny::tags$span(class = "tax-tip-chip", "Abra alba (juveniles)"),
      " \u2192 ",
      shiny::tags$span(class = "tax-tip-chip", "Abra alba")
    ),
    shiny::tags$div(
      class = "tax-tip-example",
      shiny::tags$span(class = "tax-tip-chip", "Gammarus cf. salinus"),
      " \u2192 ",
      shiny::tags$span(class = "tax-tip-chip", "Gammarus salinus")
    ),
    shiny::tags$div(
      class = "tax-tip-example",
      shiny::tags$span(class = "tax-tip-chip", "Nephtys indet."),
      " \u2192 ",
      shiny::tags$span(class = "tax-tip-chip", "Nephtys")
    )
  )

  info_icon <- shiny::tags$span(
    class = "tax-info-icon",
    tabindex = "0",
    role = "button",
    `aria-label` = "More information",
    "i"
  )

  css <- paste0("
    ", root_sel, " .tax-top .card-body { padding: 1rem; }
    ", root_sel, " .tax-top .card { height: 100%; }
    ", root_sel, " .tax-minh { min-height: 300px; }
    ", root_sel, " .tax-actions .btn { margin-right: .5rem; }
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

    ", root_sel, " .tax-check-help {
      display: flex;
      align-items: center;
      gap: .45rem;
      margin-bottom: 1rem;
      flex-wrap: wrap;
    }

    ", root_sel, " .tax-check-help .form-group {
      margin-bottom: 0;
    }

    ", root_sel, " .tax-check-help .form-check {
      margin-bottom: 0;
      min-height: auto;
    }

    ", root_sel, " .tax-check-help .form-check-label {
      display: inline-flex;
      align-items: center;
      gap: .45rem;
    }

    ", root_sel, " .tax-info-icon {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      width: 18px;
      height: 18px;
      border-radius: 50%;
      border: 1px solid #6b7280;
      color: #6b7280;
      font-size: 12px;
      font-weight: 700;
      line-height: 1;
      cursor: help;
      flex: 0 0 auto;
      background: #fff;
    }

    ", root_sel, " .tooltip {
      --bs-tooltip-max-width: 560px;
    }

    ", root_sel, " .tax-tip-wrap {
      text-align: left;
      line-height: 1.4;
    }

    ", root_sel, " .tax-tip-title {
      font-weight: 700;
      margin-bottom: .25rem;
    }

    ", root_sel, " .tax-tip-text {
      margin-bottom: .25rem;
    }

    ", root_sel, " .tax-tip-list {
      margin: 0 0 .5rem 1rem;
      padding: 0;
    }

    ", root_sel, " .tax-tip-list li {
      margin-bottom: .2rem;
    }

    ", root_sel, " .tax-tip-chip {
      display: inline-block;
      padding: .05rem .35rem;
      border-radius: .35rem;
      background: rgba(255,255,255,.14);
      color: #fff;
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;
      font-size: .92em;
      white-space: nowrap;
    }

    ", root_sel, " .tax-tip-example {
      margin-bottom: .2rem;
    }
  ")

  shiny::tagList(
    shiny::tags$style(shiny::HTML(css)),

    shiny::tags$div(
      id = ns("root"),

      bslib::card(
        bslib::card_header("Taxonomy match (WoRMS / GBIF)"),
        bslib::card_body(

          bslib::card(
            bslib::card_header("Notes"),
            bslib::card_body(
              shiny::tags$ul(
                shiny::tags$li("Matching starts from a configurable input column, ideally verbatimIdentification."),
                shiny::tags$li("normalisedQuery is always created and represents the text actually submitted to the selected database."),
                shiny::tags$li("WoRMS uses only worrms::wm_records_taxamatch()."),
                shiny::tags$li("GBIF remains available for terrestrial names."),
                shiny::tags$li("The lookup table shows one row per unique inputName."),
                shiny::tags$li("When ambiguities exist, the user must manually choose the candidate before applying results to the dataset."),
                shiny::tags$li("When applying to the dataset, the module writes only the selected output columns.")
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
                    class = "tax-check-help",
                    shiny::checkboxInput(
                      inputId = ns("generate_normalised_query"),
                      label = shiny::tagList(
                        shiny::span("Generate normalisedQuery"),
                        bslib::tooltip(
                          trigger = info_icon,
                          placement = "auto",
                          tooltip_content
                        )
                      ),
                      value = TRUE,
                      width = NULL
                    )
                  ),

                  shiny::numericInput(
                    inputId = ns("top_n"),
                    label = "Top N candidates (for manual review)",
                    value = 5,
                    min = 2,
                    max = 50,
                    step = 1
                  ),

                  shiny::tags$div(
                    class = "tax-actions d-flex gap-2 flex-wrap",
                    shiny::actionButton(ns("run_match"), "Run match", class = "btn-primary"),
                    shiny::uiOutput(ns("apply_btn_ui"), inline = TRUE)
                  ),

                  shiny::hr(),
                  shiny::uiOutput(ns("pkg_status")),
                  shiny::hr(),
                  shiny::downloadButton(ns("download_lookup"), "Export lookup (CSV)")
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
                    "normalisedQuery, taxonMatchStatus, selectedID, and nameAccordingTo are technical columns and are not included in the final dataset by default. scientificNameID already stores the main identifier used for the selected database."
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
        bslib::card_header("Name lookup table"),
        bslib::card_body(
          DT::DTOutput(ns("lookup_tbl"))
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
#' @return list(df_out, lookup, issues, ready)
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
      "key", "nubKey", "scientificName", "canonicalName", "rank",
      "taxonomicStatus", "status", "authorship", "acceptedKey",
      "accepted", "matchType", "confidence", "kingdom", "phylum",
      "order", "family", "genus", "species"
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

    technical_cols <- c("inputName", "normalisedQuery", "taxonMatchStatus", "selectedID", "nameAccordingTo")

    final_match_cols_default <- c(
      "scientificName",
      "scientificNameID",
      "acceptedNameUsage",
      "taxonomicStatus",
      "taxonRank",
      "kingdom",
      "phylum",
      "family",
      "genus",
      "authority"
    )

    rv <- shiny::reactiveValues(
      lookup = NULL,
      lookup_display = NULL,
      applied_df = NULL,
      issues = empty_issues(),
      cache = new.env(parent = emptyenv()),
      candidates_map = list(),
      choice_observers = list(),
      suppress_choice_popup = FALSE,
      pending_choice = NULL,
      ready = FALSE
    )

    add_issue <- function(idx, field, rule, severity, message) {
      rv$issues <- rbind(
        rv$issues,
        data.frame(
          row = if (length(idx) == 0 || is.null(idx) || is.na(idx)) NA_integer_ else as.integer(idx),
          field = as.character(field),
          rule = as.character(rule),
          severity = as.character(severity),
          message = as.character(message),
          stringsAsFactors = FALSE
        )
      )
    }

    destroy_choice_observers <- function() {
      if (length(rv$choice_observers) > 0) {
        for (obs in rv$choice_observers) {
          if (!is.null(obs)) {
            try(obs$destroy(), silent = TRUE)
          }
        }
      }
      rv$choice_observers <- list()
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
      if (all(is.na(x))) return(TRUE)
      !nzchar(trimws(as.character(x)))
    }

    to_chr1 <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      x <- as.character(x[1])
      if (is.na(x) || identical(x, "NULL")) return(NA_character_)
      x
    }

    current_input_col <- shiny::reactive({
      df <- df_in()
      shiny::req(is.data.frame(df))
      cols <- names(df)

      chosen <- input$input_col
      if (!is.null(chosen) && nzchar(chosen) && chosen %in% cols) return(chosen)

      if ("verbatimIdentification" %in% cols) return("verbatimIdentification")
      if ("scientificName" %in% cols) return("scientificName")
      if (length(cols) > 0) return(cols[1])

      NULL
    })

    output$input_col_ui <- shiny::renderUI({
      df <- df_in()
      shiny::req(is.data.frame(df))

      cols <- names(df)
      if (length(cols) == 0) {
        return(shiny::tags$div("No columns available."))
      }

      selected <- input$input_col
      if (is.null(selected) || !(selected %in% cols)) {
        if ("verbatimIdentification" %in% cols) {
          selected <- "verbatimIdentification"
        } else if ("scientificName" %in% cols) {
          selected <- "scientificName"
        } else {
          selected <- cols[1]
        }
      }

      shiny::selectInput(
        inputId = ns("input_col"),
        label = "Input column for matching",
        choices = cols,
        selected = selected
      )
    })

    output$apply_btn_ui <- shiny::renderUI({
      enabled <- !is.null(rv$lookup) && is.data.frame(rv$lookup) && nrow(rv$lookup) > 0

      shiny::actionButton(
        ns("apply_match"),
        "Apply to dataset",
        class = "btn-success",
        disabled = if (!enabled) "disabled" else NULL
      )
    })

    output$keep_cols_ui <- shiny::renderUI({
      choices <- if (identical(input$tax_db, "worms")) {
        c(
          "scientificName",
          "scientificNameID",
          "acceptedNameUsage",
          "taxonomicStatus",
          "taxonRank",
          "authority",
          setdiff(worms_output_cols, c("lsid"))
        )
      } else {
        c(
          "scientificName",
          "scientificNameID",
          "acceptedNameUsage",
          "taxonomicStatus",
          "taxonRank",
          "authority",
          setdiff(gbif_output_cols, c("key"))
        )
      }

      shiny::checkboxGroupInput(
        inputId = ns("keep_output_cols"),
        label = NULL,
        choices = choices,
        selected = intersect(final_match_cols_default, choices)
      )
    })

    base_names <- shiny::reactive({
      df <- df_in()
      shiny::req(is.data.frame(df))

      src <- current_input_col()
      shiny::req(!is.null(src), src %in% names(df))

      x <- normalize_ws(df[[src]])
      x <- x[x != ""]
      sort(unique(x))
    })

    normalize_query_info <- function(name, generate_normalised_query = TRUE) {
      s0 <- normalize_ws(name)

      if (!nzchar(s0)) {
        return(list(
          query = "",
          changed = FALSE,
          rule = "blank"
        ))
      }

      low <- tolower(s0)
      out <- s0

      if (isTRUE(generate_normalised_query)) {
        out <- low
        out <- gsub("\\([^)]*\\)", " ", out)
        out <- gsub("\\[[^]]*\\]", " ", out)
        out <- gsub("\\b(juvenile|juveniles|adult|adults|larva|larvae|egg|eggs)\\b", " ", out, perl = TRUE)
        out <- gsub("\\b(male|female|unknown sex)\\b", " ", out, perl = TRUE)
        out <- gsub("\\b(alive|dead)\\b", " ", out, perl = TRUE)
        out <- gsub("\\b(indet\\.?|msp\\.?|sp\\.?|cf\\.?|aff\\.?|morphotype)\\b", " ", out, perl = TRUE)
        out <- gsub("\\b\\d+\\b", " ", out, perl = TRUE)
        out <- gsub("[,;:]+", " ", out)
        out <- gsub("\\s+", " ", out)
        out <- trimws(out)

        toks <- strsplit(out, " ", fixed = TRUE)[[1]]
        toks <- toks[nzchar(toks)]
        toks <- toks[grepl("[a-z]", toks, perl = TRUE)]

        if (length(toks) >= 2) {
          toks <- toks[1:2]
        }

        if (length(toks) == 0) {
          out <- ""
        } else {
          toks[1] <- paste0(toupper(substr(toks[1], 1, 1)), substr(toks[1], 2, nchar(toks[1])))
          if (length(toks) >= 2) toks[2] <- tolower(toks[2])
          out <- normalize_ws(paste(toks, collapse = " "))
        }
      }

      list(
        query = out,
        changed = !identical(normalize_ws(out), normalize_ws(s0)),
        rule = if (!identical(normalize_ws(out), normalize_ws(s0))) "truncate_qualifiers" else "none"
      )
    }

    lower_norm <- function(x) {
      tolower(normalize_ws(x))
    }

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

    cache_key <- function(db, query, top_n, auto_apply_unique) {
      paste(db, as.integer(top_n), as.character(auto_apply_unique), query, sep = "||")
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
      } else if (is.list(x)) {
        if (length(x) >= 1) {
          df <- x[[1]]
        }
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

    worms_taxamatch_candidates <- function(query, top_n = 5) {
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
      utils::head(out[ord, , drop = FALSE], as.integer(top_n))
    }

    worms_find_candidate_row <- function(input_name, selected_id) {
      cand <- rv$candidates_map[[input_name]]
      if (is.null(cand) || !is.data.frame(cand) || nrow(cand) == 0) return(NULL)
      if (!("AphiaID" %in% names(cand))) return(NULL)

      idx <- which(as.character(cand$AphiaID) == as.character(selected_id))
      if (length(idx) == 0) return(NULL)

      cand[idx[1], , drop = FALSE]
    }

    match_one_worms <- function(input_name, normalised_query, top_n = 5, auto_apply_unique = TRUE) {
      key <- cache_key("worms", normalised_query, top_n, auto_apply_unique)
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

      cand <- worms_taxamatch_candidates(normalised_query, top_n = top_n)

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

    gbif_candidates <- function(query, top_n = 5) {
      res <- tryCatch(
        rgbif::name_lookup(q = query, limit = as.integer(top_n)),
        error = function(e) NULL
      )

      dat <- NULL
      if (!is.null(res) && is.list(res) && "data" %in% names(res)) {
        dat <- res$data
      }

      if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0) {
        return(data.frame())
      }

      keep <- intersect(
        names(dat),
        c(
          "key", "nubKey", "scientificName", "canonicalName", "rank",
          "taxonomicStatus", "status", "authorship", "acceptedKey",
          "accepted", "matchType", "confidence", "kingdom", "phylum",
          "order", "family", "genus", "species"
        )
      )

      out <- dat[, keep, drop = FALSE]

      if ("key" %in% names(out)) {
        out$key <- as.character(out$key)
        out <- out[!duplicated(out$key), , drop = FALSE]
      }

      sci <- if ("scientificName" %in% names(out)) lower_norm(out$scientificName) else rep("", nrow(out))
      can <- if ("canonicalName" %in% names(out)) lower_norm(out$canonicalName) else rep("", nrow(out))
      qn  <- lower_norm(query)

      score <- integer(nrow(out))
      score[sci == qn] <- score[sci == qn] + 3L
      score[can == qn] <- score[can == qn] + 2L

      if ("matchType" %in% names(out)) {
        mt <- toupper(as.character(out$matchType))
        score[mt == "EXACT"] <- score[mt == "EXACT"] + 2L
        score[mt == "FUZZY"] <- score[mt == "FUZZY"] + 1L
      }

      if ("confidence" %in% names(out)) {
        conf <- suppressWarnings(as.numeric(out$confidence))
        conf[is.na(conf)] <- 0
      } else {
        conf <- rep(0, nrow(out))
      }

      ord <- order(-score, -conf, sci)
      utils::head(out[ord, , drop = FALSE], as.integer(top_n))
    }

    gbif_find_candidate_row <- function(input_name, selected_id) {
      cand <- rv$candidates_map[[input_name]]
      if (is.null(cand) || !is.data.frame(cand) || nrow(cand) == 0) return(NULL)
      if (!("key" %in% names(cand))) return(NULL)

      idx <- which(as.character(cand$key) == as.character(selected_id))
      if (length(idx) == 0) return(NULL)

      cand[idx[1], , drop = FALSE]
    }

    match_one_gbif <- function(input_name, normalised_query, top_n = 5, auto_apply_unique = TRUE) {
      key <- cache_key("gbif", normalised_query, top_n, auto_apply_unique)
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

      cand <- gbif_candidates(normalised_query, top_n = top_n)

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
        selected_id <- to_chr1(top1$key)
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

    get_candidate_table <- function(i) {
      if (is.null(rv$lookup) || nrow(rv$lookup) < i) return(NULL)
      nm <- rv$lookup$inputName[i]
      rv$candidates_map[[nm]]
    }

    build_lookup_display <- function(lookup) {
      disp <- lookup
      disp$choice <- ""

      if (is.null(disp) || !is.data.frame(disp) || nrow(disp) == 0) {
        return(disp)
      }

      for (i in seq_len(nrow(disp))) {
        if (!identical(disp$taxonMatchStatus[i], "AMBIGUOUS")) next

        nm <- disp$inputName[i]
        cand <- rv$candidates_map[[nm]]
        if (is.null(cand) || !is.data.frame(cand) || nrow(cand) == 0) next

        if (identical(input$tax_db, "worms")) {
          ids <- as.character(cand$AphiaID)
          sci <- safe_col(cand, "scientificname", "")
          rk  <- safe_col(cand, "rank", "")
          au  <- safe_col(cand, "authority", "")
          st  <- safe_col(cand, "status", "")
          vn  <- safe_col(cand, "valid_name", "")
          mt  <- safe_col(cand, "match_type", "")

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
          ids <- as.character(cand$key)
          sci <- safe_col(cand, "scientificName", "")
          can <- safe_col(cand, "canonicalName", "")
          rk  <- safe_col(cand, "rank", "")
          au  <- safe_col(cand, "authorship", "")
          st  <- safe_col(cand, "taxonomicStatus", "")
          mt  <- safe_col(cand, "matchType", "")
          primary_nm <- ifelse(sci != "", sci, can)

          lab <- paste0(
            ids, " — ",
            primary_nm,
            ifelse(rk != "", paste0(" [", rk, "]"), ""),
            ifelse(st != "", paste0(" (", st, ")"), ""),
            ifelse(mt != "", paste0(" <", mt, ">"), ""),
            ifelse(au != "", paste0(" — ", au), "")
          )
        }

        choices <- c("-- select candidate --" = "", stats::setNames(ids, lab))
        input_id <- ns(paste0("cand_", i))
        selected_val <- if (!is.na(disp$selectedID[i]) && disp$selectedID[i] != "") disp$selectedID[i] else ""

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

      disp$.choice_order <- 3L
      disp$.choice_order[
        disp$taxonMatchStatus == "AMBIGUOUS" &
          (is.na(disp$selectedID) | disp$selectedID == "")
      ] <- 0L
      disp$.choice_order[
        disp$taxonMatchStatus == "AMBIGUOUS" &
          !is.na(disp$selectedID) & disp$selectedID != ""
      ] <- 1L
      disp$.choice_order[grepl("^MATCHED", disp$taxonMatchStatus)] <- 2L

      disp <- disp[order(disp$.choice_order, disp$inputName), , drop = FALSE]
      disp$.choice_order <- NULL

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
          function(x) is_blank(normalize_query_info(x, generate_normalised_query = isTRUE(input$generate_normalised_query))$query),
          logical(1)
        )

        return(
          shiny::tags$div(
            shiny::tags$p(paste0("Input column: ", src)),
            shiny::tags$p(paste0("Unique names: ", length(nms))),
            shiny::tags$p(paste0("UNRESOLVABLE (pre-match): ", sum(unres))),
            shiny::tags$p("MATCHED: 0 | AMBIGUOUS: 0 | NOT_FOUND: 0"),
            shiny::tags$p("Dataset applied: NO")
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
            "Dataset applied: ",
            if (!is.null(rv$applied_df) && is.data.frame(rv$applied_df)) "YES" else "NO"
          )
        )
      )
    })

    open_choice_popup <- function(row_index, selected_id) {
      rv$pending_choice <- list(
        row_index = row_index,
        selected_id = as.character(selected_id)
      )

      q <- rv$lookup$normalisedQuery[row_index]
      nm <- rv$lookup$inputName[row_index]
      same_nq <- which(rv$lookup$normalisedQuery == q)
      same_nq <- setdiff(same_nq, row_index)

      shiny::showModal(
        shiny::modalDialog(
          title = "Confirm taxonomic choice",
          shiny::tags$p(
            paste0(
              "You selected a candidate for '", nm, "'."
            )
          ),
          shiny::tags$p(
            paste0("This will mark the row as MATCHED_MANUAL.")
          ),
          if (length(same_nq) > 0) {
            shiny::tags$p(
              paste0(
                length(same_nq),
                " other row(s) share the same normalisedQuery ('", q, "'). ",
                "Choose whether to apply this selection only here or to all matching rows."
              )
            )
          } else {
            shiny::tags$p(
              paste0("No other rows share the same normalisedQuery ('", q, "').")
            )
          },
          shiny::checkboxInput(
            ns("choice_popup_dont_show"),
            "Do not show this again during this session",
            value = FALSE
          ),
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("choice_apply_this"), "Apply only to this row", class = "btn-primary"),
            shiny::actionButton(
              ns("choice_apply_same"),
              "Apply to all rows with same normalisedQuery",
              class = "btn-success"
            )
          )
        )
      )
    }

    open_pending_ambiguous_popup <- function(n_pending) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Resolve ambiguous taxonomy matches first",
          shiny::tags$p(
            paste0(
              "There are ", n_pending,
              " ambiguous row(s) without a selected candidate."
            )
          ),
          shiny::tags$p(
            "You must choose a candidate for every ambiguous row before applying taxonomy results to the dataset."
          ),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
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

      df <- df_in()
      shiny::req(is.data.frame(df))

      src <- current_input_col()
      shiny::req(!is.null(src), src %in% names(df))

      if (!has_needed_pkgs()) {
        add_issue(
          NA_integer_, src, "taxonomy_missing_pkg", "ERROR",
          "The required package is not installed for the selected database."
        )
        return()
      }

      nms <- base_names()
      if (length(nms) == 0) {
        add_issue(
          NA_integer_, src, "taxonomy_no_names", "WARNING",
          "No names available for validation in the selected input column."
        )
        return()
      }

      qinfo <- lapply(nms, function(nm) {
        normalize_query_info(nm, generate_normalised_query = isTRUE(input$generate_normalised_query))
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
            NA_integer_, src, qinfo[[i]]$rule, "INFO",
            paste0("normalisedQuery: '", nms[i], "' -> '", normalised[i], "'.")
          )
        }
      }

      if (any(unres)) {
        for (nm in nms[unres]) {
          add_issue(
            NA_integer_, src, "taxonomy_unresolvable", "INFO",
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
                top_n = input$top_n,
                auto_apply_unique = auto_apply_unique
              )
            } else {
              match_one_gbif(
                input_name = nm,
                normalised_query = q,
                top_n = input$top_n,
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
                NA_integer_, src, "taxonomy_not_found", "WARNING",
                paste0("Not found in the selected database: '", nm, "' (query='", q, "').")
              )
            }

            if (lookup$taxonMatchStatus[i] == "AMBIGUOUS") {
              add_issue(
                NA_integer_, src, "taxonomy_ambiguous", "WARNING",
                paste0("Multiple candidates found for: '", nm, "' (query='", q, "'). Select one in the dropdown.")
              )
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
                apply_choice_to_rows(ii, val)
                add_issue(
                  NA_integer_, src, "taxonomy_manual_resolution",
                  "INFO",
                  paste0(
                    "Row '", rv$lookup$inputName[ii],
                    "' was manually resolved and marked as MATCHED_MANUAL."
                  )
                )
              } else {
                open_choice_popup(ii, val)
              }
            }, ignoreInit = TRUE)
          })
        }

        rv$choice_observers <- obs_list
      }
    })

    shiny::observeEvent(input$choice_apply_this, {
      shiny::req(rv$pending_choice)
      row_index <- rv$pending_choice$row_index
      selected_id <- rv$pending_choice$selected_id
      src <- current_input_col()

      if (isTRUE(input$choice_popup_dont_show)) {
        rv$suppress_choice_popup <- TRUE
      }

      apply_choice_to_rows(row_index, selected_id)
      shiny::removeModal()

      add_issue(
        NA_integer_, src, "taxonomy_manual_resolution",
        "INFO",
        paste0(
          "Row '", rv$lookup$inputName[row_index],
          "' was manually resolved and marked as MATCHED_MANUAL."
        )
      )

      rv$pending_choice <- NULL
    })

    shiny::observeEvent(input$choice_apply_same, {
      shiny::req(rv$pending_choice)
      row_index <- rv$pending_choice$row_index
      selected_id <- rv$pending_choice$selected_id
      src <- current_input_col()

      if (isTRUE(input$choice_popup_dont_show)) {
        rv$suppress_choice_popup <- TRUE
      }

      q <- rv$lookup$normalisedQuery[row_index]
      same_rows <- which(rv$lookup$normalisedQuery == q)

      apply_choice_to_rows(same_rows, selected_id)
      shiny::removeModal()

      add_issue(
        NA_integer_, src, "taxonomy_manual_resolution_same_normalised",
        "INFO",
        paste0(
          "Choice applied to ",
          length(same_rows),
          " row(s) with normalisedQuery='", q, "'."
        )
      )

      rv$pending_choice <- NULL
    })

    compute_out <- function() {
      df <- df_in()
      shiny::req(is.data.frame(df))

      if (is.null(rv$lookup) || !is.data.frame(rv$lookup) || nrow(rv$lookup) == 0) {
        return(df)
      }

      src <- current_input_col()
      shiny::req(!is.null(src), src %in% names(df))

      out <- df
      src_vals <- normalize_ws(out[[src]])

      if (!("verbatimIdentification" %in% names(out))) {
        out$verbatimIdentification <- out[[src]]
      } else {
        blank_vi <- is.na(out$verbatimIdentification) | !nzchar(trimws(as.character(out$verbatimIdentification)))
        out$verbatimIdentification[blank_vi] <- out[[src]][blank_vi]
      }

      if (!("scientificName" %in% names(out))) {
        out$scientificName <- NA_character_
      }

      map_idx <- match(src_vals, rv$lookup$inputName)

      lk_col <- function(col) {
        vals <- rep(NA_character_, nrow(out))
        ok <- !is.na(map_idx) & map_idx >= 1 & map_idx <= nrow(rv$lookup)
        vals[ok] <- as.character(rv$lookup[[col]][map_idx[ok]])
        vals
      }

      status_group <- tax_status_group(lk_col("taxonMatchStatus"))

      if (identical(input$tax_db, "worms")) {
        resolved_name <- lk_col("valid_name")
        matched_name <- lk_col("scientificname")
      } else {
        resolved_name <- lk_col("accepted")
        matched_name <- lk_col("scientificName")
      }

      if (isTRUE(replace_scientificName)) {
        has_matched <- status_group == "MATCHED"
        fill_val <- ifelse(!is.na(resolved_name) & nzchar(resolved_name), resolved_name, matched_name)
        fill_ok <- has_matched & !is.na(fill_val) & nzchar(fill_val)
        out$scientificName[fill_ok] <- fill_val[fill_ok]
      }

      candidate_payload <- if (identical(input$tax_db, "worms")) {
        list(
          scientificName = out$scientificName,
          scientificNameID = lk_col("lsid"),
          acceptedNameUsage = lk_col("valid_name"),
          taxonomicStatus = lk_col("status"),
          taxonRank = lk_col("rank"),
          authority = lk_col("authority"),
          AphiaID = lk_col("AphiaID"),
          url = lk_col("url"),
          scientificname = lk_col("scientificname"),
          status = lk_col("status"),
          unacceptreason = lk_col("unacceptreason"),
          taxonRankID = lk_col("taxonRankID"),
          rank = lk_col("rank"),
          valid_AphiaID = lk_col("valid_AphiaID"),
          valid_name = lk_col("valid_name"),
          valid_authority = lk_col("valid_authority"),
          parentNameUsageID = lk_col("parentNameUsageID"),
          originalNameUsageID = lk_col("originalNameUsageID"),
          kingdom = lk_col("kingdom"),
          phylum = lk_col("phylum"),
          class = lk_col("class"),
          order = lk_col("order"),
          family = lk_col("family"),
          genus = lk_col("genus"),
          citation = lk_col("citation"),
          isMarine = lk_col("isMarine"),
          isBrackish = lk_col("isBrackish"),
          isFreshwater = lk_col("isFreshwater"),
          isTerrestrial = lk_col("isTerrestrial"),
          isExtinct = lk_col("isExtinct"),
          match_type = lk_col("match_type"),
          modified = lk_col("modified"),
          normalisedQuery = lk_col("normalisedQuery"),
          taxonMatchStatus = lk_col("taxonMatchStatus"),
          selectedID = lk_col("selectedID"),
          nameAccordingTo = lk_col("nameAccordingTo")
        )
      } else {
        list(
          scientificName = out$scientificName,
          scientificNameID = lk_col("key"),
          acceptedNameUsage = lk_col("accepted"),
          taxonomicStatus = lk_col("taxonomicStatus"),
          taxonRank = lk_col("rank"),
          authority = lk_col("authorship"),
          nubKey = lk_col("nubKey"),
          canonicalName = lk_col("canonicalName"),
          rank = lk_col("rank"),
          status = lk_col("status"),
          authorship = lk_col("authorship"),
          acceptedKey = lk_col("acceptedKey"),
          accepted = lk_col("accepted"),
          matchType = lk_col("matchType"),
          confidence = lk_col("confidence"),
          kingdom = lk_col("kingdom"),
          phylum = lk_col("phylum"),
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

      out
    }

    df_out <- shiny::reactive({
      if (!is.null(rv$applied_df) && is.data.frame(rv$applied_df)) {
        return(rv$applied_df)
      }
      df_in()
    })

    unresolved_ambiguous_rows <- function() {
      if (is.null(rv$lookup) || !is.data.frame(rv$lookup) || nrow(rv$lookup) == 0) {
        return(integer())
      }
      which(
        rv$lookup$taxonMatchStatus == "AMBIGUOUS" &
          (is.na(rv$lookup$selectedID) | rv$lookup$selectedID == "")
      )
    }

    finalize_apply <- function() {
      out <- compute_out()
      shiny::req(is.data.frame(out))
      rv$applied_df <- out
      rv$ready <- TRUE

      stat_grp <- tax_status_group(rv$lookup$taxonMatchStatus)
      n_matched <- sum(stat_grp == "MATCHED", na.rm = TRUE)
      n_amb <- sum(rv$lookup$taxonMatchStatus == "AMBIGUOUS", na.rm = TRUE)
      n_nf <- sum(rv$lookup$taxonMatchStatus == "NOT_FOUND", na.rm = TRUE)
      n_unres <- sum(rv$lookup$taxonMatchStatus == "UNRESOLVABLE", na.rm = TRUE)

      add_issue(
        NA_integer_, current_input_col(), "taxonomy_applied",
        "INFO",
        paste0(
          "Taxonomy lookup applied to dataset. ",
          "MATCHED=", n_matched,
          " | AMBIGUOUS=", n_amb,
          " | NOT_FOUND=", n_nf,
          " | UNRESOLVABLE=", n_unres,
          if (isTRUE(replace_scientificName)) {
            " | scientificName filled/replaced only for MATCHED cases."
          } else {
            " | Original scientificName preserved."
          }
        )
      )
    }

    shiny::observeEvent(input$apply_match, {
      shiny::req(is.data.frame(df_in()))

      if (is.null(rv$lookup) || !is.data.frame(rv$lookup) || nrow(rv$lookup) == 0) {
        add_issue(
          NA_integer_, current_input_col(), "taxonomy_apply_without_lookup",
          "WARNING",
          "Run the taxonomy match before applying it to the dataset."
        )
        return()
      }

      pending_rows <- unresolved_ambiguous_rows()

      if (length(pending_rows) > 0) {
        open_pending_ambiguous_popup(length(pending_rows))
        add_issue(
          NA_integer_, current_input_col(), "taxonomy_apply_blocked_pending_ambiguous",
          "WARNING",
          paste0(
            "Dataset application blocked because ",
            length(pending_rows),
            " ambiguous row(s) still need a manual choice."
          )
        )
        return()
      }

      finalize_apply()
    })

    output$download_lookup <- shiny::downloadHandler(
      filename = function() {
        paste0("taxonomy_lookup_", input$tax_db, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        lk <- rv$lookup
        if (is.null(lk) || !is.data.frame(lk)) lk <- data.frame()
        utils::write.csv(lk, file = file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    output$lookup_tbl <- DT::renderDT({
      disp <- rv$lookup_display
      if (is.null(disp) || !is.data.frame(disp)) disp <- data.frame()

      DT::datatable(
        disp,
        escape = FALSE,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = TRUE,
          info = FALSE,
          scrollX = TRUE,
          scrollY = "520px",
          scrollCollapse = TRUE,
          autoWidth = TRUE,
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
      ready = shiny::reactive(isTRUE(rv$ready))
    )
  })
}
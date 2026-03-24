# =========================================================
# Taxonomy Match Module (WoRMS/GBIF)
# - input column configurable (default: verbatimIdentification if present)
# - normalisedQuery explicit and always created
# - WoRMS cascade: wm_records_taxamatch() -> wm_records_name(exact) -> wm_records_name(fuzzy)
# - only one lookup row per unique inputName
# - apply button disabled until a lookup exists
# - selectable output columns to keep in final dataset
# - technical columns separated from final columns
# - lookup table with vertical scroll and no pagination
# - safer apply to dataset without left_join explosion
# File: R/mod_taxonomy_match.R
# =========================================================

#' Taxonomy match UI
#' @param id module id
#' @export
mod_taxonomy_match_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$style(shiny::HTML(sprintf("
      #%s .tax-top .card-body { padding: 1rem; }
      #%s .tax-top .card { height: 100%%; }
      #%s .tax-minh { min-height: 300px; }
      #%s .tax-actions .btn { margin-right: .5rem; }
      #%s .tax-scroll-note { color: #6b7280; font-size: .9rem; }
      #%s .tax-muted { color: #6b7280; }
      #%s .tax-small { font-size: .92rem; }
      #%s .tax-cols-wrap { max-height: 220px; overflow-y: auto; padding-right: .5rem; }
      #%s .tax-card-fill { height: 100%%; }
    ",
      ns("root"), ns("root"), ns("root"), ns("root"),
      ns("root"), ns("root"), ns("root"), ns("root"), ns("root")
    ))),

    shiny::tags$div(
      id = ns("root"),

      bslib::card(
        bslib::card_header("Taxonomy match (WoRMS / GBIF)"),
        bslib::card_body(
          shiny::tags$div(
            class = "tax-top",
            bslib::layout_columns(
              col_widths = c(sm = 12, md = 6, lg = 4),
              gap = "1rem",

              bslib::card(
                class = "tax-minh tax-card-fill",
                bslib::card_header("Configuração"),
                bslib::card_body(
                  shiny::uiOutput(ns("input_col_ui")),

                  shiny::radioButtons(
                    inputId = ns("tax_db"),
                    label = "Base de dados",
                    choices = c(
                      "WoRMS (marinho)" = "worms",
                      "GBIF Backbone (terrestre)" = "gbif"
                    ),
                    selected = "worms",
                    inline = FALSE
                  ),

                  shiny::checkboxInput(
                    inputId = ns("use_fuzzy"),
                    label = "Usar fuzzy matching (quando disponível)",
                    value = TRUE
                  ),

                  shiny::checkboxInput(
                    inputId = ns("try_truncate"),
                    label = "Gerar normalisedQuery limpando qualificadores (ex.: 'msp 1', 'indet.', '(juveniles)')",
                    value = TRUE
                  ),

                  shiny::checkboxInput(
                    inputId = ns("auto_apply_unique"),
                    label = "Marcar automaticamente quando houver candidato único/forte",
                    value = TRUE
                  ),

                  shiny::checkboxInput(
                    inputId = ns("replace_scientificName"),
                    label = "Ao aplicar: preencher/substituir scientificName com o nome resolvido (quando houver MATCHED)",
                    value = FALSE
                  ),

                  shiny::numericInput(
                    inputId = ns("top_n"),
                    label = "Top N candidatos (para revisão manual)",
                    value = 5,
                    min = 2,
                    max = 20,
                    step = 1
                  ),

                  shiny::tags$div(
                    class = "tax-actions d-flex gap-2 flex-wrap",
                    shiny::actionButton(ns("run_match"), "Executar match", class = "btn-primary"),
                    shiny::uiOutput(ns("apply_btn_ui"), inline = TRUE)
                  ),

                  shiny::hr(),
                  shiny::uiOutput(ns("pkg_status")),
                  shiny::hr(),
                  shiny::downloadButton(ns("download_lookup"), "Exportar lookup (CSV)")
                )
              ),

              bslib::card(
                class = "tax-minh tax-card-fill",
                bslib::card_header("Colunas a manter no dataset final"),
                bslib::card_body(
                  shiny::tags$div(
                    class = "tax-cols-wrap",
                    shiny::uiOutput(ns("keep_cols_ui"))
                  ),
                  shiny::tags$div(
                    class = "tax-scroll-note mt-2",
                    "Colunas técnicas como normalisedQuery, selectedID, matchMethod e matchScore não entram por defeito no dataset final."
                  )
                )
              ),

              bslib::card(
                class = "tax-minh tax-card-fill",
                bslib::card_header("Resumo"),
                bslib::card_body(
                  shiny::uiOutput(ns("summary_box"))
                )
              )
            )
          ),

          shiny::hr(),

          bslib::card(
            bslib::card_header("Notas"),
            bslib::card_body(
              shiny::tags$ul(
                shiny::tags$li("O match parte de uma coluna de entrada configurável, idealmente verbatimIdentification."),
                shiny::tags$li("normalisedQuery é um campo intermédio: é o texto realmente enviado à base e não entra por defeito no dataset final."),
                shiny::tags$li("WoRMS usa uma cascata: wm_records_taxamatch() -> wm_records_name(exato) -> wm_records_name(fuzzy)."),
                shiny::tags$li("A tabela mostra apenas uma linha por inputName único."),
                shiny::tags$li("Quando há AMBIGUOUS, o choice começa vazio e a escolha manual pode ser propagada a linhas com o mesmo normalisedQuery."),
                shiny::tags$li("Aplicar ao dataset só faz commit do dataframe após o lookup existir.")
              )
            )
          )
        )
      ),

      bslib::card(
        bslib::card_header("Tabela de nomes (lookup)"),
        bslib::card_body(
          DT::DTOutput(ns("lookup_tbl"))
        )
      ),

      bslib::card(
        bslib::card_header("Issues (taxonomia)"),
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
#' @return list(df_out, lookup, issues)
#' @export
mod_taxonomy_match_server <- function(id, df_in) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    `%||%` <- function(x, y) if (is.null(x)) y else x

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

    empty_lookup <- function() {
      data.frame(
        inputName = character(),
        normalisedQuery = character(),
        matchedName = character(),
        scientificNameID = character(),
        taxonomicStatus = character(),
        acceptedNameUsage = character(),
        taxonRank = character(),
        kingdom = character(),
        authority = character(),
        nameAccordingTo = character(),
        taxonMatchStatus = character(),
        matchMethod = character(),
        matchScore = numeric(),
        selectedID = character(),
        stringsAsFactors = FALSE
      )
    }

    technical_cols <- c(
      "inputName",
      "normalisedQuery",
      "taxonMatchStatus",
      "selectedID",
      "matchMethod",
      "matchScore"
    )

    final_match_cols_default <- c(
      "verbatimIdentification",
      "scientificName",
      "scientificNameID",
      "acceptedNameUsage",
      "taxonRank",
      "kingdom",
      "taxonomicStatus",
      "authority",
      "nameAccordingTo"
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
      pending_choice = NULL
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

    # ------------------------------------------------------
    # Package checks
    # ------------------------------------------------------
    has_needed_pkgs <- shiny::reactive({
      if (identical(input$tax_db, "worms")) {
        requireNamespace("worrms", quietly = TRUE)
      } else {
        requireNamespace("rgbif", quietly = TRUE)
      }
    })

    output$pkg_status <- shiny::renderUI({
      ok <- has_needed_pkgs()
      if (ok) {
        shiny::tags$div("Pacotes OK para a base de dados selecionada.")
      } else {
        if (identical(input$tax_db, "worms")) {
          shiny::tags$div("Falta o pacote 'worrms' para usar WoRMS.")
        } else {
          shiny::tags$div("Falta o pacote 'rgbif' para usar GBIF Backbone.")
        }
      }
    })

    # ------------------------------------------------------
    # Generic helpers
    # ------------------------------------------------------
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

    lower_norm <- function(x) {
      tolower(normalize_ws(x))
    }

    safe_num1 <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_real_)
      z <- suppressWarnings(as.numeric(x[1]))
      if (length(z) == 0 || is.na(z)) return(NA_real_)
      z
    }

    safe_col <- function(df, nm, default = NA_character_) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || !(nm %in% names(df))) {
        return(rep(default, if (is.null(df) || !is.data.frame(df)) 0 else nrow(df)))
      }
      as.character(df[[nm]])
    }

    tax_status_group <- function(x) {
      x <- toupper(normalize_ws(x))
      out <- x
      out[grepl("^MATCHED", x)] <- "MATCHED"
      out[!(out %in% c("MATCHED", "AMBIGUOUS", "NOT_FOUND", "UNRESOLVABLE"))] <- out[!(out %in% c("MATCHED", "AMBIGUOUS", "NOT_FOUND", "UNRESOLVABLE"))]
      out
    }

    is_unresolvable <- function(x) {
      if (length(x) == 0 || is.null(x) || is.na(x) || !nzchar(trimws(as.character(x)))) {
        return(TRUE)
      }
      x <- tolower(normalize_ws(x))
      pat <- "(\\bindet\\b|\\bmsp\\b|\\bmsp\\.\\b|\\bsp\\b|\\bsp\\.\\b|\\bcf\\b|\\bcf\\.\\b|\\baff\\b|\\baff\\.\\b|morphotype)"
      grepl(pat, x, perl = TRUE)
    }

    normalize_query_info <- function(name, try_truncate = TRUE) {
      s0 <- normalize_ws(name)

      if (!nzchar(s0)) {
        return(list(
          query = "",
          changed = FALSE,
          rule = "blank",
          extracted = NA_character_,
          identificationQualifier = NA_character_,
          lifeStage = NA_character_,
          sex = NA_character_,
          vitality = NA_character_
        ))
      }

      low <- tolower(s0)

      extracted <- s0
      if (isTRUE(try_truncate)) {
        low2 <- low
        low2 <- gsub("\\([^)]*\\)", " ", low2)
        low2 <- gsub("\\[[^]]*\\]", " ", low2)
        low2 <- gsub("\\b(juvenile|juveniles|adult|adults|larva|larvae|egg|eggs)\\b", " ", low2, perl = TRUE)
        low2 <- gsub("\\b(male|female|unknown sex)\\b", " ", low2, perl = TRUE)
        low2 <- gsub("\\b(alive|dead)\\b", " ", low2, perl = TRUE)
        low2 <- gsub("\\b(indet\\.?|msp\\.?|sp\\.?|cf\\.?|aff\\.?|morphotype)\\b", " ", low2, perl = TRUE)
        low2 <- gsub("\\b\\d+\\b", " ", low2, perl = TRUE)
        low2 <- gsub("\\s*\\.(\\s|$)", " ", low2)
        low2 <- gsub("[,;:]+", " ", low2)
        low2 <- gsub("\\s+", " ", low2)
        low2 <- trimws(low2)
        extracted <- low2
      }

      toks <- strsplit(extracted, " ", fixed = TRUE)[[1]]
      toks <- toks[nzchar(toks)]
      toks <- toks[grepl("[a-z]", toks, perl = TRUE)]

      if (length(toks) >= 2) {
        toks <- toks[1:2]
      }

      if (length(toks) == 0) {
        query <- ""
      } else {
        toks[1] <- paste0(toupper(substr(toks[1], 1, 1)), substr(toks[1], 2, nchar(toks[1])))
        if (length(toks) >= 2) toks[2] <- tolower(toks[2])
        query <- normalize_ws(paste(toks, collapse = " "))
      }

      qual <- NA_character_
      if (grepl("\\bcf\\.?\\b", low, perl = TRUE)) qual <- "cf."
      if (grepl("\\baff\\.?\\b", low, perl = TRUE)) qual <- "aff."
      if (grepl("\\bsp\\.?\\b", low, perl = TRUE)) qual <- "sp."
      if (grepl("\\bmsp\\.?\\b", low, perl = TRUE)) qual <- "msp"
      if (grepl("\\bindet\\b", low, perl = TRUE)) qual <- "indet"

      life_stage <- NA_character_
      if (grepl("\\bjuvenile|juveniles\\b", low, perl = TRUE)) life_stage <- "juvenile"
      if (grepl("\\badult|adults\\b", low, perl = TRUE)) life_stage <- "adult"
      if (grepl("\\blarva|larvae\\b", low, perl = TRUE)) life_stage <- "larva"

      sex <- NA_character_
      if (grepl("\\bmale\\b", low, perl = TRUE)) sex <- "male"
      if (grepl("\\bfemale\\b", low, perl = TRUE)) sex <- "female"

      vitality <- NA_character_
      if (grepl("\\balive\\b", low, perl = TRUE)) vitality <- "alive"
      if (grepl("\\bdead\\b", low, perl = TRUE)) vitality <- "dead"

      list(
        query = query,
        changed = !identical(normalize_ws(query), normalize_ws(s0)),
        rule = if (!identical(normalize_ws(query), normalize_ws(s0))) "truncate_qualifiers" else "none",
        extracted = if (nzchar(query)) query else s0,
        identificationQualifier = qual,
        lifeStage = life_stage,
        sex = sex,
        vitality = vitality
      )
    }

    worms_make_lsid <- function(aphia_id) {
      aphia_id <- to_chr1(aphia_id)
      if (is_blank(aphia_id)) return(NA_character_)
      paste0("urn:lsid:marinespecies.org:taxname:", aphia_id)
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
        return(shiny::tags$div("Sem colunas disponíveis."))
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
        label = "Coluna de entrada para o match",
        choices = cols,
        selected = selected
      )
    })

    output$apply_btn_ui <- shiny::renderUI({
      enabled <- !is.null(rv$lookup) && is.data.frame(rv$lookup) && nrow(rv$lookup) > 0

      shiny::actionButton(
        ns("apply_match"),
        "Aplicar ao dataset",
        class = "btn-success",
        disabled = if (!enabled) "disabled" else NULL
      )
    })

    output$keep_cols_ui <- shiny::renderUI({
      choices <- c(
        "verbatimIdentification",
        "scientificName",
        "scientificNameID",
        "acceptedNameUsage",
        "taxonRank",
        "kingdom",
        "taxonomicStatus",
        "authority",
        "nameAccordingTo",
        "initialTaxon",
        "identificationQualifier",
        "lifeStage",
        "sex",
        "vitality"
      )

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

    cache_key <- function(db, query, use_fuzzy, top_n, auto_apply_unique) {
      paste(db, as.character(use_fuzzy), as.integer(top_n), as.character(auto_apply_unique), query, sep = "||")
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

    # ------------------------------------------------------
    # WoRMS helpers
    # ------------------------------------------------------
    worms_record_by_id <- function(aphia_id) {
      tryCatch(
        worrms::wm_record(as.integer(aphia_id)),
        error = function(e) NULL
      )
    }

    worms_extract_kingdom <- function(rec) {
      if (is.null(rec)) return(NA_character_)
      nm <- names(rec)
      for (k in c("kingdom", "Kingdom")) {
        if (k %in% nm) return(to_chr1(rec[[k]]))
      }
      NA_character_
    }

    worms_normalize_candidates <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(data.frame())

      out <- data.frame(
        AphiaID = safe_col(df, "AphiaID"),
        scientificname = safe_col(df, "scientificname"),
        rank = safe_col(df, "rank"),
        authority = safe_col(df, "authority"),
        status = safe_col(df, "status"),
        valid_name = safe_col(df, "valid_name"),
        kingdom = safe_col(df, "kingdom"),
        match_type = safe_col(df, "match_type"),
        score = suppressWarnings(as.numeric(safe_col(df, "score", default = NA_character_))),
        stringsAsFactors = FALSE
      )

      if ("AphiaID" %in% names(out)) {
        out <- out[!duplicated(out$AphiaID) & !is.na(out$AphiaID) & out$AphiaID != "", , drop = FALSE]
      }
      out
    }

    worms_taxamatch_candidates <- function(query, top_n = 5) {
      res <- tryCatch(
        worrms::wm_records_taxamatch(name = query),
        error = function(e) NULL
      )

      out <- worms_normalize_candidates(res)
      if (!is.data.frame(out) || nrow(out) == 0) return(data.frame())

      qn <- lower_norm(query)
      sn <- lower_norm(out$scientificname)
      vn <- lower_norm(out$valid_name)

      rank_bonus <- ifelse(toupper(out$status) == "ACCEPTED", 3, 0)
      exact_bonus <- ifelse(sn == qn | vn == qn, 5, 0)
      score_raw <- out$score
      score_raw[is.na(score_raw)] <- 0

      ord <- order(-(exact_bonus + rank_bonus + score_raw), sn)
      utils::head(out[ord, , drop = FALSE], as.integer(top_n))
    }

    worms_records_name_candidates <- function(query, use_fuzzy = FALSE, top_n = 5) {
      recs <- tryCatch(
        worrms::wm_records_name(query, fuzzy = isTRUE(use_fuzzy)),
        error = function(e) NULL
      )

      out <- worms_normalize_candidates(recs)
      if (!is.data.frame(out) || nrow(out) == 0) return(data.frame())

      qn <- lower_norm(query)
      sn <- lower_norm(out$scientificname)
      vn <- lower_norm(out$valid_name)

      score <- numeric(nrow(out))
      score[sn == qn] <- score[sn == qn] + 5
      score[vn == qn] <- score[vn == qn] + 4
      score[toupper(out$status) == "ACCEPTED"] <- score[toupper(out$status) == "ACCEPTED"] + 2
      if (isTRUE(use_fuzzy)) score <- score + 0.5

      ord <- order(-score, sn)
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

    match_one_worms <- function(input_name, normalised_query, use_fuzzy = TRUE, top_n = 5, auto_apply_unique = TRUE) {
      key <- cache_key("worms", normalised_query, use_fuzzy, top_n, auto_apply_unique)
      cached <- cache_get(key)

      if (!is.null(cached)) {
        cached$row$inputName <- input_name
        cached$row$normalisedQuery <- normalised_query
        return(cached)
      }

      blank_row <- function(status = "NOT_FOUND", method = NA_character_, score = NA_real_) {
        data.frame(
          inputName = input_name,
          normalisedQuery = normalised_query,
          matchedName = NA_character_,
          scientificNameID = NA_character_,
          taxonomicStatus = NA_character_,
          acceptedNameUsage = NA_character_,
          taxonRank = NA_character_,
          kingdom = NA_character_,
          authority = NA_character_,
          nameAccordingTo = "WoRMS",
          taxonMatchStatus = status,
          matchMethod = method,
          matchScore = score,
          selectedID = NA_character_,
          stringsAsFactors = FALSE
        )
      }

      if (is_blank(normalised_query)) {
        res <- list(row = blank_row(status = "UNRESOLVABLE", method = "blank_query"), candidates = NULL)
        cache_set(key, res)
        return(res)
      }

      cand_taxamatch <- worms_taxamatch_candidates(normalised_query, top_n = top_n)
      cand_exact <- worms_records_name_candidates(normalised_query, use_fuzzy = FALSE, top_n = top_n)
      cand_fuzzy <- if (isTRUE(use_fuzzy)) {
        worms_records_name_candidates(normalised_query, use_fuzzy = TRUE, top_n = top_n)
      } else {
        data.frame()
      }

      all_cand <- rbind(cand_taxamatch, cand_exact, cand_fuzzy)
      if (is.data.frame(all_cand) && nrow(all_cand) > 0) {
        all_cand <- all_cand[!duplicated(all_cand$AphiaID), , drop = FALSE]
      }

      if (!is.data.frame(all_cand) || nrow(all_cand) == 0) {
        res <- list(row = blank_row(status = "NOT_FOUND", method = "worms_none"), candidates = NULL)
        cache_set(key, res)
        return(res)
      }

      qn <- lower_norm(normalised_query)
      sci <- lower_norm(all_cand$scientificname)
      vnm <- lower_norm(all_cand$valid_name)

      rank_score <- ifelse(toupper(all_cand$status) == "ACCEPTED", 3, 0)
      exact_score <- ifelse(sci == qn | vnm == qn, 5, 0)
      base_score <- exact_score + rank_score
      if ("score" %in% names(all_cand)) {
        z <- suppressWarnings(as.numeric(all_cand$score))
        z[is.na(z)] <- 0
        base_score <- base_score + z
      }

      ord <- order(-base_score, sci)
      all_cand <- all_cand[ord, , drop = FALSE]
      top1 <- all_cand[1, , drop = FALSE]

      chosen_method <- "worms_candidates"
      chosen_status <- "AMBIGUOUS"
      chosen_selected <- NA_character_

      unique_candidate <- nrow(all_cand) == 1
      strong_top <- nrow(all_cand) >= 1 && (sci[ord][1] == qn || vnm[ord][1] == qn)

      if ((unique_candidate || strong_top) && isTRUE(auto_apply_unique)) {
        chosen_status <- if (strong_top && !unique_candidate) "MATCHED_AUTO" else "MATCHED_AUTO"
        chosen_selected <- to_chr1(top1$AphiaID)
      }

      rec <- NULL
      if (!is_blank(chosen_selected)) {
        rec <- worms_record_by_id(chosen_selected)
      }

      out_row <- data.frame(
        inputName = input_name,
        normalisedQuery = normalised_query,
        matchedName = to_chr1(top1$scientificname),
        scientificNameID = if (!is_blank(chosen_selected)) worms_make_lsid(chosen_selected) else NA_character_,
        taxonomicStatus = to_chr1(top1$status),
        acceptedNameUsage = to_chr1(top1$valid_name),
        taxonRank = to_chr1(top1$rank),
        kingdom = to_chr1(top1$kingdom %||% if (!is.null(rec) && "kingdom" %in% names(rec)) rec$kingdom else NULL),
        authority = to_chr1(top1$authority),
        nameAccordingTo = "WoRMS",
        taxonMatchStatus = chosen_status,
        matchMethod = chosen_method,
        matchScore = safe_num1(base_score[ord][1]),
        selectedID = chosen_selected,
        stringsAsFactors = FALSE
      )

      if (!is_blank(chosen_selected) && !is.null(rec) && is.data.frame(rec) && nrow(rec) > 0) {
        if ("scientificname" %in% names(rec)) out_row$matchedName <- to_chr1(rec$scientificname)
        if ("status" %in% names(rec)) out_row$taxonomicStatus <- to_chr1(rec$status)
        if ("valid_name" %in% names(rec)) out_row$acceptedNameUsage <- to_chr1(rec$valid_name)
        if ("rank" %in% names(rec)) out_row$taxonRank <- to_chr1(rec$rank)
        if ("authority" %in% names(rec)) out_row$authority <- to_chr1(rec$authority)
        out_row$kingdom <- worms_extract_kingdom(rec)
      }

      res <- list(
        row = out_row,
        candidates = utils::head(all_cand, as.integer(top_n))
      )
      cache_set(key, res)
      res
    }

    # ------------------------------------------------------
    # GBIF helpers
    # ------------------------------------------------------
    gbif_record_by_id <- function(gbif_key) {
      tryCatch(
        rgbif::name_usage(key = as.integer(gbif_key), data = "all"),
        error = function(e) NULL
      )
    }

    gbif_candidates <- function(query, use_fuzzy = TRUE, top_n = 5) {
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
          "accepted", "matchType", "confidence", "kingdom"
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
      out <- out[ord, , drop = FALSE]

      if (!isTRUE(use_fuzzy)) {
        exact_idx <- which(sci == qn | can == qn)
        if (length(exact_idx) > 0) {
          out <- out[exact_idx, , drop = FALSE]
        }
      }

      utils::head(out, n = as.integer(top_n))
    }

    gbif_find_candidate_row <- function(input_name, selected_id) {
      cand <- rv$candidates_map[[input_name]]
      if (is.null(cand) || !is.data.frame(cand) || nrow(cand) == 0) {
        return(NULL)
      }
      if (!("key" %in% names(cand))) return(NULL)

      idx <- which(as.character(cand$key) == as.character(selected_id))
      if (length(idx) == 0) return(NULL)

      cand[idx[1], , drop = FALSE]
    }

    gbif_extract_accepted_name <- function(candidate_row = NULL, record = NULL) {
      if (!is.null(candidate_row) && is.data.frame(candidate_row) && nrow(candidate_row) > 0) {
        acc_txt <- to_chr1(candidate_row$accepted)
        if (!is_blank(acc_txt)) return(acc_txt)

        acc_key <- to_chr1(candidate_row$acceptedKey)
        if (!is_blank(acc_key)) {
          rec_acc <- gbif_record_by_id(acc_key)
          if (!is.null(rec_acc)) {
            nm <- to_chr1(rec_acc$scientificName %||% rec_acc$canonicalName)
            if (!is_blank(nm)) return(nm)
          }
        }
      }

      if (!is.null(record)) {
        acc_key <- to_chr1(record$acceptedKey)
        if (!is_blank(acc_key)) {
          rec_acc <- gbif_record_by_id(acc_key)
          if (!is.null(rec_acc)) {
            nm <- to_chr1(rec_acc$scientificName %||% rec_acc$canonicalName)
            if (!is_blank(nm)) return(nm)
          }
        }
      }

      NA_character_
    }

    match_one_gbif <- function(input_name, normalised_query, use_fuzzy = TRUE, top_n = 5, auto_apply_unique = TRUE) {
      key <- cache_key("gbif", normalised_query, use_fuzzy, top_n, auto_apply_unique)
      cached <- cache_get(key)

      if (!is.null(cached)) {
        cached$row$inputName <- input_name
        cached$row$normalisedQuery <- normalised_query
        return(cached)
      }

      blank_row <- function(status = "NOT_FOUND", method = NA_character_, score = NA_real_) {
        data.frame(
          inputName = input_name,
          normalisedQuery = normalised_query,
          matchedName = NA_character_,
          scientificNameID = NA_character_,
          taxonomicStatus = NA_character_,
          acceptedNameUsage = NA_character_,
          taxonRank = NA_character_,
          kingdom = NA_character_,
          authority = NA_character_,
          nameAccordingTo = "GBIF Backbone",
          taxonMatchStatus = status,
          matchMethod = method,
          matchScore = score,
          selectedID = NA_character_,
          stringsAsFactors = FALSE
        )
      }

      if (is_blank(normalised_query)) {
        res <- list(row = blank_row(status = "UNRESOLVABLE", method = "blank_query"), candidates = NULL)
        cache_set(key, res)
        return(res)
      }

      cand <- gbif_candidates(
        query = normalised_query,
        use_fuzzy = use_fuzzy,
        top_n = top_n
      )

      if (!is.data.frame(cand) || nrow(cand) == 0) {
        res <- list(row = blank_row(status = "NOT_FOUND", method = "gbif_lookup"), candidates = NULL)
        cache_set(key, res)
        return(res)
      }

      one <- cand[1, , drop = FALSE]
      gbif_key <- to_chr1(one$key)

      status_auto <- "AMBIGUOUS"
      if (nrow(cand) == 1 && isTRUE(auto_apply_unique)) {
        mt <- toupper(to_chr1(one$matchType))
        status_auto <- if (mt == "FUZZY") "MATCHED_FUZZY" else "MATCHED_AUTO"
      }

      accepted_name <- gbif_extract_accepted_name(candidate_row = one, record = NULL)

      res <- list(
        row = data.frame(
          inputName = input_name,
          normalisedQuery = normalised_query,
          matchedName = to_chr1(one$scientificName %||% one$canonicalName),
          scientificNameID = gbif_key,
          taxonomicStatus = to_chr1(one$taxonomicStatus %||% one$status),
          acceptedNameUsage = accepted_name,
          taxonRank = to_chr1(one$rank),
          kingdom = to_chr1(one$kingdom),
          authority = to_chr1(one$authorship),
          nameAccordingTo = "GBIF Backbone",
          taxonMatchStatus = status_auto,
          matchMethod = "gbif_name_lookup",
          matchScore = safe_num1(one$confidence),
          selectedID = if (grepl("^MATCHED", status_auto)) gbif_key else NA_character_,
          stringsAsFactors = FALSE
        ),
        candidates = cand
      )
      cache_set(key, res)
      res
    }

    # ------------------------------------------------------
    # Resolve helpers
    # ------------------------------------------------------
    resolve_row_worms <- function(i, selected_id) {
      if (is.null(rv$lookup) || nrow(rv$lookup) < i) return(invisible(FALSE))
      if (is_blank(selected_id)) return(invisible(FALSE))

      cand_row <- worms_find_candidate_row(rv$lookup$inputName[i], selected_id)
      rec <- worms_record_by_id(selected_id)

      rv$lookup$selectedID[i] <- as.character(selected_id)
      rv$lookup$scientificNameID[i] <- worms_make_lsid(selected_id)

      if (!is.null(rec) && is.data.frame(rec) && nrow(rec) > 0) {
        if ("scientificname" %in% names(rec)) rv$lookup$matchedName[i] <- to_chr1(rec$scientificname)
        if ("status" %in% names(rec)) rv$lookup$taxonomicStatus[i] <- to_chr1(rec$status)
        if ("valid_name" %in% names(rec)) rv$lookup$acceptedNameUsage[i] <- to_chr1(rec$valid_name)
        if ("rank" %in% names(rec)) rv$lookup$taxonRank[i] <- to_chr1(rec$rank)
        if ("authority" %in% names(rec)) rv$lookup$authority[i] <- to_chr1(rec$authority)
        rv$lookup$kingdom[i] <- worms_extract_kingdom(rec)
      } else if (!is.null(cand_row) && is.data.frame(cand_row) && nrow(cand_row) > 0) {
        rv$lookup$matchedName[i] <- to_chr1(cand_row$scientificname)
        rv$lookup$taxonomicStatus[i] <- to_chr1(cand_row$status)
        rv$lookup$acceptedNameUsage[i] <- to_chr1(cand_row$valid_name)
        rv$lookup$taxonRank[i] <- to_chr1(cand_row$rank)
        rv$lookup$authority[i] <- to_chr1(cand_row$authority)
        rv$lookup$kingdom[i] <- to_chr1(cand_row$kingdom)
      }

      rv$lookup$taxonMatchStatus[i] <- "MATCHED_MANUAL"
      rv$lookup$matchMethod[i] <- "manual_choice"
      invisible(TRUE)
    }

    resolve_row_gbif <- function(i, selected_id) {
      if (is.null(rv$lookup) || nrow(rv$lookup) < i) return(invisible(FALSE))
      if (is_blank(selected_id)) return(invisible(FALSE))

      input_name <- rv$lookup$inputName[i]
      cand_row <- gbif_find_candidate_row(input_name, selected_id)
      rec <- gbif_record_by_id(selected_id)

      rv$lookup$selectedID[i] <- as.character(selected_id)
      rv$lookup$scientificNameID[i] <- as.character(selected_id)

      rv$lookup$matchedName[i] <- to_chr1(
        (if (!is.null(rec)) rec$scientificName else NULL) %||%
          (if (!is.null(rec)) rec$canonicalName else NULL) %||%
          (if (!is.null(cand_row)) cand_row$scientificName else NULL) %||%
          (if (!is.null(cand_row)) cand_row$canonicalName else NULL)
      )

      rv$lookup$taxonomicStatus[i] <- to_chr1(
        (if (!is.null(rec)) rec$taxonomicStatus else NULL) %||%
          (if (!is.null(rec)) rec$status else NULL) %||%
          (if (!is.null(cand_row)) cand_row$taxonomicStatus else NULL) %||%
          (if (!is.null(cand_row)) cand_row$status else NULL)
      )

      rv$lookup$acceptedNameUsage[i] <- gbif_extract_accepted_name(
        candidate_row = cand_row,
        record = rec
      )

      rv$lookup$taxonRank[i] <- to_chr1(
        (if (!is.null(rec)) rec$rank else NULL) %||%
          (if (!is.null(cand_row)) cand_row$rank else NULL)
      )

      rv$lookup$kingdom[i] <- to_chr1(
        (if (!is.null(rec)) rec$kingdom else NULL) %||%
          (if (!is.null(cand_row)) cand_row$kingdom else NULL)
      )

      rv$lookup$authority[i] <- to_chr1(
        (if (!is.null(rec)) rec$authorship else NULL) %||%
          (if (!is.null(cand_row)) cand_row$authorship else NULL)
      )

      rv$lookup$taxonMatchStatus[i] <- "MATCHED_MANUAL"
      rv$lookup$matchMethod[i] <- "manual_choice"
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
      invisible(TRUE)
    }

    get_candidate_table <- function(i) {
      if (is.null(rv$lookup) || nrow(rv$lookup) < i) return(NULL)
      nm <- rv$lookup$inputName[i]
      rv$candidates_map[[nm]]
    }

    get_first_candidate_id <- function(i) {
      cand <- get_candidate_table(i)
      if (is.null(cand) || !is.data.frame(cand) || nrow(cand) == 0) return(NA_character_)

      if (identical(input$tax_db, "worms")) {
        if (!("AphiaID" %in% names(cand))) return(NA_character_)
        return(as.character(cand$AphiaID[1]))
      }

      if (!("key" %in% names(cand))) return(NA_character_)
      as.character(cand$key[1])
    }

    # ------------------------------------------------------
    # Display table with empty choice
    # ------------------------------------------------------
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

          lab <- paste0(
            ids, " — ",
            sci,
            ifelse(rk != "", paste0(" [", rk, "]"), ""),
            ifelse(st != "", paste0(" (", st, ")"), ""),
            ifelse(vn != "", paste0(" | valid: ", vn), ""),
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

        choices <- c("-- selecionar candidato --" = "", stats::setNames(ids, lab))
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

      disp
    }

    # ------------------------------------------------------
    # Summary
    # ------------------------------------------------------
    output$summary_box <- shiny::renderUI({
      nms <- base_names()
      src <- current_input_col()

      if (length(nms) == 0) {
        return(shiny::tags$div("Nenhum nome disponível na coluna escolhida."))
      }

      lk <- rv$lookup

      if (is.null(lk) || !is.data.frame(lk) || nrow(lk) == 0) {
        unres <- vapply(nms, function(x) is_blank(normalize_query_info(x, try_truncate = isTRUE(input$try_truncate))$query), logical(1))
        return(
          shiny::tags$div(
            shiny::tags$p(paste0("Coluna de entrada: ", src)),
            shiny::tags$p(paste0("Nomes únicos: ", length(nms))),
            shiny::tags$p(paste0("UNRESOLVABLE (pré-match): ", sum(unres))),
            shiny::tags$p("MATCHED: 0 | AMBIGUOUS: 0 | NOT_FOUND: 0"),
            shiny::tags$p("Dataset aplicado: NÃO")
          )
        )
      }

      stat_grp <- tax_status_group(lk$taxonMatchStatus)
      n_matched <- sum(stat_grp == "MATCHED", na.rm = TRUE)
      n_amb <- sum(lk$taxonMatchStatus == "AMBIGUOUS", na.rm = TRUE)
      n_nf <- sum(lk$taxonMatchStatus == "NOT_FOUND", na.rm = TRUE)
      n_unres <- sum(lk$taxonMatchStatus == "UNRESOLVABLE", na.rm = TRUE)

      shiny::tags$div(
        shiny::tags$p(paste0("Coluna de entrada: ", src)),
        shiny::tags$p(paste0("Nomes únicos: ", length(nms))),
        shiny::tags$p(paste0("UNRESOLVABLE: ", n_unres)),
        shiny::tags$p(paste0("MATCHED: ", n_matched, " | AMBIGUOUS: ", n_amb, " | NOT_FOUND: ", n_nf)),
        shiny::tags$p(
          paste0(
            "Dataset aplicado: ",
            if (!is.null(rv$applied_df) && is.data.frame(rv$applied_df)) "SIM" else "NÃO"
          )
        )
      )
    })

    # ------------------------------------------------------
    # Choice popup
    # ------------------------------------------------------
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
          title = "Confirmar escolha taxonómica",
          shiny::tags$p(
            paste0(
              "O candidato selecionado para '", nm,
              "' irá marcar esta linha como MATCHED_MANUAL."
            )
          ),
          shiny::tags$p(paste0("normalisedQuery: ", q)),
          if (length(same_nq) > 0) {
            shiny::tags$p(
              paste0(
                "Existem ",
                length(same_nq),
                " outra(s) linha(s) com o mesmo normalisedQuery. Deseja aplicar o mesmo choice a todas elas?"
              )
            )
          } else {
            shiny::tags$p("Não existem outras linhas com o mesmo normalisedQuery.")
          },
          shiny::checkboxInput(
            ns("choice_popup_dont_show"),
            "Não mostrar novamente nesta sessão",
            value = FALSE
          ),
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Cancelar"),
            shiny::actionButton(ns("choice_apply_this"), "Aplicar apenas nesta linha", class = "btn-primary"),
            shiny::actionButton(
              ns("choice_apply_same"),
              "Aplicar também aos mesmos normalisedQuery",
              class = "btn-success"
            )
          )
        )
      )
    }

    # ------------------------------------------------------
    # Apply pending ambiguous popup
    # ------------------------------------------------------
    open_apply_pending_popup <- function(n_pending) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Existem casos AMBIGUOUS sem escolha",
          shiny::tags$p(
            paste0(
              "Existem ",
              n_pending,
              " linha(s) AMBIGUOUS ainda sem choice manual."
            )
          ),
          shiny::tags$p(
            "Deseja usar automaticamente a primeira opção do dropdown para esses casos antes de aplicar ao dataset?"
          ),
          shiny::tags$p(
            "Também é possível continuar e manter esses casos como AMBIGUOUS."
          ),
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Cancelar"),
            shiny::actionButton(
              ns("apply_keep_ambiguous"),
              "Manter AMBIGUOUS e aplicar",
              class = "btn-secondary"
            ),
            shiny::actionButton(
              ns("apply_first_choices"),
              "Usar a primeira opção e aplicar",
              class = "btn-warning"
            )
          )
        )
      )
    }

    # ------------------------------------------------------
    # Execute match
    # ------------------------------------------------------
    shiny::observeEvent(input$run_match, {
      destroy_choice_observers()

      rv$issues <- empty_issues()
      rv$lookup <- NULL
      rv$lookup_display <- NULL
      rv$candidates_map <- list()
      rv$applied_df <- NULL
      rv$pending_choice <- NULL

      df <- df_in()
      shiny::req(is.data.frame(df))

      src <- current_input_col()
      shiny::req(!is.null(src), src %in% names(df))

      if (!has_needed_pkgs()) {
        add_issue(
          NA_integer_, src, "taxonomy_missing_pkg", "ERROR",
          "Pacote necessário não está instalado para a base selecionada."
        )
        return()
      }

      nms <- base_names()
      if (length(nms) == 0) {
        add_issue(
          NA_integer_, src, "taxonomy_no_names", "WARNING",
          "Sem nomes para validar na coluna selecionada."
        )
        return()
      }

      qinfo <- lapply(nms, function(nm) {
        normalize_query_info(nm, try_truncate = isTRUE(input$try_truncate))
      })

      normalised <- vapply(qinfo, `[[`, character(1), "query")
      unres <- !nzchar(normalised)

      lookup <- empty_lookup()
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
            paste0("Nome não resolvível automaticamente: '", nm, "'.")
          )
        }
      }

      to_do <- which(lookup$taxonMatchStatus == "PENDING")

      if (length(to_do) > 0) {
        shiny::withProgress(message = "A validar nomes...", value = 0, {
          for (k in seq_along(to_do)) {
            i <- to_do[k]
            nm <- lookup$inputName[i]
            q <- lookup$normalisedQuery[i]
            shiny::incProgress(1 / length(to_do), detail = q)

            res <- if (identical(input$tax_db, "worms")) {
              match_one_worms(
                input_name = nm,
                normalised_query = q,
                use_fuzzy = input$use_fuzzy,
                top_n = input$top_n,
                auto_apply_unique = input$auto_apply_unique
              )
            } else {
              match_one_gbif(
                input_name = nm,
                normalised_query = q,
                use_fuzzy = input$use_fuzzy,
                top_n = input$top_n,
                auto_apply_unique = input$auto_apply_unique
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
                paste0("Não encontrado na base selecionada: '", nm, "' (query='", q, "').")
              )
            }

            if (lookup$taxonMatchStatus[i] == "AMBIGUOUS") {
              add_issue(
                NA_integer_, src, "taxonomy_ambiguous", "WARNING",
                paste0("Múltiplos candidatos para: '", nm, "' (query='", q, "'). Seleciona no dropdown.")
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
                    "Linha '", rv$lookup$inputName[ii],
                    "' resolvida manualmente e marcada como MATCHED_MANUAL."
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

    # ------------------------------------------------------
    # Choice popup actions
    # ------------------------------------------------------
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
          "Linha '", rv$lookup$inputName[row_index],
          "' resolvida manualmente e marcada como MATCHED_MANUAL."
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
          "Choice aplicado a ",
          length(same_rows),
          " linha(s) com normalisedQuery='", q, "'."
        )
      )

      rv$pending_choice <- NULL
    })

    # ------------------------------------------------------
    # Compute output only when apply is confirmed
    # ------------------------------------------------------
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

      # preserve verbatimIdentification
      if (!("verbatimIdentification" %in% names(out))) {
        out$verbatimIdentification <- out[[src]]
      } else {
        blank_vi <- is.na(out$verbatimIdentification) | !nzchar(trimws(as.character(out$verbatimIdentification)))
        out$verbatimIdentification[blank_vi] <- out[[src]][blank_vi]
      }

      # preserve scientificName if absent
      if (!("scientificName" %in% names(out))) {
        out$scientificName <- NA_character_
      }

      # initialTaxon and parsed qualifiers from source text
      parsed_info <- lapply(src_vals, function(x) normalize_query_info(x, try_truncate = isTRUE(input$try_truncate)))
      initialTaxon <- vapply(parsed_info, `[[`, character(1), "query")
      identificationQualifier <- vapply(parsed_info, function(z) to_chr1(z$identificationQualifier), character(1))
      lifeStage <- vapply(parsed_info, function(z) to_chr1(z$lifeStage), character(1))
      sex <- vapply(parsed_info, function(z) to_chr1(z$sex), character(1))
      vitality <- vapply(parsed_info, function(z) to_chr1(z$vitality), character(1))

      # safe rowwise mapping by source value -> lookup row
      map_idx <- match(src_vals, rv$lookup$inputName)

      # derived columns from lookup
      lk_col <- function(col) {
        vals <- rep(NA_character_, nrow(out))
        ok <- !is.na(map_idx) & map_idx >= 1 & map_idx <= nrow(rv$lookup)
        vals[ok] <- as.character(rv$lookup[[col]][map_idx[ok]])
        vals
      }

      lk_num_col <- function(col) {
        vals <- rep(NA_real_, nrow(out))
        ok <- !is.na(map_idx) & map_idx >= 1 & map_idx <= nrow(rv$lookup)
        vals[ok] <- suppressWarnings(as.numeric(rv$lookup[[col]][map_idx[ok]]))
        vals
      }

      resolved_name <- lk_col("acceptedNameUsage")
      matched_name <- lk_col("matchedName")
      status_group <- tax_status_group(lk_col("taxonMatchStatus"))

      # Only write scientificName when MATCHED and checkbox enabled
      if (isTRUE(input$replace_scientificName)) {
        has_matched <- status_group == "MATCHED"
        fill_val <- ifelse(!is.na(resolved_name) & nzchar(resolved_name), resolved_name, matched_name)
        fill_ok <- has_matched & !is.na(fill_val) & nzchar(fill_val)
        out$scientificName[fill_ok] <- fill_val[fill_ok]
      }

      # candidate columns (only added if selected)
      selected_keep <- input$keep_output_cols %||% final_match_cols_default

      candidate_payload <- list(
        verbatimIdentification = out$verbatimIdentification,
        scientificName = out$scientificName,
        scientificNameID = lk_col("scientificNameID"),
        acceptedNameUsage = lk_col("acceptedNameUsage"),
        taxonRank = lk_col("taxonRank"),
        kingdom = lk_col("kingdom"),
        taxonomicStatus = lk_col("taxonomicStatus"),
        authority = lk_col("authority"),
        nameAccordingTo = lk_col("nameAccordingTo"),
        initialTaxon = initialTaxon,
        identificationQualifier = identificationQualifier,
        lifeStage = lifeStage,
        sex = sex,
        vitality = vitality,
        normalisedQuery = lk_col("normalisedQuery"),
        taxonMatchStatus = lk_col("taxonMatchStatus"),
        matchMethod = lk_col("matchMethod"),
        matchScore = lk_num_col("matchScore"),
        selectedID = lk_col("selectedID")
      )

      keep_payload <- names(candidate_payload)[names(candidate_payload) %in% selected_keep]

      for (nm in keep_payload) {
        if (nm == "verbatimIdentification") {
          out$verbatimIdentification <- candidate_payload[[nm]]
        } else if (nm == "scientificName") {
          out$scientificName <- candidate_payload[[nm]]
        } else {
          out[[nm]] <- candidate_payload[[nm]]
        }
      }

      # never push technical columns unless explicitly selected
      tech_not_selected <- setdiff(technical_cols, selected_keep)
      tech_not_selected <- intersect(tech_not_selected, names(out))
      if (length(tech_not_selected) > 0) {
        out[tech_not_selected] <- NULL
      }

      out
    }

    # df_out só muda após APPLY real
    df_out <- shiny::reactive({
      if (!is.null(rv$applied_df) && is.data.frame(rv$applied_df)) {
        return(rv$applied_df)
      }
      df_in()
    })

    # ------------------------------------------------------
    # Apply helpers
    # ------------------------------------------------------
    unresolved_ambiguous_rows <- function() {
      if (is.null(rv$lookup) || !is.data.frame(rv$lookup) || nrow(rv$lookup) == 0) {
        return(integer())
      }
      which(
        rv$lookup$taxonMatchStatus == "AMBIGUOUS" &
          (is.na(rv$lookup$selectedID) | rv$lookup$selectedID == "")
      )
    }

    apply_first_choice_to_pending_ambiguous <- function() {
      rows <- unresolved_ambiguous_rows()
      if (length(rows) == 0) return(invisible(FALSE))

      applied_n <- 0L

      for (i in rows) {
        first_id <- get_first_candidate_id(i)
        if (is_blank(first_id)) next

        apply_choice_to_rows(i, first_id)
        applied_n <- applied_n + 1L
      }

      add_issue(
        NA_integer_, current_input_col(), "taxonomy_apply_first_choice_pending",
        "WARNING",
        paste0(
          "Ao aplicar ao dataset, a primeira opção do dropdown foi usada automaticamente em ",
          applied_n,
          " linha(s) AMBIGUOUS sem clique manual."
        )
      )

      invisible(TRUE)
    }

    finalize_apply <- function() {
      out <- compute_out()
      shiny::req(is.data.frame(out))
      rv$applied_df <- out

      stat_grp <- tax_status_group(rv$lookup$taxonMatchStatus)
      n_matched <- sum(stat_grp == "MATCHED", na.rm = TRUE)
      n_amb <- sum(rv$lookup$taxonMatchStatus == "AMBIGUOUS", na.rm = TRUE)
      n_nf <- sum(rv$lookup$taxonMatchStatus == "NOT_FOUND", na.rm = TRUE)
      n_unres <- sum(rv$lookup$taxonMatchStatus == "UNRESOLVABLE", na.rm = TRUE)

      add_issue(
        NA_integer_, current_input_col(), "taxonomy_applied",
        "INFO",
        paste0(
          "Lookup de taxonomia aplicado ao dataset. ",
          "MATCHED=", n_matched,
          " | AMBIGUOUS=", n_amb,
          " | NOT_FOUND=", n_nf,
          " | UNRESOLVABLE=", n_unres,
          if (isTRUE(input$replace_scientificName)) {
            " | scientificName preenchido/substituído apenas para casos MATCHED."
          } else {
            " | scientificName original preservado."
          }
        )
      )
    }

    # ------------------------------------------------------
    # Apply button
    # ------------------------------------------------------
    shiny::observeEvent(input$apply_match, {
      shiny::req(is.data.frame(df_in()))

      if (is.null(rv$lookup) || !is.data.frame(rv$lookup) || nrow(rv$lookup) == 0) {
        add_issue(
          NA_integer_, current_input_col(), "taxonomy_apply_without_lookup",
          "WARNING",
          "Executa primeiro o match antes de aplicar ao dataset."
        )
        return()
      }

      pending_rows <- unresolved_ambiguous_rows()

      if (length(pending_rows) > 0) {
        open_apply_pending_popup(length(pending_rows))
        return()
      }

      finalize_apply()
    })

    shiny::observeEvent(input$apply_first_choices, {
      shiny::removeModal()
      apply_first_choice_to_pending_ambiguous()
      finalize_apply()
    })

    shiny::observeEvent(input$apply_keep_ambiguous, {
      shiny::removeModal()

      n_pending <- length(unresolved_ambiguous_rows())
      add_issue(
        NA_integer_, current_input_col(), "taxonomy_keep_ambiguous_on_apply",
        "WARNING",
        paste0(
          "Aplicação ao dataset concluída mantendo ",
          n_pending,
          " linha(s) como AMBIGUOUS sem escolha manual."
        )
      )

      finalize_apply()
    })

    # ------------------------------------------------------
    # Export lookup CSV
    # ------------------------------------------------------
    output$download_lookup <- shiny::downloadHandler(
      filename = function() {
        paste0("taxonomy_lookup_", Sys.Date(), ".csv")
      },
      content = function(file) {
        lk <- rv$lookup
        if (is.null(lk) || !is.data.frame(lk)) lk <- data.frame()
        utils::write.csv(lk, file = file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # ------------------------------------------------------
    # Tables
    # ------------------------------------------------------
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
      issues = shiny::reactive(rv$issues)
    )
  })
}
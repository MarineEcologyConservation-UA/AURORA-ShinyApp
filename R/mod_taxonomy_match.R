# =========================================================
# Taxonomy Match Module (WoRMS/GBIF) + AMBIGUOUS + Cache
# + normalisedQuery (truncation) + Export CSV
# + choice starts empty
# + popup on choice selection
# + apply/commit to dataset
# + GBIF refactored to same pattern as WoRMS
# File: R/mod_taxonomy_match.R
# =========================================================

#' Taxonomy match UI
#' @param id module id
#' @export
mod_taxonomy_match_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$style(shiny::HTML(sprintf("
      /* Taxonomy module tweaks */
      #%s .tax-top .card-body { padding: 1rem; }
      #%s .tax-top .card { height: 100%%; }
      #%s .tax-minh { min-height: 260px; }
      #%s .tax-actions .btn { margin-right: .5rem; }
    ", ns("root"), ns("root"), ns("root"), ns("root")))),

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
                class = "tax-minh",
                bslib::card_header("Configuração"),
                bslib::card_body(
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
                    label = "Tentar truncar nomes incompletos (ex.: 'msp 1', 'indet.')",
                    value = TRUE
                  ),

                  shiny::checkboxInput(
                    inputId = ns("auto_apply_unique"),
                    label = "Marcar como MATCHED automaticamente quando houver candidato único",
                    value = TRUE
                  ),

                  shiny::checkboxInput(
                    inputId = ns("replace_scientificName"),
                    label = "Ao aplicar: substituir scientificName pelo nome válido (quando existir)",
                    value = FALSE
                  ),

                  shiny::numericInput(
                    inputId = ns("top_n"),
                    label = "Top N candidatos (apenas para AMBIGUOUS)",
                    value = 5,
                    min = 2,
                    max = 20,
                    step = 1
                  ),

                  shiny::tags$div(
                    class = "tax-actions d-flex gap-2 flex-wrap",
                    shiny::actionButton(ns("run_match"), "Executar match", class = "btn-primary"),
                    shiny::actionButton(ns("apply_match"), "Aplicar ao dataset", class = "btn-success")
                  ),

                  shiny::hr(),
                  shiny::uiOutput(ns("pkg_status")),
                  shiny::hr(),
                  shiny::downloadButton(ns("download_lookup"), "Exportar lookup (CSV)")
                )
              ),

              bslib::card(
                class = "tax-minh",
                bslib::card_header("Resumo"),
                bslib::card_body(
                  shiny::uiOutput(ns("summary_box"))
                )
              ),

              bslib::card(
                class = "tax-minh",
                bslib::card_header("Notas"),
                bslib::card_body(
                  shiny::tags$ul(
                    shiny::tags$li("normalisedQuery indica o texto realmente enviado à base (pode ser truncado)."),
                    shiny::tags$li("AMBIGUOUS aparece quando existem múltiplos candidatos; o choice começa vazio."),
                    shiny::tags$li("Ao escolher um candidato, abre popup de confirmação."),
                    shiny::tags$li("WoRMS e GBIF seguem agora a mesma lógica de candidatos e resolução manual."),
                    shiny::tags$li("Aplicar ao dataset faz commit real do dataframe que seguirá para o build_dwca_tables.")
                  )
                )
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
    # Helpers
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

    is_unresolvable <- function(x) {
      if (length(x) == 0 || is.null(x) || is.na(x) || !nzchar(trimws(as.character(x)))) {
        return(TRUE)
      }
      x <- tolower(normalize_ws(x))
      pat <- "(\\bindet\\b|\\bmsp\\b|\\bmsp\\.\\b|\\bsp\\b|\\bsp\\.\\b|\\bcf\\b|\\bcf\\.\\b|\\baff\\b|\\baff\\.\\b|morphotype)"
      grepl(pat, x, perl = TRUE)
    }

    truncate_query <- function(name) {
      s0 <- normalize_ws(name)
      s <- s0

      s <- gsub("[,;]+$", "", s)
      s <- gsub("\\s+", " ", s)
      s <- trimws(s)

      low <- tolower(s)

      low <- gsub("\\([^)]*\\)", " ", low)
      low <- gsub("\\[[^]]*\\]", " ", low)
      low <- gsub("\\b(indet\\.?|msp\\.?|sp\\.?|cf\\.?|aff\\.?|morphotype)\\b", " ", low, perl = TRUE)
      low <- gsub("\\b\\d+\\b", " ", low, perl = TRUE)
      low <- gsub("\\s*\\.(\\s|$)", " ", low)
      low <- gsub("[,;:]+", " ", low)
      low <- gsub("\\s+", " ", low)
      low <- trimws(low)

      if (!nzchar(low)) {
        return(list(query = s0, changed = FALSE, rule = "none"))
      }

      toks <- strsplit(low, " ", fixed = TRUE)[[1]]
      toks <- toks[nzchar(toks)]
      toks <- toks[grepl("[a-z]", toks, perl = TRUE)]

      if (length(toks) == 0) {
        return(list(query = s0, changed = FALSE, rule = "none"))
      }

      if (length(toks) >= 2) toks <- toks[1:2]

      toks[1] <- paste0(toupper(substr(toks[1], 1, 1)), substr(toks[1], 2, nchar(toks[1])))
      if (length(toks) >= 2) toks[2] <- tolower(toks[2])

      q <- normalize_ws(paste(toks, collapse = " "))

      list(
        query = if (nzchar(q)) q else s0,
        changed = !identical(normalize_ws(q), normalize_ws(s0)),
        rule = if (!identical(normalize_ws(q), normalize_ws(s0))) "truncate_qualifiers" else "none"
      )
    }

    base_names <- shiny::reactive({
      df <- df_in()
      shiny::req(is.data.frame(df))

      if (!("scientificName" %in% names(df))) return(character())

      x <- normalize_ws(df$scientificName)
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
    worms_candidates <- function(query, use_fuzzy = TRUE, top_n = 5) {
      recs <- tryCatch(
        worrms::wm_records_name(query, fuzzy = isTRUE(use_fuzzy)),
        error = function(e) NULL
      )

      if (is.null(recs) || !is.data.frame(recs) || nrow(recs) == 0) {
        return(data.frame())
      }

      keep <- intersect(
        names(recs),
        c("AphiaID", "scientificname", "rank", "authority", "status", "valid_name")
      )

      out <- recs[, keep, drop = FALSE]

      if ("AphiaID" %in% names(out)) {
        out <- out[!duplicated(out$AphiaID), , drop = FALSE]
      }

      utils::head(out, n = as.integer(top_n))
    }

    worms_record_by_id <- function(aphia_id) {
      tryCatch(
        worrms::wm_record(as.integer(aphia_id)),
        error = function(e) NULL
      )
    }

    match_one_worms <- function(input_name, normalised_query, use_fuzzy = TRUE, top_n = 5, auto_apply_unique = TRUE) {
      key <- cache_key("worms", normalised_query, use_fuzzy, top_n, auto_apply_unique)
      cached <- cache_get(key)

      if (!is.null(cached)) {
        cached$row$inputName <- input_name
        cached$row$normalisedQuery <- normalised_query
        return(cached)
      }

      cand <- worms_candidates(normalised_query, use_fuzzy = use_fuzzy, top_n = top_n)

      if (!is.data.frame(cand) || nrow(cand) == 0) {
        res <- list(
          row = data.frame(
            inputName = input_name,
            normalisedQuery = normalised_query,
            matchedName = NA_character_,
            scientificNameID = NA_character_,
            taxonomicStatus = NA_character_,
            acceptedNameUsage = NA_character_,
            taxonRank = NA_character_,
            authority = NA_character_,
            nameAccordingTo = "WoRMS",
            taxonMatchStatus = "NOT_FOUND",
            selectedID = NA_character_,
            stringsAsFactors = FALSE
          ),
          candidates = NULL
        )
        cache_set(key, res)
        return(res)
      }

      if (nrow(cand) == 1) {
        one <- cand[1, , drop = FALSE]
        aphia <- if ("AphiaID" %in% names(one)) as.character(one$AphiaID) else NA_character_
        status_auto <- if (isTRUE(auto_apply_unique)) "MATCHED" else "AMBIGUOUS"

        res <- list(
          row = data.frame(
            inputName = input_name,
            normalisedQuery = normalised_query,
            matchedName = if ("scientificname" %in% names(one)) as.character(one$scientificname) else NA_character_,
            scientificNameID = aphia,
            taxonomicStatus = if ("status" %in% names(one)) as.character(one$status) else NA_character_,
            acceptedNameUsage = if ("valid_name" %in% names(one)) as.character(one$valid_name) else NA_character_,
            taxonRank = if ("rank" %in% names(one)) as.character(one$rank) else NA_character_,
            authority = if ("authority" %in% names(one)) as.character(one$authority) else NA_character_,
            nameAccordingTo = "WoRMS",
            taxonMatchStatus = status_auto,
            selectedID = if (isTRUE(auto_apply_unique)) aphia else NA_character_,
            stringsAsFactors = FALSE
          ),
          candidates = cand
        )
        cache_set(key, res)
        return(res)
      }

      res <- list(
        row = data.frame(
          inputName = input_name,
          normalisedQuery = normalised_query,
          matchedName = NA_character_,
          scientificNameID = NA_character_,
          taxonomicStatus = NA_character_,
          acceptedNameUsage = NA_character_,
          taxonRank = NA_character_,
          authority = NA_character_,
          nameAccordingTo = "WoRMS",
          taxonMatchStatus = "AMBIGUOUS",
          selectedID = NA_character_,
          stringsAsFactors = FALSE
        ),
        candidates = cand
      )
      cache_set(key, res)
      res
    }

    # ------------------------------------------------------
    # GBIF helpers (refactored like WoRMS)
    # ------------------------------------------------------
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
          "accepted", "matchType", "confidence"
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

      ord <- order(-score, -conf)
      out <- out[ord, , drop = FALSE]

      if (!isTRUE(use_fuzzy)) {
        exact_idx <- which(
          sci == qn |
            can == qn
        )
        if (length(exact_idx) > 0) {
          out <- out[exact_idx, , drop = FALSE]
        }
      }

      utils::head(out, n = as.integer(top_n))
    }

    gbif_record_by_id <- function(gbif_key) {
      tryCatch(
        rgbif::name_usage(key = as.integer(gbif_key), data = "all"),
        error = function(e) NULL
      )
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
      # 1) try accepted/scientific fields from candidate row
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

      # 2) try acceptedKey from detailed record
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

      cand <- gbif_candidates(
        query = normalised_query,
        use_fuzzy = use_fuzzy,
        top_n = top_n
      )

      if (!is.data.frame(cand) || nrow(cand) == 0) {
        res <- list(
          row = data.frame(
            inputName = input_name,
            normalisedQuery = normalised_query,
            matchedName = NA_character_,
            scientificNameID = NA_character_,
            taxonomicStatus = NA_character_,
            acceptedNameUsage = NA_character_,
            taxonRank = NA_character_,
            authority = NA_character_,
            nameAccordingTo = "GBIF Backbone",
            taxonMatchStatus = "NOT_FOUND",
            selectedID = NA_character_,
            stringsAsFactors = FALSE
          ),
          candidates = NULL
        )
        cache_set(key, res)
        return(res)
      }

      if (nrow(cand) == 1) {
        one <- cand[1, , drop = FALSE]
        gbif_key <- to_chr1(one$key)
        status_auto <- if (isTRUE(auto_apply_unique)) "MATCHED" else "AMBIGUOUS"

        res <- list(
          row = data.frame(
            inputName = input_name,
            normalisedQuery = normalised_query,
            matchedName = to_chr1(one$scientificName %||% one$canonicalName),
            scientificNameID = gbif_key,
            taxonomicStatus = to_chr1(one$taxonomicStatus %||% one$status),
            acceptedNameUsage = gbif_extract_accepted_name(candidate_row = one, record = NULL),
            taxonRank = to_chr1(one$rank),
            authority = to_chr1(one$authorship),
            nameAccordingTo = "GBIF Backbone",
            taxonMatchStatus = status_auto,
            selectedID = if (isTRUE(auto_apply_unique)) gbif_key else NA_character_,
            stringsAsFactors = FALSE
          ),
          candidates = cand
        )
        cache_set(key, res)
        return(res)
      }

      res <- list(
        row = data.frame(
          inputName = input_name,
          normalisedQuery = normalised_query,
          matchedName = NA_character_,
          scientificNameID = NA_character_,
          taxonomicStatus = NA_character_,
          acceptedNameUsage = NA_character_,
          taxonRank = NA_character_,
          authority = NA_character_,
          nameAccordingTo = "GBIF Backbone",
          taxonMatchStatus = "AMBIGUOUS",
          selectedID = NA_character_,
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

      rec <- worms_record_by_id(selected_id)
      if (is.null(rec) || !is.data.frame(rec) || nrow(rec) == 0) {
        return(invisible(FALSE))
      }

      rv$lookup$selectedID[i] <- as.character(selected_id)
      rv$lookup$scientificNameID[i] <- as.character(selected_id)

      if ("scientificname" %in% names(rec)) rv$lookup$matchedName[i] <- as.character(rec$scientificname[1])
      if ("status" %in% names(rec)) rv$lookup$taxonomicStatus[i] <- as.character(rec$status[1])
      if ("valid_name" %in% names(rec)) rv$lookup$acceptedNameUsage[i] <- as.character(rec$valid_name[1])
      if ("rank" %in% names(rec)) rv$lookup$taxonRank[i] <- as.character(rec$rank[1])
      if ("authority" %in% names(rec)) rv$lookup$authority[i] <- as.character(rec$authority[1])

      rv$lookup$taxonMatchStatus[i] <- "MATCHED"
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

      rv$lookup$authority[i] <- to_chr1(
        (if (!is.null(rec)) rec$authorship else NULL) %||%
          (if (!is.null(cand_row)) cand_row$authorship else NULL)
      )

      rv$lookup$taxonMatchStatus[i] <- "MATCHED"
      invisible(TRUE)
    }

    apply_choice_to_rows <- function(row_ids, selected_id) {
      if (length(row_ids) == 0) return(invisible(FALSE))

      if (identical(input$tax_db, "worms")) {
        for (ii in row_ids) {
          resolve_row_worms(ii, selected_id)
        }
      } else {
        for (ii in row_ids) {
          resolve_row_gbif(ii, selected_id)
        }
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

          sci <- if ("scientificname" %in% names(cand)) as.character(cand$scientificname) else rep("", nrow(cand))
          rk  <- if ("rank" %in% names(cand)) as.character(cand$rank) else rep("", nrow(cand))
          au  <- if ("authority" %in% names(cand)) as.character(cand$authority) else rep("", nrow(cand))
          st  <- if ("status" %in% names(cand)) as.character(cand$status) else rep("", nrow(cand))

          rk[is.na(rk) | rk == "NA"] <- ""
          au[is.na(au) | au == "NA"] <- ""
          st[is.na(st) | st == "NA"] <- ""

          lab <- paste0(
            ids, " — ",
            sci,
            ifelse(rk != "", paste0(" [", rk, "]"), ""),
            ifelse(st != "", paste0(" (", st, ")"), ""),
            ifelse(au != "", paste0(" — ", au), "")
          )
        } else {
          ids <- as.character(cand$key)

          sci <- if ("scientificName" %in% names(cand)) as.character(cand$scientificName) else rep("", nrow(cand))
          can <- if ("canonicalName" %in% names(cand)) as.character(cand$canonicalName) else rep("", nrow(cand))
          rk  <- if ("rank" %in% names(cand)) as.character(cand$rank) else rep("", nrow(cand))
          au  <- if ("authorship" %in% names(cand)) as.character(cand$authorship) else rep("", nrow(cand))
          st  <- if ("taxonomicStatus" %in% names(cand)) as.character(cand$taxonomicStatus) else rep("", nrow(cand))
          mt  <- if ("matchType" %in% names(cand)) as.character(cand$matchType) else rep("", nrow(cand))

          sci[is.na(sci) | sci == "NA"] <- ""
          can[is.na(can) | can == "NA"] <- ""
          rk[is.na(rk) | rk == "NA"] <- ""
          au[is.na(au) | au == "NA"] <- ""
          st[is.na(st) | st == "NA"] <- ""
          mt[is.na(mt) | mt == "NA"] <- ""

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
      if (length(nms) == 0) {
        return(shiny::tags$div("Nenhum scientificName disponível (verifica o mapping)."))
      }

      lk <- rv$lookup

      if (is.null(lk) || !is.data.frame(lk) || nrow(lk) == 0) {
        unres <- vapply(nms, is_unresolvable, logical(1))
        return(
          shiny::tags$div(
            shiny::tags$p(paste0("Nomes únicos: ", length(nms))),
            shiny::tags$p(paste0("UNRESOLVABLE: ", sum(unres))),
            shiny::tags$p("MATCHED: 0 | AMBIGUOUS: 0 | NOT_FOUND: 0"),
            shiny::tags$p("Dataset aplicado: NÃO")
          )
        )
      }

      n_matched <- sum(lk$taxonMatchStatus == "MATCHED", na.rm = TRUE)
      n_amb <- sum(lk$taxonMatchStatus == "AMBIGUOUS", na.rm = TRUE)
      n_nf <- sum(lk$taxonMatchStatus == "NOT_FOUND", na.rm = TRUE)
      n_unres <- sum(lk$taxonMatchStatus == "UNRESOLVABLE", na.rm = TRUE)

      shiny::tags$div(
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
              "' irá marcar esta linha como MATCHED."
            )
          ),
          shiny::tags$p(
            paste0("normalisedQuery: ", q)
          ),
          if (length(same_nq) > 0) {
            shiny::tags$p(
              paste0(
                "Existem ",
                length(same_nq),
                " outras linha(s) com o mesmo normalisedQuery. Deseja aplicar o mesmo choice a todas elas?"
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
      shiny::req("scientificName" %in% names(df))

      if (!has_needed_pkgs()) {
        add_issue(
          NA_integer_, "scientificName", "taxonomy_missing_pkg", "ERROR",
          "Pacote necessário não está instalado para a base selecionada."
        )
        return()
      }

      nms <- base_names()
      if (length(nms) == 0) {
        add_issue(
          NA_integer_, "scientificName", "taxonomy_no_names", "WARNING",
          "Sem nomes para validar."
        )
        return()
      }

      qinfo <- lapply(nms, function(nm) {
        if (isTRUE(input$try_truncate) && is_unresolvable(nm)) {
          truncate_query(nm)
        } else {
          list(query = normalize_ws(nm), changed = FALSE, rule = "none")
        }
      })

      normalised <- vapply(qinfo, `[[`, character(1), "query")
      unres0 <- vapply(nms, is_unresolvable, logical(1))
      unres <- unres0 & !vapply(qinfo, `[[`, logical(1), "changed")

      lookup <- data.frame(
        inputName = nms,
        normalisedQuery = normalised,
        matchedName = NA_character_,
        scientificNameID = NA_character_,
        taxonomicStatus = NA_character_,
        acceptedNameUsage = NA_character_,
        taxonRank = NA_character_,
        authority = NA_character_,
        nameAccordingTo = if (identical(input$tax_db, "worms")) "WoRMS" else "GBIF Backbone",
        taxonMatchStatus = ifelse(unres, "UNRESOLVABLE", "PENDING"),
        selectedID = NA_character_,
        stringsAsFactors = FALSE
      )

      for (i in seq_along(nms)) {
        if (isTRUE(input$try_truncate) && isTRUE(qinfo[[i]]$changed)) {
          add_issue(
            NA_integer_, "scientificName", qinfo[[i]]$rule, "INFO",
            paste0("Truncation: '", nms[i], "' → query '", normalised[i], "'.")
          )
        }
      }

      if (any(unres)) {
        for (nm in nms[unres]) {
          add_issue(
            NA_integer_, "scientificName", "taxonomy_unresolvable", "INFO",
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

            if (!is.null(res$candidates) &&
                is.data.frame(res$candidates) &&
                nrow(res$candidates) > 0) {
              rv$candidates_map[[nm]] <- res$candidates
            }

            if (lookup$taxonMatchStatus[i] == "NOT_FOUND") {
              add_issue(
                NA_integer_, "scientificName", "taxonomy_not_found", "WARNING",
                paste0("Não encontrado na base selecionada: '", nm, "' (query='", q, "').")
              )
            }

            if (lookup$taxonMatchStatus[i] == "AMBIGUOUS") {
              add_issue(
                NA_integer_, "scientificName", "taxonomy_ambiguous", "WARNING",
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
                  NA_integer_, "scientificName", "taxonomy_manual_resolution",
                  "INFO",
                  paste0(
                    "Linha '", rv$lookup$inputName[ii],
                    "' resolvida manualmente e marcada como MATCHED."
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

      if (isTRUE(input$choice_popup_dont_show)) {
        rv$suppress_choice_popup <- TRUE
      }

      apply_choice_to_rows(row_index, selected_id)
      shiny::removeModal()

      add_issue(
        NA_integer_, "scientificName", "taxonomy_manual_resolution",
        "INFO",
        paste0(
          "Linha '", rv$lookup$inputName[row_index],
          "' resolvida manualmente e marcada como MATCHED."
        )
      )

      rv$pending_choice <- NULL
    })

    shiny::observeEvent(input$choice_apply_same, {
      shiny::req(rv$pending_choice)
      row_index <- rv$pending_choice$row_index
      selected_id <- rv$pending_choice$selected_id

      if (isTRUE(input$choice_popup_dont_show)) {
        rv$suppress_choice_popup <- TRUE
      }

      q <- rv$lookup$normalisedQuery[row_index]
      same_rows <- which(rv$lookup$normalisedQuery == q)

      apply_choice_to_rows(same_rows, selected_id)
      shiny::removeModal()

      add_issue(
        NA_integer_, "scientificName", "taxonomy_manual_resolution_same_normalised",
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
    # Build output only when apply is confirmed
    # ------------------------------------------------------
    compute_out <- function() {
      df <- df_in()
      shiny::req(is.data.frame(df))

      if (is.null(rv$lookup) || !is.data.frame(rv$lookup) || nrow(rv$lookup) == 0) {
        return(df)
      }

      if (!requireNamespace("dplyr", quietly = TRUE)) {
        return(df)
      }

      out_df <- df

      if (!("scientificName_original" %in% names(out_df))) {
        out_df$scientificName_original <- out_df$scientificName
      }

      lk <- rv$lookup
      names(lk)[names(lk) == "inputName"] <- "scientificName_original"

      out <- dplyr::left_join(out_df, lk, by = "scientificName_original")

      if (isTRUE(input$replace_scientificName) && "scientificName" %in% names(out)) {
        has_valid <- !is.na(out$acceptedNameUsage) & out$acceptedNameUsage != ""
        has_match <- !is.na(out$matchedName) & out$matchedName != ""

        out$scientificName[has_valid] <- out$acceptedNameUsage[has_valid]
        out$scientificName[!has_valid & has_match] <- out$matchedName[!has_valid & has_match]
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
        NA_integer_, "scientificName", "taxonomy_apply_first_choice_pending",
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

      n_matched <- sum(rv$lookup$taxonMatchStatus == "MATCHED", na.rm = TRUE)
      n_amb <- sum(rv$lookup$taxonMatchStatus == "AMBIGUOUS", na.rm = TRUE)
      n_nf <- sum(rv$lookup$taxonMatchStatus == "NOT_FOUND", na.rm = TRUE)
      n_unres <- sum(rv$lookup$taxonMatchStatus == "UNRESOLVABLE", na.rm = TRUE)

      add_issue(
        NA_integer_, "scientificName", "taxonomy_applied",
        "INFO",
        paste0(
          "Lookup de taxonomia aplicado ao dataset. ",
          "MATCHED=", n_matched,
          " | AMBIGUOUS=", n_amb,
          " | NOT_FOUND=", n_nf,
          " | UNRESOLVABLE=", n_unres,
          if (isTRUE(input$replace_scientificName)) {
            " | scientificName substituído quando houve acceptedNameUsage/matchedName."
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
          NA_integer_, "scientificName", "taxonomy_apply_without_lookup",
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
        NA_integer_, "scientificName", "taxonomy_keep_ambiguous_on_apply",
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
          pageLength = 10,
          scrollX = TRUE,
          preDrawCallback = DT::JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
          drawCallback = DT::JS("function() { Shiny.bindAll(this.api().table().node()); }")
        )
      )
    })

    output$issues_tbl <- DT::renderDT({
      DT::datatable(
        rv$issues,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
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
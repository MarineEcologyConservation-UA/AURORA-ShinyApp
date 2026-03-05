# =========================================================
# Taxonomy Match Module (WoRMS/GBIF) + AMBIGUOUS + Cache
# + normalisedQuery (truncation) + Export CSV
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
                    shiny::tags$li("AMBIGUOUS aparece quando existem múltiplos candidatos; escolhe no dropdown."),
                    shiny::tags$li("O cache evita repetir chamadas na mesma sessão.")
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

    rv <- shiny::reactiveValues(
      lookup = NULL,
      lookup_display = NULL,
      issues = data.frame(
        row = integer(),
        field = character(),
        rule = character(),
        severity = character(),
        message = character(),
        stringsAsFactors = FALSE
      ),
      cache = new.env(parent = emptyenv()),
      worms_candidates_map = list()
    )

    add_issue <- function(idx, field, rule, severity, message) {
      rv$issues <- rbind(
        rv$issues,
        data.frame(
          row = as.integer(idx),
          field = as.character(field),
          rule = as.character(rule),
          severity = as.character(severity),
          message = as.character(message),
          stringsAsFactors = FALSE
        )
      )
    }

    has_needed_pkgs <- shiny::reactive({
      if (input$tax_db == "worms") {
        requireNamespace("worrms", quietly = TRUE)
      } else {
        requireNamespace("taxize", quietly = TRUE)
      }
    })

    output$pkg_status <- shiny::renderUI({
      ok <- has_needed_pkgs()
      if (ok) {
        shiny::tags$div("Pacotes OK para a base de dados selecionada.")
      } else {
        if (input$tax_db == "worms") {
          shiny::tags$div("Falta o pacote 'worrms' para usar WoRMS.")
        } else {
          shiny::tags$div("Falta o pacote 'taxize' para usar GBIF Backbone.")
        }
      }
    })

    # ---- helpers: normalização / truncation ----
    is_unresolvable <- function(x) {
      x <- tolower(trimws(as.character(x)))
      x <- gsub("\\s+", " ", x)
      if (is.na(x) || x == "") return(TRUE)
      pat <- "(\\bindet\\b|\\bmsp\\b|\\bmsp\\.\\b|\\bsp\\b|\\bsp\\.\\b|\\bcf\\b|\\bcf\\.\\b|\\baff\\b|\\baff\\.\\b|morphotype)"
      grepl(pat, x, perl = TRUE)
    }

    normalize_ws <- function(x) {
      x <- trimws(as.character(x))
      x <- gsub("\\s+", " ", x)
      x
    }

    # tentativa de truncar para o rank conhecido:
    # - remove tokens comuns (indet, msp, sp., cf., aff.)
    # - remove numeração final (ex.: "msp 1")
    # - mantém 1 ou 2 tokens (Genus species), mas se houver só 1 token (family/filo) mantém 1
    truncate_query <- function(name) {
      s0 <- normalize_ws(name)
      s <- s0

      s <- gsub("[,;]+$", "", s)
      s <- gsub("\\s+", " ", s)
      s <- trimws(s)

      low <- tolower(s)

      # remover anotações entre parênteses e colchetes
      low <- gsub("\\([^)]*\\)", " ", low)   # (juveniles), (adult), etc.
      low <- gsub("\\[[^]]*\\]", " ", low)   # [something]

      # remover qualificadores comuns
      low <- gsub("\\b(indet\\.?|msp\\.?|sp\\.?|cf\\.?|aff\\.?|morphotype)\\b", " ", low, perl = TRUE)
      low <- gsub("\\b\\d+\\b", " ", low, perl = TRUE)

      # limpar pontuação residual
      low <- gsub("\\s*\\.(\\s|$)", " ", low)
      low <- gsub("[,;:]+", " ", low)
      low <- gsub("\\s+", " ", low)
      low <- trimws(low)

      if (!nzchar(low)) return(list(query = s0, changed = FALSE, rule = "none"))

      toks <- strsplit(low, " ", fixed = TRUE)[[1]]
      toks <- toks[nzchar(toks)]

      # manter apenas tokens com letras (remove "." ou lixo)
      toks <- toks[grepl("[a-z]", toks, perl = TRUE)]

      if (length(toks) == 0) return(list(query = s0, changed = FALSE, rule = "none"))

      # no máximo 2 tokens (Genus species)
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
      x <- x[!is.na(x) & x != ""]
      sort(unique(x))
    })

    output$summary_box <- shiny::renderUI({
      nms <- base_names()
      if (length(nms) == 0) {
        return(shiny::tags$div("Nenhum scientificName disponível (verifica o mapping)."))
      }

      unres <- vapply(nms, is_unresolvable, logical(1))
      lk <- rv$lookup
      n_matched <- if (is.null(lk)) 0 else sum(lk$taxonMatchStatus == "MATCHED", na.rm = TRUE)
      n_amb <- if (is.null(lk)) 0 else sum(lk$taxonMatchStatus == "AMBIGUOUS", na.rm = TRUE)
      n_nf <- if (is.null(lk)) 0 else sum(lk$taxonMatchStatus == "NOT_FOUND", na.rm = TRUE)
      n_unres <- if (is.null(lk)) sum(unres) else sum(lk$taxonMatchStatus == "UNRESOLVABLE", na.rm = TRUE)

      shiny::tags$div(
        shiny::tags$p(paste0("Nomes únicos: ", length(nms))),
        shiny::tags$p(paste0("UNRESOLVABLE: ", n_unres)),
        shiny::tags$p(paste0("MATCHED: ", n_matched, " | AMBIGUOUS: ", n_amb, " | NOT_FOUND: ", n_nf))
      )
    })

    # ---- cache helpers ----
    cache_key <- function(db, query, use_fuzzy) {
      paste(db, as.character(use_fuzzy), query, sep = "||")
    }
    cache_get <- function(key) {
      if (exists(key, envir = rv$cache, inherits = FALSE)) get(key, envir = rv$cache, inherits = FALSE) else NULL
    }
    cache_set <- function(key, value) {
      assign(key, value, envir = rv$cache); invisible(TRUE)
    }

    # ---- WoRMS helpers ----
    worms_candidates <- function(query, use_fuzzy = TRUE, top_n = 5) {
      recs <- tryCatch(
        worrms::wm_records_name(query, fuzzy = isTRUE(use_fuzzy)),
        error = function(e) NULL
      )
      if (is.null(recs) || !is.data.frame(recs) || nrow(recs) == 0) {
        return(data.frame())
      }

      keep <- intersect(names(recs), c("AphiaID", "scientificname", "rank", "authority", "status", "valid_name"))
      out <- recs[, keep, drop = FALSE]
      if ("AphiaID" %in% names(out)) out <- out[!duplicated(out$AphiaID), , drop = FALSE]
      utils::head(out, n = as.integer(top_n))
    }

    worms_record_by_id <- function(aphia_id) {
      tryCatch(worrms::wm_record(as.integer(aphia_id)), error = function(e) NULL)
    }

    # ---- match engines with normalisedQuery ----
    match_one_worms <- function(input_name, normalised_query, use_fuzzy = TRUE, top_n = 5) {
      key <- cache_key("worms", normalised_query, use_fuzzy)
      cached <- cache_get(key)
      if (!is.null(cached)) {
        # cached é baseado na query, mas precisamos preservar inputName e normalisedQuery
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
            taxonMatchStatus = "MATCHED",
            selectedID = aphia,
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

    match_one_gbif <- function(input_name, normalised_query) {
      key <- cache_key("gbif", normalised_query, FALSE)
      cached <- cache_get(key)
      if (!is.null(cached)) {
        cached$row$inputName <- input_name
        cached$row$normalisedQuery <- normalised_query
        return(cached)
      }

      gbif_id <- tryCatch(taxize::get_gbifid(normalised_query), error = function(e) NA)
      if (length(gbif_id) == 0) gbif_id <- NA
      gbif_id <- as.character(gbif_id[1])

      if (is.na(gbif_id) || gbif_id == "" || gbif_id == "NA") {
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

      cls <- tryCatch(taxize::classification(gbif_id, db = "gbif"), error = function(e) NULL)
      matched <- NA_character_
      rank <- NA_character_

      if (!is.null(cls) && length(cls) >= 1 && is.data.frame(cls[[1]]) && nrow(cls[[1]]) >= 1) {
        last <- cls[[1]][nrow(cls[[1]]), , drop = FALSE]
        if ("name" %in% names(last)) matched <- as.character(last$name)
        if ("rank" %in% names(last)) rank <- as.character(last$rank)
      }

      res <- list(
        row = data.frame(
          inputName = input_name,
          normalisedQuery = normalised_query,
          matchedName = matched,
          scientificNameID = gbif_id,
          taxonomicStatus = NA_character_,
          acceptedNameUsage = NA_character_,
          taxonRank = rank,
          authority = NA_character_,
          nameAccordingTo = "GBIF Backbone",
          taxonMatchStatus = "MATCHED",
          selectedID = gbif_id,
          stringsAsFactors = FALSE
        ),
        candidates = NULL
      )
      cache_set(key, res)
      res
    }

    # ---- construir lookup_display com dropdowns ----
    build_lookup_display <- function(lookup) {
      disp <- lookup
      disp$choice <- ""

      for (i in seq_len(nrow(disp))) {
        if (!identical(disp$taxonMatchStatus[i], "AMBIGUOUS")) next

        nm <- disp$inputName[i]
        cand <- rv$worms_candidates_map[[nm]]
        if (is.null(cand) || !is.data.frame(cand) || nrow(cand) == 0) next

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

        # CORREÇÃO: label = lab, value = ids
        choices <- stats::setNames(ids, lab)

        input_id <- ns(paste0("cand_", i))

        disp$choice[i] <- as.character(
          shiny::selectInput(
            inputId = input_id,
            label = NULL,
            choices = choices,
            selected = if (!is.na(disp$selectedID[i]) && disp$selectedID[i] != "") disp$selectedID[i] else ids[1],
            width = "100%"
          )
        )
      }

      disp
    }

    # ---- executar match ----
    shiny::observeEvent(input$run_match, {
      rv$issues <- rv$issues[0, , drop = FALSE]
      rv$lookup <- NULL
      rv$lookup_display <- NULL
      rv$worms_candidates_map <- list()

      df <- df_in()
      shiny::req(is.data.frame(df))
      shiny::req("scientificName" %in% names(df))

      if (!has_needed_pkgs()) {
        add_issue(NA_integer_, "scientificName", "taxonomy_missing_pkg",
                  "ERROR", "Pacote necessário não está instalado para a base selecionada.")
        return()
      }

      nms <- base_names()
      if (length(nms) == 0) {
        add_issue(NA_integer_, "scientificName", "taxonomy_no_names",
                  "WARNING", "Sem nomes para validar.")
        return()
      }

      # construir normalisedQuery por nome
      qinfo <- lapply(nms, function(nm) {
        if (isTRUE(input$try_truncate) && is_unresolvable(nm)) {
          truncate_query(nm)
        } else {
          list(query = normalize_ws(nm), changed = FALSE, rule = "none")
        }
      })
      normalised <- vapply(qinfo, `[[`, character(1), "query")

      # regra UNRESOLVABLE:
      # - continua UNRESOLVABLE se após truncation não mudou e contém qualificadores
      # - caso tenha truncation e mudou, vira PENDING (vamos tentar match)
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
        nameAccordingTo = if (input$tax_db == "worms") "WoRMS" else "GBIF Backbone",
        taxonMatchStatus = ifelse(unres, "UNRESOLVABLE", "PENDING"),
        selectedID = NA_character_,
        stringsAsFactors = FALSE
      )

      # log truncation aplicada
      for (i in seq_along(nms)) {
        if (isTRUE(input$try_truncate) && isTRUE(qinfo[[i]]$changed)) {
          add_issue(NA_integer_, "scientificName", qinfo[[i]]$rule,
                    "INFO", paste0("Truncation: '", nms[i], "' → query '", normalised[i], "'."))
        }
      }

      # log unresolvable
      if (any(unres)) {
        for (nm in nms[unres]) {
          add_issue(NA_integer_, "scientificName", "taxonomy_unresolvable",
                    "INFO", paste0("Nome não resolvível automaticamente: '", nm, "'."))
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

            res <- if (input$tax_db == "worms") {
              match_one_worms(
                input_name = nm,
                normalised_query = q,
                use_fuzzy = input$use_fuzzy,
                top_n = input$top_n
              )
            } else {
              match_one_gbif(
                input_name = nm,
                normalised_query = q
              )
            }

            row <- res$row
            lookup[i, names(row)] <- row[1, names(row)]

            if (input$tax_db == "worms" &&
                !is.null(res$candidates) &&
                is.data.frame(res$candidates) &&
                nrow(res$candidates) > 0) {
              rv$worms_candidates_map[[nm]] <- res$candidates
            }

            if (lookup$taxonMatchStatus[i] == "NOT_FOUND") {
              add_issue(NA_integer_, "scientificName", "taxonomy_not_found",
                        "WARNING", paste0("Não encontrado na base selecionada: '", nm, "' (query='", q, "')."))
            }
            if (lookup$taxonMatchStatus[i] == "AMBIGUOUS") {
              add_issue(NA_integer_, "scientificName", "taxonomy_ambiguous",
                        "WARNING", paste0("Múltiplos candidatos para: '", nm, "' (query='", q, "'). Seleciona no dropdown."))
            }
          }
        })
      }

      rv$lookup <- lookup
      rv$lookup_display <- build_lookup_display(lookup)

      # observers para dropdowns
      amb_idx <- which(rv$lookup$taxonMatchStatus == "AMBIGUOUS")
      if (length(amb_idx) > 0) {
        for (i in amb_idx) {
          local({
            ii <- i
            input_name <- paste0("cand_", ii)
            shiny::observeEvent(input[[input_name]], {
              val <- input[[input_name]]
              if (!is.null(val) && !is.na(val) && val != "") {
                rv$lookup$selectedID[ii] <- as.character(val)
              }
            }, ignoreInit = TRUE)
          })
        }
      }
    })

    # ---- aplicar lookup ao dataset ----
    df_out <- shiny::reactive({
      df <- df_in()
      shiny::req(is.data.frame(df))

      if (is.null(rv$lookup) || !is.data.frame(rv$lookup) || nrow(rv$lookup) == 0) {
        return(df)
      }

      if (!requireNamespace("dplyr", quietly = TRUE)) {
        return(df)
      }

      if (!("scientificName_original" %in% names(df))) {
        df$scientificName_original <- df$scientificName
      }

      lk <- rv$lookup
      names(lk)[names(lk) == "inputName"] <- "scientificName_original"

      out <- dplyr::left_join(df, lk, by = "scientificName_original")

      # resolver AMBIGUOUS escolhidos (WoRMS)
      if (input$tax_db == "worms" && requireNamespace("worrms", quietly = TRUE)) {
        amb_rows <- which(out$taxonMatchStatus == "AMBIGUOUS" & !is.na(out$selectedID) & out$selectedID != "")
        if (length(amb_rows) > 0) {
          for (r in amb_rows) {
            rec <- worms_record_by_id(out$selectedID[r])
            if (is.null(rec)) next

            if ("scientificname" %in% names(rec)) out$matchedName[r] <- rec$scientificname
            out$scientificNameID[r] <- as.character(out$selectedID[r])
            if ("status" %in% names(rec)) out$taxonomicStatus[r] <- rec$status
            if ("valid_name" %in% names(rec)) out$acceptedNameUsage[r] <- rec$valid_name
            if ("rank" %in% names(rec)) out$taxonRank[r] <- rec$rank
            if ("authority" %in% names(rec)) out$authority[r] <- rec$authority

            out$taxonMatchStatus[r] <- "MATCHED"
          }
        }
      }

      # substituir scientificName pelo nome válido (se solicitado)
      if (isTRUE(input$replace_scientificName)) {
        has_valid <- !is.na(out$acceptedNameUsage) & out$acceptedNameUsage != ""
        out$scientificName[has_valid] <- out$acceptedNameUsage[has_valid]
      }

      out
    })

    shiny::observeEvent(input$apply_match, {
      out <- df_out()
      shiny::req(is.data.frame(out))
      add_issue(NA_integer_, "scientificName", "taxonomy_applied",
                "INFO", "Lookup de taxonomia aplicado ao dataset (join por scientificName_original).")
    })

    # ---- export lookup CSV ----
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

    output$lookup_tbl <- DT::renderDT({
      disp <- rv$lookup_display
      if (is.null(disp) || !is.data.frame(disp)) disp <- data.frame()

      DT::datatable(
        disp,
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$issues_tbl <- DT::renderDT({
      DT::datatable(
        rv$issues,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    list(
      df_out = df_out,
      lookup = shiny::reactive(rv$lookup),
      issues = shiny::reactive(rv$issues)
    )
  })
}
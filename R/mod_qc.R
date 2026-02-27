# =========================================================
# QC Page (BioCheck-like) + QC Engine
# File: R/mod_qc.R
# =========================================================

#' QC page UI (BioCheck-like)
#'
#' @param id Module id
#' @return Shiny UI
#' @export
mod_qc_ui <- function(id) {
  ns <- shiny::NS(id)

  has_leaflet <- requireNamespace("leaflet", quietly = TRUE)

  bslib::nav_panel(
    title = "QC & Diagnostics",
    value = "qc",

    shiny::tags$style(shiny::HTML("
      .qc-title { margin-top: 8px; margin-bottom: 12px; }
      .qc-muted { color: #6b7280; }
      .qc-box { border: 1px solid #e5e7eb; border-radius: 10px; padding: 12px 14px; background: #fff; }
      .qc-kpi { font-size: 26px; font-weight: 700; }
      .qc-kpi-label { font-size: 12px; color: #6b7280; text-transform: uppercase; letter-spacing: .04em; }
      .qc-warn { color: #b45309; }
      .qc-err { color: #b91c1c; }
      .qc-ok { color: #047857; }

      /* ---- DT “report style” (overview tables) ---- */
      .qc-dt-report .dataTables_wrapper .dataTables_length,
      .qc-dt-report .dataTables_wrapper .dataTables_filter,
      .qc-dt-report .dataTables_wrapper .dataTables_info,
      .qc-dt-report .dataTables_wrapper .dataTables_paginate {
        display: none !important;
      }

      .qc-dt-report table.dataTable {
        border-collapse: collapse !important;
        width: auto !important;
      }

      .qc-dt-report table.dataTable thead th {
        font-weight: 600;
        border-bottom: 1px solid #e5e7eb !important;
        padding: 10px 12px !important;
      }

      .qc-dt-report table.dataTable tbody td {
        border-top: 1px solid #f1f5f9 !important;
        padding: 10px 12px !important;
      }

      .qc-dt-report .dataTables_wrapper {
        padding-top: 6px;
      }

      /* Wrapper com altura fixa */
      .qc-map-wrap {
        height: 420px !important;
        min-height: 420px !important;
        width: 100%;
      }

      /* O widget do leaflet deve preencher o wrapper */
      .qc-map-wrap .leaflet.html-widget,
      .qc-map-wrap .leaflet-container {
        height: 100% !important;
        min-height: 420px !important;
      }

      /* Proteção contra colapso por html-fill-item do bslib */
      .qc-map-wrap.html-fill-item,
      .qc-map-wrap .html-fill-item {
        flex: 0 0 auto !important;
      }
      
    ")),

    bslib::navset_card_tab(
      id = ns("qc_tabs"),
      selected = "data_overview",
      title = "Quality Control",

      # -----------------------------------------------------
      # Data overview
      # -----------------------------------------------------
      bslib::nav_panel(
        "Data overview",
        value = "data_overview",

        shiny::div(
          class = "qc-title",
          shiny::h3("Quality Control & Diagnostics"),
          shiny::p(
            class = "qc-muted",
            "Esta página avalia a qualidade do DwC-A gerado (Event / Occurrence / eMoF). ",
            "O export será bloqueado se existirem erros (ERROR)."
          )
        ),

        shiny::hr(),
        shiny::h5("DEBUG: colunas recebidas no QC"),
        shiny::verbatimTextOutput(ns("debug_cols")),
        shiny::hr(),

        shiny::h5("DEBUG: overview_event_occ"),
        shiny::verbatimTextOutput(ns("debug_overview_event_occ")),
        shiny::hr(),

        shiny::h5("DEBUG: overview_map"),
        shiny::verbatimTextOutput(ns("debug_overview_map")),
        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            3,
            shiny::div(
              class = "qc-box",
              shiny::div(class = "qc-kpi-label", "Export status"),
              shiny::div(shiny::uiOutput(ns("export_status_ui")))
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "qc-box",
              shiny::div(class = "qc-kpi-label", "Errors"),
              shiny::div(class = "qc-kpi qc-err", shiny::textOutput(ns("kpi_errors")))
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "qc-box",
              shiny::div(class = "qc-kpi-label", "Warnings"),
              shiny::div(class = "qc-kpi qc-warn", shiny::textOutput(ns("kpi_warnings")))
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "qc-box",
              shiny::div(class = "qc-kpi-label", "Records (occurrence)"),
              shiny::div(class = "qc-kpi", shiny::textOutput(ns("kpi_n_occ")))
            )
          )
        ),

        shiny::hr(),

        shiny::h4("Overview of event and occurrence records"),
        shiny::div(class = "qc-dt-report", DT::DTOutput(ns("overview_event_occ_tbl"))),

        shiny::hr(),

        shiny::h4("Overview of measurement or fact records (types and units)"),
        shiny::div(
          style = "max-width: 1100px; margin: 0 auto;",
          class = "qc-dt-report",
          DT::DTOutput(ns("overview_emof_types_tbl"))
        ),

        shiny::hr(),

        shiny::h4("Geographical cover of the dataset"),
        if (has_leaflet) {
          shiny::div(
            class = "qc-map-wrap",
            leaflet::leafletOutput(ns("overview_map"), height = "100%")
          )
        } else {
          shiny::div(
            class = "qc-muted",
            "Mapa indisponível: instala o pacote 'leaflet' para ativar esta secção."
          )
        },

        shiny::hr(),

        shiny::h4("Taxonomic cover of the dataset"),
        shiny::fluidRow(
          shiny::column(4, shiny::uiOutput(ns("kingdom_ui"))),
          shiny::column(8, shiny::plotOutput(ns("overview_tax_plot"), height = "320px"))
        )
      ),

      # -----------------------------------------------------
      # Issues found
      # -----------------------------------------------------
      bslib::nav_panel(
        "Issues found",
        value = "issues_found",

        shiny::h4("Overview of all issues"),
        DT::DTOutput(ns("issues_summary_tbl")),

        shiny::hr(),
        shiny::h4("Details (filterable)"),
        DT::DTOutput(ns("issues_detailed_tbl")),

        shiny::hr(),
        shiny::h4("Taxonomic issues"),
        DT::DTOutput(ns("tax_issues_tbl")),

        shiny::hr(),
        shiny::h4("eMoF issues"),
        DT::DTOutput(ns("emof_issues_tbl"))
      ),

      # -----------------------------------------------------
      # Issues on map
      # -----------------------------------------------------
      bslib::nav_panel(
        "Issues on map",
        value = "issues_map",
        shiny::p(
          class = "qc-muted",
          "Mostra apenas issues que possuem coordenadas válidas (em Event ou Occurrence)."
        ),
        if (has_leaflet) {
          leaflet::leafletOutput(ns("issues_map"), height = 560)
        } else {
          shiny::div(
            class = "qc-muted",
            "Mapa indisponível: instala o pacote 'leaflet' para ativar esta secção."
          )
        }
      ),

      # -----------------------------------------------------
      # Invalid records
      # -----------------------------------------------------
      bslib::nav_panel("Invalid Event Records", value = "invalid_event", DT::DTOutput(ns("invalid_event_tbl"))),
      bslib::nav_panel("Invalid Occurrence Records", value = "invalid_occ", DT::DTOutput(ns("invalid_occ_tbl"))),
      bslib::nav_panel("Invalid eMoF Records", value = "invalid_emof", DT::DTOutput(ns("invalid_emof_tbl"))),

      # -----------------------------------------------------
      # Event hierarchy
      # -----------------------------------------------------
      bslib::nav_panel(
        "OBIS Event Hierarchy tree",
        value = "event_hierarchy",
        shiny::p(
          class = "qc-muted",
          "Primeira versão (Fase 1): lista de inconsistências parentEventID e amostra de edges."
        ),
        DT::DTOutput(ns("hierarchy_issues_tbl")),
        shiny::hr(),
        DT::DTOutput(ns("hierarchy_edges_tbl"))
      ),

      # -----------------------------------------------------
      # About
      # -----------------------------------------------------
      bslib::nav_panel(
        "About",
        value = "about",
        shiny::h4("Checks implemented"),
        shiny::uiOutput(ns("about_ui"))
      )
    )
  )
}

# =========================================================
# QC engine helpers
# =========================================================

`%||%` <- rlang::`%||%`

.qc_issue <- function(table, row_id, field, rule, message,
                      severity = "ERROR", extra = list()) {
  data.frame(
    table = as.character(table),
    row_id = as.character(row_id),
    field = as.character(field),
    rule = as.character(rule),
    message = as.character(message),
    severity = as.character(severity),
    stringsAsFactors = FALSE
  )
}

.qc_bind <- function(x) {
  if (length(x) == 0) {
    return(data.frame(
      table = character(),
      row_id = character(),
      field = character(),
      rule = character(),
      message = character(),
      severity = character(),
      stringsAsFactors = FALSE
    ))
  }
  x <- Filter(function(z) is.data.frame(z) && nrow(z) > 0, x)
  if (length(x) == 0) {
    return(data.frame(
      table = character(),
      row_id = character(),
      field = character(),
      rule = character(),
      message = character(),
      severity = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, x)
  rownames(out) <- NULL
  out
}

.qc_is_blank <- function(x) {
  if (is.null(x)) return(TRUE)

  if (is.factor(x)) x <- as.character(x)

  if (is.character(x)) {
    return(is.na(x) | trimws(x) == "")
  }

  is.na(x)
}

.qc_num <- function(x) suppressWarnings(as.numeric(x))

.qc_parse_year <- function(x) {
  if (is.null(x)) return(rep(NA_integer_, 0))
  if (inherits(x, "Date")) return(as.integer(format(x, "%Y")))
  xi <- suppressWarnings(as.integer(x))
  ifelse(is.na(xi), NA_integer_, xi)
}

.qc_extract_year_from_eventDate <- function(eventDate) {
  if (is.null(eventDate)) return(NULL)
  x <- trimws(as.character(eventDate))
  y <- suppressWarnings(as.integer(substr(x, 1, 4)))
  y[!is.finite(y)] <- NA_integer_
  y
}

.qc_required_field <- function(df, table, id_field, fields_required) {
  out <- list()
  if (!is.data.frame(df) || nrow(df) == 0) return(.qc_bind(out))

  rid <- if (id_field %in% names(df)) df[[id_field]] else seq_len(nrow(df))

  for (f in fields_required) {
    if (!f %in% names(df)) {
      out[[length(out) + 1]] <- .qc_issue(
        table = table,
        row_id = "*",
        field = f,
        rule = "required_field_missing_column",
        message = paste0("Missing required column: ", f),
        severity = "ERROR"
      )
      next
    }
    miss <- .qc_is_blank(df[[f]])
    if (any(miss, na.rm = TRUE)) {
      ii <- which(miss)
      out[[length(out) + 1]] <- .qc_bind(lapply(ii, function(i) {
        .qc_issue(
          table = table,
          row_id = rid[i] %||% as.character(i),
          field = f,
          rule = "required_field_empty_value",
          message = paste0("Empty value for required field ", f),
          severity = "ERROR"
        )
      }))
    }
  }

  .qc_bind(out)
}

.qc_unique_id <- function(df, table, id_field) {
  if (!is.data.frame(df) || nrow(df) == 0) return(.qc_bind(list()))
  if (!id_field %in% names(df)) {
    return(.qc_issue(
      table = table,
      row_id = "*",
      field = id_field,
      rule = "id_missing_column",
      message = paste0("Missing ID column: ", id_field),
      severity = "ERROR"
    ))
  }

  id <- as.character(df[[id_field]])
  blank <- .qc_is_blank(id)
  out <- list()

  if (any(blank, na.rm = TRUE)) {
    ii <- which(blank)
    out[[length(out) + 1]] <- .qc_bind(lapply(ii, function(i) {
      .qc_issue(
        table = table,
        row_id = as.character(i),
        field = id_field,
        rule = "id_empty",
        message = paste0(id_field, " is empty"),
        severity = "ERROR"
      )
    }))
  }

  id2 <- id[!blank]
  dup <- duplicated(id2) | duplicated(id2, fromLast = TRUE)
  if (any(dup, na.rm = TRUE)) {
    vals <- unique(id2[dup])
    for (v in vals) {
      idx <- which(id == v)
      out[[length(out) + 1]] <- .qc_bind(lapply(idx, function(i) {
        .qc_issue(
          table = table,
          row_id = id[i],
          field = id_field,
          rule = "id_duplicated",
          message = paste0(id_field, " duplicated: ", v),
          severity = "ERROR"
        )
      }))
    }
  }

  .qc_bind(out)
}

.qc_coords_range <- function(df, table, id_field,
                             lat_field = "decimalLatitude",
                             lon_field = "decimalLongitude") {
  if (!is.data.frame(df) || nrow(df) == 0) return(.qc_bind(list()))
  if (!(lat_field %in% names(df) && lon_field %in% names(df))) return(.qc_bind(list()))

  rid <- if (id_field %in% names(df)) df[[id_field]] else seq_len(nrow(df))
  lat <- .qc_num(df[[lat_field]])
  lon <- .qc_num(df[[lon_field]])

  out <- list()

  bad <- (is.finite(lat) & (lat < -90 | lat > 90)) |
    (is.finite(lon) & (lon < -180 | lon > 180))
  bad[is.na(bad)] <- FALSE

  if (any(bad)) {
    ii <- which(bad)
    out[[length(out) + 1]] <- .qc_bind(lapply(ii, function(i) {
      .qc_issue(
        table = table,
        row_id = rid[i] %||% as.character(i),
        field = "coordinates_error",
        rule = "coords_out_of_range",
        message = "Out-of-range coordinates (lat must be -90..90, lon must be -180..180)",
        severity = "ERROR"
      )
    }))
  }

  .qc_bind(out)
}

.qc_eventDate_basic <- function(event, id_field = "eventID", field = "eventDate") {
  if (!is.data.frame(event) || nrow(event) == 0) return(.qc_bind(list()))
  if (!field %in% names(event)) return(.qc_bind(list()))

  rid <- if (id_field %in% names(event)) event[[id_field]] else seq_len(nrow(event))
  x <- trimws(as.character(event[[field]]))
  out <- list()

  ok <- grepl("^\\d{4}($|-\\d{2}($|-\\d{2}$))", x) |
    grepl("^\\d{4}.*\\/\\d{4}", x)
  ok[.qc_is_blank(x)] <- TRUE

  bad <- !ok
  bad[is.na(bad)] <- FALSE
  if (any(bad)) {
    ii <- which(bad)
    out[[length(out) + 1]] <- .qc_bind(lapply(ii, function(i) {
      .qc_issue(
        table = "event",
        row_id = rid[i] %||% as.character(i),
        field = field,
        rule = "eventDate_invalid_format",
        message = paste0(field, " does not seem to be a valid ISO-8601 date"),
        severity = "ERROR"
      )
    }))
  }

  .qc_bind(out)
}

.qc_rel_occ_event <- function(occ, event) {
  if (!is.data.frame(occ) || nrow(occ) == 0) return(.qc_bind(list()))
  if (!("eventID" %in% names(occ))) return(.qc_bind(list()))
  if (!is.data.frame(event) || nrow(event) == 0) {
    return(.qc_issue(
      table = "occurrence",
      row_id = "*",
      field = "eventID",
      rule = "event_table_missing",
      message = "Event table missing; cannot validate eventID links",
      severity = "ERROR"
    ))
  }
  if (!("eventID" %in% names(event))) return(.qc_bind(list()))

  ev_ids <- unique(as.character(event$eventID))
  occ_ids <- as.character(occ$occurrenceID %||% seq_len(nrow(occ)))

  miss <- !.qc_is_blank(occ$eventID) & !(as.character(occ$eventID) %in% ev_ids)

  if (!any(miss, na.rm = TRUE)) return(.qc_bind(list()))

  ii <- which(miss)
  .qc_bind(lapply(ii, function(i) {
    .qc_issue(
      table = "occurrence",
      row_id = occ_ids[i] %||% as.character(i),
      field = "eventID",
      rule = "eventID_no_corresponding_event",
      message = "This eventID has no corresponding eventID in the core",
      severity = "ERROR"
    )
  }))
}

.qc_parentEventID <- function(event) {
  if (!is.data.frame(event) || nrow(event) == 0) return(.qc_bind(list()))
  if (!("eventID" %in% names(event)) || !("parentEventID" %in% names(event))) {
    return(.qc_bind(list()))
  }

  ev_ids <- unique(as.character(event$eventID))
  pid <- as.character(event$parentEventID)
  rid <- as.character(event$eventID)

  miss <- !.qc_is_blank(pid) & !(pid %in% ev_ids)
  if (!any(miss, na.rm = TRUE)) return(.qc_bind(list()))

  ii <- which(miss)
  .qc_bind(lapply(ii, function(i) {
    .qc_issue(
      table = "event",
      row_id = rid[i] %||% as.character(i),
      field = "parentEventID",
      rule = "parentEventID_no_corresponding_event",
      message = "parentEventID has no corresponding eventID",
      severity = "ERROR"
    )
  }))
}

.qc_emof_duplicates <- function(emof) {
  if (!is.data.frame(emof) || nrow(emof) == 0) return(.qc_bind(list()))
  if (!("measurementType" %in% names(emof))) return(.qc_bind(list()))

  link_field <- if ("eventID" %in% names(emof)) {
    "eventID"
  } else if ("occurrenceID" %in% names(emof)) {
    "occurrenceID"
  } else {
    NULL
  }
  if (is.null(link_field)) return(.qc_bind(list()))

  key <- paste0(as.character(emof[[link_field]]), "||", as.character(emof$measurementType))
  ok <- !.qc_is_blank(emof[[link_field]]) & !.qc_is_blank(emof$measurementType)
  key2 <- key[ok]

  dup <- duplicated(key2) | duplicated(key2, fromLast = TRUE)
  if (!any(dup, na.rm = TRUE)) return(.qc_bind(list()))

  vals <- unique(key2[dup])
  rid <- if ("measurementID" %in% names(emof)) {
    as.character(emof$measurementID)
  } else {
    as.character(seq_len(nrow(emof)))
  }

  out <- list()
  for (v in vals) {
    idx <- which(key == v)
    out[[length(out) + 1]] <- .qc_bind(lapply(idx, function(i) {
      .qc_issue(
        table = "emof",
        row_id = rid[i],
        field = "measurementType",
        rule = "duplicate_measurementType_per_link",
        message = paste0("Duplicate measurementType linked to the same ", link_field),
        severity = "WARN"
      )
    }))
  }
  .qc_bind(out)
}

.qc_emof_required <- function(emof) {
  if (!is.data.frame(emof) || nrow(emof) == 0) return(.qc_bind(list()))
  rid <- if ("measurementID" %in% names(emof)) {
    as.character(emof$measurementID)
  } else {
    as.character(seq_len(nrow(emof)))
  }

  out <- list()
  for (f in c("measurementType", "measurementValue")) {
    if (!f %in% names(emof)) next
    miss <- .qc_is_blank(emof[[f]])
    if (any(miss, na.rm = TRUE)) {
      ii <- which(miss)
      out[[length(out) + 1]] <- .qc_bind(lapply(ii, function(i) {
        .qc_issue(
          table = "emof",
          row_id = rid[i],
          field = f,
          rule = "required_field_empty_value",
          message = paste0("Empty value for required field ", f),
          severity = "ERROR"
        )
      }))
    }
  }
  .qc_bind(out)
}

# -----------------------------
# Phase 2 (optional) - WoRMS
# -----------------------------
.qc_worms_resolve <- function(occ) {
  if (!is.data.frame(occ) || nrow(occ) == 0) {
    return(list(issues = .qc_bind(list()), tables = list()))
  }
  if (!requireNamespace("worrms", quietly = TRUE)) {
    issues <- .qc_issue(
      table = "occurrence",
      row_id = "*",
      field = "scientificNameID",
      rule = "worms_dependency_missing",
      message = "WoRMS checks skipped (package 'worrms' not installed)",
      severity = "WARN"
    )
    return(list(
      issues = issues,
      tables = list(unmatched = data.frame(), broken = data.frame())
    ))
  }

  if (!("scientificName" %in% names(occ))) {
    return(list(
      issues = .qc_bind(list()),
      tables = list(unmatched = data.frame(), broken = data.frame())
    ))
  }

  sci <- trimws(as.character(occ$scientificName))
  sci[.qc_is_blank(sci)] <- NA_character_

  u <- unique(stats::na.omit(sci))
  if (length(u) == 0) {
    return(list(
      issues = .qc_bind(list()),
      tables = list(unmatched = data.frame(), broken = data.frame())
    ))
  }

  aphia <- rep(NA_integer_, length(u))
  for (i in seq_along(u)) {
    res <- tryCatch(worrms::wm_records_name(u[i]), error = function(e) NULL)
    if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) next
    if ("AphiaID" %in% names(res)) {
      aphia[i] <- suppressWarnings(as.integer(res$AphiaID[1]))
    }
  }

  map <- data.frame(scientificName = u, AphiaID = aphia, stringsAsFactors = FALSE)

  unmatched <- map[is.na(map$AphiaID), , drop = FALSE]
  if (nrow(unmatched) > 0) {
    tab_sci <- table(sci)
    unmatched$count <- as.integer(tab_sci[unmatched$scientificName])
    unmatched$count[is.na(unmatched$count)] <- 1L
  }

  broken <- data.frame()
  issues <- .qc_bind(list())

  if ("scientificNameID" %in% names(occ)) {
    sid <- trimws(as.character(occ$scientificNameID))
    tmp <- data.frame(
      scientificName = sci,
      scientificNameID = sid,
      stringsAsFactors = FALSE
    )
    tmp <- tmp[!is.na(tmp$scientificName) & !.qc_is_blank(tmp$scientificNameID), , drop = FALSE]

    if (nrow(tmp) > 0) {
      agg <- stats::aggregate(scientificNameID ~ scientificName, tmp, function(z) length(unique(z)))
      bad <- agg$scientificNameID > 1
      if (any(bad)) {
        bad_names <- agg$scientificName[bad]
        broken <- tmp[tmp$scientificName %in% bad_names, , drop = FALSE]
        broken$n <- ave(broken$scientificNameID, broken$scientificName, FUN = function(z) length(unique(z)))
        broken$issue <- "multiple scientificNameID per scientificName"

        rid <- as.character(occ$occurrenceID %||% seq_len(nrow(occ)))
        ii <- which(!is.na(sci) & sci %in% bad_names)
        issues <- .qc_bind(lapply(ii, function(i) {
          .qc_issue(
            table = "occurrence",
            row_id = rid[i] %||% as.character(i),
            field = "scientificNameID",
            rule = "broken_one_to_one_taxa_relationship",
            message = "Broken one-to-one taxa relationship (multiple scientificNameID per scientificName)",
            severity = "WARN"
          )
        }))
      }
    }
  }

  list(issues = issues, tables = list(unmatched = unmatched, broken = broken))
}

# -----------------------------
# Phase 2 (optional) - URI resolve for measurementTypeID/measurementUnitID
# -----------------------------
.qc_uri_resolve <- function(emof, field, table = "emof") {
  if (!is.data.frame(emof) || nrow(emof) == 0) return(.qc_bind(list()))
  if (!field %in% names(emof)) return(.qc_bind(list()))

  if (!requireNamespace("httr", quietly = TRUE)) {
    return(.qc_issue(
      table = table,
      row_id = "*",
      field = field,
      rule = "uri_dependency_missing",
      message = "URI checks skipped (package 'httr' not installed)",
      severity = "WARN"
    ))
  }

  x <- trimws(as.character(emof[[field]]))
  x[.qc_is_blank(x)] <- NA_character_
  u <- unique(stats::na.omit(x))
  if (length(u) == 0) return(.qc_bind(list()))

  status <- stats::setNames(rep(NA_integer_, length(u)), u)
  for (i in seq_along(u)) {
    r <- tryCatch(httr::HEAD(u[i], httr::timeout(3)), error = function(e) NULL)
    if (is.null(r)) next
    status[i] <- httr::status_code(r)
  }

  bad <- names(status)[!is.na(status) & status >= 400]
  if (length(bad) == 0) return(.qc_bind(list()))

  rid <- if ("measurementID" %in% names(emof)) {
    as.character(emof$measurementID)
  } else {
    as.character(seq_len(nrow(emof)))
  }
  ii <- which(!is.na(x) & x %in% bad)

  .qc_bind(lapply(ii, function(i) {
    .qc_issue(
      table = table,
      row_id = rid[i],
      field = field,
      rule = "uri_does_not_resolve",
      message = paste0(field, " does not resolve (HTTP >= 400)"),
      severity = "WARN"
    )
  }))
}

# -----------------------------
# Robust summary builder (fix for 0,1 / 0,2 crash)
# -----------------------------
.qc_make_summary <- function(issues_detailed) {
  empty <- data.frame(
    field = character(),
    message = character(),
    table = character(),
    severity = character(),
    count = integer(),
    stringsAsFactors = FALSE
  )

  if (!is.data.frame(issues_detailed) || nrow(issues_detailed) == 0) return(empty)

  f <- as.character(issues_detailed$field)
  m <- as.character(issues_detailed$message)
  t <- as.character(issues_detailed$table)
  s <- as.character(issues_detailed$severity)

  f[is.na(f)] <- ""
  m[is.na(m)] <- ""
  t[is.na(t)] <- ""
  s[is.na(s)] <- ""

  key <- paste(f, m, t, s, sep = "||")

  tab <- sort(table(key), decreasing = TRUE)
  if (length(tab) == 0) return(empty)

  parts_list <- strsplit(names(tab), "||", fixed = TRUE)
  parts_list <- lapply(parts_list, function(p) {
    p <- as.character(p)
    length(p) <- 4
    p[is.na(p)] <- ""
    p
  })

  parts <- do.call(rbind, parts_list)
  if (is.null(parts) || nrow(parts) == 0) return(empty)

  data.frame(
    field = parts[, 1],
    message = parts[, 2],
    table = parts[, 3],
    severity = parts[, 4],
    count = as.integer(tab),
    stringsAsFactors = FALSE
  )
}

# =========================================================
# Main QC runner
# =========================================================

#' Run QC checks on DwC-A tables (Phase 1 + Phase 2 with fallbacks)
#'
#' @param event Event table data.frame
#' @param occurrence Occurrence table data.frame
#' @param emof eMoF table data.frame (may be NULL)
#' @param pre_issues Optional data.frame of pre-split issues (mapping cleaning)
#' @return list with overview, issues_summary, issues_detailed, invalid tables, map_issues, hierarchy, can_export
#' @export
run_qc_dwca <- function(event, occurrence, emof = NULL, pre_issues = NULL) {
  if (is.null(emof)) emof <- data.frame()

  issues_list <- list()

  issues_list[[length(issues_list) + 1]] <- .qc_unique_id(event, "event", "eventID")
  issues_list[[length(issues_list) + 1]] <- .qc_unique_id(occurrence, "occurrence", "occurrenceID")

  if ("eventDate" %in% names(event)) {
    issues_list[[length(issues_list) + 1]] <- .qc_eventDate_basic(event, "eventID", "eventDate")
  }

  issues_list[[length(issues_list) + 1]] <- .qc_required_field(
    occurrence, "occurrence", "occurrenceID", c("scientificName")
  )

  issues_list[[length(issues_list) + 1]] <- .qc_coords_range(event, "event", "eventID")
  issues_list[[length(issues_list) + 1]] <- .qc_coords_range(occurrence, "occurrence", "occurrenceID")

  issues_list[[length(issues_list) + 1]] <- .qc_rel_occ_event(occurrence, event)
  issues_list[[length(issues_list) + 1]] <- .qc_parentEventID(event)

  if (is.data.frame(emof) && nrow(emof) > 0) {
    issues_list[[length(issues_list) + 1]] <- .qc_emof_required(emof)
    issues_list[[length(issues_list) + 1]] <- .qc_emof_duplicates(emof)
  }

  if (is.data.frame(pre_issues) && nrow(pre_issues) > 0) {
    pi <- pre_issues
    if (!"severity" %in% names(pi)) pi$severity <- "WARN"
    if (!"field" %in% names(pi)) pi$field <- "pre_split"
    if (!"rule" %in% names(pi)) pi$rule <- "pre_split_issue"
    if (!"message" %in% names(pi)) pi$message <- "Pre-split issue"
    if (!"row" %in% names(pi)) pi$row <- "*"

    pi_out <- data.frame(
      table = "pre_split",
      row_id = as.character(pi$row),
      field = as.character(pi$field),
      rule = as.character(pi$rule),
      message = as.character(pi$message),
      severity = as.character(pi$severity),
      stringsAsFactors = FALSE
    )
    issues_list[[length(issues_list) + 1]] <- pi_out
  }

  issues_detailed <- .qc_bind(issues_list)

  worms <- .qc_worms_resolve(occurrence)
  if (is.data.frame(worms$issues) && nrow(worms$issues) > 0) {
    issues_detailed <- rbind(issues_detailed, worms$issues)
  }

  if (is.data.frame(emof) && nrow(emof) > 0) {
    if ("measurementTypeID" %in% names(emof)) {
      issues_detailed <- rbind(issues_detailed, .qc_uri_resolve(emof, "measurementTypeID", "emof"))
    }
    if ("measurementUnitID" %in% names(emof)) {
      issues_detailed <- rbind(issues_detailed, .qc_uri_resolve(emof, "measurementUnitID", "emof"))
    }
  }

  issues_detailed$severity <- toupper(as.character(issues_detailed$severity))
  issues_detailed$table <- tolower(as.character(issues_detailed$table))

  issues_summary <- .qc_make_summary(issues_detailed)

  .make_invalid <- function(df, table_name, id_field, issues_det) {
    if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
    sub <- issues_det[issues_det$table == table_name &
      issues_det$severity %in% c("ERROR", "WARN"), , drop = FALSE]
    if (nrow(sub) == 0) return(data.frame())
    if (!id_field %in% names(df)) return(data.frame())

    ids_bad <- unique(sub$row_id[sub$row_id != "*" & !is.na(sub$row_id)])
    if (length(ids_bad) == 0) return(data.frame())

    out <- df[as.character(df[[id_field]]) %in% ids_bad, , drop = FALSE]
    if (nrow(out) == 0) return(data.frame())

    fields <- unique(as.character(sub$field))
    for (f in fields) {
      coln <- paste0(f, "_error")
      msg_map <- sub[sub$field == f, c("row_id", "message"), drop = FALSE]
      msg_map <- msg_map[!duplicated(msg_map$row_id), , drop = FALSE]
      out[[coln]] <- msg_map$message[match(as.character(out[[id_field]]), msg_map$row_id)]
    }

    err_cols <- grep("_error$", names(out), value = TRUE)
    out$row <- seq_len(nrow(out))
    keep <- c("row", err_cols, setdiff(names(out), c("row", err_cols)))
    out <- out[, unique(keep), drop = FALSE]
    out
  }

  invalid_event <- .make_invalid(event, "event", "eventID", issues_detailed)
  invalid_occ <- .make_invalid(occurrence, "occurrence", "occurrenceID", issues_detailed)

  if (!is.data.frame(emof)) emof <- data.frame()
  if (nrow(emof) > 0 && !("measurementID" %in% names(emof))) emof$row <- seq_len(nrow(emof))
  id_emof <- if ("measurementID" %in% names(emof)) {
    "measurementID"
  } else if ("row" %in% names(emof)) {
    "row"
  } else {
    "row"
  }
  invalid_emof <- .make_invalid(emof, "emof", id_emof, issues_detailed)

  # Coordenadas e issues para mapa
  map_issues <- data.frame()

  get_coords <- function(df, id_field) {
    if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
    if (!all(c("decimalLatitude", "decimalLongitude") %in% names(df))) return(NULL)
    if (!id_field %in% names(df)) return(NULL)

    lat <- .qc_num(df$decimalLatitude)
    lon <- .qc_num(df$decimalLongitude)
    ok <- is.finite(lat) & is.finite(lon)
    if (!any(ok)) return(NULL)

    data.frame(
      id = as.character(df[[id_field]][ok]),
      decimalLatitude = lat[ok],
      decimalLongitude = lon[ok],
      stringsAsFactors = FALSE
    )
  }

  occ_coords <- get_coords(occurrence, "occurrenceID")
  ev_coords <- get_coords(event, "eventID")

  coords_src <- "occurrence"
  coords <- occ_coords
  if (is.null(coords)) {
    coords_src <- "event"
    coords <- ev_coords
  }

  bounds <- NULL
  if (!is.null(coords) && is.data.frame(coords) && nrow(coords) > 0) {
    bounds <- list(
      lng1 = min(coords$decimalLongitude, na.rm = TRUE),
      lat1 = min(coords$decimalLatitude,  na.rm = TRUE),
      lng2 = max(coords$decimalLongitude, na.rm = TRUE),
      lat2 = max(coords$decimalLatitude,  na.rm = TRUE)
    )
  }

  if (!is.null(coords) && is.data.frame(coords) && nrow(issues_detailed) > 0) {
    det2 <- issues_detailed[issues_detailed$table %in% c("occurrence", "event"), , drop = FALSE]
    det2 <- det2[det2$row_id != "*" & !is.na(det2$row_id), , drop = FALSE]
    if (nrow(det2) > 0) {
      joined <- merge(det2, coords, by.x = "row_id", by.y = "id", all = FALSE)
      if (nrow(joined) > 0) {
        map_issues <- data.frame(
          decimalLatitude = joined$decimalLatitude,
          decimalLongitude = joined$decimalLongitude,
          id = joined$row_id,
          table = joined$table,
          field = joined$field,
          issue = joined$message,
          severity = joined$severity,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  hierarchy_issues <- .qc_parentEventID(event)
  edges <- data.frame()
  if (is.data.frame(event) && nrow(event) > 0 &&
    all(c("eventID", "parentEventID") %in% names(event))) {
    edges <- data.frame(
      from = as.character(event$parentEventID),
      to = as.character(event$eventID),
      stringsAsFactors = FALSE
    )
    edges <- edges[!(.qc_is_blank(edges$from) | .qc_is_blank(edges$to)), , drop = FALSE]
  }

  n_errors <- sum(issues_detailed$severity == "ERROR", na.rm = TRUE)
  n_warn <- sum(issues_detailed$severity == "WARN", na.rm = TRUE)
  can_export <- (n_errors == 0)

  overview_event_occ <- data.frame()
  if (is.data.frame(event) && is.data.frame(occurrence)) {
    event_type <- rep("Event", nrow(event))
    if ("parentEventID" %in% names(event)) {
      event_type[.qc_is_blank(event$parentEventID)] <- "Cruise/root"
      event_type[!.qc_is_blank(event$parentEventID)] <- "Sample/child"
    }

    n_events_tab <- table(event_type)
    ev_tab <- data.frame(
      event_type = names(n_events_tab),
      n_events = as.integer(n_events_tab),
      stringsAsFactors = FALSE
    )

    bor <- if ("basisOfRecord" %in% names(occurrence)) {
      tb <- sort(table(occurrence$basisOfRecord), decreasing = TRUE)
      if (length(tb) > 0) names(tb)[1] else NA_character_
    } else {
      NA_character_
    }

    n_present <- if ("occurrenceStatus" %in% names(occurrence)) {
      sum(tolower(trimws(as.character(occurrence$occurrenceStatus))) == "present", na.rm = TRUE)
    } else {
      NA_integer_
    }

    n_na <- if ("occurrenceStatus" %in% names(occurrence)) {
      sum(.qc_is_blank(occurrence$occurrenceStatus), na.rm = TRUE)
    } else {
      NA_integer_
    }

    ev_tab$basisOfRecord <- bor
    ev_tab$n_present <- n_present
    ev_tab$n_NA <- n_na

    overview_event_occ <- ev_tab
  }

  overview_emof_types <- data.frame()
  if (is.data.frame(emof) && nrow(emof) > 0 && "measurementType" %in% names(emof)) {
    idlink <- if ("eventID" %in% names(emof) && !all(.qc_is_blank(emof$eventID))) {
      "eventMoF"
    } else {
      "occurrenceMoF"
    }
    mt <- as.character(emof$measurementType)
    mu <- if ("measurementUnit" %in% names(emof)) {
      as.character(emof$measurementUnit)
    } else {
      NA_character_
    }
    mv <- if ("measurementValue" %in% names(emof)) {
      .qc_num(emof$measurementValue)
    } else {
      NA_real_
    }

    key <- paste(idlink, mt, mu, sep = "||")
    tab <- sort(table(key), decreasing = TRUE)

    if (length(tab) > 0) {
      parts_list <- strsplit(names(tab), "||", fixed = TRUE)
      parts_list <- lapply(parts_list, function(p) {
        p <- as.character(p)
        length(p) <- 3
        p[is.na(p)] <- ""
        p
      })
      parts <- do.call(rbind, parts_list)

      overview_emof_types <- data.frame(
        IDlink = parts[, 1],
        measurementType = parts[, 2],
        measurementUnit = parts[, 3],
        count = as.integer(tab),
        stringsAsFactors = FALSE
      )

      if (!all(is.na(mv))) {
        overview_emof_types$minValue <- NA_real_
        overview_emof_types$maxValue <- NA_real_
        for (i in seq_len(nrow(overview_emof_types))) {
          sel <- (mt == overview_emof_types$measurementType[i]) &
            (mu == overview_emof_types$measurementUnit[i])
          if (any(sel, na.rm = TRUE)) {
            v <- mv[sel]
            v <- v[is.finite(v)]
            if (length(v) > 0) {
              overview_emof_types$minValue[i] <- min(v)
              overview_emof_types$maxValue[i] <- max(v)
            }
          }
        }
      }
    }
  }

  list(
    overview = list(
      event_occ = overview_event_occ,
      emof_types = overview_emof_types,
      coords_source = coords_src,
      coords_bounds = bounds
    ),
    issues_summary = issues_summary,
    issues_detailed = issues_detailed,
    invalid = list(event = invalid_event, occurrence = invalid_occ, emof = invalid_emof),
    map_issues = map_issues,
    hierarchy = list(issues = hierarchy_issues, edges = edges),
    phase2_tables = worms$tables %||% list(),
    counts = list(errors = n_errors, warnings = n_warn),
    can_export = can_export
  )
}

# =========================================================
# QC module server
# =========================================================

#' QC page server
#'
#' @param id Module id
#' @param event_in Reactive returning event table
#' @param occ_in Reactive returning occurrence table
#' @param emof_in Reactive returning emof table (may be NULL or empty)
#' @param pre_issues_in Reactive returning pre-split issues (optional)
#' @return list with qc_res reactive and can_export reactive
#' @export
mod_qc_server <- function(id, event_in, occ_in,
                          emof_in = NULL, pre_issues_in = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    # ---------------------------------------------------------
    # 1) Garantir que outputs críticos NÃO ficam suspensos
    # ---------------------------------------------------------
    session$onFlushed(function() {
      shiny::outputOptions(output, "debug_cols", suspendWhenHidden = FALSE)
      shiny::outputOptions(output, "debug_overview_event_occ", suspendWhenHidden = FALSE)
      shiny::outputOptions(output, "debug_overview_map", suspendWhenHidden = FALSE)

      shiny::outputOptions(output, "overview_event_occ_tbl", suspendWhenHidden = FALSE)
      shiny::outputOptions(output, "overview_emof_types_tbl", suspendWhenHidden = FALSE)

      shiny::outputOptions(output, "overview_map", suspendWhenHidden = FALSE)
      shiny::outputOptions(output, "issues_map", suspendWhenHidden = FALSE)

      shiny::outputOptions(output, "export_status_ui", suspendWhenHidden = FALSE)
      shiny::outputOptions(output, "kpi_errors", suspendWhenHidden = FALSE)
      shiny::outputOptions(output, "kpi_warnings", suspendWhenHidden = FALSE)
      shiny::outputOptions(output, "kpi_n_occ", suspendWhenHidden = FALSE)

      shiny::outputOptions(output, "overview_tax_plot", suspendWhenHidden = TRUE)
    }, once = TRUE)

    map_ready <- shiny::reactiveVal(FALSE)
    last_coords <- shiny::reactiveVal(NULL)
    last_coords_src <- shiny::reactiveVal(NULL)
    last_issues <- shiny::reactiveVal(NULL)

    shiny::observe({
      cat("\nmap_ready:", map_ready(), " tab:", input$qc_tabs, "\n")
    })

    # ---------------------------------------------------------
    # QC runner
    # ---------------------------------------------------------
    qc_res <- shiny::reactive({
      ev <- event_in()
      oc <- occ_in()
      em <- if (!is.null(emof_in)) emof_in() else data.frame()
      pi <- if (!is.null(pre_issues_in)) pre_issues_in() else NULL

      shiny::req(is.data.frame(ev), is.data.frame(oc))

      tryCatch(
        run_qc_dwca(ev, oc, em, pi),
        error = function(e) {
          issues <- .qc_issue(
            table = "qc",
            row_id = "*",
            field = "run_qc_dwca",
            rule = "qc_internal_error",
            message = conditionMessage(e),
            severity = "ERROR"
          )
          list(
            overview = list(
              event_occ = data.frame(),
              emof_types = data.frame(),
              coords_source = NA_character_,
              coords_bounds = NULL
            ),
            issues_summary = .qc_make_summary(issues),
            issues_detailed = issues,
            invalid = list(
              event = data.frame(),
              occurrence = data.frame(),
              emof = data.frame()
            ),
            map_issues = data.frame(),
            hierarchy = list(issues = data.frame(), edges = data.frame()),
            phase2_tables = list(),
            counts = list(errors = 1L, warnings = 0L),
            can_export = FALSE
          )
        }
      )
    })

    can_export <- shiny::reactive({
      isTRUE(qc_res()$can_export)
    })

    # ---------------------------------------------------------
    # DEBUG outputs
    # ---------------------------------------------------------
    output$debug_cols <- shiny::renderPrint({
      safe_call <- function(fn) {
        tryCatch(
          {
            x <- fn()
            list(
              ok = TRUE,
              class = class(x),
              is_df = is.data.frame(x),
              dim = if (is.data.frame(x)) dim(x) else NULL,
              cols = if (is.data.frame(x)) names(x) else NULL
            )
          },
          error = function(e) {
            list(ok = FALSE, error = conditionMessage(e))
          }
        )
      }

      list(
        qc_tabs_value = input$qc_tabs %||% "(NULL)",
        event_in = safe_call(event_in),
        occ_in  = safe_call(occ_in),
        emof_in = if (!is.null(emof_in)) safe_call(emof_in) else list(ok = TRUE, class = "NULL (param)")
      )
    })

    output$debug_overview_event_occ <- shiny::renderPrint({
      x <- qc_res()$overview$event_occ
      list(
        is_df = is.data.frame(x),
        dim = if (is.data.frame(x)) dim(x) else NULL,
        cols = if (is.data.frame(x)) names(x) else NULL,
        head = if (is.data.frame(x)) utils::head(x, 5) else NULL
      )
    })

    output$debug_overview_map <- shiny::renderPrint({
      ev <- tryCatch(event_in(), error = function(e) NULL)
      oc <- tryCatch(occ_in(), error = function(e) NULL)
      mi <- tryCatch(qc_res()$map_issues, error = function(e) NULL)

      has_ev_coords <- is.data.frame(ev) && all(c("decimalLatitude","decimalLongitude") %in% names(ev))
      has_oc_coords <- is.data.frame(oc) && all(c("decimalLatitude","decimalLongitude") %in% names(oc))

      list(
        leaflet_installed = requireNamespace("leaflet", quietly = TRUE),
        event_dim = if (is.data.frame(ev)) dim(ev) else NULL,
        occ_dim = if (is.data.frame(oc)) dim(oc) else NULL,
        has_event_coords = has_ev_coords,
        has_occ_coords = has_oc_coords,
        map_issues_dim = if (is.data.frame(mi)) dim(mi) else NULL,
        map_issues_head = if (is.data.frame(mi)) utils::head(mi, 3) else NULL
      )
    })

    # ---------------------------------------------------------
    # Console debug (quando o tab muda)
    # ---------------------------------------------------------
    shiny::observeEvent(input$qc_tabs, {
      ev <- tryCatch(event_in(), error = function(e) NULL)
      oc <- tryCatch(occ_in(), error = function(e) NULL)
      em <- tryCatch(if (!is.null(emof_in)) emof_in() else data.frame(), error = function(e) NULL)

      cat(
        "\n--- DEBUG QC ---",
        "\nQC tab active?:", input$qc_tabs,
        "\nev:", if (is.data.frame(ev)) paste(dim(ev), collapse = " x ") else class(ev)[1] %||% "NULL",
        "\noc:", if (is.data.frame(oc)) paste(dim(oc), collapse = " x ") else class(oc)[1] %||% "NULL",
        "\nemof:", if (is.data.frame(em)) paste(dim(em), collapse = " x ") else class(em)[1] %||% "NULL",
        "\n"
      )
    }, ignoreInit = FALSE)

    # ---------------------------------------------------------
    # KPIs / status
    # ---------------------------------------------------------
    output$kpi_errors <- shiny::renderText({ qc_res()$counts$errors })
    output$kpi_warnings <- shiny::renderText({ qc_res()$counts$warnings })
    output$kpi_n_occ <- shiny::renderText({
      oc <- occ_in()
      if (!is.data.frame(oc)) return(NA_character_)
      nrow(oc)
    })

    output$export_status_ui <- shiny::renderUI({
      ok <- isTRUE(can_export())
      if (ok) {
        shiny::tags$div(class = "qc-kpi qc-ok", "OK (export permitido)")
      } else {
        shiny::tags$div(class = "qc-kpi qc-err", "BLOCKED (há ERROR)")
      }
    })

    # ---------------------------------------------------------
    # Overview tables
    # ---------------------------------------------------------
    output$overview_event_occ_tbl <- DT::renderDT({
      x <- qc_res()$overview$event_occ
      if (!is.data.frame(x) || nrow(x) == 0) {
        x <- data.frame(
          event_type = character(),
          n_events = integer(),
          basisOfRecord = character(),
          n_present = integer(),
          n_NA = integer(),
          stringsAsFactors = FALSE
        )
      }

      DT::datatable(
        x,
        rownames = FALSE,
        options = list(
          dom = "t",
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          ordering = FALSE,
          autoWidth = TRUE
        ),
        width = "auto"
      )
    })

    output$overview_emof_types_tbl <- DT::renderDT({
      x <- qc_res()$overview$emof_types

      if (!is.data.frame(x) || nrow(x) == 0) {
        x <- data.frame(
          IDlink = character(),
          measurementType = character(),
          measurementUnit = character(),
          count = integer(),
          minValue = numeric(),
          maxValue = numeric(),
          stringsAsFactors = FALSE
        )
      }

      if ("count" %in% names(x)) {
        x <- x[order(x$count, decreasing = TRUE), , drop = FALSE]
      }

      DT::datatable(
        x,
        rownames = FALSE,
        options = list(
          dom = "t",
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          ordering = FALSE,
          autoWidth = TRUE
        ),
        width = "auto"
      )
    })

    # TODO:
    # Enrich overview_emof_types with NVS metadata
    # - Use measurementTypeID (if available)
    # - Lookup NERC Vocabulary Server
    # - Add columns:
    #   * TypeID_standardUnit
    #   * TypeID_name
    #   * TypeID_definition
    # Must implement with:
    # - Unique URI lookup
    # - Cache
    # - Timeout protection

    # ---------------------------------------------------------
    # Overview map: sempre aparece (mapa base) desde o início
    # ---------------------------------------------------------
    output$overview_map <- leaflet::renderLeaflet({
      if (!requireNamespace("leaflet", quietly = TRUE)) return(NULL)

      map_ready(TRUE)

      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addTiles(
          urlTemplate = "https://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
          group = "OpenSeaMap",
          options = leaflet::tileOptions(opacity = 0.9)
        ) |>
        leaflet::addLayersControl(
          baseGroups = c("OSM"),
          overlayGroups = c("OpenSeaMap", "Records", "Issues"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::setView(lng = 0, lat = 0, zoom = 2)

      htmlwidgets::onRender(
        m,
        "
        function(el, x) {
          var map = this;

          // 1) Tenta logo após render
          setTimeout(function(){ map.invalidateSize(); }, 0);
          setTimeout(function(){ map.invalidateSize(); }, 250);
          setTimeout(function(){ map.invalidateSize(); }, 1000);

          // 2) Observa mudanças de tamanho do container
          if (window.ResizeObserver) {
            var ro = new ResizeObserver(function(){
              map.invalidateSize();
            });
            ro.observe(el);
          }
        }
        "
      )
    })

    # Atualiza markers via leafletProxy quando os dados existirem
    shiny::observeEvent(
      list(map_ready(), qc_res(), input$qc_tabs),
      {
        cat("\n==============================\n")
        cat("[OBS] DISPAROU\n")
        cat("[OBS] map_ready=", map_ready(), "\n")
        cat("[OBS] qc_tabs=", if (is.null(input$qc_tabs)) "NULL" else paste0("'", input$qc_tabs, "'"), "\n")

        if (!requireNamespace("leaflet", quietly = TRUE)) {
          cat("[OBS] RETURN: leaflet NAO instalado\n")
          return(NULL)
        }

        if (!isTRUE(map_ready())) {
          cat("[OBS] RETURN: map_ready ainda FALSE\n")
          return(NULL)
        }

        # tab_ok: só para operações sensíveis (fitBounds / invalidateSize, etc.)
        tab_ok <- isTRUE(
          !is.null(input$qc_tabs) &&
            nzchar(input$qc_tabs) &&
            identical(input$qc_tabs, "data_overview")
        )
        cat("[OBS] tab_ok=", tab_ok, "\n")

        cat("[OBS] OK: vai tentar ler dados (mesmo se qc_tabs for NULL)\n")

        ev <- tryCatch(event_in(), error = function(e) {
          cat("[OBS] ERRO event_in():", conditionMessage(e), "\n")
          NULL
        })
        oc <- tryCatch(occ_in(), error = function(e) {
          cat("[OBS] ERRO occ_in():", conditionMessage(e), "\n")
          NULL
        })
        mi <- tryCatch(qc_res()$map_issues, error = function(e) {
          cat("[OBS] ERRO qc_res()$map_issues:", conditionMessage(e), "\n")
          data.frame()
        })

        cat("[OBS] ev:", if (is.data.frame(ev)) paste(dim(ev), collapse="x") else "NOT_DF", "\n")
        cat("[OBS] oc:", if (is.data.frame(oc)) paste(dim(oc), collapse="x") else "NOT_DF", "\n")
        cat("[OBS] mi:", if (is.data.frame(mi)) paste(dim(mi), collapse="x") else "NOT_DF", "\n")

        if (is.data.frame(ev)) cat("[OBS] ev_cols:", paste(names(ev), collapse=", "), "\n")
        if (is.data.frame(oc)) cat("[OBS] oc_cols:", paste(names(oc), collapse=", "), "\n")

        get_coords <- function(df, id_field, label) {
          if (!is.data.frame(df) || nrow(df) == 0) {
            cat("[OBS] get_coords(", label, "): DF vazio/NA\n", sep = "")
            return(NULL)
          }
          if (!all(c("decimalLatitude", "decimalLongitude") %in% names(df))) {
            cat("[OBS] get_coords(", label, "): sem colunas decimalLatitude/decimalLongitude\n", sep = "")
            return(NULL)
          }
          if (!id_field %in% names(df)) {
            cat("[OBS] get_coords(", label, "): sem id_field=", id_field, "\n", sep = "")
            return(NULL)
          }

          lat <- suppressWarnings(as.numeric(df$decimalLatitude))
          lon <- suppressWarnings(as.numeric(df$decimalLongitude))
          ok <- is.finite(lat) & is.finite(lon)

          cat("[OBS] get_coords(", label, "): n=", nrow(df),
              " ok=", sum(ok, na.rm=TRUE),
              " lat_na=", sum(!is.finite(lat)),
              " lon_na=", sum(!is.finite(lon)),
              "\n", sep = "")

          if (!any(ok)) return(NULL)

          data.frame(
            id = as.character(df[[id_field]][ok]),
            decimalLatitude = lat[ok],
            decimalLongitude = lon[ok],
            stringsAsFactors = FALSE
          )
        }

        coords <- get_coords(oc, "occurrenceID", "occurrence")
        coords_src <- "occurrence"

        if (is.null(coords)) {
          coords <- get_coords(ev, "eventID", "event")
          coords_src <- "event"
        }

        cat("[OBS] coords_src=", coords_src,
            " coords_n=", if (is.null(coords)) "NULL" else nrow(coords),
            "\n", sep = "")
        
        last_coords(coords)
        last_coords_src(coords_src)
        last_issues(mi)
        cat("[OBS] cache: coords=", if (is.null(coords)) "NULL" else nrow(coords),
            " issues=", if (is.data.frame(mi)) nrow(mi) else "NA", "\n")

        cat("[OBS] leafletProxy: limpar grupos\n")
        proxy <- leaflet::leafletProxy("overview_map", session) |>
          leaflet::clearGroup("Records") |>
          leaflet::clearGroup("Issues")

        if (is.data.frame(coords) && nrow(coords) > 0) {
          cat("[OBS] addCircleMarkers Records: n=", nrow(coords), "\n", sep="")

          if (nrow(coords) > 5000) {
            coords <- coords[sample.int(nrow(coords), 5000), , drop = FALSE]
            cat("[OBS] Records: downsample para ", nrow(coords), "\n", sep="")
          }

          proxy <- proxy |>
            leaflet::addCircleMarkers(
              lng = coords$decimalLongitude,
              lat = coords$decimalLatitude,
              radius = 4,
              stroke = FALSE,
              fillOpacity = 0.7,
              group = "Records",
              popup = paste0(
                "<b>source:</b> ", coords_src, "<br/>",
                "<b>id:</b> ", coords$id
              )
            )

          # fitBounds só quando o tab estiver realmente ativo
          if (isTRUE(tab_ok)) {
            proxy <- proxy |>
              leaflet::fitBounds(
                lng1 = min(coords$decimalLongitude, na.rm = TRUE),
                lat1 = min(coords$decimalLatitude,  na.rm = TRUE),
                lng2 = max(coords$decimalLongitude, na.rm = TRUE),
                lat2 = max(coords$decimalLatitude,  na.rm = TRUE)
              )
            cat("[OBS] fitBounds aplicado (tab_ok=TRUE)\n")
          } else {
            cat("[OBS] SKIP fitBounds (tab_ok=FALSE)\n")
          }

        } else {
          cat("[OBS] SKIP Records: coords NULL/0\n")
        }

        if (is.data.frame(mi) && nrow(mi) > 0 &&
            all(c("decimalLongitude","decimalLatitude") %in% names(mi))) {
          cat("[OBS] addCircleMarkers Issues: n=", nrow(mi), "\n", sep="")

          proxy <- proxy |>
            leaflet::addCircleMarkers(
              lng = mi$decimalLongitude,
              lat = mi$decimalLatitude,
              radius = 5,
              stroke = FALSE,
              fillOpacity = 0.8,
              group = "Issues",
              popup = paste0(
                "<b>id:</b> ", mi$id, "<br/>",
                "<b>table:</b> ", mi$table, "<br/>",
                "<b>severity:</b> ", mi$severity, "<br/>",
                "<b>issue:</b> ", mi$issue
              )
            )
        } else {
          cat("[OBS] SKIP Issues: mi vazio/sem colunas coords\n")
        }

        cat("[OBS] FIM\n")
      },
      ignoreInit = FALSE
    )

    # Força repaint quando o tab data_overview abre (evita leaflet com tamanho 0)
    shiny::observeEvent(
      input$qc_tabs,
      {
        if (!requireNamespace("leaflet", quietly = TRUE)) return(NULL)

        tab_ok <- isTRUE(!is.null(input$qc_tabs) && nzchar(input$qc_tabs) &&
                        identical(input$qc_tabs, "data_overview"))
        cat("[TAB] qc_tabs=", if (is.null(input$qc_tabs)) "NULL" else input$qc_tabs,
            " tab_ok=", tab_ok, "\n")

        if (!tab_ok) return(NULL)
        if (!isTRUE(map_ready())) {
          cat("[TAB] map_ready ainda FALSE\n")
          return(NULL)
        }

        coords <- last_coords()
        coords_src <- last_coords_src()
        mi <- last_issues()

        cat("[TAB] reapply: coords=", if (is.null(coords)) "NULL" else nrow(coords),
            " src=", coords_src,
            " issues=", if (is.data.frame(mi)) nrow(mi) else "NA", "\n")

        proxy <- leaflet::leafletProxy("overview_map", session) |>
          leaflet::clearGroup("Records") |>
          leaflet::clearGroup("Issues") |>
          leaflet::invalidateSize()

        if (is.data.frame(coords) && nrow(coords) > 0) {
          proxy <- proxy |>
            leaflet::addCircleMarkers(
              lng = coords$decimalLongitude,
              lat = coords$decimalLatitude,
              radius = 4,
              stroke = FALSE,
              fillOpacity = 0.7,
              group = "Records",
              popup = paste0("<b>source:</b> ", coords_src, "<br/>",
                            "<b>id:</b> ", coords$id)
            ) |>
            leaflet::fitBounds(
              lng1 = min(coords$decimalLongitude, na.rm = TRUE),
              lat1 = min(coords$decimalLatitude,  na.rm = TRUE),
              lng2 = max(coords$decimalLongitude, na.rm = TRUE),
              lat2 = max(coords$decimalLatitude,  na.rm = TRUE)
            )

          cat("[TAB] desenhou Records e aplicou fitBounds\n")
        } else {
          cat("[TAB] sem coords para desenhar\n")
        }

        if (is.data.frame(mi) && nrow(mi) > 0 &&
            all(c("decimalLongitude","decimalLatitude") %in% names(mi))) {
          proxy <- proxy |>
            leaflet::addCircleMarkers(
              lng = mi$decimalLongitude,
              lat = mi$decimalLatitude,
              radius = 5,
              stroke = FALSE,
              fillOpacity = 0.8,
              group = "Issues"
            )
          cat("[TAB] desenhou Issues\n")
        }
      },
      ignoreInit = FALSE
    )

    # ---------------------------------------------------------
    # Taxonomic UI/plot
    # ---------------------------------------------------------
    output$kingdom_ui <- shiny::renderUI({
      oc <- occ_in()
      if (!is.data.frame(oc) || nrow(oc) == 0) return(NULL)
      if (!("kingdom" %in% names(oc))) {
        return(shiny::tags$div(class = "qc-muted", "Campo 'kingdom' não disponível."))
      }
      choices <- sort(unique(stats::na.omit(as.character(oc$kingdom))))
      if (length(choices) == 0) return(NULL)
      shiny::selectInput(
        session$ns("kingdom_sel"),
        "Select kingdom",
        choices = choices,
        selected = choices[1]
      )
    })

    output$overview_tax_plot <- shiny::renderPlot({
      oc <- occ_in()
      if (!is.data.frame(oc) || nrow(oc) == 0) return(invisible(NULL))
      if (!("kingdom" %in% names(oc))) return(invisible(NULL))
      if (!("class" %in% names(oc))) return(invisible(NULL))

      k <- input$kingdom_sel %||% NA_character_
      sel <- as.character(oc$kingdom) == k
      x <- as.character(oc$class[sel])
      x[.qc_is_blank(x)] <- NA_character_
      x <- stats::na.omit(x)
      if (length(x) == 0) return(invisible(NULL))

      tab <- sort(table(x), decreasing = TRUE)
      tab <- head(tab, 15)

      oldpar <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(oldpar), add = TRUE)
      graphics::par(mar = c(4, 8, 1.5, 1))
      graphics::barplot(tab, horiz = TRUE, las = 1, main = "")
    })

    # ---------------------------------------------------------
    # Issues tables
    # ---------------------------------------------------------
    output$issues_summary_tbl <- DT::renderDT({
      x <- qc_res()$issues_summary
      if (!is.data.frame(x)) x <- data.frame()
      DT::datatable(x, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
    })

    output$issues_detailed_tbl <- DT::renderDT({
      x <- qc_res()$issues_detailed
      if (!is.data.frame(x)) x <- data.frame()
      DT::datatable(x, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 20))
    })

    output$tax_issues_tbl <- DT::renderDT({
      tabs <- qc_res()$phase2_tables
      unmatched <- tabs$unmatched %||% data.frame()
      broken <- tabs$broken %||% data.frame()

      out <- data.frame()
      if (is.data.frame(unmatched) && nrow(unmatched) > 0) {
        unmatched$type <- "unmatched_taxa"
        out <- rbind(out, unmatched)
      }
      if (is.data.frame(broken) && nrow(broken) > 0) {
        broken$type <- "broken_one_to_one"
        out <- rbind(out, broken)
      }
      DT::datatable(out, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$emof_issues_tbl <- DT::renderDT({
      x <- qc_res()$issues_detailed
      if (!is.data.frame(x)) x <- data.frame()
      x <- x[x$table == "emof", , drop = FALSE]
      DT::datatable(x, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15))
    })

    # ---------------------------------------------------------
    # Issues map (como já tinhas)
    # ---------------------------------------------------------
    output$issues_map <- leaflet::renderLeaflet({
      if (!requireNamespace("leaflet", quietly = TRUE)) return(NULL)

      mi <- qc_res()$map_issues
      leaf <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap") |>
        leaflet::addProviderTiles("OpenSeaMap", group = "OpenSeaMap") |>
        leaflet::addLayersControl(
          baseGroups = c("OpenStreetMap"),
          overlayGroups = c("OpenSeaMap"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )

      if (!is.data.frame(mi) || nrow(mi) == 0) return(leaf)

      leaf |>
        leaflet::addCircleMarkers(
          lng = mi$decimalLongitude,
          lat = mi$decimalLatitude,
          radius = 5,
          stroke = FALSE,
          fillOpacity = 0.75,
          popup = paste0(
            "<b>decimalLatitude:</b> ", mi$decimalLatitude, "<br/>",
            "<b>decimalLongitude:</b> ", mi$decimalLongitude, "<br/>",
            "<b>id:</b> ", mi$id, "<br/>",
            "<b>table:</b> ", mi$table, "<br/>",
            "<b>severity:</b> ", mi$severity, "<br/>",
            "<b>issue:</b> ", mi$issue
          )
        )
    })

    # ---------------------------------------------------------
    # Invalid tables
    # ---------------------------------------------------------
    output$invalid_event_tbl <- DT::renderDT({
      x <- qc_res()$invalid$event
      if (!is.data.frame(x)) x <- data.frame()
      DT::datatable(x, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$invalid_occ_tbl <- DT::renderDT({
      x <- qc_res()$invalid$occurrence
      if (!is.data.frame(x)) x <- data.frame()
      DT::datatable(x, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$invalid_emof_tbl <- DT::renderDT({
      x <- qc_res()$invalid$emof
      if (!is.data.frame(x)) x <- data.frame()
      DT::datatable(x, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    # ---------------------------------------------------------
    # Hierarchy
    # ---------------------------------------------------------
    output$hierarchy_issues_tbl <- DT::renderDT({
      x <- qc_res()$hierarchy$issues
      if (!is.data.frame(x)) x <- data.frame()
      DT::datatable(x, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$hierarchy_edges_tbl <- DT::renderDT({
      x <- qc_res()$hierarchy$edges
      if (!is.data.frame(x)) x <- data.frame()
      DT::datatable(x, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
    })

    # ---------------------------------------------------------
    # About
    # ---------------------------------------------------------
    output$about_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::tags$h5("Phase 1 (deterministic)"),
        shiny::tags$ul(
          shiny::tags$li("eventID / occurrenceID presence + uniqueness"),
          shiny::tags$li("Required field checks (scientificName)"),
          shiny::tags$li("Coordinate range checks (lat/lon)"),
          shiny::tags$li("Occurrence eventID must exist in Event core"),
          shiny::tags$li("parentEventID must exist in Event core"),
          shiny::tags$li("eMoF required fields + duplicate measurementType per link")
        ),
        shiny::tags$h5("Phase 2 (optional with fallbacks)"),
        shiny::tags$ul(
          shiny::tags$li("WoRMS name resolution (package worrms)"),
          shiny::tags$li("Broken one-to-one taxa relationship using scientificNameID (if present)"),
          shiny::tags$li("URI resolution checks for measurementTypeID / measurementUnitID (package httr)")
        ),
        shiny::tags$p(
          class = "qc-muted",
          "Export policy: bloqueia export se existir qualquer issue com severity = ERROR."
        )
      )
    })

    list(
      qc_res = qc_res,
      can_export = can_export
    )
  })
}
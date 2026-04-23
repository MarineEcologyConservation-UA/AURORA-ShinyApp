# =========================================================
# Build DwC-A tables from flat mapped dataframe
# File: R/build_dwca_tables.R
# =========================================================

#' Build Darwin Core Archive tables from input data and field mapping
#'
#' @export
build_dwca_tables <- function(df,
                              dwc_terms,
                              target_database = "GBIF",
                              resource_type = "sampling_event",
                              selected_terms = NULL,
                              emof_spec = NULL) {

  `%||%` <- function(x, y) if (is.null(x)) y else x
  .data <- rlang::.data
  qc_messages <- character(0)

  if (is.null(df) || !is.data.frame(df)) {
    stop("df must be a data.frame.")
  }
  if (nrow(df) == 0) {
    stop("df has 0 rows.")
  }
  if (is.null(dwc_terms) || !is.data.frame(dwc_terms)) {
    stop("dwc_terms must be a data.frame.")
  }
  if (!all(c("Table", "Term") %in% names(dwc_terms))) {
    stop("dwc_terms must contain columns: Table, Term.")
  }
  if (!target_database %in% names(dwc_terms)) {
    stop(paste0("dwc_terms must contain repository column: ", target_database))
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
    x <- as.character(x)
    x <- trimws(x)

    out <- rep(NA_character_, length(x))
    out[x == "Event"] <- "Event"
    out[x == "Occurrence"] <- "Occurrence"
    out[x == "Occurrence.Core"] <- "Occurrence.Core"
    out[x == "eMoF"] <- "eMoF"
    out
  }

  normalize_status <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- trimws(tolower(x))
    x[x %in% c("strongly_recommended", "strongly-recommended")] <- "strongly recommended"
    x
  }

  .first_non_empty <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) return(NA_character_)
    x[[1]]
  }

  .normalize_aurora_types <- function(x) {
    if (!is.data.frame(x)) {
      return(x)
    }

    if (".aurora_origin_row" %in% names(x)) {
      x$.aurora_origin_row <- suppressWarnings(as.integer(x$.aurora_origin_row))
    }

    if (".aurora_origin_id" %in% names(x)) {
      x$.aurora_origin_id <- as.character(x$.aurora_origin_id)
    }

    x
  }

  resource_type <- normalize_resource_type(resource_type)
  df_work <- df

  df_work <- .normalize_aurora_types(df_work)

  aurora_trace_cols <- intersect(
    aurora_internal_cols(),
    names(df_work)
  )

  repo_status <- as.character(dwc_terms[[target_database]])
  repo_status[is.na(repo_status)] <- ""
  repo_status <- normalize_status(repo_status)

  dwc_terms2 <- dwc_terms
  dwc_terms2$.table_norm <- normalize_table_name(dwc_terms2$Table)
  dwc_terms2$.repo_status <- repo_status

  dwc_terms2 <- dwc_terms2[
    !is.na(dwc_terms2$.table_norm) &
      dwc_terms2$.table_norm != "" &
      dwc_terms2$.repo_status != "" &
      dwc_terms2$.repo_status != "na",
    ,
    drop = FALSE
  ]

  cfg <- if (identical(resource_type, "sampling_event")) {
    list(
      resource_type = "sampling_event",
      event_table_key = "Event",
      occurrence_table_key = "Occurrence",
      build_event = TRUE,
      build_occurrence = TRUE,
      allow_emof_event = TRUE,
      allow_emof_occurrence = TRUE,
      architecture_label = "Sampling event (Event core)"
    )
  } else {
    list(
      resource_type = "occurrence_core",
      event_table_key = NULL,
      occurrence_table_key = "Occurrence.Core",
      build_event = FALSE,
      build_occurrence = TRUE,
      allow_emof_event = FALSE,
      allow_emof_occurrence = TRUE,
      architecture_label = "Occurrence (Occurrence core)"
    )
  }

  if ("eventID" %in% names(df_work)) {
    df_work$eventID <- as.character(df_work$eventID)
    df_work$eventID[is.na(df_work$eventID)] <- ""
  }

  if ("occurrenceID" %in% names(df_work)) {
    df_work$occurrenceID <- as.character(df_work$occurrenceID)
    df_work$occurrenceID[is.na(df_work$occurrenceID)] <- ""
  }

  if ("parentEventID" %in% names(df_work)) {
    df_work$parentEventID <- as.character(df_work$parentEventID)
    df_work$parentEventID[trimws(df_work$parentEventID) == ""] <- NA_character_
  }

  if (isTRUE(cfg$build_event)) {
    if (!"eventID" %in% names(df_work)) {
      qc_messages <- c(qc_messages, "ERROR: Input dataframe is missing eventID for Event core architecture.")
    } else if (any(trimws(df_work$eventID) == "")) {
      qc_messages <- c(qc_messages, "ERROR: Some rows have blank eventID in input dataframe.")
    }
  }

  if (isTRUE(cfg$build_occurrence)) {
    if (!"occurrenceID" %in% names(df_work)) {
      qc_messages <- c(qc_messages, "ERROR: Input dataframe is missing occurrenceID.")
    } else if (any(trimws(df_work$occurrenceID) == "")) {
      qc_messages <- c(qc_messages, "ERROR: Some rows have blank occurrenceID in input dataframe.")
    }
  }

  get_repo_terms <- function(tbl) {
    if (is.null(tbl) || !nzchar(tbl)) {
      return(character(0))
    }

    x <- dwc_terms2 |>
      dplyr::filter(.data$.table_norm == tbl) |>
      dplyr::pull(.data$Term) |>
      unique()

    x <- intersect(x, names(df_work))
    sort(unique(x))
  }

  repo_event_terms <- get_repo_terms(cfg$event_table_key)
  repo_occ_terms <- get_repo_terms(cfg$occurrence_table_key)

  event_terms <- character(0)
  occurrence_terms <- character(0)

  if (!is.null(selected_terms) && is.list(selected_terms)) {
    if (isTRUE(cfg$build_event)) {
      event_terms <- unique(c(
        "eventID",
        "parentEventID",
        selected_terms$event %||% character(0),
        aurora_trace_cols
      ))
    }

    if (isTRUE(cfg$build_occurrence)) {
      occurrence_terms <- unique(c(
        "occurrenceID",
        selected_terms$occurrence %||% character(0),
        aurora_trace_cols
      ))

      if (identical(resource_type, "sampling_event")) {
        occurrence_terms <- unique(c("eventID", occurrence_terms))
      }
    }
  } else {
    if (isTRUE(cfg$build_event)) {
      event_terms <- unique(c(
        "eventID",
        "parentEventID",
        repo_event_terms,
        aurora_trace_cols
      ))
    }

    if (isTRUE(cfg$build_occurrence)) {
      occurrence_terms <- unique(c(
        "occurrenceID",
        repo_occ_terms,
        aurora_trace_cols
      ))

      if (identical(resource_type, "sampling_event")) {
        occurrence_terms <- unique(c("eventID", occurrence_terms))
      }
    }
  }

  event_terms <- intersect(event_terms, names(df_work))
  occurrence_terms <- intersect(occurrence_terms, names(df_work))

  event_table <- data.frame()
  occurrence_table <- data.frame()

  if (isTRUE(cfg$build_event)) {
    if ("eventID" %in% names(df_work)) {
      event_table <- df_work |>
        dplyr::select(dplyr::any_of(event_terms)) |>
        dplyr::distinct(.data$eventID, .keep_all = TRUE)

      event_table <- .normalize_aurora_types(event_table)
    }
  }

  if (isTRUE(cfg$build_occurrence)) {
    if ("occurrenceID" %in% names(df_work)) {
      occurrence_table <- df_work |>
        dplyr::select(dplyr::any_of(occurrence_terms)) |>
        dplyr::distinct(.data$occurrenceID, .keep_all = TRUE)

      occurrence_table <- .normalize_aurora_types(occurrence_table)
    }
  }

  emof_table <- NULL

  if (!is.null(emof_spec) &&
      !is.null(emof_spec$columns) &&
      length(emof_spec$columns) > 0) {

    miss <- setdiff(emof_spec$columns, names(df_work))
    if (length(miss) > 0) {
      qc_messages <- c(
        qc_messages,
        paste0(
          "WARNING: eMoF columns missing and ignored: ",
          paste(miss, collapse = ", ")
        )
      )
    }

    cols_ok <- intersect(emof_spec$columns, names(df_work))

    if (length(cols_ok) > 0) {
      levels <- emof_spec$levels

      if (is.null(levels) || length(levels) == 0) {
        stop("emof_spec$levels must be a named list mapping column -> level.")
      }

      bad <- cols_ok[!cols_ok %in% names(levels)]
      if (length(bad) > 0) {
        stop(paste0(
          "Missing eMoF levels for columns: ",
          paste(bad, collapse = ", ")
        ))
      }

      event_cols <- cols_ok[
        vapply(cols_ok, function(x) identical(levels[[x]], "event"), logical(1))
      ]
      occurrence_cols <- cols_ok[
        vapply(cols_ok, function(x) identical(levels[[x]], "occurrence"), logical(1))
      ]

      if (!isTRUE(cfg$allow_emof_event) && length(event_cols) > 0) {
        stop(
          "Event-level eMoF is not allowed for the selected resource type.",
          call. = FALSE
        )
      }

      if (!isTRUE(cfg$allow_emof_occurrence) && length(occurrence_cols) > 0) {
        stop(
          "Occurrence-level eMoF is not allowed for the selected resource type.",
          call. = FALSE
        )
      }

      emof_parts <- list()

      if (length(event_cols) > 0) {
        if (!"eventID" %in% names(df_work)) {
          qc_messages <- c(
            qc_messages,
            "ERROR: Cannot build event-level eMoF because eventID is missing."
          )
        } else {
          for (cc in event_cols) {
            conflicts <- df_work |>
              dplyr::mutate(.tmp_val = as.character(.data[[cc]])) |>
              dplyr::mutate(.tmp_val = trimws(.data$.tmp_val)) |>
              dplyr::filter(!is.na(.data$.tmp_val) & .data$.tmp_val != "") |>
              dplyr::group_by(.data$eventID) |>
              dplyr::summarise(
                n_values = dplyr::n_distinct(.data$.tmp_val),
                .groups = "drop"
              ) |>
              dplyr::filter(.data$n_values > 1)

            if (nrow(conflicts) > 0) {
              qc_messages <- c(
                qc_messages,
                paste0(
                  "WARNING: event-level eMoF column '", cc,
                  "' has conflicting values within the same eventID; keeping the first non-empty value per event."
                )
              )
            }
          }

          event_emof_wide <- df_work |>
            dplyr::select(
              .data$eventID,
              dplyr::any_of(aurora_trace_cols),
              dplyr::all_of(event_cols)
            ) |>
            dplyr::group_by(.data$eventID) |>
            dplyr::summarise(
              dplyr::across(
                dplyr::all_of(c(aurora_trace_cols, event_cols)),
                .first_non_empty
              ),
              .groups = "drop"
            )

          event_emof <- event_emof_wide |>
            tidyr::pivot_longer(
              cols = dplyr::all_of(event_cols),
              names_to = "measurementType",
              values_to = "measurementValue",
              values_transform = list(measurementValue = as.character)
            ) |>
            dplyr::filter(!is.na(.data$measurementValue) & trimws(.data$measurementValue) != "") |>
            dplyr::mutate(occurrenceID = "")

          event_emof <- .normalize_aurora_types(event_emof)
          emof_parts[["event"]] <- event_emof
        }
      }

      if (length(occurrence_cols) > 0) {
        if (!"occurrenceID" %in% names(df_work)) {
          qc_messages <- c(
            qc_messages,
            "ERROR: Cannot build occurrence-level eMoF because occurrenceID is missing."
          )
        } else {
          if (!"eventID" %in% names(df_work)) {
            df_work$eventID <- ""
          }

          occurrence_emof <- df_work |>
            dplyr::select(
              .data$eventID,
              .data$occurrenceID,
              dplyr::any_of(aurora_trace_cols),
              dplyr::all_of(occurrence_cols)
            ) |>
            dplyr::distinct() |>
            tidyr::pivot_longer(
              cols = dplyr::all_of(occurrence_cols),
              names_to = "measurementType",
              values_to = "measurementValue",
              values_transform = list(measurementValue = as.character)
            ) |>
            dplyr::filter(!is.na(.data$measurementValue) & trimws(.data$measurementValue) != "")

          occurrence_emof <- .normalize_aurora_types(occurrence_emof)
          emof_parts[["occurrence"]] <- occurrence_emof
        }
      }

      if (length(emof_parts) > 0) {
        emof_parts <- lapply(emof_parts, .normalize_aurora_types)

        emof_table <- dplyr::bind_rows(emof_parts)
        emof_table <- .normalize_aurora_types(emof_table)

        for (nm in c(
          "measurementTypeID",
          "measurementValueID",
          "measurementUnit",
          "measurementUnitID"
        )) {
          if (!nm %in% names(emof_table)) emof_table[[nm]] <- ""
        }

        emof_table <- emof_table |>
          dplyr::select(
            .data$eventID,
            .data$occurrenceID,
            dplyr::any_of(aurora_trace_cols),
            .data$measurementType,
            .data$measurementTypeID,
            .data$measurementValue,
            .data$measurementValueID,
            .data$measurementUnit,
            .data$measurementUnitID
          ) |>
          dplyr::distinct()

        emof_table <- .normalize_aurora_types(emof_table)

        dup_event_measurements <- emof_table |>
          dplyr::filter(.data$occurrenceID == "") |>
          dplyr::count(.data$eventID, .data$measurementType, name = "n") |>
          dplyr::filter(.data$n > 1)

        if (nrow(dup_event_measurements) > 0) {
          qc_messages <- c(
            qc_messages,
            "WARNING: duplicate event-level eMoF records were detected and collapsed."
          )

          emof_table_event <- emof_table |>
            dplyr::filter(.data$occurrenceID == "") |>
            dplyr::distinct(.data$eventID, .data$measurementType, .keep_all = TRUE)

          emof_table_occ <- emof_table |>
            dplyr::filter(.data$occurrenceID != "")

          emof_table <- dplyr::bind_rows(emof_table_event, emof_table_occ) |>
            dplyr::distinct()

          emof_table <- .normalize_aurora_types(emof_table)
        }
      }
    }
  }

  if (isTRUE(cfg$build_event)) {
    if (!is.data.frame(event_table) || nrow(event_table) == 0) {
      qc_messages <- c(qc_messages, "ERROR: event table is empty.")
    } else {
      if (!"eventID" %in% names(event_table)) {
        qc_messages <- c(qc_messages, "ERROR: event table missing eventID.")
      } else {
        event_ids <- trimws(as.character(event_table$eventID))
        if (any(is.na(event_ids) | event_ids == "")) {
          qc_messages <- c(qc_messages, "ERROR: event table contains blank eventID.")
        }
        if (anyDuplicated(event_ids) > 0) {
          qc_messages <- c(qc_messages, "ERROR: Duplicate eventID detected in event table.")
        }
      }

      if ("parentEventID" %in% names(event_table)) {
        pid <- trimws(as.character(event_table$parentEventID))
        pid[is.na(pid) | pid == ""] <- NA_character_
        if (any(!is.na(pid) & !(pid %in% as.character(event_table$eventID)))) {
          qc_messages <- c(
            qc_messages,
            "WARNING: Some parentEventID values do not match any eventID in event table."
          )
        }
      }
    }
  } else {
    event_table <- data.frame()
  }

  if (isTRUE(cfg$build_occurrence)) {
    if (!is.data.frame(occurrence_table) || nrow(occurrence_table) == 0) {
      qc_messages <- c(qc_messages, "WARNING: occurrence table is empty.")
    } else {
      if (!"occurrenceID" %in% names(occurrence_table)) {
        qc_messages <- c(qc_messages, "ERROR: occurrence table missing occurrenceID.")
      } else {
        occurrence_ids <- trimws(as.character(occurrence_table$occurrenceID))
        if (any(is.na(occurrence_ids) | occurrence_ids == "")) {
          qc_messages <- c(qc_messages, "ERROR: occurrence table contains blank occurrenceID.")
        }
        if (anyDuplicated(occurrence_ids) > 0) {
          qc_messages <- c(qc_messages, "ERROR: Duplicate occurrenceID detected in occurrence table.")
        }
      }

      if (identical(resource_type, "sampling_event") &&
          "eventID" %in% names(occurrence_table) &&
          "eventID" %in% names(event_table)) {
        occ_event_ids <- trimws(as.character(occurrence_table$eventID))
        ev_ids <- trimws(as.character(event_table$eventID))
        missing_links <- !is.na(occ_event_ids) & occ_event_ids != "" & !(occ_event_ids %in% ev_ids)
        if (any(missing_links)) {
          qc_messages <- c(
            qc_messages,
            "WARNING: Some occurrence eventID values do not match any eventID in event table."
          )
        }
      }
    }
  } else {
    occurrence_table <- data.frame()
  }

  if (!is.null(emof_table)) {
    if (!"occurrenceID" %in% names(emof_table)) {
      qc_messages <- c(qc_messages, "ERROR: eMoF table missing occurrenceID.")
    }

    if (identical(resource_type, "sampling_event")) {
      if (!"eventID" %in% names(emof_table)) {
        qc_messages <- c(qc_messages, "ERROR: eMoF table missing eventID.")
      }

      if (all(c("eventID", "occurrenceID", "measurementType") %in% names(emof_table))) {
        dup_event_measurements <- emof_table |>
          dplyr::filter(.data$occurrenceID == "") |>
          dplyr::count(.data$eventID, .data$measurementType, name = "n") |>
          dplyr::filter(.data$n > 1)

        if (nrow(dup_event_measurements) > 0) {
          qc_messages <- c(
            qc_messages,
            "ERROR: Duplicate measurementType linked to the same eventID still exists in eMoF."
          )
        }
      }
    }
  }

  if (length(qc_messages) == 0) {
    qc_messages <- "OK: build completed with no QC messages."
  } else {
    qc_messages <- unique(qc_messages)
  }

  list(
    event = event_table,
    occurrence = occurrence_table,
    emof = emof_table,
    qc = qc_messages
  )
}
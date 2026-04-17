# =========================================================
# Build DwC-A tables from flat mapped dataframe
# - Assumes eventID, occurrenceID, parentEventID, and remarks
#   were already created upstream when applicable.
# - This function only splits the flat dataframe into DwC-A
#   tables, optionally builds eMoF, and performs basic QC.
# - Keeps .aurora traceability columns in event, occurrence,
#   and eMoF tables when present.
# =========================================================

#' @export
build_dwca_tables <- function(df,
                              dwc_terms,
                              emof_spec = NULL) {

  .data <- rlang::.data
  qc_messages <- character(0)

  if (is.null(df) || !is.data.frame(df)) {
    stop("df must be a data.frame.")
  }
  if (nrow(df) == 0) {
    stop("df has 0 rows.")
  }
  if (is.null(dwc_terms) || !is.data.frame(dwc_terms)) {
    stop("dwc_terms must be a data.frame (with columns Table, Term).")
  }
  if (!all(c("Table", "Term") %in% names(dwc_terms))) {
    stop("dwc_terms must contain columns: Table, Term.")
  }

  df_work <- df

  # -------------------------------------------------------
  # helpers
  # -------------------------------------------------------
  .first_non_empty <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) return(NA_character_)
    x[[1]]
  }

  # Keep internal traceability columns if present
  aurora_trace_cols <- intersect(
    aurora_internal_cols(),
    names(df_work)
  )

  # -------------------------------------------------------
  # 1) BASIC PRESENCE CHECKS ON INPUT DF
  # -------------------------------------------------------
  if (!"eventID" %in% names(df_work)) {
    qc_messages <- c(qc_messages, "ERROR: Input dataframe is missing eventID.")
  } else {
    df_work$eventID <- as.character(df_work$eventID)
    df_work$eventID[is.na(df_work$eventID)] <- ""
    if (any(trimws(df_work$eventID) == "")) {
      qc_messages <- c(qc_messages, "ERROR: Some rows have blank eventID in input dataframe.")
    }
  }

  if ("occurrenceID" %in% names(df_work)) {
    df_work$occurrenceID <- as.character(df_work$occurrenceID)
    df_work$occurrenceID[is.na(df_work$occurrenceID)] <- ""
    if (any(trimws(df_work$occurrenceID) == "")) {
      qc_messages <- c(qc_messages, "ERROR: Some rows have blank occurrenceID in input dataframe.")
    }
  } else {
    qc_messages <- c(qc_messages, "WARNING: Input dataframe does not contain occurrenceID.")
  }

  if ("parentEventID" %in% names(df_work)) {
    df_work$parentEventID <- as.character(df_work$parentEventID)
    df_work$parentEventID[trimws(df_work$parentEventID) == ""] <- NA_character_
  }

  # -------------------------------------------------------
  # 2) SPLIT TABLES (USING dwc_terms)
  # -------------------------------------------------------
  event_terms <- dwc_terms |>
    dplyr::filter(.data$Table == "Event") |>
    dplyr::pull(.data$Term) |>
    unique()

  occ_terms <- dwc_terms |>
    dplyr::filter(.data$Table == "Occurrence") |>
    dplyr::pull(.data$Term) |>
    unique()

  # Always keep structural IDs when present + aurora traceability cols
  event_terms <- unique(c("eventID", "parentEventID", event_terms, aurora_trace_cols))
  occ_terms <- unique(c("eventID", "occurrenceID", occ_terms, aurora_trace_cols))

  # EVENT TABLE
  if ("eventID" %in% names(df_work)) {
    event_table <- df_work |>
      dplyr::select(dplyr::any_of(event_terms)) |>
      dplyr::distinct(.data$eventID, .keep_all = TRUE)
  } else {
    event_table <- data.frame()
  }

  # OCCURRENCE TABLE
  if ("occurrenceID" %in% names(df_work)) {
    occurrence_table <- df_work |>
      dplyr::select(dplyr::any_of(occ_terms)) |>
      dplyr::distinct(.data$occurrenceID, .keep_all = TRUE)
  } else {
    occurrence_table <- data.frame()
  }

  # -------------------------------------------------------
  # 3) EMOF BUILD (LEVEL PER COLUMN)
  # -------------------------------------------------------
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

      emof_parts <- list()

      # -----------------------------------------------
      # 3a) EVENT-LEVEL EMOF
      # one row per eventID + measurementType
      # -----------------------------------------------
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
                  "' has conflicting values within the same eventID; ",
                  "keeping the first non-empty value per event."
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

          emof_parts[["event"]] <- event_emof
        }
      }

      # -----------------------------------------------
      # 3b) OCCURRENCE-LEVEL EMOF
      # one row per eventID + occurrenceID + measurementType
      # -----------------------------------------------
      if (length(occurrence_cols) > 0) {

        if (!"occurrenceID" %in% names(df_work)) {
          qc_messages <- c(
            qc_messages,
            "ERROR: Cannot build occurrence-level eMoF because occurrenceID is missing."
          )
        } else {

          if (!"eventID" %in% names(df_work)) {
            qc_messages <- c(
              qc_messages,
              "WARNING: occurrence-level eMoF is being built without eventID in input dataframe."
            )
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

          emof_parts[["occurrence"]] <- occurrence_emof
        }
      }

      if (length(emof_parts) > 0) {
        emof_table <- dplyr::bind_rows(emof_parts)

        for (nm in c("measurementTypeID", "measurementValueID",
                     "measurementUnit", "measurementUnitID")) {
          if (!nm %in% names(emof_table)) emof_table[[nm]] <- ""
        }

        emof_table <- emof_table |>
          dplyr::select(
            .data$eventID, .data$occurrenceID,
            dplyr::any_of(aurora_trace_cols),
            .data$measurementType, .data$measurementTypeID,
            .data$measurementValue, .data$measurementValueID,
            .data$measurementUnit, .data$measurementUnitID
          ) |>
          dplyr::distinct()

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
        }
      }
    }
  }

  # -------------------------------------------------------
  # 4) BASIC QC
  # -------------------------------------------------------
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

    if ("eventID" %in% names(occurrence_table) && "eventID" %in% names(event_table)) {
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

  if (!is.null(emof_table)) {
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
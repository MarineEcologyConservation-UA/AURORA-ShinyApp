# =========================================================
# Build DwC-A tables from flat mapped dataframe
# =========================================================

#' @export
build_dwca_tables <- function(df,
                              dwc_terms,
                              id_spec,
                              parent_spec = NULL,
                              emof_spec = NULL,
                              remarks_spec = NULL) {

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
  # 1) REMARKS JOIN
  # -------------------------------------------------------
  if (!is.null(remarks_spec)) {
    if (!is.null(remarks_spec$columns) &&
        length(remarks_spec$columns) > 0 &&
        !is.null(remarks_spec$target) &&
        nzchar(remarks_spec$target)) {

      miss <- setdiff(remarks_spec$columns, names(df_work))
      if (length(miss) > 0) {
        qc_messages <- c(
          qc_messages,
          paste0(
            "WARNING: remarks columns missing and ignored: ",
            paste(miss, collapse = ", ")
          )
        )
      }

      cols_ok <- intersect(remarks_spec$columns, names(df_work))
      if (length(cols_ok) > 0) {
        joined <- apply(
          df_work[, cols_ok, drop = FALSE],
          1,
          function(x) {
            x <- as.character(x)
            x <- x[!is.na(x) & x != ""]
            paste(x, collapse = " | ")
          }
        )
        df_work[[remarks_spec$target]] <- joined
      }
    }
  }

  # -------------------------------------------------------
  # helper: safe concat
  # -------------------------------------------------------
  .safe_concat <- function(row_vals, sep = ":") {
    x <- as.character(row_vals)
    x[is.na(x)] <- ""
    x <- trimws(x)
    x <- x[x != ""]
    if (length(x) == 0) return("")
    paste(x, collapse = sep)
  }

  # helper: first non-empty value
  .first_non_empty <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) return(NA_character_)
    x[[1]]
  }

  # -------------------------------------------------------
  # 2) EVENT ID
  # -------------------------------------------------------
  if (id_spec$event_mode == "use") {

    if (is.null(id_spec$event_column) || !nzchar(id_spec$event_column)) {
      stop("event_mode='use' requires id_spec$event_column.")
    }
    if (!id_spec$event_column %in% names(df_work)) {
      stop("Selected eventID column does not exist.")
    }

    df_work$eventID <- as.character(df_work[[id_spec$event_column]])
    df_work$eventID[is.na(df_work$eventID)] <- ""

  } else if (id_spec$event_mode == "concat") {

    cols <- id_spec$event_concat
    if (is.null(cols) || length(cols) == 0) {
      stop("event_mode='concat' requires id_spec$event_concat (>= 1 column).")
    }

    miss <- setdiff(cols, names(df_work))
    if (length(miss) > 0) {
      stop(paste0(
        "event_mode='concat' columns missing: ",
        paste(miss, collapse = ", ")
      ))
    }

    df_work$eventID <- apply(
      df_work[, cols, drop = FALSE],
      1,
      .safe_concat,
      sep = ":"
    )

  } else {

    df_work$eventID <- paste0("event_", seq_len(nrow(df_work)))
    qc_messages <- c(qc_messages, "WARNING: eventID auto-generated (not stable).")
  }

  if (any(df_work$eventID == "")) {
    qc_messages <- c(qc_messages, "ERROR: Some rows have blank eventID.")
  }

  # -------------------------------------------------------
  # 3) OCCURRENCE ID
  # -------------------------------------------------------
  if (id_spec$occ_mode == "use") {

    if (is.null(id_spec$occ_column) || !nzchar(id_spec$occ_column)) {
      stop("occ_mode='use' requires id_spec$occ_column.")
    }
    if (!id_spec$occ_column %in% names(df_work)) {
      stop("Selected occurrenceID column does not exist.")
    }

    df_work$occurrenceID <- as.character(df_work[[id_spec$occ_column]])
    df_work$occurrenceID[is.na(df_work$occurrenceID)] <- ""

  } else {
    # event_seq (default)
    df_work <- df_work |>
      dplyr::group_by(.data$eventID) |>
      dplyr::mutate(
        occurrenceID = paste0(.data$eventID, "_occ_", dplyr::row_number())
      ) |>
      dplyr::ungroup()
  }

  if (any(df_work$occurrenceID == "")) {
    qc_messages <- c(qc_messages, "ERROR: Some rows have blank occurrenceID.")
  }

  # -------------------------------------------------------
  # 4) PARENT EVENT
  # -------------------------------------------------------
  parent_events <- NULL

  if (!is.null(parent_spec) && isTRUE(parent_spec$enabled)) {

    cols <- parent_spec$columns
    if (is.null(cols) || length(cols) == 0) {
      stop("parentEvent enabled but parent_spec$columns is empty.")
    }

    miss <- setdiff(cols, names(df_work))
    if (length(miss) > 0) {
      stop(paste0(
        "parentEventID columns missing: ",
        paste(miss, collapse = ", ")
      ))
    }

    df_work$parentEventID <- apply(
      df_work[, cols, drop = FALSE],
      1,
      .safe_concat,
      sep = ":"
    )

    parent_events <- df_work |>
      dplyr::distinct(.data$parentEventID) |>
      dplyr::filter(!is.na(.data$parentEventID) & .data$parentEventID != "") |>
      dplyr::rename(eventID = .data$parentEventID)

    df_work$parentEventID[df_work$parentEventID == ""] <- NA_character_
  }

  # -------------------------------------------------------
  # 5) SPLIT TABLES (USING dwc_terms)
  # -------------------------------------------------------
  event_terms <- dwc_terms |>
    dplyr::filter(.data$Table == "Event") |>
    dplyr::pull(.data$Term) |>
    unique()

  occ_terms <- dwc_terms |>
    dplyr::filter(.data$Table == "Occurrence") |>
    dplyr::pull(.data$Term) |>
    unique()

  event_terms <- unique(c("eventID", "parentEventID", event_terms))
  occ_terms <- unique(c("eventID", "occurrenceID", occ_terms))

  event_table <- df_work |>
    dplyr::select(dplyr::any_of(event_terms)) |>
    dplyr::distinct(.data$eventID, .keep_all = TRUE)

  if (!is.null(parent_events) && nrow(parent_events) > 0) {
    missing_cols <- setdiff(names(event_table), names(parent_events))
    if (length(missing_cols) > 0) {
      for (cc in missing_cols) parent_events[[cc]] <- NA
    }
    parent_events <- parent_events[, names(event_table), drop = FALSE]
    event_table <- dplyr::bind_rows(parent_events, event_table) |>
      dplyr::distinct(.data$eventID, .keep_all = TRUE)
  }

  occurrence_table <- df_work |>
    dplyr::select(dplyr::any_of(occ_terms)) |>
    dplyr::distinct()

  # -------------------------------------------------------
  # 6) EMOF BUILD (LEVEL PER COLUMN)
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
      # 6a) EVENT-LEVEL EMOF
      # one row per eventID + measurementType
      # -----------------------------------------------
      if (length(event_cols) > 0) {

        # warn if same event has conflicting values for same event-level variable
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
          dplyr::select(.data$eventID, dplyr::all_of(event_cols)) |>
          dplyr::group_by(.data$eventID) |>
          dplyr::summarise(
            dplyr::across(
              dplyr::all_of(event_cols),
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
          dplyr::filter(!is.na(.data$measurementValue) & .data$measurementValue != "") |>
          dplyr::mutate(occurrenceID = "")

        emof_parts[["event"]] <- event_emof
      }

      # -----------------------------------------------
      # 6b) OCCURRENCE-LEVEL EMOF
      # one row per eventID + occurrenceID + measurementType
      # -----------------------------------------------
      if (length(occurrence_cols) > 0) {

        occurrence_emof <- df_work |>
          dplyr::select(
            .data$eventID,
            .data$occurrenceID,
            dplyr::all_of(occurrence_cols)
          ) |>
          dplyr::distinct() |>
          tidyr::pivot_longer(
            cols = dplyr::all_of(occurrence_cols),
            names_to = "measurementType",
            values_to = "measurementValue",
            values_transform = list(measurementValue = as.character)
          ) |>
          dplyr::filter(!is.na(.data$measurementValue) & .data$measurementValue != "")

        emof_parts[["occurrence"]] <- occurrence_emof
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
            .data$measurementType, .data$measurementTypeID,
            .data$measurementValue, .data$measurementValueID,
            .data$measurementUnit, .data$measurementUnitID
          ) |>
          dplyr::distinct()

        # final safeguard against duplicate measurementType per event for event-level rows
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
  # 7) BASIC QC
  # -------------------------------------------------------
  if (!"eventID" %in% names(event_table)) {
    qc_messages <- c(qc_messages, "ERROR: event table missing eventID.")
  } else if (anyDuplicated(event_table$eventID) > 0) {
    qc_messages <- c(qc_messages, "ERROR: Duplicate eventID detected in event table.")
  }

  if (!"occurrenceID" %in% names(occurrence_table)) {
    qc_messages <- c(qc_messages, "ERROR: occurrence table missing occurrenceID.")
  } else if (anyDuplicated(occurrence_table$occurrenceID) > 0) {
    qc_messages <- c(qc_messages, "ERROR: Duplicate occurrenceID detected in occurrence table.")
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
  }

  list(
    event = event_table,
    occurrence = occurrence_table,
    emof = emof_table,
    qc = qc_messages
  )
}
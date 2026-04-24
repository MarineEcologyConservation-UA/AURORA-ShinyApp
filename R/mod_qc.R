# =========================================================
# QC Page (BioCheck-like) + QC Engine
# File: R/mod_qc.R
# =========================================================

#' @importFrom rlang %||%
NULL

`%||%` <- rlang::`%||%`

# =========================================================
# Helpers
# =========================================================

.qc_empty_issues <- function() {
  data.frame(
    table = character(),
    row_id = character(),
    field = character(),
    rule = character(),
    message = character(),
    severity = character(),
    original_row = character(),
    original_id = character(),
    stringsAsFactors = FALSE
  )
}

.qc_issue <- function(table, row_id, field, rule, message, severity = "ERROR") {
  data.frame(
    table = as.character(table),
    row_id = as.character(row_id),
    field = as.character(field),
    rule = as.character(rule),
    message = as.character(message),
    severity = toupper(as.character(severity)),
    stringsAsFactors = FALSE
  )
}

.qc_bind <- function(x) {
  base_cols <- c(
    "table",
    "row_id",
    "field",
    "rule",
    "message",
    "severity",
    "original_row",
    "original_id"
  )

  empty <- data.frame(
    table = character(),
    row_id = character(),
    field = character(),
    rule = character(),
    message = character(),
    severity = character(),
    original_row = character(),
    original_id = character(),
    stringsAsFactors = FALSE
  )

  if (is.null(x)) {
    return(empty)
  }

  if (is.data.frame(x)) {
    if (nrow(x) == 0) {
      return(empty)
    }

    for (nm in setdiff(base_cols, names(x))) {
      x[[nm]] <- NA_character_
    }

    x <- x[, base_cols, drop = FALSE]
    rownames(x) <- NULL
    return(x)
  }

  if (!is.list(x) || length(x) == 0) {
    return(empty)
  }

  x_filtered <- Filter(function(z) {
    is.data.frame(z) && nrow(z) > 0
  }, x)

  if (length(x_filtered) == 0) {
    return(empty)
  }

  x_fixed <- lapply(x_filtered, function(z) {
    for (nm in setdiff(base_cols, names(z))) {
      z[[nm]] <- NA_character_
    }
    z <- z[, base_cols, drop = FALSE]
    rownames(z) <- NULL
    z
  })

  out <- do.call(rbind, x_fixed)
  rownames(out) <- NULL
  out
}

.qc_is_blank <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) return(is.na(x) | trimws(x) == "")
  is.na(x)
}

.qc_has_nonblank <- function(x) {
  !all(.qc_is_blank(x))
}

.qc_num <- function(x) {
  if (is.null(x)) return(numeric(0))
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

.qc_parse_partial_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  out <- rep(as.Date(NA), length(x))

  full_idx <- grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  ym_idx <- grepl("^\\d{4}-\\d{2}$", x)
  y_idx <- grepl("^\\d{4}$", x)

  out[full_idx] <- suppressWarnings(as.Date(x[full_idx]))
  out[ym_idx] <- suppressWarnings(as.Date(paste0(x[ym_idx], "-01")))
  out[y_idx] <- suppressWarnings(as.Date(paste0(x[y_idx], "-01-01")))

  out
}

.qc_dt <- function(data,
                   colnames = NULL,
                   page_length = 15,
                   compact = FALSE,
                   report = FALSE,
                   scroll_x = TRUE,
                   escape = TRUE,
                   selection = "none") {
  if (!is.data.frame(data)) data <- data.frame()

  dom <- if (report) "t" else "Bfrtip"
  buttons <- if (report) list() else list(list(extend = "colvis", text = "Columns"))
  cls <- if (compact) "compact stripe hover order-column" else "stripe hover order-column"

  args <- list(
    data = data,
    rownames = FALSE,
    class = cls,
    escape = escape,
    selection = selection,
    width = "100%",
    options = list(
      dom = dom,
      buttons = buttons,
      paging = !report,
      searching = !report,
      info = !report,
      ordering = !report,
      pageLength = page_length,
      scrollX = scroll_x,
      autoWidth = FALSE,
      destroy = TRUE
    )
  )

  if (!is.null(colnames)) {
    args$colnames <- colnames
  }

  if (!report) {
    args$extensions <- "Buttons"
    args$callback <- DT::JS(
      "var t = table;
       setTimeout(function(){ t.columns.adjust().draw(false); }, 50);
       setTimeout(function(){ t.columns.adjust().draw(false); }, 250);"
    )
  }

  do.call(DT::datatable, args)
}

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

.qc_required_field <- function(df, table, id_field, fields_required) {
  out <- list()
  if (!is.data.frame(df) || nrow(df) == 0) return(.qc_bind(out))

  rid <- if (id_field %in% names(df)) as.character(df[[id_field]]) else as.character(seq_len(nrow(df)))

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
      issue_list <- lapply(ii, function(i) {
        .qc_issue(
          table = table,
          row_id = rid[i],
          field = f,
          rule = "required_field_empty_value",
          message = paste0("Empty value for required field: ", f),
          severity = "ERROR"
        )
      })
      out[[length(out) + 1]] <- .qc_bind(issue_list)
    }
  }

  .qc_bind(out)
}

.qc_unique_id <- function(df, table, id_field) {
  if (!is.data.frame(df) || nrow(df) == 0) return(.qc_empty_issues())

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
    issue_list <- lapply(ii, function(i) {
      .qc_issue(
        table = table,
        row_id = as.character(i),
        field = id_field,
        rule = "id_empty",
        message = paste0(id_field, " is empty"),
        severity = "ERROR"
      )
    })
    out[[length(out) + 1]] <- .qc_bind(issue_list)
  }

  id2 <- id[!blank]
  dup <- duplicated(id2) | duplicated(id2, fromLast = TRUE)

  if (any(dup, na.rm = TRUE)) {
    vals <- unique(id2[dup])

    for (v in vals) {
      idx <- which(id == v)

      issue_list <- lapply(idx, function(i) {
        .qc_issue(
          table = table,
          row_id = id[i],
          field = id_field,
          rule = "id_duplicated",
          message = paste0(id_field, " is duplicated: ", v),
          severity = "ERROR"
        )
      })

      out[[length(out) + 1]] <- .qc_bind(issue_list)
    }
  }

  .qc_bind(out)
}

.qc_coords_range <- function(df, table, id_field,
                             lat_field = "decimalLatitude",
                             lon_field = "decimalLongitude") {
  if (!is.data.frame(df) || nrow(df) == 0) return(.qc_empty_issues())
  if (!(lat_field %in% names(df) && lon_field %in% names(df))) return(.qc_empty_issues())

  rid <- if (id_field %in% names(df)) as.character(df[[id_field]]) else as.character(seq_len(nrow(df)))
  lat <- .qc_num(df[[lat_field]])
  lon <- .qc_num(df[[lon_field]])

  bad <- (is.finite(lat) & (lat < -90 | lat > 90)) |
    (is.finite(lon) & (lon < -180 | lon > 180))

  bad[is.na(bad)] <- FALSE

  if (!any(bad)) return(.qc_empty_issues())

  ii <- which(bad)

  issue_list <- lapply(ii, function(i) {
    .qc_issue(
      table = table,
      row_id = rid[i],
      field = "coordinates_error",
      rule = "coords_out_of_range",
      message = "Out-of-range coordinates (latitude must be -90 to 90, longitude must be -180 to 180)",
      severity = "ERROR"
    )
  })

  .qc_bind(issue_list)
}

.qc_eventDate_basic <- function(event,
                                id_field = "eventID",
                                field = "eventDate",
                                table_name = "event") {
  if (!is.data.frame(event) || nrow(event) == 0) return(.qc_empty_issues())
  if (!field %in% names(event)) return(.qc_empty_issues())

  rid <- if (id_field %in% names(event)) {
    as.character(event[[id_field]])
  } else {
    as.character(seq_len(nrow(event)))
  }

  x <- trimws(as.character(event[[field]]))

  if (!requireNamespace("parsedate", quietly = TRUE)) {
    return(.qc_issue(
      table = table_name,
      row_id = "*",
      field = field,
      rule = "parsedate_missing",
      message = "Package 'parsedate' is not installed; ISO 8601 validation could not be performed.",
      severity = "WARN"
    ))
  }

  parsed <- suppressWarnings(
    tryCatch(
      parsedate::parse_iso_8601(x),
      error = function(e) {
        rep(as.POSIXct(NA), length(x))
      }
    )
  )

  ok <- !is.na(parsed)
  ok[.qc_is_blank(x)] <- TRUE

  bad <- !ok
  bad[is.na(bad)] <- FALSE

  if (!any(bad)) return(.qc_empty_issues())

  ii <- which(bad)

  issue_list <- lapply(ii, function(i) {
    .qc_issue(
      table = table_name,
      row_id = rid[i],
      field = field,
      rule = "eventDate_invalid_iso8601",
      message = paste0(field, " is not a valid ISO 8601 date/time"),
      severity = "ERROR"
    )
  })

  .qc_bind(issue_list)
}

.qc_rel_occ_event <- function(occ, event) {
  if (!is.data.frame(occ) || nrow(occ) == 0) return(.qc_empty_issues())
  if (!("eventID" %in% names(occ))) return(.qc_empty_issues())

  if (!is.data.frame(event) || nrow(event) == 0 || !("eventID" %in% names(event))) {
    return(.qc_empty_issues())
  }

  ev_ids <- unique(as.character(event$eventID))
  occ_ids <- if ("occurrenceID" %in% names(occ)) as.character(occ$occurrenceID) else as.character(seq_len(nrow(occ)))

  miss <- !.qc_is_blank(occ$eventID) & !(as.character(occ$eventID) %in% ev_ids)

  if (!any(miss, na.rm = TRUE)) return(.qc_empty_issues())

  ii <- which(miss)

  issue_list <- lapply(ii, function(i) {
    .qc_issue(
      table = "occurrence",
      row_id = occ_ids[i],
      field = "eventID",
      rule = "eventID_no_corresponding_event",
      message = "This eventID has no corresponding eventID in the event core table",
      severity = "ERROR"
    )
  })

  .qc_bind(issue_list)
}

.qc_parentEventID <- function(event) {
  if (!is.data.frame(event) || nrow(event) == 0) return(.qc_empty_issues())

  if (!("eventID" %in% names(event)) || !("parentEventID" %in% names(event))) {
    return(.qc_empty_issues())
  }

  ev_ids <- unique(as.character(event$eventID))
  pid <- as.character(event$parentEventID)
  rid <- if ("eventID" %in% names(event)) as.character(event$eventID) else as.character(seq_len(nrow(event)))

  miss <- !.qc_is_blank(pid) & !(pid %in% ev_ids)

  if (!any(miss, na.rm = TRUE)) return(.qc_empty_issues())

  ii <- which(miss)

  issue_list <- lapply(ii, function(i) {
    .qc_issue(
      table = "event",
      row_id = rid[i],
      field = "parentEventID",
      rule = "parentEventID_no_corresponding_event",
      message = "parentEventID has no corresponding eventID in the event table",
      severity = "ERROR"
    )
  })

  .qc_bind(issue_list)
}

.qc_emof_duplicates <- function(emof) {
  if (!is.data.frame(emof) || nrow(emof) == 0) return(.qc_empty_issues())
  if (!("measurementType" %in% names(emof))) return(.qc_empty_issues())

  has_event_col <- "eventID" %in% names(emof)
  has_occ_col <- "occurrenceID" %in% names(emof)

  if (!has_event_col && !has_occ_col) {
    return(.qc_empty_issues())
  }

  rid <- if ("measurementID" %in% names(emof)) {
    as.character(emof$measurementID)
  } else {
    as.character(seq_len(nrow(emof)))
  }

  event_id <- if (has_event_col) {
    trimws(as.character(emof$eventID))
  } else {
    rep("", nrow(emof))
  }

  occurrence_id <- if (has_occ_col) {
    trimws(as.character(emof$occurrenceID))
  } else {
    rep("", nrow(emof))
  }

  measurement_type <- trimws(as.character(emof$measurementType))

  event_id[is.na(event_id)] <- ""
  occurrence_id[is.na(occurrence_id)] <- ""
  measurement_type[is.na(measurement_type)] <- ""

  ok <- measurement_type != "" & (event_id != "" | occurrence_id != "")

  if (!any(ok)) return(.qc_empty_issues())

  key <- paste(event_id, occurrence_id, measurement_type, sep = "||")
  key2 <- key[ok]

  dup <- duplicated(key2) | duplicated(key2, fromLast = TRUE)

  if (!any(dup, na.rm = TRUE)) return(.qc_empty_issues())

  vals <- unique(key2[dup])
  out <- list()

  for (v in vals) {
    idx <- which(ok & key == v)

    issue_list <- lapply(idx, function(i) {
      .qc_issue(
        table = "emof",
        row_id = rid[i],
        field = "measurementType",
        rule = "duplicate_measurementType_per_event_occurrence",
        message = "Duplicate measurementType for the same eventID + occurrenceID combination",
        severity = "WARN"
      )
    })

    out[[length(out) + 1]] <- .qc_bind(issue_list)
  }

  .qc_bind(out)
}

.qc_emof_required <- function(emof) {
  if (!is.data.frame(emof) || nrow(emof) == 0) return(.qc_empty_issues())

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

      issue_list <- lapply(ii, function(i) {
        .qc_issue(
          table = "emof",
          row_id = rid[i],
          field = f,
          rule = "required_field_empty_value",
          message = paste0("Empty value for required field: ", f),
          severity = "ERROR"
        )
      })

      out[[length(out) + 1]] <- .qc_bind(issue_list)
    }
  }

  .qc_bind(out)
}

.qc_make_invalid <- function(df, table_name, id_field, issues_det) {
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

  df_work <- df
  work_id <- id_field

  if (!id_field %in% names(df_work)) {
    df_work$.row_id_temp <- as.character(seq_len(nrow(df_work)))
    work_id <- ".row_id_temp"
  }

  sub <- issues_det[
    issues_det$table == table_name &
      issues_det$severity %in% c("ERROR", "WARN"),
    ,
    drop = FALSE
  ]

  if (nrow(sub) == 0) return(data.frame())

  ids_bad <- unique(sub$row_id[sub$row_id != "*" & !is.na(sub$row_id)])

  if (length(ids_bad) == 0) return(data.frame())

  out <- df_work[as.character(df_work[[work_id]]) %in% ids_bad, , drop = FALSE]

  if (nrow(out) == 0) return(data.frame())

  fields <- unique(as.character(sub$field))

  for (f in fields) {
    coln <- paste0(f, "_error")
    msg_map <- sub[sub$field == f, c("row_id", "message"), drop = FALSE]
    msg_map <- msg_map[!duplicated(msg_map$row_id), , drop = FALSE]
    out[[coln]] <- msg_map$message[match(as.character(out[[work_id]]), msg_map$row_id)]
  }

  err_cols <- grep("_error$", names(out), value = TRUE)
  out$row <- seq_len(nrow(out))

  if (work_id == ".row_id_temp") {
    out$.row_id_temp <- NULL
  }

  keep <- c("row", err_cols, setdiff(names(out), c("row", err_cols)))
  out[, unique(keep), drop = FALSE]
}

.qc_get_coords <- function(df, id_field) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
  if (!all(c("decimalLatitude", "decimalLongitude") %in% names(df))) return(NULL)

  id_vec <- if (id_field %in% names(df)) {
    as.character(df[[id_field]])
  } else {
    as.character(seq_len(nrow(df)))
  }

  lat <- .qc_num(df$decimalLatitude)
  lon <- .qc_num(df$decimalLongitude)

  ok <- is.finite(lat) & is.finite(lon)

  if (!any(ok)) return(NULL)

  out <- data.frame(
    id = id_vec[ok],
    decimalLatitude = lat[ok],
    decimalLongitude = lon[ok],
    stringsAsFactors = FALSE
  )

  out <- out[
    is.finite(out$decimalLatitude) & is.finite(out$decimalLongitude),
    ,
    drop = FALSE
  ]

  out <- out[!duplicated(out[, c("id", "decimalLatitude", "decimalLongitude")]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.qc_get_map_records <- function(event, occurrence) {
  occ_coords <- .qc_get_coords(occurrence, "occurrenceID")
  ev_coords <- .qc_get_coords(event, "eventID")

  out <- data.frame()

  if (is.data.frame(occ_coords) && nrow(occ_coords) > 0) {
    tmp <- occ_coords
    tmp$source <- "occurrence"
    out <- rbind(out, tmp)
  }

  if (is.data.frame(ev_coords) && nrow(ev_coords) > 0) {
    tmp <- ev_coords
    tmp$source <- "event"
    out <- rbind(out, tmp)
  }

  if (!is.data.frame(out) || nrow(out) == 0) return(NULL)

  rownames(out) <- NULL
  out
}

.qc_extract_date_values <- function(x) {
  if (is.null(x)) return(character())

  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA_character_
  x
}

.qc_overview_date_counts <- function(event, occurrence) {
  out <- data.frame(date = character(), n = integer(), source = character(), stringsAsFactors = FALSE)

  if (is.data.frame(event) && nrow(event) > 0 && "eventDate" %in% names(event)) {
    d <- .qc_extract_date_values(event$eventDate)
    d <- d[!is.na(d)]

    if (length(d) > 0) {
      tb <- sort(table(d), decreasing = FALSE)
      out <- data.frame(
        date = names(tb),
        n = as.integer(tb),
        source = "event",
        stringsAsFactors = FALSE
      )
    }
  }

  if (nrow(out) == 0 &&
      is.data.frame(occurrence) &&
      nrow(occurrence) > 0 &&
      "eventDate" %in% names(occurrence)) {
    d <- .qc_extract_date_values(occurrence$eventDate)
    d <- d[!is.na(d)]

    if (length(d) > 0) {
      tb <- sort(table(d), decreasing = FALSE)
      out <- data.frame(
        date = names(tb),
        n = as.integer(tb),
        source = "occurrence",
        stringsAsFactors = FALSE
      )
    }
  }

  if (nrow(out) == 0) return(out)

  rownames(out) <- NULL
  out
}

.qc_build_nested_taxonomy <- function(oc) {
  tax_levels <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
  oc_filt <- oc

  if (!is.data.frame(oc_filt) || nrow(oc_filt) == 0) return(NULL)

  available_levels <- tax_levels[tax_levels %in% names(oc_filt)]

  if (length(available_levels) == 0) return(NULL)

  hierarchy_data <- oc_filt[, available_levels, drop = FALSE]

  for (col in available_levels) {
    hierarchy_data[[col]] <- as.character(hierarchy_data[[col]])
    hierarchy_data[[col]][.qc_is_blank(hierarchy_data[[col]])] <- NA_character_
  }

  hierarchy_data <- hierarchy_data[
    rowSums(is.na(hierarchy_data)) < length(available_levels),
    ,
    drop = FALSE
  ]

  if (nrow(hierarchy_data) == 0) return(NULL)

  build_hierarchy_recursive <- function(data, levels, current_level = 1, parent_label = "", parent_id = "") {
    if (current_level > length(levels) || nrow(data) == 0) return(NULL)

    current_col <- levels[current_level]
    level_values <- unique(data[[current_col]])
    level_values <- level_values[!is.na(level_values)]

    if (length(level_values) == 0) {
      return(build_hierarchy_recursive(data, levels, current_level + 1, parent_label, parent_id))
    }

    result_list <- list()

    for (val in level_values) {
      subset_mask <- as.character(data[[current_col]]) == as.character(val)
      count <- sum(subset_mask, na.rm = TRUE)

      current_id <- if (parent_id == "") as.character(val) else paste0(parent_id, "|", val)

      result_list[[length(result_list) + 1]] <- data.frame(
        ids = current_id,
        labels = as.character(val),
        parents = parent_id,
        values = count,
        stringsAsFactors = FALSE
      )

      if (current_level < length(levels)) {
        subset_data <- data[subset_mask, , drop = FALSE]
        child_hierarchy <- build_hierarchy_recursive(
          subset_data,
          levels,
          current_level + 1,
          as.character(val),
          current_id
        )

        if (!is.null(child_hierarchy)) {
          result_list[[length(result_list) + 1]] <- child_hierarchy
        }
      }
    }

    do.call(rbind, result_list)
  }

  root_id <- "All"

  root_node <- data.frame(
    ids = root_id,
    labels = "All",
    parents = "",
    values = nrow(hierarchy_data),
    stringsAsFactors = FALSE
  )

  child_nodes <- build_hierarchy_recursive(hierarchy_data, available_levels, 1, "All", root_id)

  res <- rbind(root_node, child_nodes)
  res[!duplicated(res$ids), , drop = FALSE]
}

.qc_add_origin_columns <- function(df) {
  if (!is.data.frame(df)) {
    df <- data.frame(stringsAsFactors = FALSE)
  }

  n <- nrow(df)

  if (!"original_row" %in% names(df)) {
    df[["original_row"]] <- rep(NA_character_, n)
  }

  if (!"original_id" %in% names(df)) {
    df[["original_id"]] <- rep(NA_character_, n)
  }

  df
}

.qc_enrich_issues_with_origin <- function(issues, df, table_name, id_field) {
  if (!is.data.frame(issues) || nrow(issues) == 0) {
    return(.qc_add_origin_columns(issues))
  }

  issues <- .qc_add_origin_columns(issues)

  if (!is.data.frame(df) || nrow(df) == 0) return(issues)
  if (!id_field %in% names(df)) return(issues)
  if (!all(c(".aurora_origin_row", ".aurora_origin_id") %in% names(df))) return(issues)

  idx <- which(as.character(issues$table) == as.character(table_name))

  if (length(idx) == 0) return(issues)

  sub <- issues[idx, , drop = FALSE]
  joinable <- !is.na(sub$row_id) & sub$row_id != "*"

  if (!any(joinable)) return(issues)

  map_df <- unique(data.frame(
    row_id = as.character(df[[id_field]]),
    original_row = as.character(df$.aurora_origin_row),
    original_id = as.character(df$.aurora_origin_id),
    stringsAsFactors = FALSE
  ))

  merged <- merge(
    sub[joinable, , drop = FALSE],
    map_df,
    by = "row_id",
    all.x = TRUE,
    suffixes = c("", ".new")
  )

  if ("original_row.new" %in% names(merged)) {
    merged$original_row <- ifelse(
      .qc_is_blank(merged$original_row),
      merged$original_row.new,
      merged$original_row
    )
    merged$original_row.new <- NULL
  }

  if ("original_id.new" %in% names(merged)) {
    merged$original_id <- ifelse(
      .qc_is_blank(merged$original_id),
      merged$original_id.new,
      merged$original_id
    )
    merged$original_id.new <- NULL
  }

  merged <- merged[match(sub$row_id[joinable], merged$row_id), , drop = FALSE]

  sub$original_row[joinable] <- merged$original_row
  sub$original_id[joinable] <- merged$original_id

  issues[idx, c("original_row", "original_id")] <- sub[, c("original_row", "original_id"), drop = FALSE]
  issues
}

.qc_export_strip_internal <- function(df) {
  if (!is.data.frame(df)) {
    return(data.frame())
  }

  if (nrow(df) == 0 && ncol(df) == 0) {
    return(df)
  }

  drop_cols <- unique(c(
    grep("^\\.aurora", names(df), value = TRUE),
    grep("^original_", names(df), value = TRUE)
  ))

  keep_cols <- setdiff(names(df), drop_cols)
  df[, keep_cols, drop = FALSE]
}

.qc_emof_idlink_by_row <- function(emof) {
  if (!is.data.frame(emof) || nrow(emof) == 0) {
    return(character(0))
  }

  out <- rep("unknown", nrow(emof))

  if ("occurrenceID" %in% names(emof)) {
    idx_occ <- !.qc_is_blank(emof$occurrenceID)
    out[idx_occ] <- "occurrenceMoF"
  }

  if ("eventID" %in% names(emof)) {
    idx_ev <- out == "unknown" & !.qc_is_blank(emof$eventID)
    out[idx_ev] <- "eventMoF"
  }

  out
}

.qc_has_event_table <- function(event) {
  is.data.frame(event) &&
    nrow(event) > 0 &&
    "eventID" %in% names(event)
}

.qc_has_occurrence_table <- function(occurrence) {
  is.data.frame(occurrence) &&
    nrow(occurrence) > 0 &&
    "occurrenceID" %in% names(occurrence)
}

.qc_infer_resource_type <- function(event, occurrence) {
  if (isTRUE(.qc_has_event_table(event))) {
    return("sampling_event")
  }

  if (isTRUE(.qc_has_occurrence_table(occurrence))) {
    return("occurrence_core")
  }

  "unknown"
}

.qc_make_overview_event_occ <- function(event, occurrence, resource_type) {
  bor <- if (is.data.frame(occurrence) && "basisOfRecord" %in% names(occurrence)) {
    tb <- sort(table(occurrence$basisOfRecord), decreasing = TRUE)
    if (length(tb) > 0) names(tb)[1] else NA_character_
  } else {
    NA_character_
  }

  n_present <- if (is.data.frame(occurrence) && "occurrenceStatus" %in% names(occurrence)) {
    sum(tolower(trimws(as.character(occurrence$occurrenceStatus))) == "present", na.rm = TRUE)
  } else {
    NA_integer_
  }

  n_na <- if (is.data.frame(occurrence) && "occurrenceStatus" %in% names(occurrence)) {
    sum(.qc_is_blank(occurrence$occurrenceStatus), na.rm = TRUE)
  } else {
    NA_integer_
  }

  n_occ <- if (is.data.frame(occurrence)) nrow(occurrence) else 0L

  if (identical(resource_type, "sampling_event") && is.data.frame(event) && nrow(event) > 0) {
    event_type <- rep("Event", nrow(event))

    if ("parentEventID" %in% names(event)) {
      event_type[.qc_is_blank(event$parentEventID)] <- "Cruise/root"
      event_type[!.qc_is_blank(event$parentEventID)] <- "Sample/child"
    }

    n_events_tab <- table(event_type)

    ev_tab <- data.frame(
      resource_type = "Sampling event (Event core)",
      table_type = names(n_events_tab),
      n_records = as.integer(n_events_tab),
      n_occurrences = n_occ,
      basisOfRecord = bor,
      n_present = n_present,
      n_NA = n_na,
      stringsAsFactors = FALSE
    )

    rownames(ev_tab) <- NULL
    return(ev_tab)
  }

  out <- data.frame(
    resource_type = "Occurrence (Occurrence core)",
    table_type = "Occurrence.Core",
    n_records = n_occ,
    n_occurrences = n_occ,
    basisOfRecord = bor,
    n_present = n_present,
    n_NA = n_na,
    stringsAsFactors = FALSE
  )

  rownames(out) <- NULL
  out
}

# =========================================================
# Main QC runner
# =========================================================

#' Run QC checks on DwC-A tables
#'
#' @param event Event table data frame.
#' @param occurrence Occurrence table data frame.
#' @param emof eMoF table data frame, may be NULL.
#' @param pre_issues Optional data frame of pre-existing issues.
#'
#' @return A list with overview tables, issues, invalid records, map issues,
#'   counts, and export status.
#' @export
run_qc_dwca <- function(event, occurrence, emof = NULL, pre_issues = NULL) {
  if (!is.data.frame(event)) event <- data.frame()
  if (!is.data.frame(occurrence)) occurrence <- data.frame()
  if (is.null(emof) || !is.data.frame(emof)) emof <- data.frame()

  resource_type <- .qc_infer_resource_type(event, occurrence)
  has_event_table <- identical(resource_type, "sampling_event")
  has_occurrence_table <- isTRUE(.qc_has_occurrence_table(occurrence))

  issues_list <- list()

  .add_issues <- function(result) {
    if (is.data.frame(result) && nrow(result) > 0) {
      return(result)
    }
    return(.qc_empty_issues())
  }

  if (isTRUE(has_event_table)) {
    issues_list[[length(issues_list) + 1]] <- .add_issues(
      .qc_unique_id(event, "event", "eventID")
    )

    if ("eventDate" %in% names(event)) {
      issues_list[[length(issues_list) + 1]] <- .add_issues(
        .qc_eventDate_basic(event, "eventID", "eventDate", "event")
      )
    }

    issues_list[[length(issues_list) + 1]] <- .add_issues(
      .qc_coords_range(event, "event", "eventID")
    )

    issues_list[[length(issues_list) + 1]] <- .add_issues(
      .qc_rel_occ_event(occurrence, event)
    )

    issues_list[[length(issues_list) + 1]] <- .add_issues(
      .qc_parentEventID(event)
    )
  }

  if (isTRUE(has_occurrence_table)) {
    issues_list[[length(issues_list) + 1]] <- .add_issues(
      .qc_unique_id(occurrence, "occurrence", "occurrenceID")
    )

    if (!isTRUE(has_event_table) && "eventDate" %in% names(occurrence)) {
      issues_list[[length(issues_list) + 1]] <- .add_issues(
        .qc_eventDate_basic(occurrence, "occurrenceID", "eventDate", "occurrence")
      )
    }

    issues_list[[length(issues_list) + 1]] <- .add_issues(
      .qc_required_field(
        occurrence,
        "occurrence",
        "occurrenceID",
        c("scientificName")
      )
    )

    issues_list[[length(issues_list) + 1]] <- .add_issues(
      .qc_coords_range(occurrence, "occurrence", "occurrenceID")
    )
  } else {
    issues_list[[length(issues_list) + 1]] <- .add_issues(
      .qc_issue(
        table = "occurrence",
        row_id = "*",
        field = "occurrenceID",
        rule = "occurrence_table_missing_or_empty",
        message = "Occurrence table is missing or empty",
        severity = "ERROR"
      )
    )
  }

  if (is.data.frame(emof) && nrow(emof) > 0) {
    issues_list[[length(issues_list) + 1]] <- .add_issues(.qc_emof_required(emof))
    issues_list[[length(issues_list) + 1]] <- .add_issues(.qc_emof_duplicates(emof))
  }

  if (is.data.frame(pre_issues) && nrow(pre_issues) > 0) {
    pi <- pre_issues

    if (!"severity" %in% names(pi)) pi$severity <- "WARN"
    if (!"field" %in% names(pi)) pi$field <- "pre_split"
    if (!"rule" %in% names(pi)) pi$rule <- "pre_split_issue"
    if (!"message" %in% names(pi)) pi$message <- "Pre-split issue"
    if (!"row" %in% names(pi)) pi$row <- "*"
    if (!"original_row" %in% names(pi)) pi$original_row <- NA_character_
    if (!"original_id" %in% names(pi)) pi$original_id <- NA_character_

    pi_out <- data.frame(
      table = "pre_split",
      row_id = as.character(pi$row),
      field = as.character(pi$field),
      rule = as.character(pi$rule),
      message = as.character(pi$message),
      severity = as.character(pi$severity),
      original_row = as.character(pi$original_row),
      original_id = as.character(pi$original_id),
      stringsAsFactors = FALSE
    )

    issues_list[[length(issues_list) + 1]] <- .add_issues(pi_out)
  }

  issues_detailed <- .qc_bind(issues_list)

  if (!is.data.frame(issues_detailed)) {
    issues_detailed <- .qc_empty_issues()
  }

  issues_detailed <- .qc_add_origin_columns(issues_detailed)
  issues_detailed$severity <- toupper(as.character(issues_detailed$severity))
  issues_detailed$table <- tolower(as.character(issues_detailed$table))

  if (isTRUE(has_event_table)) {
    issues_detailed <- .qc_enrich_issues_with_origin(
      issues_detailed,
      event,
      "event",
      "eventID"
    )
  }

  if (isTRUE(has_occurrence_table)) {
    issues_detailed <- .qc_enrich_issues_with_origin(
      issues_detailed,
      occurrence,
      "occurrence",
      "occurrenceID"
    )
  }

  if (is.data.frame(emof) && nrow(emof) > 0 && "measurementID" %in% names(emof)) {
    issues_detailed <- .qc_enrich_issues_with_origin(
      issues_detailed,
      emof,
      "emof",
      "measurementID"
    )
  }

  issues_summary <- .qc_make_summary(issues_detailed)

  invalid_event <- if (isTRUE(has_event_table)) {
    .qc_make_invalid(event, "event", "eventID", issues_detailed)
  } else {
    data.frame()
  }

  invalid_occ <- .qc_make_invalid(occurrence, "occurrence", "occurrenceID", issues_detailed)

  id_emof <- if ("measurementID" %in% names(emof)) {
    "measurementID"
  } else {
    "row"
  }

  invalid_emof <- .qc_make_invalid(emof, "emof", id_emof, issues_detailed)

  map_records <- .qc_get_map_records(event, occurrence)

  bounds <- NULL

  if (!is.null(map_records) && is.data.frame(map_records) && nrow(map_records) > 0) {
    bounds <- list(
      lng1 = min(map_records$decimalLongitude, na.rm = TRUE),
      lat1 = min(map_records$decimalLatitude, na.rm = TRUE),
      lng2 = max(map_records$decimalLongitude, na.rm = TRUE),
      lat2 = max(map_records$decimalLatitude, na.rm = TRUE)
    )
  }

  map_issues <- data.frame()

  if (!is.null(map_records) && is.data.frame(map_records) && nrow(issues_detailed) > 0) {
    det2 <- issues_detailed[
      issues_detailed$table %in% c("occurrence", "event"),
      ,
      drop = FALSE
    ]

    det2 <- det2[det2$row_id != "*" & !is.na(det2$row_id), , drop = FALSE]

    if (nrow(det2) > 0) {
      joined <- merge(
        det2,
        map_records[, c("id", "decimalLatitude", "decimalLongitude", "source"), drop = FALSE],
        by.x = "row_id",
        by.y = "id",
        all = FALSE
      )

      if (nrow(joined) > 0) {
        map_issues <- data.frame(
          decimalLatitude = joined$decimalLatitude,
          decimalLongitude = joined$decimalLongitude,
          id = joined$row_id,
          table = joined$table,
          field = joined$field,
          issue = joined$message,
          severity = joined$severity,
          source = joined$source,
          original_row = joined$original_row,
          original_id = joined$original_id,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  n_errors <- sum(issues_detailed$severity == "ERROR", na.rm = TRUE)
  n_warn <- sum(issues_detailed$severity == "WARN", na.rm = TRUE)
  can_export <- n_errors == 0

  overview_event_occ <- .qc_make_overview_event_occ(event, occurrence, resource_type)

  overview_emof_types <- data.frame()

  if (is.data.frame(emof) && nrow(emof) > 0 && "measurementType" %in% names(emof)) {
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

    base <- data.frame(
      IDlink = .qc_emof_idlink_by_row(emof),
      measurementType = mt,
      measurementUnit = mu,
      measurementValue = mv,
      stringsAsFactors = FALSE
    )

    count_df <- stats::aggregate(
      rep(1L, nrow(base)),
      by = base[c("IDlink", "measurementType", "measurementUnit")],
      FUN = sum
    )

    names(count_df)[names(count_df) == "x"] <- "count"

    val_fun <- function(z) {
      z <- z[is.finite(z)]
      if (length(z) == 0) return(c(minValue = NA_real_, maxValue = NA_real_))
      c(minValue = min(z), maxValue = max(z))
    }

    value_df <- stats::aggregate(
      base$measurementValue,
      by = base[c("IDlink", "measurementType", "measurementUnit")],
      FUN = val_fun
    )

    if (nrow(value_df) > 0 && !is.null(value_df$x)) {
      mm <- tryCatch(
        {
          do.call(rbind, value_df$x)
        },
        error = function(e) {
          matrix(
            NA_real_,
            nrow = nrow(value_df),
            ncol = 2,
            dimnames = list(NULL, c("minValue", "maxValue"))
          )
        }
      )

      value_df$x <- NULL

      if (is.matrix(mm)) {
        value_df$minValue <- mm[, "minValue"]
        value_df$maxValue <- mm[, "maxValue"]
      } else {
        value_df$minValue <- NA_real_
        value_df$maxValue <- NA_real_
      }
    } else {
      value_df$minValue <- NA_real_
      value_df$maxValue <- NA_real_
      value_df$x <- NULL
    }

    overview_emof_types <- merge(
      count_df,
      value_df,
      by = c("IDlink", "measurementType", "measurementUnit"),
      all = TRUE
    )

    overview_emof_types <- overview_emof_types[
      order(overview_emof_types$count, decreasing = TRUE),
      ,
      drop = FALSE
    ]

    rownames(overview_emof_types) <- NULL
  }

  list(
    resource_type = resource_type,
    overview = list(
      event_occ = overview_event_occ,
      emof_types = overview_emof_types,
      map_records = map_records,
      coords_bounds = bounds,
      date_counts = .qc_overview_date_counts(event, occurrence)
    ),
    issues_summary = issues_summary,
    issues_detailed = issues_detailed,
    invalid = list(
      event = invalid_event,
      occurrence = invalid_occ,
      emof = invalid_emof
    ),
    map_issues = map_issues,
    counts = list(
      errors = n_errors,
      warnings = n_warn
    ),
    can_export = can_export
  )
}

# =========================================================
# QC module UI
# =========================================================

#' QC page UI
#'
#' @param id Module ID.
#'
#' @return A `bslib::nav_panel()` object.
#' @export
mod_qc_ui <- function(id) {
  ns <- shiny::NS(id)
  has_leaflet <- requireNamespace("leaflet", quietly = TRUE)
  has_plotly <- requireNamespace("plotly", quietly = TRUE)

  bslib::nav_panel(
    title = "DwC Tables Overview & QC",
    value = "qc",

    shiny::tags$style(shiny::HTML("
      .qc-title { margin-top: 8px; margin-bottom: 12px; }
      .qc-muted { color: #6b7280; }
      .qc-box { border: 1px solid #e5e7eb; border-radius: 10px; padding: 12px 14px; background: #fff; }
      .qc-kpi { font-size: 26px; font-weight: 700; }
      .qc-kpi-label { font-size: 12px; color: #6b7280; text-transform: uppercase; letter-spacing: .04em; }
      .qc-warn { color: #ff6f00; }
      .qc-err { color: #b91c1c; }
      .qc-ok { color: #047857; }
      .qc-dt-report .dataTables_wrapper .dataTables_length,
      .qc-dt-report .dataTables_wrapper .dataTables_filter,
      .qc-dt-report .dataTables_wrapper .dataTables_info,
      .qc-dt-report .dataTables_wrapper .dataTables_paginate { display: none !important; }
      .qc-map-wrap { height: 420px !important; min-height: 420px !important; width: 100%; }
      .qc-map-wrap .leaflet.html-widget,
      .qc-map-wrap .leaflet-container { height: 100% !important; min-height: 420px !important; }
      .qc-dt { width: 100%; overflow-x: auto; clear: both; }
      .qc-export-wrap {
        display: flex;
        align-items: center;
        gap: 10px;
        flex-wrap: wrap;
      }
      .qc-export-wrap .btn {
        white-space: nowrap;
      }
      .qc-export-btn {
        padding: 0.45rem 0.8rem;
      }
      .qc-warning {
        background-color: #fffdf5; 
        border: 1px solid #d1d1d1; 
        border-radius: 6px; 
        padding: 15px; 
        margin: 10px 0px 20px 0px;
        color: #303d34;
      }
    ")),

    shiny::tags$div(
      class = "qc-warning",
      shiny::tags$strong("Note: "),
      "This section provides a brief summary of the DwC tables content and issues still present that need to be solved. For a more rigorous assessment, you can perform a comprehensive quality control check using the ",
      shiny::tags$a(
        href = "https://rshiny.lifewatch.be/BioCheck/",
        target = "_blank",
        "LifeWatch & EMODnet Biology QC tool."
      )
    ),

    bslib::navset_card_tab(
      id = ns("qc_tabs"),
      selected = "data_overview",
      title = "Data overview",

      bslib::nav_panel(
        "Data overview",
        value = "data_overview",

        shiny::div(
          class = "qc-title",
          shiny::h3("Data Overview & Summary")
        ),

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
        shiny::h4(shiny::uiOutput(ns("overview_event_occ_title"))),
        shiny::div(class = "qc-dt-report", DT::DTOutput(ns("overview_event_occ_tbl"))),

        shiny::hr(),
        shiny::h4("Overview of measurement or fact records"),
        shiny::div(class = "qc-dt-report", DT::DTOutput(ns("overview_emof_types_tbl"))),

        shiny::hr(),
        shiny::h4("Geographic coverage of the dataset"),
        shiny::uiOutput(ns("overview_map_info")),

        if (has_leaflet) {
          shiny::div(class = "qc-map-wrap", leaflet::leafletOutput(ns("overview_map"), height = "100%"))
        } else {
          shiny::div(class = "qc-muted", "Map unavailable: install 'leaflet' to enable this section.")
        },

        shiny::hr(),
        shiny::h4(shiny::uiOutput(ns("overview_date_title"))),
        shiny::div(
          style = "width: 100%; overflow: visible; box-sizing: border-box;",
          shiny::plotOutput(
            ns("overview_date_plot"),
            height = "380px",
            width = "100%"
          )
        ),

        shiny::hr(),
        shiny::h4("Taxonomic coverage of the dataset"),
        shiny::fluidRow(
          shiny::column(
            12,
            if (has_plotly) {
              plotly::plotlyOutput(ns("overview_tax_plot"), height = "500px")
            } else {
              shiny::div(class = "qc-muted", "Sunburst chart unavailable: install 'plotly' to enable this visualization.")
            }
          )
        )
      ),

      bslib::nav_panel(
        "Issues found",
        value = "issues_found",
        shiny::h4("Overview of all issues"),
        shiny::div(class = "qc-dt", DT::DTOutput(ns("issues_summary_tbl"))),
        shiny::hr(),
        shiny::h4("Details"),
        shiny::div(class = "qc-dt", DT::DTOutput(ns("issues_detailed_tbl"))),
        shiny::hr(),
        shiny::h4("eMoF issues"),
        shiny::div(class = "qc-dt", DT::DTOutput(ns("emof_issues_tbl")))
      )
    )
  )
}

.qc_format_table_display <- function(x) {
  x <- tolower(trimws(as.character(x)))
  out <- x

  out[x == "pre_split"] <- "Original"
  out[x == "event"] <- "Event"
  out[x == "occurrence"] <- "Occurrence"
  out[x == "emof"] <- "eMoF"

  out
}

.qc_prepare_issues_display <- function(x) {
  if (!is.data.frame(x)) {
    return(data.frame())
  }

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  if ("table" %in% names(x)) {
    x$table <- .qc_format_table_display(x$table)
  }

  drop_cols <- intersect("original_id", names(x))

  if (length(drop_cols) > 0) {
    x <- x[, setdiff(names(x), drop_cols), drop = FALSE]
  }

  if ("row_id" %in% names(x)) {
    names(x)[names(x) == "row_id"] <- "table_row_id"
  }

  if ("original_row" %in% names(x)) {
    names(x)[names(x) == "original_row"] <- "original_row_id"
  }

  preferred <- c(
    "table",
    "table_row_id",
    "original_row_id",
    "field",
    "rule",
    "message",
    "severity"
  )

  x <- x[, c(intersect(preferred, names(x)), setdiff(names(x), preferred)), drop = FALSE]

  rownames(x) <- NULL
  x
}

# =========================================================
# QC module server
# =========================================================

#' QC page server
#'
#' @param id Module ID.
#' @param event_in Reactive returning event table.
#' @param occ_in Reactive returning occurrence table.
#' @param emof_in Reactive returning eMoF table, may be NULL.
#' @param pre_issues_in Reactive returning pre-existing issues, may be NULL.
#'
#' @return A list with `qc_res` and `can_export` reactives.
#' @export
mod_qc_server <- function(id, event_in, occ_in, emof_in = NULL, pre_issues_in = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    has_leaflet <- requireNamespace("leaflet", quietly = TRUE)
    has_plotly <- requireNamespace("plotly", quietly = TRUE)

    qc_res <- shiny::reactive({
      ev <- event_in()
      oc <- occ_in()
      em <- if (!is.null(emof_in)) emof_in() else data.frame()
      pi <- if (!is.null(pre_issues_in)) pre_issues_in() else NULL

      if (!is.data.frame(ev)) ev <- data.frame()
      if (!is.data.frame(oc)) oc <- data.frame()
      if (!is.data.frame(em)) em <- data.frame()

      shiny::req(is.data.frame(oc))

      run_qc_dwca(ev, oc, em, pi)
    })

    can_export <- shiny::reactive({
      isTRUE(qc_res()$can_export)
    })

    export_tables <- shiny::reactive({
      ev <- event_in()
      oc <- occ_in()
      em <- if (!is.null(emof_in)) emof_in() else data.frame()

      ev_out <- if (is.data.frame(ev) && nrow(ev) > 0) {
        .qc_export_strip_internal(ev)
      } else {
        data.frame()
      }

      oc_out <- if (is.data.frame(oc) && nrow(oc) > 0) {
        .qc_export_strip_internal(oc)
      } else {
        data.frame()
      }

      em_out <- if (is.data.frame(em) && nrow(em) > 0) {
        .qc_export_strip_internal(em)
      } else {
        data.frame()
      }

      list(
        event = ev_out,
        occurrence = oc_out,
        emof = em_out
      )
    })

    output$download_dwca_zip <- shiny::downloadHandler(
      filename = function() {
        paste0("dwca_tables_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        shiny::req(isTRUE(can_export()))

        tabs <- export_tables()

        tmp_dir <- tempfile("dwca_export_")
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

        files_to_zip <- character(0)

        if (is.data.frame(tabs$event) && nrow(tabs$event) > 0) {
          event_path <- file.path(tmp_dir, "event.csv")
          utils::write.csv(tabs$event, event_path, row.names = FALSE, na = "")
          files_to_zip <- c(files_to_zip, event_path)
        }

        if (is.data.frame(tabs$occurrence) && nrow(tabs$occurrence) > 0) {
          occ_path <- file.path(tmp_dir, "occurrence.csv")
          utils::write.csv(tabs$occurrence, occ_path, row.names = FALSE, na = "")
          files_to_zip <- c(files_to_zip, occ_path)
        }

        if (is.data.frame(tabs$emof) && nrow(tabs$emof) > 0) {
          emof_path <- file.path(tmp_dir, "emof.csv")
          utils::write.csv(tabs$emof, emof_path, row.names = FALSE, na = "")
          files_to_zip <- c(files_to_zip, emof_path)
        }

        if (length(files_to_zip) == 0) {
          placeholder_path <- file.path(tmp_dir, "README.txt")
          writeLines("No exportable DwC-A tables were available.", placeholder_path, useBytes = TRUE)
          files_to_zip <- c(files_to_zip, placeholder_path)
        }

        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)

        setwd(tmp_dir)

        utils::zip(
          zipfile = file,
          files = basename(files_to_zip)
        )
      }
    )

    output$kpi_errors <- shiny::renderText({
      qc_res()$counts$errors
    })

    output$kpi_warnings <- shiny::renderText({
      qc_res()$counts$warnings
    })

    output$kpi_n_occ <- shiny::renderText({
      oc <- occ_in()
      if (!is.data.frame(oc)) return(NA_character_)
      nrow(oc)
    })

    output$export_status_ui <- shiny::renderUI({
      ok <- isTRUE(can_export())

      shiny::div(
        class = "qc-export-wrap",
        if (ok) {
          shiny::tags$div(class = "qc-kpi qc-ok", "OK")
        } else {
          shiny::tags$div(class = "qc-kpi qc-err", "ERROR found")
        },
        if (ok) {
          shiny::downloadButton(
            outputId = ns("download_dwca_zip"),
            label = "Download ZIP",
            class = "btn btn-outline-success qc-export-btn"
          )
        }
      )
    })

    output$overview_event_occ_title <- shiny::renderUI({
      rt <- qc_res()$resource_type

      if (identical(rt, "sampling_event")) {
        return(shiny::span("Overview of event and occurrence records"))
      }

      if (identical(rt, "occurrence_core")) {
        return(shiny::span("Overview of occurrence core records"))
      }

      shiny::span("Overview of Darwin Core records")
    })

    output$overview_event_occ_tbl <- DT::renderDT({
      x <- qc_res()$overview$event_occ

      if (!is.data.frame(x) || nrow(x) == 0) {
        x <- data.frame(
          resource_type = character(),
          table_type = character(),
          n_records = integer(),
          n_occurrences = integer(),
          basisOfRecord = character(),
          n_present = integer(),
          n_NA = integer(),
          stringsAsFactors = FALSE
        )
      }

      cols <- c(
        "Resource type" = "resource_type",
        "Table type" = "table_type",
        "Number of records" = "n_records",
        "Number of occurrences" = "n_occurrences",
        "basisOfRecord" = "basisOfRecord",
        "Number of presences" = "n_present",
        "Missing data" = "n_NA"
      )

      .qc_dt(x, colnames = cols, report = TRUE)
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

      .qc_dt(x, report = TRUE)
    })

    output$overview_map_info <- shiny::renderUI({
      rec <- qc_res()$overview$map_records

      if (!is.data.frame(rec) || nrow(rec) == 0) {
        return(shiny::div(class = "qc-muted", "No valid coordinates found in event or occurrence tables."))
      }

      src_tab <- sort(table(rec$source), decreasing = TRUE)
      txt <- paste0(names(src_tab), ": ", as.integer(src_tab), collapse = " | ")

      shiny::div(
        class = "qc-muted",
        paste0("Mapped points: ", nrow(rec), " (", txt, ")")
      )
    })

    if (has_leaflet) {
      output$overview_map <- leaflet::renderLeaflet({
        rec <- qc_res()$overview$map_records

        m <- leaflet::leaflet() |>
          leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
          leaflet::addProviderTiles("Esri.OceanBasemap", group = "Esri Ocean") |>
          leaflet::addProviderTiles("Esri.WorldImagery", group = "Hybrid") |>
          leaflet::addProviderTiles("CartoDB.PositronOnlyLabels", group = "Hybrid") |>
          leaflet::addLayersControl(
            baseGroups = c("OSM", "Esri Ocean", "Hybrid"),
            overlayGroups = c("Occurrence records", "Event records", "Issues"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          ) |>
          leaflet::setView(lng = 0, lat = 0, zoom = 2)

        if (is.data.frame(rec) && nrow(rec) > 0) {
          rec_occ <- rec[rec$source == "occurrence", , drop = FALSE]
          rec_ev <- rec[rec$source == "event", , drop = FALSE]

          if (nrow(rec_occ) > 0) {
            m <- m |>
              leaflet::addCircleMarkers(
                lng = rec_occ$decimalLongitude,
                lat = rec_occ$decimalLatitude,
                radius = 4,
                stroke = FALSE,
                fillOpacity = 0.7,
                color = "#2563eb",
                group = "Occurrence records",
                popup = paste0("<b>source:</b> occurrence<br><b>id:</b> ", rec_occ$id)
              )
          }

          if (nrow(rec_ev) > 0) {
            m <- m |>
              leaflet::addCircleMarkers(
                lng = rec_ev$decimalLongitude,
                lat = rec_ev$decimalLatitude,
                radius = 4,
                stroke = FALSE,
                fillOpacity = 0.6,
                color = "#059669",
                group = "Event records",
                popup = paste0("<b>source:</b> event<br><b>id:</b> ", rec_ev$id)
              )
          }

          b <- qc_res()$overview$coords_bounds

          if (!is.null(b)) {
            m <- m |> leaflet::fitBounds(b$lng1, b$lat1, b$lng2, b$lat2)
          }
        }

        mi <- qc_res()$map_issues

        if (is.data.frame(mi) && nrow(mi) > 0) {
          m <- m |>
            leaflet::addCircleMarkers(
              lng = mi$decimalLongitude,
              lat = mi$decimalLatitude,
              radius = 5,
              stroke = FALSE,
              fillOpacity = 0.8,
              color = "#b91c1c",
              group = "Issues",
              popup = paste0(
                "<b>id:</b> ", mi$id,
                "<br><b>table:</b> ", mi$table,
                "<br><b>severity:</b> ", mi$severity,
                "<br><b>issue:</b> ", mi$issue,
                "<br><b>original_row:</b> ", mi$original_row,
                "<br><b>original_id:</b> ", mi$original_id
              )
            )
        }

        m
      })

      output$issues_map <- leaflet::renderLeaflet({
        mi <- qc_res()$map_issues

        m <- leaflet::leaflet() |>
          leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
          leaflet::addProviderTiles("Esri.OceanBasemap", group = "Esri Ocean") |>
          leaflet::addProviderTiles("Esri.WorldImagery", group = "Hybrid") |>
          leaflet::addProviderTiles("CartoDB.PositronOnlyLabels", group = "Hybrid") |>
          leaflet::addLayersControl(
            baseGroups = c("OSM", "Esri Ocean", "Hybrid"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )

        if (!is.data.frame(mi) || nrow(mi) == 0) return(m)

        m |>
          leaflet::addCircleMarkers(
            lng = mi$decimalLongitude,
            lat = mi$decimalLatitude,
            radius = 5,
            stroke = FALSE,
            fillOpacity = 0.75,
            popup = paste0(
              "<b>id:</b> ", mi$id,
              "<br><b>table:</b> ", mi$table,
              "<br><b>source:</b> ", mi$source,
              "<br><b>severity:</b> ", mi$severity,
              "<br><b>issue:</b> ", mi$issue,
              "<br><b>original_row:</b> ", mi$original_row,
              "<br><b>original_id:</b> ", mi$original_id
            )
          )
      })
    }

    output$overview_date_title <- shiny::renderUI({
      dc <- qc_res()$overview$date_counts

      if (is.data.frame(dc) && nrow(dc) > 0 && "source" %in% names(dc)) {
        src <- unique(dc$source)
        src <- src[!is.na(src)]

        if (length(src) > 0 && identical(src[[1]], "occurrence")) {
          return(shiny::span("Records by date (Occurrences)"))
        }
      }

      shiny::span("Records by date (Events)")
    })

    output$overview_date_plot <- shiny::renderPlot(
      expr = {
        dc <- qc_res()$overview$date_counts

        if (!is.data.frame(dc) || nrow(dc) == 0 || !"date" %in% names(dc) || !"n" %in% names(dc)) {
          return(NULL)
        }

        df_dates <- data.frame(
          date_str = as.character(dc$date),
          count = as.numeric(dc$n),
          stringsAsFactors = FALSE
        )

        df_dates$date_parsed <- suppressWarnings(as.Date(df_dates$date_str, format = "%Y-%m-%d"))
        df_dates <- df_dates[!is.na(df_dates$date_parsed), , drop = FALSE]

        if (nrow(df_dates) == 0) return(NULL)

        plot_data <- data.frame(
          date = df_dates$date_parsed,
          count = df_dates$count,
          stringsAsFactors = FALSE
        )

        ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = count)) +
          ggplot2::geom_col(fill = "#5985a7", color = "white", width = 0.8) +
          ggplot2::scale_x_date(
            date_labels = "%Y-%m-%d",
            breaks = scales::pretty_breaks(n = 6),
            expand = ggplot2::expansion(mult = c(0.05, 0.05))
          ) +
          ggplot2::scale_y_continuous(
            breaks = scales::pretty_breaks(n = 5),
            labels = scales::label_number(big.mark = ","),
            expand = ggplot2::expansion(mult = c(0, 0.1))
          ) +
          ggplot2::labs(
            title = NULL,
            x = "Event Date",
            y = "Count of Records"
          ) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
            plot.margin = ggplot2::margin(t = 10, r = 15, b = 20, l = 15),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(color = "#e5e7eb", linewidth = 0.3),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 11, color = "black"),
            axis.text.y = ggplot2::element_text(size = 11, color = "black"),
            axis.title = ggplot2::element_text(size = 12, face = "bold")
          )
      },
      height = 380,
      width = 1200,
      res = 96,
      bg = "white"
    )

    output$overview_tax_plot <- if (has_plotly) {
      plotly::renderPlotly({
        oc <- occ_in()

        shiny::req(is.data.frame(oc), nrow(oc) > 0)

        hierarchy_data <- .qc_build_nested_taxonomy(oc)

        shiny::req(!is.null(hierarchy_data), nrow(hierarchy_data) > 0)

        plotly::plot_ly(
          data = hierarchy_data,
          ids = ~ids,
          labels = ~labels,
          parents = ~parents,
          values = ~values,
          type = "sunburst",
          branchvalues = "total",
          marker = list(colorscale = "Viridis"),
          textinfo = "label+value",
          hovertemplate = "<b>%{label}</b><br>Count: %{value}<extra></extra>"
        ) |>
          plotly::layout(
            margin = list(l = 0, r = 0, t = 40, b = 0)
          )
      })
    }

    output$issues_summary_tbl <- DT::renderDT({
      x <- qc_res()$issues_summary
      x <- .qc_prepare_issues_display(x)
      .qc_dt(x, page_length = 15)
    })

    output$issues_detailed_tbl <- DT::renderDT({
      x <- qc_res()$issues_detailed
      x <- .qc_prepare_issues_display(x)
      .qc_dt(x, page_length = 20)
    })

    output$emof_issues_tbl <- DT::renderDT({
      x <- qc_res()$issues_detailed
      x <- x[x$table == "emof", , drop = FALSE]
      x <- .qc_prepare_issues_display(x)
      .qc_dt(x, page_length = 15)
    })

    output$invalid_event_tbl <- DT::renderDT({
      .qc_dt(qc_res()$invalid$event, page_length = 10)
    })

    output$invalid_occ_tbl <- DT::renderDT({
      .qc_dt(qc_res()$invalid$occurrence, page_length = 10)
    })

    output$invalid_emof_tbl <- DT::renderDT({
      .qc_dt(qc_res()$invalid$emof, page_length = 10)
    })

    output$about_ui <- shiny::renderUI({
      rt <- qc_res()$resource_type

      event_checks <- if (identical(rt, "sampling_event")) {
        list(
          shiny::tags$li("Unique IDs for Event."),
          shiny::tags$li("ISO 8601 validation of eventDate in Event."),
          shiny::tags$li("Occurrence-event link consistency."),
          shiny::tags$li("parentEventID consistency.")
        )
      } else {
        list(
          shiny::tags$li("Event-core checks are skipped when no Event table is present."),
          shiny::tags$li("ISO 8601 validation of eventDate in Occurrence, when available.")
        )
      }

      shiny::tagList(
        shiny::h4("Checks implemented"),
        shiny::tags$ul(
          event_checks,
          shiny::tags$li("Unique IDs for Occurrence."),
          shiny::tags$li("Required scientificName in Occurrence."),
          shiny::tags$li("Coordinate range checks."),
          shiny::tags$li("Required eMoF fields and duplicate measurementType checks."),
          shiny::tags$li("Overview plots for eventDate counts, taxonomy, and mapped coordinates.")
        )
      )
    })

    list(
      qc_res = qc_res,
      can_export = can_export
    )
  })
}
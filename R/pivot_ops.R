# R/pivot_ops.R

#' Apply a safe pivot_longer operation (wide -> long)
#'
#' Pure helper (no Shiny). Intended to be called by modules.
#'
#' @param df data.frame
#' @param id_cols character vector of ID columns to keep
#' @param pivot_cols character vector of columns to pivot
#' @param names_to string, column name for pivoted column names
#' @param values_to string, column name for pivoted values
#' @param drop_na logical, drop NA values after pivot
#' @param drop_zero logical, drop zero values after pivot (numeric only)
#' @param trim_names logical, trim whitespace in names_to column
#'
#' @return list(data=..., dropped_na=..., dropped_zero=..., meta=...)
apply_pivot_longer <- function(df,
                               id_cols,
                               pivot_cols,
                               names_to = "variable",
                               values_to = "value",
                               drop_na = TRUE,
                               drop_zero = FALSE,
                               trim_names = TRUE) {
  stopifnot(is.data.frame(df))

  id_cols <- unique(stats::na.omit(id_cols))
  pivot_cols <- unique(stats::na.omit(pivot_cols))

  if (length(pivot_cols) == 0) {
    stop("No pivot columns selected.")
  }

  if (!nzchar(names_to)) {
    stop("`names_to` cannot be empty.")
  }

  if (!nzchar(values_to)) {
    stop("`values_to` cannot be empty.")
  }

  if (length(intersect(id_cols, pivot_cols)) > 0) {
    stop("ID columns cannot also be pivot columns.")
  }

  internal_cols <- intersect(aurora_internal_cols(), names(df))
  if (length(intersect(pivot_cols, internal_cols)) > 0) {
    stop("Internal AURORA trace columns cannot be pivoted.")
  }

  missing_cols <- setdiff(c(id_cols, pivot_cols), names(df))
  if (length(missing_cols) > 0) {
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }

  out <- tidyr::pivot_longer(
    data = df,
    cols = tidyselect::all_of(pivot_cols),
    names_to = names_to,
    values_to = values_to
  )

  if (isTRUE(trim_names)) {
    out[[names_to]] <- trimws(as.character(out[[names_to]]))
  }

  dropped_na <- out[0, , drop = FALSE]
  dropped_zero <- out[0, , drop = FALSE]

  if (isTRUE(drop_na)) {
    idx_na <- is.na(out[[values_to]])
    if (any(idx_na)) {
      dropped_na <- out[idx_na, , drop = FALSE]
      out <- out[!idx_na, , drop = FALSE]
    }
  }

  if (isTRUE(drop_zero)) {
    v <- out[[values_to]]
    if (is.numeric(v)) {
      idx_zero <- !is.na(v) & v == 0
      if (any(idx_zero)) {
        dropped_zero <- out[idx_zero, , drop = FALSE]
        out <- out[!idx_zero, , drop = FALSE]
      }
    }
  }

  list(
    data = out,
    dropped_na = dropped_na,
    dropped_zero = dropped_zero,
    meta = list(
      n_before = nrow(df),
      n_after = nrow(out),
      n_dropped_na = nrow(dropped_na),
      n_dropped_zero = nrow(dropped_zero),
      pivot_cols = pivot_cols,
      id_cols = id_cols,
      names_to = names_to,
      values_to = values_to
    )
  )
}
#' Internal helper: null coalesce
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


# =========================================================
# AURORA internal columns (traceability only)
# =========================================================

aurora_internal_cols <- function() {
  c(".aurora_origin_row", ".aurora_origin_id")
}

aurora_is_internal_col <- function(x) {
  grepl("^\\.aurora_", x)
}

# return only user-visible columns
aurora_user_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(character())
  names(df)[!aurora_is_internal_col(names(df))]
}

# drop internal columns for UI display
aurora_drop_internal_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  df[, !aurora_is_internal_col(names(df)), drop = FALSE]
}


# =========================================================
# Add origin tracking columns
# =========================================================

aurora_add_origin_cols <- function(df) {
  stopifnot(is.data.frame(df))

  if (!".aurora_origin_row" %in% names(df)) {
    df$.aurora_origin_row <- seq_len(nrow(df))
  }

  if (!".aurora_origin_id" %in% names(df)) {
    df$.aurora_origin_id <- sprintf("src_%06d", seq_len(nrow(df)))
  }

  df
}
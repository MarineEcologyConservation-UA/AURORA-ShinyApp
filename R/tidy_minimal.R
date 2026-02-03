#' Minimal tidying for a data.frame
#'
#' Performs a lightweight, safe cleanup for user-provided tabular data:
#' removes empty-name columns, drops common index columns (e.g. `Unnamed: 0`,
#' `...1`), converts character columns to UTF-8 using the provided encoding,
#' trims whitespace, and removes fully empty rows.
#'
#' Important: this function intentionally DOES NOT normalize column names
#' (e.g. it will not turn `eventID` into `event_id`) to avoid breaking valid
#' Darwin Core term names before the mapping step.
#'
#' @param df A [data.frame].
#' @param encoding_used Character scalar (optional). Original encoding used to
#'   read the file. Used as the `from=` argument in [iconv()].
#'
#' @return A cleaned [data.frame].
#'
#' @examples
#' df <- data.frame(bad_col = c("", ""), Unnamed..0 = 1:2, Name = c(" A ", "B"))
#' names(df)[1] <- ""  # simula coluna sem nome
#' tidy_minimal(df, encoding_used = "UTF-8")
#'
#' @export
tidy_minimal <- function(df, encoding_used = NULL) {
  if (is.null(df)) return(NULL)

  nm <- names(df)
  bad_names <- is.na(nm) | nm == ""
  if (any(bad_names)) df <- df[, !bad_names, drop = FALSE]
  if (ncol(df) == 0) return(df)

  # Drop common index columns created by exports (pandas, Excel, etc.)
  idx_cols <- grepl("^(Unnamed(\\.|:|\\s)|\\.\\.\\.[0-9]+$)", names(df))
  if (any(idx_cols)) df <- df[, !idx_cols, drop = FALSE]

  is_text <- vapply(
    df,
    function(x) is.character(x) || is.factor(x),
    logical(1)
  )

  df[is_text] <- lapply(df[is_text], function(x) {
    x <- as.character(x)

    if (!is.null(encoding_used) && !is.na(encoding_used) &&
          encoding_used != "") {
      x2 <- iconv(x, from = encoding_used, to = "UTF-8", sub = "byte")

      if (anyNA(x2)) {
        x2[is.na(x2)] <- iconv(
          x[is.na(x2)],
          from = "Windows-1252",
          to = "UTF-8",
          sub = "byte"
        )
      }

      x <- x2
      x[is.na(x)] <- ""
    } else {
      x[is.na(x)] <- ""
    }

    trimws(x)
  })

  all_empty <- apply(df, 1, function(r) all(is.na(r) | r == ""))
  df <- df[!all_empty, , drop = FALSE]

  df
}

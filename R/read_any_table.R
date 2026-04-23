#' Read a tabular file (CSV only) into a data.frame
#'
#' Reads a CSV file from disk and returns its content as a base
#' [data.frame]. It attempts multiple encodings in sequence until one succeeds.
#' The encoding that worked is stored in attribute "encoding_used".
#'
#' @param path Character scalar. Path to the input file.
#' @param ext Character scalar (optional). File extension override. If NULL, inferred from path.
#' @param delim Character scalar. Field delimiter (default ",").
#' @param has_header Logical scalar. Whether the first row contains column names.
#' @param encoding Character scalar. Preferred encoding to try first.
#'
#' @return A data.frame with attribute "encoding_used".
#' @export
read_any_table <- function(path,
                           ext = NULL,
                           delim = ",",
                           has_header = TRUE,
                           encoding = "UTF-8") {

  if (is.null(ext)) {
    ext <- tools::file_ext(path)
  }

  ext <- tolower(ext)

  # -------- HARD BLOCK: only CSV --------
  if (!identical(ext, "csv")) {
    stop("Only .csv files are supported.")
  }

  encodings <- unique(c(
    encoding,
    "UTF-8",
    "Windows-1252",
    "Latin1",
    "ISO-8859-1",
    "ISO-8859-2"
  ))

  last_error <- NULL

  for (enc in encodings) {
    res <- tryCatch(
      readr::read_delim(
        file = path,
        delim = delim,
        col_names = has_header,
        locale = readr::locale(encoding = enc),
        show_col_types = FALSE,
        progress = FALSE
      ),
      error = function(e) e
    )

    if (!inherits(res, "error")) {
      out <- as.data.frame(res, stringsAsFactors = FALSE)
      attr(out, "encoding_used") <- enc
      return(out)
    }

    last_error <- conditionMessage(res)
  }

  stop(
    "Failed to read CSV file. Tried encodings: ",
    paste(encodings, collapse = ", "),
    "\nLast error: ", last_error
  )
}
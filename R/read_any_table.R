#' Read a CSV file into a data.frame
#'
#' Reads a CSV file from disk and returns its content as a base
#' [data.frame]. It attempts multiple encodings in sequence until one
#' succeeds. The encoding that worked is stored in an attribute
#' `"encoding_used"` in the returned object.
#'
#' Supported extension:
#' - `.csv` via [readr::read_delim()]
#'
#' @param path Character scalar. Path to the input file.
#' @param ext Character scalar (optional). File extension override. If `NULL`, it
#'   is inferred from `path`.
#' @param delim Character scalar. Field delimiter used for the CSV file.
#' @param has_header Logical scalar. Whether the first row contains column names.
#' @param encoding Character scalar. Preferred encoding to try first
#'   (e.g., `"UTF-8"`, `"Windows-1252"`, `"Latin1"`).
#'
#' @return A [data.frame] with the imported data. The returned object includes
#'   attribute `"encoding_used"` indicating which encoding successfully read the
#'   file.
#'
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' writeLines(c("a,b", "1,2", "3,4"), tmp)
#' df <- read_any_table(tmp)
#' attr(df, "encoding_used")
#'
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

  if (!identical(ext, "csv")) {
    stop("Unsupported file extension: ", ext, ". Only .csv is supported.")
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
    "Failed to read file. Tried encodings: ",
    paste(encodings, collapse = ", "),
    "\nLast error: ", last_error
  )
}
#' Internal helper: null coalesce
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

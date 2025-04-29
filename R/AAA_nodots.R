## helper to parse parameter lists with defaults
.nodots <- function(...) {
  l <- list(...)
  if (length(l) > 0L) {
    warning("In ", deparse(sys.calls()[[sys.nframe()-1]]), ":\n",
      "  Unknown arguments: ",
            paste(names(l), "=", l, collapse = ", "), call. = FALSE)
  }
}
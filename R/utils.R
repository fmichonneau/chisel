`%||%`  <- function(x, y) {
    if (is.null(x)) return(y)
    x
}

get_env_var <- function(var) {
  res <- Sys.getenv(var)

  if (identical(res, "")) {
    stop(sQuote(var), " not set as environment variable.", call. = FALSE)
  }

  res
}

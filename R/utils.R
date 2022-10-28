`%||%`  <- function(x, y) {
    if (is.null(x)) return(y)
    x
}

get_env_var <- function(var) {
  res <- Sys.getenv(var)

  if (identical(res, "")) {
    msg <- paste0(sQuote(var), " not set as environment variable.")
    if (startsWith(var, "REDASH_QUERY") && interactive()) {
      url <- "https://redash.carpentries.org/queries/"
      url <- paste0(url, sub("REDASH_QUERY", "", var))
      message(msg)
      message(sprintf("Launching %s...\n", url))
      browseURL(url)
      message("Click on the kebab menu in the top left, select 'Show API Key', and copy the API Key\n(DO NOT CLICK 'REGENERATE')")
      res <- list(askpass::askpass("Paste the API key here: "))
      names(res) <- var
      do.call(Sys.setenv, res)
      res <- res[[var]]
    } else {
      stop(msg, call. = FALSE)
    }
  }
  res
}

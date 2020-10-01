##' @importFrom gh gh
get_raw_repositories <- function(org, ...) {

  init_res  <- gh::gh("GET /orgs/:org/repos", org = org)
  res <- list()
  test <- TRUE
  i <- 1

  while (test) {
    message("Getting page: ", i, " for ", sQuote(org))
    res <- append(res, init_res)

    init_res <- tryCatch({
      gh::gh_next(init_res)
    },
    error = function() {
      test <<- FALSE
      NULL
    })
    i <- i + 1
  }

  res
}

##' @importFrom purrr map_df
##' @importFrom dplyr filter
##' @importFrom rlang .data
get_repositories <- function(org, filter = NULL) {
    raw_repo <- get_raw_repositories(org)
    res <- purrr::map_df(raw_repo, function(x) {
      if (!grepl("archive|deprecated|ideas|hackathon|^[0-9]{4}", x[["name"]],
        ignore.case = TRUE) & grepl("-", x[["name"]])) {
        list(
          id = x[["id"]],
          name = x[["name"]],
          full_name = x[["full_name"]]
        )
    }
    })

    if (!is.null(filter)) {
      res <- res %>%
        dplyr::filter(grepl(filter, .data$name))
    }

    res
}

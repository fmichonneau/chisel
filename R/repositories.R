get_raw_repositories <- function(org, ...) {
    i <- 1
    tmp_res <- NULL
    res <- vector("list", 100)
    while (is.null(tmp_res) || tmp_res != "") {
        tmp_res <- gh::gh("GET /orgs/:org/repos",  org = org,
                          type = "public", page = i, ...)
        res[[i]] <- tmp_res
        i <- i + 1
    }
    unlist(res[!vapply(res, is.null, logical(1)) & nzchar(res)], recursive = FALSE)
}


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
            dplyr::filter(grepl(filter, name))
    }

    res
}

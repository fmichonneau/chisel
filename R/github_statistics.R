github_stats_repo_raw <- function(org = "datacarpentry", repo = "organization-geospatial",
                                  min_date = "2018-04-15") {
    res <- vector("list", 1000)
    i <- 1
    browser()
    last_date <- as.Date(min_date) + 1
    while (i > 0 && last_date > min_date) {
        res[[i]] <- gh::gh("GET /users/:username/events/orgs/:org", org = org, username = "fmichonneau",
                           per_page = 100, page = i)
        last_date <- min(as.Date(purrr::map_chr(res[[i]], "created_at")))
        i <- i + 1
    }

    unlist(res[!vapply(res, is.null, logical(1)) & nzchar(res)], recursive = FALSE)

}


get_pull_requests <- function(owner = "datacarpentry", repo = "organization-geospatial") {


    all_prs <- gh::gh("GET /repos/:owner/:repo/pulls",
                      owner = owner, repo = repo,
                      state = "all",
                      per_page = 100)

    purrr::map_df(all_prs, ~ list(
                              user = .[["user"]][["login"]],
                              number = .[["number"]],
                              state = .[["state"]],
                              created_at = .[["created_at"]],
                              updated_at = .[["updated_at"]],
                              closed_at = .[["closed_at"]],
                              merged_at = .[["merged_at"]]
                          )) %>%
        dplyr::mutate(repo = repo)
}

get_issues <- function(owner = "datacarpentry", repo = "organization-geospatial",
                       min_date = "2018-04-01") {

    all_issues <- gh::gh("GET /repos/:owner/:repo/issues",
                         owner = owner, repo = repo,
                          state = "all", since = min_date,
                         per_page = 100)

    message(repo)

    if (identical(all_issues[[1]], "")) return(NULL)

    purrr::map_df(all_issues, function(iss) {

        is_pr <- grepl("pull", iss$html_url)
        list(
            user = iss[["user"]][["login"]],
            number = iss[["number"]],
            is_pr = is_pr,
            state = iss[["state"]],
                created_at = iss[["created_at"]],
                updated_at = iss[["updated_at"]],
                closed_at = iss[["closed_at"]] %||% NA_character_
            )
        }) %>%
    dplyr::mutate(repo = repo)

}


get_repo_traffic <- function(owner, repo) {
    res <- gh::gh("GET /repos/:owner/:repo/traffic/views",
                  owner = owner, repo = repo)
    purrr::map_df(res[["views"]], ~ list(
                                     timestamp =  .[["timestamp"]],
                                     count = .[["count"]],
                                     uniques = .[["uniques"]]
                                 ))
}



if (FALSE) {
    reps <- get_repositories(org = "datacarpentry",
                             filter = "socialsci|geospatial|genomics|ecology")

    res_dc <- reps %>%
        dplyr::pull(name) %>%
        purrr::map_df(~ get_issues(repo = .))

    res_swc <- c("r-novice-gapminder-es", "r-novice-inflammation") %>%
        purrr::map_df(~ get_issues(owner = "swcarpentry", repo = .))

    res_inst <- get_issues(owner = "carpentries", repo = "instructor-training")

    res <- dplyr::bind_rows(datacarpentry = res_dc,
                            swcarpentry = res_swc,
                            carpentries = res_inst,
                            .id = "organization")


    readr::write_csv(res, "data/issues_bug_bbq.csv")


    tidy_repos <- reps %>%
        dplyr::mutate(owner = "datacarpentry") %>%
        dplyr::select(owner, repo = name) %>%
        dplyr::bind_rows(
                   tribble(
                       ~ owner, ~repo,
                       "swcarpentry", "r-novice-gapminder-es",
                       "swcarpentry", "r-novice-inflammation",
                       "carpentries", "instructor-training"
                   )
               ) %>%
        dplyr::mutate(repo_id = row_number())

    stats_repos <- tidy_repos %>%
        dplyr::mutate(stats = purrr::pmap(., get_repo_traffic))

    stats_repos <- stats_repos %>%
        tidyr::unnest() %>%
        readr::write_csv("data/visitors.csv")





    tidyr::gather(res, event, date,
                  -user, -number, -is_pr,
                  -state, -repo, -organization) %>%
        dplyr::filter(event %in% c("created_at", "closed_at")) %>%
            ggplot(aes(x = factor(event, levels = c("created_at", "closed_at")),
                       y = as.Date(date))) +
            geom_point(aes(color = organization)) +
            geom_line(aes(group = interaction(repo, number)))

}

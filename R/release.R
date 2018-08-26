
git_url <- function(owner, repo, provider = c("github", "github-ssh")) {
    provider <- match.arg(provider)
    if (identical(provider, "github")) {
        url <- glue::glue("https://github.com/{owner}/{repo}",
                          owner = owner, repo = repo)
    } else if (identical(provider, "github-ssh")) {
        url <- glue::glue("git@github.com:{owner}/{repo}.git",
                          owner = owner, repo = repo)
    }
    url
}


get_repo <- function(owner, repo, provider = "github") {
    url <- git_url(owner, repo, provider)
    pth <- file.path(tempdir(),
                     paste(owner, repo, ids::proquint(1, 1), sep = "-"))
    dir.create(pth, recursive = TRUE)

    git2r::clone(url, pth)
}

extract_repo_history <- function(repos) {

    if (!inherits(repos, "list"))
        repos <- list(...)

    stopifnot(!is.null(names(repos)))
    stopifnot(all(purrr::map_lgl(repos,
                                 ~ inherits(., "git_repository"))))

  purrr::map_df(repos, function(x) {
    git2r::commits(x) %>%
      purrr::map_df(~ list(sha = .x@sha,
                           name = .x@author@name,
                           email = .x@author@email)
                    )
  }, .id = "repo")
}

extract_shortlog_history <- function(repos) {
    fout <- tempfile()

    if (!inherits(repos, "list"))
        repos <- list(...)

    stopifnot(!is.null(names(repos)))
    stopifnot(all(purrr::map_lgl(repos,
                                 ~ inherits(., "git_repository"))))

    purrr::map_df(repos, function(x) {
      copy_master_mailmap(x$path)
      system(paste("cd ", x$path, ";",
                   'git shortlog --format=\"%H|%aN|%aE\" | grep \"|\" > ', fout))

      readr::read_delim(fout, delim = "|",
                        col_names = FALSE, trim_ws = TRUE,
                        col_types = "ccc") %>%
            rlang::set_names("sha", "name", "email")
    }, .id = "repo")
}

copy_master_mailmap <- function(repo_path, mailmap = "inst/mailmap/.mailmap") {

  ## The mailmap copy in this repository should point to the email address used
  ## in AMY by the user, so we can match to name + ORCID

  dest_mailmap <- file.path(repo_path, ".mailmap")
  if (file.exists(dest_mailmap)) {
    orig_mailmap <- readLines(dest_mailmap, warn = FALSE)
  } else {
    orig_mailmap <- character(0)
  }

  to_add <- readLines(mailmap, warn = FALSE)

  writeLines(c(orig_mailmap, to_add), sep = "\n",
             con = dest_mailmap)

}

get_origin_repo <- function(repo_list,
                            main_ignore =
                              tibble::tibble(email =
                                               c("erinstellabecker@gmail.com",
                                                 "francois.michonneau@gmail.com"))) {

  stopifnot("main" %in% repo_list$name)

  res <- repo_list %>%
        purrr::pmap(function(owner, repo, ...) {
            get_repo(owner, repo)
        }) %>%
        rlang::set_names(repo_list$name) %>%
        extract_shortlog_history()

    if (!is.null(main_ignore)) {
        res <- dplyr::filter(res, !(email %in% main_ignore$email &
                                  repo == "main"))

    }

    res_split <- split(res, res$repo)
    .r <- vector("list", length(res_split))
    i_split <- seq_along(res_split)
    for (i in i_split) {
        focus_src <- res_split[[i]]
        other_src <- dplyr::bind_rows(res_split[-i])
        focus_src <- dplyr::anti_join(focus_src, other_src, by = "sha")
        .r[[i]] <- dplyr::count(focus_src, name, email, sort = TRUE)
    }

    dplyr::bind_rows(.r) %>%
        dplyr::distinct(email, .keep_all = TRUE)
}


if (FALSE) {

    ## Git novice ES release
    res <- tibble::tribble(
      ~name,      ~owner,        ~repo,
      "main",     "swcarpentry", "git-novice-es",
      "source",   "swcarpentry", "git-novice",
      "template", "swcarpentry", "styles-es"
      ) %>%
        generate_zenodo_json(local_path = "~/git/git-novice-es/",
                             editors = c("Rayna M Harris"))

    ## Shell novice ES release
    res <-  tibble::tribble(
      ~name,      ~owner,        ~repo,
      "main",     "swcarpentry", "shell-novice-es",
      "source",   "swcarpentry", "shell-novice",
      "template", "swcarpentry", "styles-es"
      ) %>%
        generate_zenodo_json(local_path = "~/git/shell-novice-es/",
                             editors = c("Heladia Saldago"))

    ## R novice gapminder
    res <-  tibble::tribble(
                        ~name,      ~owner,        ~repo,
                        "main",     "swcarpentry", "r-novice-gapminder-es",
                        "source",   "swcarpentry", "r-novice-gapminder",
                        "template", "swcarpentry", "styles-es"
                    ) %>%
        generate_zenodo_json(local_path = "~/git/r-novice-gapminder-es/",
                             editors = c("Rayna Harris", "Verónica Jiménez",
                                         "Silvana Pereyra", "Heladia Salgado"))

    ## openrefine social sciences
    res <- tibble::tribble(
                       ~name, ~owner, ~repo,
                       "main", "datacarpentry", "openrefine-socialsci",
                       "template", "swcarpentry", "styles"
                   ) %>%
        generate_zenodo_json(local_path = "~/git/openrefine-socialsci/",
                             editors = c("Geoff LaFlair", "Peter Smyth"))

    ## spreadsheets social sciences
    res <- tibble::tribble(
                       ~name, ~owner, ~repo,
                       "main", "datacarpentry", "spreadsheets-socialsci",
                       "template", "swcarpentry", "styles"
                   ) %>%
        generate_zenodo_json(local_path = "~/git/spreadsheets-socialsci/",
                             editors = c("Chris Prener", "Peter Smyth"))

    ## R social sciences
    res <- tibble::tribble(
                       ~name, ~owner, ~repo,
                       "main", "datacarpentry", "r-socialsci",
                       "template", "swcarpentry", "styles"
                   ) %>%
        generate_zenodo_json(local_path = "~/git/r-socialsci/",
                             editors = c("Juan Fung", "Peter Smyth"))

    ## Social sciences workshop
    res <- tibble::tribble(
                       ~name, ~owner, ~repo,
                       "main", "datacarpentry", "socialsci-workshop",
                       "template", "swcarpentry", "styles"
                   ) %>%
        generate_zenodo_json(local_path = "~/git/socialsci-workshop/",
                             editors = c("Stephen Childs", "Juan Fung",
                                         "Geoff LaFlair", "Rachel Gibson",
                                         "Chris Prener", "Peter Smyth"))

}

generate_zenodo_json <- function(repos, local_path, editors) {
    creators <- repos %>%
        get_origin_repo() %>%
        dplyr::pull(name) %>%
        purrr::map(~ list(name = .))

    creators <- list(creators = creators)

    eds <- purrr::map(editors, ~ list(type = "Editor", name = .))
    eds <- list(contributors = eds)

    lic <- list(license =  list(id =  "CC-BY-4.0"))

    ## typ <- list(resource_type = list(title = "Lesson", type = "lesson"))

    res <- c(eds, creators, lic) #, typ)
    cat(jsonlite::toJSON(res, auto_unbox = TRUE),
        file = file.path(local_path, ".zenodo.json"))
}




generate_citation <- function(authors = "AUTHORS",
                              editors = eds,
                              doi = "10.5281/zenodo.569338",
                              title = "Data Carpentry: R for data analysis and visualization of Ecological Data") {

    stopifnot(inherits(editors, "person"))

    aut <- readLines(authors)

    # remove first line
    aut <- aut[-1]

    aut <- as.person(aut)

    bibentry(
        bibtype = "Misc",
        author = personList(aut),
        title = title,
        editor = editors,
        month = format(Sys.Date(), "%B"),
        year = format(Sys.Date(), "%Y"),
        url = "http://datacarpentry.org/R-ecology-lesson/",
        doi = doi
    )

}


##system("python3 /usr/local/bin/update-copyright.py")
##generate_zenodo_json(editors = eds)

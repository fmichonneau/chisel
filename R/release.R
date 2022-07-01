
git_url <- function(owner, repo, provider = c("github", "github-ssh")) {
  provider <- match.arg(provider)
  if (identical(provider, "github")) {
    url <- glue::glue("https://github.com/{owner}/{repo}.git",
      owner = owner, repo = repo)
  } else if (identical(provider, "github-ssh")) {
    url <- glue::glue("git@github.com:{owner}/{repo}.git",
      owner = owner, repo = repo)
  }
  url
}


get_repo_fetch_hook <- function(key, namespace) {
  pth <- file.path(
    "/tmp/repos",
    paste(key, ids::proquint(1, 1), sep = "-")
  )
  dir.create(pth, recursive = TRUE)
  pth
}


get_repo <- function(owner, repo, provider = "github",
                     path = "/tmp/repos") {
  url <- git_url(owner, repo, provider)

  st <- storr::storr_external(
    storr::driver_rds(tempdir(), mangle_key = TRUE),
    get_repo_fetch_hook
  )

  pth <- st$get(paste0(owner, "-", repo))

  pth_git <- file.path(pth, ".git")

  if (dir.exists(pth_git)) {
    git2r::repository(pth)
  } else {
    git2r::clone(url, pth)
  }
}

extract_repo_history <- function(repos) {

  if (!inherits(repos, "list"))
    repos <- list(repos)

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

extract_shortlog_history <- function(repos, since = NULL) {
  fout <- tempfile()

  if (!inherits(repos, "list"))
    repos <- list(repos)

  if (!is.null(since)) {
    since <- paste0("--since=", since)
  } else {
    since <- character(0)
  }

  stopifnot(!is.null(names(repos)))
  stopifnot(all(purrr::map_lgl(repos,
    ~ inherits(., "git_repository"))))

  purrr::map_df(repos, function(x) {
    copy_master_mailmap(x$path)
    system(paste("cd ", x$path, ";",
      'git shortlog --format=\"%H|%aN|%aE\"',
      since, '| grep \"|\" > ', fout))

    readr::read_delim(fout, delim = "|",
      col_names = FALSE, trim_ws = TRUE,
      col_types = "ccc", lazy = FALSE) %>%
      rlang::set_names("sha", "name", "email")
  }, .id = "repo")
}

copy_master_mailmap <- function(repo_path,
                                mailmap = system.file("mailmap/.mailmap", package = "chisel")) {

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

##' @importFrom tibble tibble
##' # mail_ignore should be a 1 column tibble named email e.g.:
##' main_ignore = tibble::tibble(
##'   email = c(
##'     "ebecker@carpentries.org",
##'    "francois.michonneau@gmail.com")
##' )
get_origin_repo <- function(repo_list,
                            main_ignore = NULL,
                            since = NULL) {

  stopifnot("main" %in% repo_list$name)

  res <- repo_list %>%
    purrr::pmap(function(owner, repo, ...) {
      get_repo(owner, repo)
    }) %>%
    rlang::set_names(repo_list$name) %>%
    extract_shortlog_history(since = since)

  if (!is.null(main_ignore)) {
    res <- dplyr::filter(res, !(.data$email %in% main_ignore$email &
                                  .data$repo == "main"))

  }

  res_split <- split(res, res$repo)
  .r <- vector("list", length(res_split))
  i_split <- seq_along(res_split)
  for (i in i_split) {
    focus_src <- res_split[[i]]
    other_src <- dplyr::bind_rows(res_split[-i])
    focus_src <- dplyr::anti_join(focus_src, other_src, by = "sha")
    .r[[i]] <- dplyr::count(focus_src, .data$name, .data$email, sort = TRUE)
  }

  dplyr::bind_rows(.r) %>%
    dplyr::distinct(.data$email, .keep_all = TRUE)
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

  ## R novice gapminder ES release
  res <-  tibble::tribble(
    ~name,      ~owner,        ~repo,
    "main",     "swcarpentry", "r-novice-gapminder-es",
    "source",   "swcarpentry", "r-novice-gapminder",
    "template", "swcarpentry", "styles-es"
  ) %>%
    generate_zenodo_json(local_path = "~/git/r-novice-gapminder-es/",
      editors = c("Rayna Harris", "Verónica Jiménez",
        "Silvana Pereyra", "Heladia Salgado"))

  ## python ecology ES release (2019-01-09)
  res <-  tibble::tribble(
    ~name,      ~owner,        ~repo,
    "main",     "datacarpentry", "python-ecology-lesson-es",
    "source",   "datacarpentry", "python-ecology-lesson",
    "template", "carpentries", "styles-es"
  ) %>%
    generate_zenodo_json(
      local_path = "~/git/ecology-lessons-es/python-ecology-lesson-es",
      editors = c("Paula Andrea Martinez",
        "Heladia Salgado", "Rayna Harris"))



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

  ## R r-intro geospatial
  res <- tibble::tribble(
    ~ name, ~owner,  ~repo,
    "main", "datacarpentry", "r-intro-geospatial",
    "source", "swcarpentry", "r-novice-gapminder",
    "template", "carpentries", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/geospatial-lessons/r-intro-geospatial/",
      editors = c("Janani Selvaraj", "Lachlan Deer",
        "Juan Fung"))

  ## Organization geospatial
  res <- tibble::tribble(
    ~ name, ~owner,  ~repo,
    "main", "datacarpentry", "organization-geospatial",
    "template", "carpentries", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/geospatial-lessons/organization-geospatial/",
      editors = c("Tyson Swetnam", "Chris Prener"),
      ignore = c("neondataskills@neoninc.org",
        "francois.michonneau@gmail.com"))

  ## Geospatial workshop
  res <- tibble::tribble(
    ~name,  ~owner, ~repo,
    "main", "datacarpentry", "geospatial-workshop",
    "template", "carpentries", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/geospatial-lessons/geospatial-workshop/",
      editors =  c("Anne Fouilloux", "Arthur Endsley",
        "Chris Prener", "Jeff Hollister",
        "Joseph Stachelek", "Leah Wasser",
        "Michael Sumner", "Michele Tobias",
        "Stace Maples"),
      ignore = c("ebecker@carpentries.org",
        "francois.michonneau@gmail.com"))

  ## R-raster-vector
  res <- tibble::tribble(
    ~name,  ~owner, ~repo,
    "main", "datacarpentry", "r-raster-vector-geospatial",
    "template", "carpentries", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/geospatial-lessons/r-raster-vector-geospatial/",
      editors = c("Joseph Stachelek", "Lauren O'Brien",
        "Jane Wyngaard"),
      ignore = c("francois.michonneau@gmail.com"))


  res <- tibble::tribble(
    ~name,  ~owner, ~repo,
    "main", "datacarpentry", "genomics-workshop",
    "template", "carpentries", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/genomics-lessons/genomics-workshop//",
      editors =  c("foo"),
      ignore = c("ebecker@carpentries.org",
        "francois.michonneau@gmail.com"))

  res <- tibble::tribble(
    ~name,  ~owner, ~repo,
    "main", "datacarpentry", "genomics-workshop",
    "template", "carpentries", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/genomics-lessons/genomics-workshop//",
      editors =  c("foo"),
      ignore = c("ebecker@carpentries.org",
        "francois.michonneau@gmail.com"))
}

add_pub_name <- function(.data) {

  ## for when calling add_pub_name for list of editors using their GitHub
  if (!exists("name", .data)) {
    .data <- .data %>%
      mutate(name = person_name_with_middle)
  }

  .data %>%
    dplyr::mutate(pub_name = dplyr::case_when(
      ## default on AMY profile info
      ## first use profile info if user specified it's what they wanted
      lesson_publication_consent == "amy" |
        lesson_publication_consent == "unset" ~ person_name_with_middle,
      ## then orcid info
      lesson_publication_consent == "orcid" &
        is_valid_orcid(clean_up_orcid(orcid)) ~ get_orcid_name(clean_up_orcid(orcid)),
      ## then github (just return GitHub username)
      lesson_publication_consent == "github" ~  get_github_name(github),
      ## if all else fails, use git info
      TRUE ~ name
    )) %>%
    dplyr::mutate(
      pub_name = gsub("\\s+", " ", pub_name)
    )

}

get_lesson_creators <- function(repos, since = NULL) {
  creators <- repos %>%
    get_origin_repo(since = since) %>%
    dplyr::left_join(all_people(), by = "email")

  creators %>%
    add_pub_name()
}

write_name <- function(first, middle, family) {
  res <- paste(
    first,
    dplyr::if_else(!is.na(middle) & nzchar(middle), middle, ""),
    family
  )
  gsub("\\s+", " ", res)
}

clean_up_orcid <- function(orcid) {
  orcid <- gsub("^https?://", "", orcid)
  orcid <- gsub("^\\s*orcid.org/", "", orcid)
  ## The zero width space unicode character
  orcid <- gsub("\\xE2\\x80\\x8B", "", orcid, useBytes = TRUE)
  orcid[!grepl("^\\d{4}-\\d{4}-\\d{4}-(\\d{3}X|\\d{4})$", orcid)] <- ""
  orcid
}

is_valid_orcid <- function(orcid) {
  !is.na(orcid) & nzchar(orcid) &
    grepl("^\\d{4}-\\d{4}-\\d{4}-(\\d{3}X|\\d{4})$", orcid)
}

get_orcid_name <- function(orcid) {
  purrr::map_chr(orcid, function(.x) {
    if (is.na(.x) || !nzchar(.x)) return(NA_character_)
    res <- rorcid::as.orcid(.x)
    if (!is.null(res[[1]]$name$`credit-name`$value)) {
      res <- res[[1]]$name$`credit-name`$value
    } else {
      res <- paste(res[[1]]$name$`given-names`, res[[1]]$name$`family-name`)
    }
    if (!length(res)) {
      return(NA_character_)
    }
    res
  })
}


get_github_name_hook <- function(key, namespace) {
  if (is.na(key)) return(NA_character_)
  res <- try(
    gh::gh("GET /users/:username", username = key),
    silent = TRUE
  )
  if (inherits(res, "try-error") || is.null(res$name))
    NA_character_
  else
    res$name

}


get_github_store <- function() {
  st <- storr::storr_external(
    storr::driver_rds(file.path("local_data/github_names"), mangle_key = TRUE),
    get_github_name_hook
  )
}

get_github_name <- function(github) {
  purrr::map_chr(github, function(.github) {
    get_github_store()$get(.github)
  })
}

##' @param editors_github the github username of the editors as it appears in AMY
generate_zenodo_json <- function(repos, local_path, editors_github,
                                 since = NULL,
                                 ignore = character(0)) {

  creators_df <- get_lesson_creators(repos, since = since) %>%
    dplyr::filter(
      is.na(lesson_publication_consent) | lesson_publication_consent != "no"
    )

  creators  <- creators_df %>%
    dplyr::anti_join(tibble::tibble(email = ignore), by = "email") %>%
    dplyr::select(.data$pub_name, .data$orcid) %>%
    purrr::pmap(function(pub_name, orcid) {
      if (is_valid_orcid(orcid)) {
        return(list(name = pub_name, orcid = orcid))
      } else {
        list(name = pub_name)
      }
    })

  creators <- list(creators = creators)

  eds <- purrr::map(editors_github, function(.x) {
    res <- all_people() %>%
      mutate(github = tolower(github)) %>%
      filter(github == tolower(.x)) %>%
      add_pub_name()

    if (nrow(res) != 1L)
      stop("issue with github name provided for editor: ", .x)

    list(
      name = res$pub_name, orcid = clean_up_orcid(res$orcid)
    ) %>%
      purrr::keep(~ !is.na(.) & nzchar(.))
  })

  eds <- purrr::map(eds, ~ c(type = "Editor",  .))
  eds <- list(contributors = eds)

  lic <- list(license =  list(id =  "CC-BY-4.0"))

  ## typ <- list(resource_type = list(title = "Lesson", type = "lesson"))

  res <- c(eds, creators, lic) #, typ)
  cat(jsonlite::toJSON(res, auto_unbox = TRUE, pretty = TRUE),
    file = file.path(local_path, ".zenodo.json"))

  creators_df
}



##' @importFrom utils as.person bibentry personList
generate_citation <- function(authors = "AUTHORS",
                              editors,
                              doi = "10.5281/zenodo.569338",
                              url = "https://datacarpentry.org/R-ecology-lesson/",
                              title = "Data Carpentry: R for data analysis and visualization of Ecological Data") {

  stopifnot(inherits(editors, "person"))

  aut <- readLines(authors)

  ## remove first line
  aut <- aut[-1]

  aut <- utils::as.person(aut)

  utils::bibentry(
    bibtype = "Misc",
    author = utlis::personList(aut),
    title = title,
    editor = editors,
    month = format(Sys.Date(), "%B"),
    year = format(Sys.Date(), "%Y"),
    url = url,
    doi = doi
  )

}

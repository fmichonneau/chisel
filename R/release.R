# N.B. This content was written by François Michonneau. 
# Documentation provided by Zhian N. Kamvar

#' Generate a git URL to use with git depending on provider method
#'
#'
#' @param owner the repository owner
#' @param repo the repository name
#' @param provider one of "github" or "github-ssh".
#'
#' @return a url to use with git in the form of either https://github.com/ for
#' https protocols (default) or git@@github.com/ for ssh protocols.
#'
#' @noRd
#' @examples
#' git_url("carpentries", "chisel", provider = "github")
#' # https://github.com/carpentries/chisel.git
#' git_url("carpentries", "chisel", provider = "github-ssh")
#' # git@github.com/carpentries/chisel.git
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


#' Utility function to fetch a resource when the key is not found
#'
#' This function is used by [storr::storr_external()] to fetch data when the
#' key is not found. In this case, it creates an empty temporary directory to
#' store the github repository in.
#'
#' @param key a single character string in the format "owner-repo"
#' @param namespace unused, but required by {storr}
#'
#' @note this creates a repository in a hard-coded path because there is no 
#'   clear way to get a consistent path name into this fetch hook function.
#'   That being said, this hardcoded path provides a way to store the downloaded
#'   repositories outside the life of the R session in case errors occur, which
#'   will reduce the network load and the number of API calls.
#' 
#' @return a pathname
#' @noRd
#' @seealso [get_repo()]
get_repo_fetch_hook <- function(key, namespace) {
  pth <- file.path(
    "/tmp/repos",
    paste(key, ids::proquint(1, 1), sep = "-")
  )
  dir.create(pth, recursive = TRUE)
  pth
}


#' Fetch a repo and store it in a local temporary directory.
#'
#' If the repository does not exist, it will be downloaded. If it does exist,
#' then the repository will be fetched from storr memory. Note that
#' 
#' @param owner the repository owner
#' @param repo the repository name
#' @param provider one of "github" or "github-ssh".
#' @param path unused
#'
#' @return the path to the git repository
#' @noRd
get_repo <- function(owner, repo, provider = "github",
                     path = "/tmp/repos") {
  url <- git_url(owner, repo, provider)

  # create a storr object to reference git2r repository objects
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

#' Generate a data frame of commits from a list of repositories
#'
#' @param repos a list of "git_repository" objects from {git2r}.
#' 
#' @return a data frame with commits in rows and the following columns:
#'   - sha a hash of a given commit
#'   - name the author name
#'   - email the author email
#'   - repo the name of the repository
#'
#' @note This function appears to be superseded by [extract_shortlog_history()]
#'
#' @noRd
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


#' Extract the git history from the shortlog
#'
#' @param repos a named list of "git_repository" objects from {git2r}.
#' @param since a date string in ISO8601 format to define the date since the
#'   last release
#' @return a data frame with commits in rows and the following columns:
#'   - sha a hash of a given commit
#'   - name the author name
#'   - email the author email
#'   - repo the _type_ of repository, if run via [get_origin_repo()] (see note)
#' 
#' @note The names of the `repos` list have meaning when generating the zenodo
#'   JSON file: 
#'   - "main" for the lesson repository to credit, 
#'   - "source" for the originating repository if "main" is a translation,
#'   - "template" for the lesson template repository, depending on translation
#'
#' @noRd
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

#' Make a copy of the master mailmap stored in local version of chisel
#'
#' @param repo_path path to a local copy of a lesson
#' @param mailmap path to the master mailmap file (in chisel)
#'
#' @note this is used to identify duplicate names in git commits and filter out
#'   anyone who does not consent to publication in AMY. The main mailmap file is
#'   kept private due to privacy concerns.
#' 
#' @seealso the mailmap docs here <https://git-scm.com/docs/gitmailmap>
#' @return TRUE if the copy was successful, FALSE if otherwise
#' @noRd
copy_master_mailmap <- function(repo_path,
                                mailmap = system.file("mailmap/mailmap.txt", package = "chisel")) {

  # The mailmap copy in this repository should point to the email address used
  # in AMY by the user, so we can match to name + ORCID

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

#' Retrieve the shortlog for a list of repositories
#'
#' @inheritParams get_origin_repo
#' @return a data frame with the output of [extract_shortlog_history()]
get_repo_shortlog <- function(repo_list, since = NULL) {
  repo_list %>%
    purrr::pmap(function(owner, repo, ...) {
      get_repo(owner, repo)
    }) %>%
    rlang::set_names(repo_list$name) %>%
    extract_shortlog_history(since = since)
}

#' Get a table of commits for lesson material
#'
#' @importFrom tibble tibble
#' @param repo_list a data frame that has three columns:
#'   - name the type of repository. "main" for the repository to credit, 
#'        "source" for the originating repository if "main" is a translation,
#'        "template" for the lesson template repository, depending on translation
#'   - owner the github owner name
#'   - repo the repository name
#' @param mail_ignore should be a 1 column tibble named email e.g.:
#' mail_ignore = tibble::tibble(
#'   email = c(
#'     "ebecker@carpentries.org",
#'    "francois.michonneau@gmail.com")
#' )
#' @param since a date string in ISO 8601 format as a start date to gather 
#'   commits.
#' @return a data frame with unique contributors for a lesson with four columns:
#'   - name: the author name
#'   - email: the author email
#'   - repo: the _type_ of the repository (names from `repo_list`)
#'   - n: the number of commits by this author
#' @seealso [generate_zenodo_json()] which uses [get_lesson_creators()], which
#'   uses this function. 
#' @noRd
get_origin_repo <- function(repo_list,
                            mail_ignore = NULL,
                            since = NULL) {

  stopifnot("main" %in% repo_list$name)

  # `res` is a data frame with four columns. It is important to note that the
  # data frame produced here is _not_ a transformation of the `repo_list` data
  # frame, even though they both contain columns called "name" and "repo".
  # `res` will contain four columns generated by [extract_shortlog_history()]:
  #
  # - sha a hash of a given commit
  # - name the author name
  # - email the author email
  # - repo the _type_ of repository ("main", "source", or "template")
  #
  res <- get_repo_shortlog(repo_list, since)

  if (!is.null(mail_ignore)) {
    res <- dplyr::filter(res, !(.data$email %in% mail_ignore$email &
                                  .data$repo == "main"))
  }
  # split rows of the data frame by repository, creating a list of data frames
  # for each of the repositories represented
  res_split <- split(res, res$repo)

  # The final list to count contributions
  .r <- vector("list", length(res_split))

  i_split <- seq_along(res_split)
  for (i in i_split) {
    # Filtering step: we want to get the commits that are UNIQUE to each source
    focus_src <- res_split[[i]]
    other_src <- dplyr::bind_rows(res_split[-i])
    # The anti-join here should produce commits distinctly produced in the
    # lesson and not those from styles. We assume here that the lessons are 
    # updated so that the steps of the loop with styles result in data frames
    # with no rows. 
    focus_src <- dplyr::anti_join(focus_src, other_src, by = "sha")
    # Aggregate the number of commits from each person by email, sorted.
    # NOTE: ZNK: I would like to add a way to collapse the commits into a list
    # column so that I can query them easily
    .r[[i]] <- dplyr::count(focus_src, .data$name, .data$email, sort = TRUE)
  }

  # return a data frame that has all the contributors to the lesson labelled
  # with "main". This works because we have sorted the data and distinct() will
  # keep the first match, which means that if someone has made a contribution to
  # both styles and the lesson, they will still get credit for contributing to
  # the lesson.
  #
  # We have a list of data frames with two columns:
  #
  # - name the author name
  # - email the author email
  dplyr::bind_rows(.r) %>%
    dplyr::distinct(.data$email, .keep_all = TRUE)
}


# INTERACTIVE PART {{{-------------------------------------------------------------

#' Lesson releases from the past
#'
#' This section (never evaluated in the package) contains R code that can be run
#' interactively to produce zenodo JSON files for repositories.
if (FALSE) {

  # Git novice ES release
  res <- tibble::tribble(
    ~name,      ~owner,        ~repo,
    "main",     "swcarpentry", "git-novice-es",
    "source",   "swcarpentry", "git-novice",
    "template", "swcarpentry", "styles-es"
  ) %>%
    generate_zenodo_json(local_path = "~/git/git-novice-es/",
      editors = c("Rayna M Harris"))

  # Shell novice ES release
  res <-  tibble::tribble(
    ~name,      ~owner,        ~repo,
    "main",     "swcarpentry", "shell-novice-es",
    "source",   "swcarpentry", "shell-novice",
    "template", "swcarpentry", "styles-es"
  ) %>%
    generate_zenodo_json(local_path = "~/git/shell-novice-es/",
      editors = c("Heladia Saldago"))

  # R novice gapminder ES release
  res <-  tibble::tribble(
    ~name,      ~owner,        ~repo,
    "main",     "swcarpentry", "r-novice-gapminder-es",
    "source",   "swcarpentry", "r-novice-gapminder",
    "template", "swcarpentry", "styles-es"
  ) %>%
    generate_zenodo_json(local_path = "~/git/r-novice-gapminder-es/",
      editors = c("Rayna Harris", "Verónica Jiménez",
        "Silvana Pereyra", "Heladia Salgado"))

  # python ecology ES release (2019-01-09)
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



  # openrefine social sciences
  res <- tibble::tribble(
    ~name, ~owner, ~repo,
    "main", "datacarpentry", "openrefine-socialsci",
    "template", "swcarpentry", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/openrefine-socialsci/",
      editors = c("Geoff LaFlair", "Peter Smyth"))

  # spreadsheets social sciences
  res <- tibble::tribble(
    ~name, ~owner, ~repo,
    "main", "datacarpentry", "spreadsheets-socialsci",
    "template", "swcarpentry", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/spreadsheets-socialsci/",
      editors = c("Chris Prener", "Peter Smyth"))

  # R social sciences
  res <- tibble::tribble(
    ~name, ~owner, ~repo,
    "main", "datacarpentry", "r-socialsci",
    "template", "swcarpentry", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/r-socialsci/",
      editors = c("Juan Fung", "Peter Smyth"))

  # Social sciences workshop
  res <- tibble::tribble(
    ~name, ~owner, ~repo,
    "main", "datacarpentry", "socialsci-workshop",
    "template", "swcarpentry", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/socialsci-workshop/",
      editors = c("Stephen Childs", "Juan Fung",
        "Geoff LaFlair", "Rachel Gibson",
        "Chris Prener", "Peter Smyth"))

  # R r-intro geospatial
  res <- tibble::tribble(
    ~ name, ~owner,  ~repo,
    "main", "datacarpentry", "r-intro-geospatial",
    "source", "swcarpentry", "r-novice-gapminder",
    "template", "carpentries", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/geospatial-lessons/r-intro-geospatial/",
      editors = c("Janani Selvaraj", "Lachlan Deer",
        "Juan Fung"))

  # Organization geospatial
  res <- tibble::tribble(
    ~ name, ~owner,  ~repo,
    "main", "datacarpentry", "organization-geospatial",
    "template", "carpentries", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/git/geospatial-lessons/organization-geospatial/",
      editors = c("Tyson Swetnam", "Chris Prener"),
      ignore = c("neondataskills@neoninc.org",
        "francois.michonneau@gmail.com"))

  # Geospatial workshop
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

  # R-raster-vector
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


  # Release of DC image-processing -- 2023-01-25
  # NOTE: this was a challenging one because it has a hybrid history where it
    # started out using styles, and then switched to use remote themes in late
    # 2019. On 2021-08-17, styles was once again merged into the repo:
    # https://github.com/datacarpentry/image-processing/pull/160
    # I believe Toby had successfully resolved the conflicts to have the styling
    # removed, but it still kept the history.
    #
    # Because the processing also can take a repository that _should_ be
    # considered in the contribution count (in terms of translation 
    # repositories), the commits since 2021-08-17 were mixed in and the zenodo
    # json file had three extra contributors. 
  res <- tibble::tribble(
    ~name,  ~owner, ~repo,
    "main", "datacarpentry", "image-processing",
    "template", "carpentries", "styles"
  ) %>%
    generate_zenodo_json(local_path = "~/Documents/Carpentries/Git/datacarpentry/image-processing",
      editors = c("tobyhodges", "uschille", "bobturneruk", "quist00", "K-Meech"),
      ignore = c("francois.michonneau@gmail.com", "zkamvar@carpentries.org",
        # These are additional styles contributions to ignore
        "renato.alves@embl.de", "ashwinvis+carp@protonmail.com", "smb@sarahmbrown.org"))

  # R-raster-vector -- 2023-01-26
  {
  rrvg <- tibble::tribble(
    ~name,  ~owner, ~repo,
    "main", "datacarpentry", "r-raster-vector-geospatial",
    "template", "carpentries", "styles"
  ) 
  res <- generate_zenodo_json(rrvg, 
    local_path = "~/Documents/Carpentries/Git/datacarpentry/r-raster-vector-geospatial",
    since = "2019-07-27",
    editors = c("jsta", "drakeasberry", "arreyves"),
    ignore = c("francois.michonneau@gmail.com", "zkamvar@carpentries.org", "tbyhdgs@gmail.com")
  )
  }
  # R-socialsci -- 2023-01-26
  {
  rss <- tibble::tribble(
    ~name,  ~owner, ~repo,
    "main", "datacarpentry", "r-socialsci",
    "template", "carpentries", "styles"
  ) 
  res <- generate_zenodo_json(rss, 
    local_path = "~/Documents/Carpentries/Git/datacarpentry/r-socialsci",
    since = "2019-07-02",
    editors = c("juanfung", "eirini-zormpa", "jessesadler"),
    ignore = c("francois.michonneau@gmail.com", "zkamvar@carpentries.org", "tbyhdgs@gmail.com")
  )
  }
  # python-ecology-lesson-es -- 2023-01-27
  {
  pels <- tibble::tribble(
    ~name,  ~owner, ~repo,
    "main", "datacarpentry", "python-ecology-lesson-es",
    # NOTE: 2023-01-27, ZNK: 
    # I am commenting this one out because I do not know how much influence the
    # english lesson has had on the spanish lesson since 2019. The english
    # source files in the spanish repository have not changed for four years,
    # so I am thinking it would be better to assign authorship just to the
    # contributions to the spanish repository. --------------------------------
    # "english", "datacarpentry", "python-ecology-lesson", 
    "template", "carpentries", "styles",
    "template-es", "carpentries", "styles-es"
  ) 
  res <- generate_zenodo_json(pels, 
    local_path = "~/Documents/Carpentries/Git/datacarpentry/python-ecology-lesson-es",
    since = "2019-01-09",
    editors = c("iramosp", "aguspesce", "vinisalazar"),
    ignore = c("francois.michonneau@gmail.com", "zkamvar@carpentries.org", "tbyhdgs@gmail.com")
  )
  }
  # instructor-training -- 2023-01-27
  {
  cit <- tibble::tribble(
    ~name,  ~owner, ~repo,
    "main", "carpentries", "instructor-training",
    "template", "carpentries", "styles"
  ) 
  res <- generate_zenodo_json(cit, 
    local_path = "~/Documents/Carpentries/Git/carpentries/instructor-training",
    since = "2021-11-17",
    editors = c("brownsarahm", "jt14den", "dpshelio", "ndporter",
      "jonathanwheeler01", "karenword"),
    ignore = c("francois.michonneau@gmail.com", "zkamvar@carpentries.org", "tbyhdgs@gmail.com")
  )
  }


}
# INTERACTIVE PART }}}----------------------------------------------------------

#' Determine the publication name for a data frame of creators
#'
#' This will process data generated by [get_origin_repo()] joined with 
#' [all_people()]. It will add someone to a lesson publication if the following
#' criteria have been met:
#'
#' 1. They have at least one commit in the lesson repository
#' 2. They have not revoked consent to publish their name in lesson publications
#'
#' @note if the person does not have an AMY profile, we will publish the name
#'   associated with their git commit. This could lead to duplicates.
#'
#' @param .data a data frame with at least the following columns:
#'   - name OR person_name_with_middle: the author name
#'   - email: the author email
#'   - lesson_publication_consent: the name of the repository
#'   - github: the author github username
#'   - orcid: the author orcid ID
#' 
#' @return the above data frame with the column `pub_name` modified.
#' 
#' @seealso [all_people()], [get_lesson_creators()] which uses this function
#'   internally.
#' @noRd
add_pub_name <- function(.data) {

  # for when calling add_pub_name for list of editors using their GitHub
  if (!exists("name", .data)) {
    .data <- .data %>%
      mutate(name = person_name_with_middle)
  }

  .data %>%
    dplyr::mutate(pub_name = dplyr::case_when(
      # default on AMY profile info
      # first use profile info if user specified it's what they wanted
      lesson_publication_consent == "amy" |
        lesson_publication_consent == "unset" ~ person_name_with_middle,
      # then orcid info
      lesson_publication_consent == "orcid" &
        is_valid_orcid(clean_up_orcid(orcid)) ~ get_orcid_name(clean_up_orcid(orcid)),
      # then github (just return GitHub username)
      # 2023-01-27, ZNK: AMY's github consent is for the _handle_ not the name.
      lesson_publication_consent == "github" ~  github, #get_github_name(github),
      # if all else fails, use git info
      TRUE ~ name
    )) %>%
    dplyr::mutate(
      pub_name = gsub("\\s+", " ", pub_name)
    )

}

#' Get a data frame of creators for a given repository
#'
#' This will fetch the data of lesson creators for a given repository and then
#' attempt to match the emails to those in our AMY database. If it can not match
#' emails, it will attempt to do so by github username and append the master
#' mailmap file.
#'
#' @param repos a data frame that has three columns:
#'   - name the type of repository. "main" for the repository to credit, 
#'        "source" for the originating repository if "main" is a translation,
#'        "template" for the lesson template repository, depending on translation
#'   - owner the github owner name
#'   - repo the repository name
#' @param since a date string in ISO 8601 format as a start date to gather 
#'   commits.
#' @return a data frame with the same columns as [all_people()] plus the output
#'   of [get_origin_repo()] joined by "email":
#'   - name: the author name
#'   - repo: the _type_ of the repository ("main", "source", or "template")
#'   - n: the number of commits by this author
#' @noRd
#' @seealso [generate_zenodo_json()], [append_master_mailmap()]
get_lesson_creators <- function(repos, since = NULL) {
  # Get the filtered shortlog data with name, repository type, and number of
  # commits.
  creators_df <- get_origin_repo(repos, since = since)
  print(creators_df)
  AP <- all_people()
  creators <- creators_df |>
    dplyr::left_join(AP, by = c("email"), na_matches = "never")
  # check if there are any emails that were not auto-joined. This would be
  # indicated by a github-style email
  github_emails <- git_user_from_email(creators$email)
  if (any(!is.na(github_emails))) {
    creators_df$github <- github_emails
    # when we have github style emails, then we need to append the mailmap to 
    # make sure they are disambiguated
    creators_by_github <- creators_df |>
      dplyr::rename(github_email = "email") |> 
      dplyr::filter(!is.na(.data$github)) |> 
      dplyr::left_join(AP, by = c("github"), na_matches = "never") |>
      append_master_mailmap()
    # now that the master mailmap is appended, we can get to work
    creators_df <- get_origin_repo(repos, since = since)
    creators <- creators_df |>
      dplyr::left_join(AP, by = c("email"), na_matches = "never")
  }
  # join on email (note this depends on 
  add_pub_name(creators)
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
  # The zero width space unicode character
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

#' Generate a JSON file compatible with Zenodo to record lesson releases 
#' 
#' @param repos a data frame that has three columns:
#'   - name the type of repository. "main" for the repository to credit, 
#'        "source" for the originating repository if "main" is a translation,
#'        "template" for the lesson template repository, depending on translation
#'   - owner the github owner name
#'   - repo the repository name
#' @param local_path path to the local version of the repository
#' @param editors_github the github username of the editors as it appears in AMY
#' @param since a date string in ISO 8601 format as a start date to gather 
#'   commits.
#'
#' @note This is the main driver function for generating the ZENODO json file. 
#'   I think one thing that is not immediately clear is that "editors" in 
#'   zenodo terminology is the equivalent of our "maintainers". 
#'
#' @export
generate_zenodo_json <- function(repos, local_path, editors_github,
                                 since = NULL,
                                 ignore = character(0)) {

  gives_consent <- function(consent_var) {
    is.na(consent_var) | consent_var != "no"
  }

  creators_df <- get_lesson_creators(repos, since = since) %>%
    dplyr::filter(gives_consent(lesson_publication_consent)) %>%
    dplyr::anti_join(tibble::tibble(email = ignore), by = "email") 

  creators  <- creators_df %>%
    dplyr::select(.data$pub_name, .data$orcid) %>%
    purrr::pmap(function(pub_name, orcid) {
      this_orcid <- clean_up_orcid(orcid)
      if (is_valid_orcid(this_orcid)) {
        return(list(name = pub_name, orcid = this_orcid))
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

  # typ <- list(resource_type = list(title = "Lesson", type = "lesson"))

  res <- c(eds, creators, lic) #, typ)
  json <- jsonlite::toJSON(res, auto_unbox = TRUE, pretty = TRUE)
  cat(json, file = file.path(local_path, ".zenodo.json"))

  list(data = creators_df, json = json)
}



#' @importFrom utils as.person bibentry personList
generate_citation <- function(authors = "AUTHORS",
                              editors,
                              doi = "10.5281/zenodo.569338",
                              url = "https://datacarpentry.org/R-ecology-lesson/",
                              title = "Data Carpentry: R for data analysis and visualization of Ecological Data") {

  stopifnot(inherits(editors, "person"))

  aut <- readLines(authors)

  # remove first line
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

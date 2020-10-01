##' @importFrom colorspace hex2RGB
font_color <- function(hexcode) {
    rgb <- colorspace::hex2RGB(hexcode)
    rgbR <- rgb@coords[, "R"]
    rgbG <- rgb@coords[, "G"]
    rgbB <- rgb@coords[, "B"]
    luma <- ((0.299 * rgbR) + (0.587 * rgbG) + (0.114 * rgbB))
    res <- rep("#ffffff", length(hexcode))
    res[luma > .5] <- "#222222"
    res
}


##' Creates an HTML formatted table or list to be included in the Carpentry
##' handbook or other resources
##'
##' @details \code{summarize_github_labels} creates a table, while
##'     \code{document_github_labels} creates a list.
##'
##' See [create_github_labels()] for a description of the CSV file format
##' expected by this function.
##'
##' @title Summary of GitHub labels from CSV file
##' @param label_csv the path to a CSV file that describes the labels. It should
##'     have the following columns: print_order, type, label, color, use_prefix,
##'     description, long_description. The canonical version of this file lives
##'     in The Carpentries repository's
##'     (\url{https://github.com/carpentries/handbook/}) \code{data/} folder.
##' @param format format of the output table (passed to \code{kable}).
##' @param escape should the HTML be escaped (passed to \code{kable}).
##' @param ... additional arguments to be passed to `kable`.
##' @return An HTML formatted table that shows the type of label, its color and
##'     its description.
##' @export
##' @md
##' @importFrom readr read_csv
##' @importFrom dplyr mutate %>%
##' @importFrom kableExtra cell_spec
##' @importFrom knitr kable
##' @importFrom rlang .data
summarize_github_labels <- function(label_csv, format = "html", escape = FALSE,
                                    ...) {
  readr::read_csv(label_csv) %>%
    dplyr::mutate(prefix = dplyr::case_when(
      type == "status" & use_prefix ~ "status:",
      type == "type"   & use_prefix ~ "type:",
      TRUE ~ ""),
      label = paste0(.data$prefix, .data$label)) %>%
    dplyr::arrange(.data$label) %>%
    dplyr::mutate(color = kableExtra::cell_spec(
      .data$color,
      background = .data$color,
      monospace = TRUE,
      color = font_color(.data$color)
    )) %>%
    dplyr::select(.data$label, .data$color, .data$description,
      .data$long_description) %>%
    knitr::kable(format = format, escape = escape, ...) %>%
    kableExtra::kable_styling("bordered", font_size = ".85em")
}

##' @rdname summarize_github_labels
##' @param out path to file to write the HTML file documenting the labels
##' @export
##' @importFrom readr read_csv
##' @importFrom dplyr mutate arrange case_when
##' @importFrom purrr pmap
##' @importFrom glue glue_collapse
document_github_labels <- function(label_csv, out = NULL) {
  render_one_label <- function(label, color, description, long_description, type, print_header, ...) {
    text_color <- font_color(color)
    if (print_header)
      hdr <- glue::glue("<h3>\"{hdr}\" labels</h3>\n", hdr = unique(type)[[1]])
    else hdr <- character(0)

    paste(hdr,
      "<ul>",
      glue::glue(
        '<li><span style="font-family: monospace; font-weight: bold; font-size: 1.2em; color: {text_color}; background-color: {color}; border-radius: 4px; padding: 4px;">{label}</span>
                     <ul>
                       <li><b>Hex code:</b> {color}</li>
                       <li><b>Short Description:</b> {description} </li>
                       <li><b>Long Description:</b> {long_description} </li>
                    </ul>
                   </li>'
      ),
      "</ul>",
      sep = "\n"
    )
  }

  res <- readr::read_csv(label_csv) %>%
    dplyr::mutate(prefix = dplyr::case_when(
      type == "status" & use_prefix ~ "status:",
      type == "type" & use_prefix ~ "type:",
      TRUE ~ ""),
      label = paste0(.data$prefix, .data$label)) %>%
    dplyr::arrange(.data$print_order) %>%
    dplyr::group_by(.data$type) %>%
    dplyr::mutate(print_header = .data$print_order == min(.data$print_order)) %>%
    purrr::pmap(render_one_label)

  res <- glue::glue_collapse(res, sep = "")

  if (!is.null(out)) {
    cat(res, file = out)
  } else {
    res
  }

}

pythonize_github_labels <- function(label_csv) {

  labels <- readr::read_csv(label_csv) %>%
    dplyr::mutate(prefix = dplyr::case_when(
      type == "status" & use_prefix ~ "status:",
      type == "type" & use_prefix ~ "type:",
      TRUE ~ ""),
      label = paste0(.data$prefix, .data$label))

  cat(
    "EXPECTED = {",
    glue::glue_collapse(
      glue::glue_data(labels, "    '{label}' : '{color}'",
        color = tolower(gsub("#", "", .data$color))),
      sep = ", \n",
      ),
    "}",
    sep = "\n")

}


##' Create, from a CSV file, labels for issues and pull requests on GitHub.
##'
##' If `delete_previous` is set to `TRUE`, any label that were present
##' in the repository (and that do not match one of the labels specified in the
##' CSV file) will be deleted.
##'
##' Labels that already exists (with the same name as the one provided in the
##' CSV file) will be updated to match the specification of the spreadsheet.
##'
##' The CSV file is expected to have the following columns:
##'
##' - `print_order`: order in which the labels should be displayed (used by
##' [document_github_labels()]).
##' - `type`: label type. Used to categorize the tags.
##' - `label`: the name to be used for the label.
##' - `color`: the hexadecimal value for the background color of the label.
##' - `prefix`: boolean (`TRUE`/`FALSE`) to indicate whether the content of the
##'   `type` column should be used as a prefix for the label. If `type` is
##'   `status`, `label` is `in progress`,  and `prefix` is `TRUE`, then the
##'   label will appear as `status: in progress` on GitHub.
##' - `description`: the short description that will be displayed on GitHub next
##'   to the label.
##' - `long description`: a long description to be used by
##'    [document_github_labels()].
##'
##' An example of the CSV file is available on the Carpentries handbook
##' [repository](https://github.com/carpentries/handbook/blob/925720f80725b5fef11fa3f1535cea364f179818/data/github_labels.csv).
##'
##' @title Create GitHub labels for issues and PR from CSV file
##' @param label_csv the path to the CSV file with the labels. It should have
##'   the following columns: type, label, color, description, long_description.
##' @param owner the owner of the GitHub repository
##' @param repo the name of the GitHub repository where to create the labels
##' @param delete_previous should already existing labels in the repository be
##'   deleted? (boolean)
##' @return \code{TRUE} invisibly
##' @export
##' @md
##' @importFrom readr read_csv
##' @importFrom dplyr mutate %>% case_when
##' @importFrom gh gh
##' @importFrom purrr pmap map_chr map
##' @importFrom rlang .data
create_github_labels <- function(label_csv, owner, repo,
                                 delete_previous = FALSE) {

  lbl <- readr::read_csv(label_csv) %>%
    dplyr::mutate(
      prefix = dplyr::case_when(
        type == "status" & use_prefix ~ "status:",
        type == "type" & use_prefix ~ "type:",
        TRUE ~ ""),
      label = paste0(.data$prefix, .data$label))

  res <- purrr::pmap(lbl,
    function(type, label, color, description, long_description, ...) {
      color <- gsub("^#", "", color)

      ## If label already exists, update it
      lbl_exists <- try(
        gh::gh("GET /repos/:owner/:repo/labels/:name",
          owner = owner, repo = repo,
          name = label),
        silent = TRUE
      )

      if (!inherits(lbl_exists, "try-error")) {
        cmd <- "PATCH /repos/:owner/:repo/labels/:name"
      } else {
        cmd <- "POST /repos/:owner/:repo/labels"
      }

      .res <- gh::gh(
        cmd, owner = owner, repo = repo,
        name = label, color = color, description = description,
        .send_headers = c("Accept" = "application/vnd.github.symmetra-preview+json"))
      .res
  })

  if (delete_previous) {
    all_lbls_raw <- gh::gh("GET /repos/:owner/:repo/labels",
      owner = owner, repo = repo)
    all_lbls <- purrr::map_chr(all_lbls_raw, "name")
    to_rm <- setdiff(all_lbls, lbl$label)
    message("Deleting: ", paste(to_rm, collapse = ", "))
    .res <- purrr::map(to_rm, function(x) {
      gh::gh("DELETE /repos/:owner/:repo/labels/:name",
        owner = owner, repo = repo, name = x)
    })
  }
  invisible(TRUE)
}


##' @importFrom utils URLencode
update_github_labels <- function(label_csv, match_table,
                                 owner, repo) {

    new_labels <- readr::read_csv(label_csv)
    match_labels <- readr::read_csv(match_table) %>%
        dplyr::filter(!skip)

    ## To update
  to_update <- dplyr::left_join(
    match_labels, new_labels, by = c("new_label" = "label")) %>%
    dplyr::filter(new_label != "delete") %>%
    dplyr::mutate(prefix = dplyr::case_when(
      type == "status"  & use_prefix ~ "status:",
      type == "type" & use_prefix ~ "type:",
      TRUE ~ ""),
      new_label = paste0(.data$prefix, .data$new_label))

  res_update <- purrr::pmap(to_update,
    function(old_label, new_label, type, color, use_prefix,
             description, ...) {
        message(old_label, " --> ", new_label)
        color <- gsub("^#", "", color)

        ## If label already exists, update it
        lbl_exists <- try(
          gh::gh("GET /repos/:owner/:repo/labels/:name",
            owner = owner, repo = repo,
            name = old_label),
            silent = TRUE
        )

        if (!inherits(lbl_exists, "try-error")) {
            if (identical(new_label, old_label)) {
              .res <- gh::gh(
                "PATCH /repos/:owner/:repo/labels/:name",
                owner = owner, repo = repo,
                name = old_label, color = color, description = description,
                .send_headers = c("Accept" = "application/vnd.github.symmetra-preview+json"))

            } else {
              cmd <- paste(
                "PATCH", utils::URLencode(
                  glue::glue("/repos/{owner}/{repo}/labels/{name}",
                    owner = owner, repo = repo,
                    name = old_label)))

              .res <- gh::gh(
                cmd, name = new_label, color = color,
                description = description,
                .send_headers = c("Accept" = "application/vnd.github.symmetra-preview+json"))
            }
        } else {
            warning(old_label, ": doesn't exist!")
            return(NULL)
        }

        .res

    })

  res_update

}


##' List and delete all GitHub labels from a repository.
##'
##' @title List and delete GitHub labels
##' @param owner the owner of the GitHub repository
##' @param repo the name of the GitHub repository
##' @rdname list_github_labels
##' @export
list_all_github_labels <- function(owner, repo) {
  gh::gh("GET /repos/:owner/:repo/labels",
    owner = owner, repo = repo,
    .send_headers = c("Accept" = "application/vnd.github.symmetra-preview+json")) %>%
    purrr::map_df(~ list(label = .x[["name"]]))
}

##' @rdname list_github_labels
##' @export
delete_all_github_labels <- function(owner, repo) {
  all_lbls_raw <- gh::gh("GET /repos/:owner/:repo/labels",
    owner = owner, repo = repo)
  purrr::map_chr(all_lbls_raw, "name") %>%
    purrr::map(function(x) {
      gh::gh("DELETE /repos/:owner/:repo/labels/:name",
        owner = owner, repo = repo, name = x)
    })
  invisible(TRUE)
}


non_standard_github_labels <- function(labels_csv, owner, repo) {
  all_lbls <- list_all_github_labels(owner, repo)
  std_labels <- readr::read_csv(labels_csv) %>%
        dplyr::mutate(prefix = dplyr::case_when(type == "status"  & use_prefix ~ "status:",
                                                type == "type" & use_prefix ~ "type:",
                                                TRUE ~ ""),
                      label = paste0(.data$prefix, .data$label))

    dplyr::anti_join(all_lbls, std_labels)

}




if (FALSE) {

    ## list of repos for pilot
  pilot_repos <- tibble::tribble(
    ~owner,      ~repo,
    "datacarpentry", "R-ecology-lesson",
    "swcarpentry", "python-novice-gapminder",
    "swcarpentry", "r-novice-gapminder",
    "swcarpentry", "git-novice"
                           )

    es_repos <- tibble::tribble(
                            ~owner, ~repo,
                            "swcarpentry", "git-novice-es")

    ## instructor training:
    ##
    ## create_github_labels("~/git/carpentry-handbook/data/github_labels.csv",
    ##                      owner="carpentries", repo="instructor-training")



}

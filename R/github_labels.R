##' Creates an HTML formatted table to be included in the Carpentry handbook or other resources
##'
##' @title Summary table of GitHub labels from CSV file
##' @param label_csv the path to a CSV file that describes the labels. It should
##'     have the following columns: type, label, color, description,
##'     long_description.
##' @param format format of the output table (passed to \code{kable}).
##' @param escape should the HTML be escaped (passed to \code{kable}).
##' @param ... additional arguments to be passed to `kable`.
##' @return An HTML formatted table that shows the type of label, its color and
##'     its description.
##' @export
##' @importFrom readr read_csv
##' @importFrom dplyr mutate %>%
##' @importFrom kableExtra cell_spec
##' @importFrom knitr kable
##' @importFrom rlang .data
summarize_github_labels <- function(label_csv, format = "html", escape = FALSE,
                                    ...) {

    label_csv %>%
        readr::read_csv() %>%
        dplyr::mutate(color = kableExtra::cell_spec(
                                              .data$color, background = .data$color
                                          )) %>%
        knitr::kable(format = format, escape = escape, ...)
}


##' Labels that already exists (with the same name as the one provided in the
##' CSV file) will be updated to match the specification of the spreadsheet.
##'
##' If \code{delete_previous} is set to \code{TRUE}, any label that were present
##' in the repository (and that do not match one of the labels specified in the
##' CSV file) will be deleted.
##'
##' @title Create GitHub labels for issues and PR from CSV file
##' @param label_csv the path to the CSV file with the labels. It should have
##'     the following columns: type, label, color, description, long_description.
##' @param owner the owner of the GitHub repository
##' @param repo the name of the GitHub repository where to create the labels
##' @param delete_previous should already existing labels in the repository be deleted? (boolean)
##' @return \code{TRUE} invisibly
##' @export
##' @importFrom readr read_csv
##' @importFrom dplyr mutate %>% case_when
##' @importFrom gh gh
##' @importFrom purrr pmap map_chr map
##' @importFrom rlang .data
create_github_labels <- function(label_csv, owner = "fmichonneau",
                                 repo = "test-repo-labels",
                                 delete_previous = TRUE) {


    lbl <- readr::read_csv(label_csv) %>%
        dplyr::mutate(prefix = dplyr::case_when(type == "status" ~ "status:",
                                                type == "type" ~ "type:",
                                                TRUE ~ ""),
                      label = paste0(.data$prefix, .data$label))


    res <- purrr::pmap(lbl, function(type, label, color, description, long_description, ...) {
        color <- gsub("^#", "", color)

        ## If label already exists, update it
        lbl_exists <- try(
            gh::gh("GET /repos/:owner/:repo/labels/:name", owner = owner, repo = repo,
                   name = label),
            silent = TRUE
        )

        if (!inherits(lbl_exists, "try-error")) {
            cmd <- "PATCH /repos/:owner/:repo/labels/:name"
        } else {
            cmd <- "POST /repos/:owner/:repo/labels"
        }

        .res <- gh::gh(cmd, owner = owner, repo = repo,
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

##' Opens one issue for each file found in a GitHub repository
##'
##' @title One issue per file
##' @param owner user or organization who owns the repository
##' @param repo name of the repository
##' @param path subfolder to look for files
##' @param pattern regular expression to select the files
##' @param issue_title string to use for the title of the issue
##' @param issue_body string to use for the content of the issue
##' @param ... additional parameters to be passed to `gh`
##' @importFrom purrr walk2
##' @importFrom glue glue
##' @export
##' @md
one_issue_per_file <- function(owner = "fmichonneau", repo = "git-novice-es",
                               path = "_episodes", pattern = "md$",
                               issue_title = "Review translation of {name}",
                               issue_body = "Review the translation of [{path}]({html_url}) \n Due date: 2018-03-05",
                               ...) {

    files <- gh_list_files(owner = owner, repo = repo, path = path, ...)
    files <- files[grepl(pattern, files$name), ]

    titles <- glue::glue(issue_title, name = files$name)
    bodies <- glue::glue(issue_body, path = files$path, html_url = files$html_url)

    res <- purrr::walk2(titles, bodies,
                        function(x, y) {
                            gh("POST /repos/:owner/:repo/issues",
                               owner = owner, repo = repo,
                               title = x, body = y, ...)
                        })

    res
}

##' Gets the list of files from a GitHub repository.
##'
##' By default, it only returns the files in the root of the repository. You can
##' specify a `path` to get the content of a sub-folder. There is no way to get
##' all content from a repository at this stage.
##'
##' `gh_list_files` returns the name of the file, its path, and the URL for the
##' HTML rendering of the file.
##' @title GitHub files
##' @param owner user or organization for the repository
##' @param repo name of the repository
##' @param path optional sub-folder within the repository
##' @param ... additional parameters to be passed to `gh` (e.g. `.token`)
##' @return
##'   * `gh_list_files_raw` returns a list (default from `gh`).
##'   * `gh_list_files` returns a tibble.
##' @export
##' @importFrom gh gh
##' @rdname gh_list_files
##' @md
gh_list_files_raw <- function(owner, repo, path = NULL, ...) {
    endpoint <- "GET /repos/:owner/:repo/contents"

    if (!is.null(path)) {
        endpoint <- paste0(endpoint, "/:path")
        gh::gh(endpoint, owner = owner, repo = repo, path = path, ...)
    } else {
        gh::gh(endpoint, owner = owner, repo = repo, ...)
    }

}


##' @export
##' @importFrom purrr map_df
##' @rdname gh_list_files
gh_list_files <- function(owner, repo, path = NULL, ...) {
    res <- gh_list_files_raw(owner, repo, path, ...)
    purrr::map_df(res, ~ list(name = .[["name"]], path = .[["path"]], html_url = .[["html_url"]]))
}

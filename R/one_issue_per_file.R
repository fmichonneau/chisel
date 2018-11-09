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
one_issue_per_file <- function(owner = "carpentries-es", repo = "python-ecology-lesson-es",
                               path = "_episodes", pattern = "md$",
                               issue_title = "Translation of {name}",
                               issue_body = " :+1: :tada: ¡En primer lugar, gracias por tomarte el tiempo para contribuir! :tada: :+1: \n\nÉste repositorio está siendo traducido y nos gustaría mucho que puedas contribuir. Lo siguiente es un conjunto de pautas, no reglas, que hemos desarrollado para otros proyectos de traducción y nos han sido muy útiles para tener una traducción homogénea. [Convenciones](https://github.com/Carpentries-ES/board/blob/master/Convenciones_Traduccion.md). \n\n Aquí puedes comentar sobre preguntas y progreso en la tradución de este episodio. \n \n Episodio: [{path}]({html_url}).",
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

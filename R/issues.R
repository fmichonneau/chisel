june_release <- "
If your Maintainer team has decided not to participate in the [June 2019 lesson release](https://carpentries.topicbox.com/groups/maintainers/T61c75ac1a8a89538/timeline-and-process-for-june-lesson-release), please close this issue.

To have this lesson included in the 18 June 2019 release, please confirm that the following items are true:

- [ ] Example code chunks run as expected
- [ ] Challenges / exercises run as expected
- [ ] Challenge / exercise solutions are correct
- [ ] Call out boxes (exercises, discussions, tips, etc) render correctly
- [ ] A schedule appears on the lesson homepage (e.g. not “00:00”)
- [ ] Each episode includes learning objectives
- [ ] Each episode includes questions
- [ ] Each episode includes key points
- [ ] Setup instructions are up-to-date, correct, clear, and complete
- [ ] File structure is clean (e.g. delete deprecated files, insure filenames are consistent)
- [ ] Some Instructor notes are provided
- [ ] Lesson links work as expected

When all checkboxes above are completed, this lesson will be added to the 18 June lesson release. Please leave a comment on carpentries/lesson-infrastructure#26 or contact Erin Becker with questions (ebecker@carpentries.org).
"

##' Create issues and milestones in repositories
##'
##' @title Issues and milestones
##' @param owner the owner of the GitHub repository
##' @param repo the name of the GitHub repository
##' @param title the title of the issue or milestone
##' @param body the body of the issue
##' @param ... additional parameters being passed to gh::gh
##' @importFrom gh gh
##' @rdname create_issue
##' @return JSON
##' @export
create_issue <- function(owner, repo, title, body, ...) {

    gh::gh("POST /repos/:owner/:repo/issues",
           owner = owner, repo = repo, title = title,
           body = body, labels = c("help wanted"), ...)
}

##' @export
##' @rdname create_issue
create_milestone <- function(owner, repo, title, ...) {

  gh::gh("POST /repos/:owner/:repo/milestones",
    owner = owner, repo = repo, title = title, ...)

}

if (FALSE) {
  library(tidyverse)
  repos <- readr::read_csv("/tmp/2019-04-25-lessons.csv")

  sub_repos <- repos %>%
    dplyr::filter(! life_cycle %in%  c("pre-alpha", "on hold"),
      is_official) %>%
    dplyr::rename(repo = repository)

  res1 <- sub_repos %>%
    dplyr::slice(1:2) %>%
    purrr::pmap(function(repo, owner, ...) {
      create_milestone(owner, repo,
        title = "June 2019 Release")
    })

  res2 <- sub_repos %>%
    dplyr::slice(3:nrow(sub_repos)) %>%
    purrr::pmap(function(repo, owner, ...) {
      create_milestone(owner, repo,
        title = "June 2019 Release")
    })
}

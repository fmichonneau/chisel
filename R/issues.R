update_labels_issue_body <- "
The lesson infrastructure committee unanimously [approved](https://github.com/carpentries/lesson-infrastructure/blob/master/2018-05-23-minutes.md#github-labels-to-use-across-our-lessons) the proposal of using the same [set of labels](https://docs.carpentries.org/topic_folders/maintainers/github_labels.html) across all our repositories during its last meeting on May 23rd, 2018.

This repository has now been converted to use the standard set of labels.

If this repository used the previous set of recommended labels by Software Carpentry, they have been converted to the new one using the following rules:

| SWC legacy labels   | New 'The Carpentries' labels   |
|--------------------|-----------------------------|
| bug                | type:bug                    |
| discussion         | type:discussion             |
| enhancement        | type:enhancement            |
| help-wanted        | help wanted                 |
| newcomer-friendly  | good first issue            |
| template-and-tools | type:template and tools     |
| work-in-progress   | status:in progress          |

The label `instructor-training` was removed as it is not used in the workflow of certifying new instructors anymore. The label `question` was left as is when it was in use, and removed otherwise. If your repository used custom labels (and issues were flagged with these labels), they were left as is.

The lesson infrastructure committee hopes the standard set of labels will make it easier for you to manage the issues you receive on the repositories you manage.

The lesson infrastructure committee will evaluate how the labels are being used in the next few months and we will solicit your feedback at this stage. In the meantime, if you have any questions or concerns, please leave a comment on [this issue](https://github.com/carpentries/lesson-infrastructure/issues/1).

-- The Lesson Infrastructure subcommittee

PS: we will close this issue in 30 days if there is no activity.
"


create_issue <- function(owner, repo, title, body, ...) {

    gh::gh("POST /repos/:owner/:repo/issues",
           owner = owner, repo = repo, title = title,
           body = body, ...)
}

if (FALSE) {
    repos <- readr::read_csv("/tmp/mozilla_francois0/Repositories-Grid view.csv")

    repos <- repos %>%
        filter(`Use standard labels` == "checked") %>%
        rename(owner = `GitHub org`,
               repo = Repository)

    res1 <- repos %>%
        dplyr::slice(3:4) %>%
        dplyr::select(repo, owner) %>%
        purrr::pmap(function(repo, owner, ...) {
            create_issue(owner, repo,
                         title = "Transition to standardized GitHub labels",
                         labels = list("type:template and tools"),
                         body = update_labels_issue_body)
        })

    res2 <- repos %>%
        slice(5:nrow(repos)) %>%
        dplyr::select(repo, owner) %>%
        purrr::pmap(function(repo, owner, ...) {
            create_issue(owner, repo,
                         title = "Transition to standardized GitHub labels",
                         labels = list("type:template and tools"),
                         body = update_labels_issue_body)
        })


}

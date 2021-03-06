% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github_labels.R
\name{create_github_labels}
\alias{create_github_labels}
\title{Create GitHub labels for issues and PR from CSV file}
\usage{
create_github_labels(label_csv, owner, repo, delete_previous = FALSE)
}
\arguments{
\item{label_csv}{the path to the CSV file with the labels. It should have
the following columns: type, label, color, description, long_description.}

\item{owner}{the owner of the GitHub repository}

\item{repo}{the name of the GitHub repository where to create the labels}

\item{delete_previous}{should already existing labels in the repository be
deleted? (boolean)}
}
\value{
\code{TRUE} invisibly
}
\description{
Create, from a CSV file, labels for issues and pull requests on GitHub.
}
\details{
If \code{delete_previous} is set to \code{TRUE}, any label that were present
in the repository (and that do not match one of the labels specified in the
CSV file) will be deleted.

Labels that already exists (with the same name as the one provided in the
CSV file) will be updated to match the specification of the spreadsheet.

The CSV file is expected to have the following columns:
\itemize{
\item \code{print_order}: order in which the labels should be displayed (used by
\code{\link[=document_github_labels]{document_github_labels()}}).
\item \code{type}: label type. Used to categorize the tags.
\item \code{label}: the name to be used for the label.
\item \code{color}: the hexadecimal value for the background color of the label.
\item \code{prefix}: boolean (\code{TRUE}/\code{FALSE}) to indicate whether the content of the
\code{type} column should be used as a prefix for the label. If \code{type} is
\code{status}, \code{label} is \verb{in progress},  and \code{prefix} is \code{TRUE}, then the
label will appear as \verb{status: in progress} on GitHub.
\item \code{description}: the short description that will be displayed on GitHub next
to the label.
\item \verb{long description}: a long description to be used by
\code{\link[=document_github_labels]{document_github_labels()}}.
}

An example of the CSV file is available on the Carpentries handbook
\href{https://github.com/carpentries/handbook/blob/925720f80725b5fef11fa3f1535cea364f179818/data/github_labels.csv}{repository}.
}

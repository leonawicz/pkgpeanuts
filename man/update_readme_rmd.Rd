% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pour.R
\name{update_readme_rmd}
\alias{update_readme_rmd}
\title{Update README.Rmd template}
\usage{
update_readme_rmd(repo, host = "github", public = TRUE)
}
\arguments{
\item{repo}{character, the repository name, e.g. \code{leonawicz/pkgpeanuts}.}

\item{host}{character, options are \code{"github"} or \code{"bitbucket"}.}

\item{public, }{logical, whether the remote repository is public or private.}
}
\value{
side effect of updating file.
}
\description{
Update the README.Rmd template file created by \code{usethis::use_readme_rmd}.
}
\details{
The file must exist. This function is for updating the Rmd file, not an md file.
It replaces the content in the \code{Installation} section of the original template.
Replacement content is conditional on \code{host} and whether or not \code{public} is \code{TRUE} or \code{FALSE}.
The package name is also inserted as needed, taken from \code{repo}.
}
\examples{
\dontrun{update_readme_rmd}
}

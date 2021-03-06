% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pour.R
\name{pour}
\alias{pour}
\title{Generate package scaffolding for robust package setup}
\usage{
pour(path = ".", account, name = NULL, description = NULL,
  license = c("mit", "gpl3", "apl2", "cc0"), host = "github",
  public = TRUE, testthat = TRUE, pkgdown = TRUE, appveyor = TRUE,
  travis = TRUE, codecov = TRUE, lintr = c("none", "user", "test"),
  revdep = TRUE, data_raw = TRUE, hex = TRUE, news = TRUE,
  code_of_conduct = TRUE, cran_comments = TRUE,
  clone_comments = TRUE, readme = TRUE, vignette = TRUE,
  depends = NULL, imports = NULL, suggests = NULL, remotes = NULL,
  spellcheck = TRUE, tibble = FALSE, pipe = FALSE,
  github_args = NULL)
}
\arguments{
\item{path}{character, package directory. Package name used by \code{pour} is taken from the path's \code{basename}.}

\item{account}{character, user account.}

\item{name}{character, given and family name \code{"given family"}, appears in licensing and \code{pkgdown} metadata. You can leave this \code{NULL} if pulling from global options or \code{.Rprofile} instead.}

\item{description}{a named list providing fields to \code{usethis::use_description} or \code{NULL} to pull from defaults.
Consider setting default fields in \code{options}, or even \code{.Rprofile} if you create a lot of packages.}

\item{license}{character, one of \code{"mit"}, \code{"gpl3"}, \code{"apl2"} or \code{"cc0"}.}

\item{host}{\code{"github"} (default) or \code{"bitbucket"}.}

\item{public}{logical, public remote repository.}

\item{testthat}{logical, use \code{testthat}.}

\item{pkgdown}{logical, use \code{pkgdown}.}

\item{appveyor}{logical, use Appveyor. Applicable if \code{host = "github"}.}

\item{travis}{logical, use Travis-CI. Applicable if \code{host = "github"}.}

\item{codecov}{logical, use \code{covr} package and integrate with \code{codecov.io}. Applicable if \code{host = "github"}.}

\item{lintr}{character, use \code{lintr} package. See details.}

\item{revdep}{logical, use revdep.}

\item{data_raw}{logical, use \code{data-raw} directory for raw repository data and package dataset preparation.}

\item{hex}{logical, place default hex sticker package logo at \code{man/figures/logo.png} (see details) and a template script for customization at \code{data-raw/hex.R}.}

\item{news}{logical, use \code{NEWS.md}.}

\item{code_of_conduct}{logical, include \code{CODE_OF_CONDUCT.md}.}

\item{cran_comments}{logical, add \code{cran-comments.md} template.}

\item{clone_comments}{logical, add \code{clone-comments.md} template.}

\item{readme}{logical, add \code{README.Rmd} template and update installation section. See \code{\link{update_readme_rmd}}.}

\item{vignette}{logical, add package vignette template.}

\item{depends}{character, vector of dependencies, e.g., \code{"R (>= 3.5.0)", "showtext"}.}

\item{imports}{character, as above.}

\item{suggests}{character, as above.}

\item{remotes}{character, as above.}

\item{spellcheck}{logical, spell checking as unit test and add \code{WORDLIST} file for whitelisted words.}

\item{tibble}{logical, for importing and re-exporting \code{tibble}.}

\item{pipe}{logical, for importing and re-exporting \code{magrittr} pipe operator {\%>\%}.}

\item{github_args}{named list, if you need to provide arguments to \code{usethis::use_github}.}
}
\value{
side effect of setting up various package files and configurations.
}
\description{
Wrapper function around several package setup functions from the \code{usethis} and \code{pkgpeanuts} packages.
}
\details{
Run this function from the new package's root directory. The project should be brand new, but must exist. Make a new package project in RStudio.
The eventual goal is to start a project from scratch, but currently this does not work due to issues involving \code{git2r} and \code{usethis} and making the initial git commit and push to new GitHub remote repo.
For now, just leave \code{path = "."}.

\code{pour} wraps around the following functions: \code{use_description}, \code{use_license},
\code{use_github_links}, \code{use_clone_comments}, \code{use_cran_comments},\code{use_data_raw}, \code{use_news_md},
\code{use_testthat}, \code{use_vignette}, \code{use_readme_rmd}, \code{use_revdep}, \code{use_lintr}, \code{use_appveyor},
 \code{use_travis}, \code{use_coverage}, \code{use_hex}, and others. Most are optional, see arguments.

If using \code{lintr} with the new package,  set \code{lintr = "user"} for interactive-only use of the \code{lintr} package.
Set to \code{"test"} for setting up linting as a component of unit testing. The default is \code{lintr = "none"}.
See \code{\link{use_lintr}} for important details regarding unit testing with \code{lintr} in an R package.

If \code{hex = TRUE}, the default \code{logo.png} will not be created if the \code{magick} package is not installed.
Instead, a message is printed notifying of this requirement. Being able to generate a default logo (that will surely be replaced later) is a very minor and unimportant feature.
Therefore, \code{pkgpeanuts} does not have package dependencies or system requirements in this regard. This is optional.

If \code{pkgdown = TRUE}, a \code{pkgdown} site for R package documentation is also initialized, using a \code{pkgdown} directory in the package root
directory containing template \code{_pkgdown.yml} and \code{extra.css} files.
The \code{docs} directory is initially set for package website files and should be specified likewise in the remote repository settings for hosting.
}
\examples{
# Create new R package project with RStudio. Run command inside package root directory, e.g.:
\dontrun{pour(account = "github_username")}
}
\seealso{
\code{\link{use_lintr}}, \code{\link{use_hex}}, \code{\link{use_clone_comments}}, \code{\link{update_readme_rmd}}
}

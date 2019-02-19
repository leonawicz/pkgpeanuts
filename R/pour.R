repo <- function(){
  r <- git2r::repository(".", discover = TRUE)
  remote <- git2r::remotes(r)
  r <- git2r::remote_url(r, remote)
  r <- strsplit(r, ":")[[1]][2]
  list(account = dirname(r), repo = gsub("\\.git", "", basename(r)))
}

.pkg_author <- function(first, last, email, role, comment = NULL){
  role <- .author_field(role)
  if(!is.null(comment)) comment <- .author_field(comment)
  person <- paste0("\"", first, "\", \"", last, "\", email = \"", email, "\", role = ", role)
  if(!is.null(comment)) person <- paste0(person, ", comment = ", comment)
  person <- paste0("person(", person, ")")
  author <- list("Authors@R" = person)
  author
}

.author_field <- function(x){
  if(is.null(names(x))){
    x <- sapply(x, function(x) paste0("\"", x, "\""))
  } else {
    x <- sapply(seq_along(x), function(i) paste0(names(x)[i], " = \"", x[i], "\""))
  }
  if(length(x) > 1) x <- paste0(x, collapse = ", ")
  paste0("c(", x, ")")
}

#' Add \code{lintr} to R package
#'
#' Add scaffolding for \code{lintr} package usage to R package.
#'
#' Note: This function is currently being reworked.
#'
#' If \code{lint_as_test = FALSE} (default), a \code{.lintr} file is created in the package root directory.
#' Set to \code{TRUE} if you intend to use \code{lintr} with unit testing so that problems caught by the linter cause test failure.
#' Do not set to \code{TRUE} simply because you are using \code{testthat}, but apart from \code{lintr}.
#'
#' When \code{lint_as_test = TRUE}, this function creates the template \code{.lintr} file in the \code{inst} directory and
#' adds a symbolic link to this file in the package root directory.
#' It also adds both files (as applicable) to \code{.Rbuildignore}. It also creates a unit test for lint-free package code only when \code{lint_as_test = TRUE}.
#'
#' NOTE: You will have to run R as Administrator on Windows when \code{lint_as_test = TRUE} or the function will fail to create the symbolic link.
#' Similar issues could present on other systems depending on user permissions.
#'
#' @param base_path package root directory.
#' @param lint_as_test logical, if \code{TRUE} (default), adds \code{tests/testthat/test-lintr.R} to package. See details.
#'
#' @return side effect of adding \code{lintr} scaffolding to package.
#' @export
#'
#' @examples
#' \dontrun{use_lintr()}
use_lintr <- function(base_path = ".", lint_as_test = FALSE){
  if(lint_as_test){
    dir.create(file.path(base_path, "inst"), showWarnings = FALSE)
    .lintr_loc <- "inst/.lintr"
  } else {
    .lintr_loc <- ".lintr"
  }
  sink(.lintr_loc)
  cat(paste0("linters: with_defaults(\n  ",
             "line_length_linter(80),\n  ",
             "infix_spaces_linter = NULL,\n  ",
             "camel_case_linter = NULL,\n  ",
             "snake_case_linter = NULL,\n  ",
             "spaces_left_parentheses_linter = NULL)\n"))
  sink()
  if(lint_as_test && !file.exists(".lintr")) file.symlink("inst/.lintr", ".lintr")
  usethis::use_build_ignore(".lintr")
  if(lint_as_test){
    usethis::use_build_ignore("inst/.lintr")
  sink("tests/testthat/test-lintr.R")
    cat(paste0("if (requireNamespace(\"lintr\", quietly = TRUE)) {\n  ",
      "context(\"lints\")\n  ",
      "test_that(\"Package Style\", {\n    ",
      "lintr::expect_lint_free()\n  ",
      "})\n", "}\n"))
    sink()
  }
  usethis::use_package("lintr", "suggests")
}

#' Add \code{clone-comments.md} to package
#'
#' Add \code{clone-comments.md} to package if there are special instructions or information other developers cloning the package repository should know.
#'
#' @param lint_as_test logical, if set to \code{TRUE} a statement is added to the file regarding use of \code{lintr} with \code{testthat}.
#'
#' @return side effect of creating file.
#' @export
#'
#' @examples
#' \dontrun{use_clone_comments}
use_clone_comments <- function(lint_as_test = FALSE){
  if(lint_as_test){
    x <- paste0("Cloning this repository for local development may require you to remove and regenerate the top-level .lintr symbolic link if it is no longer a symbolic link to inst/.lintr following cloning.\n\n", # nolint
                "There are no other comments for this respository.\n")
  } else {
    x <- "Enter comments here...\n"
  }
  sink("clone-comments.md")
  cat("# Cloning this repository\n\n", x)
  sink()
  usethis::use_git_ignore("docs/clone-comments.html")
  usethis::use_build_ignore(c("docs", "pkgdown", "clone-comments.md"))
}

#' Add \code{hex.R} to package
#'
#' Add \code{hex.R} to package \code{data-raw} directory. Create a default logo.
#'
#' This script provides a template for package hex sticker icons, a large icon in \code{data-raw} and a small icon in \code{inst}.
#' A default logo is generated and placed at \code{man/figures/logo.png}. However, it will require user customization after it is generated.
#' Adapt the provided script and rerun to make a new logo.
#'
#' The default \code{logo.png} will not be created if the \code{magick} package is not installed.
#' Instead, a message is printed notifying of this requirement. Being able to generate a default logo (that will surely be replaced later) is a minor, optional feature.
#' Therefore, \code{pkgpeanuts} does not have package dependencies in this regard.
#'
#' @param account user account.
#' @param host \code{"github"} or \code{"bitbucket"}.
#'
#' @return side effect of creating file.
#' @export
#'
#' @examples
#' \dontrun{use_hex(account = "username", host = "github")}
use_hex <- function(account, host = "github"){
  if(!file.exists("data-raw")) usethis::use_data_raw()
  file.copy(system.file(package = "pkgpeanuts", "resources/hex.R"), "data-raw/hex.R")
  if(!requireNamespace("magick", quietly = TRUE)){
    message(paste("hex = TRUE but the 'magick' package is not installed.",
                  "hex.R template script copied but default hex logo not created."))
    return(invisible())
  }
  pkg <- basename(getwd())
  url <- paste0(account, ".", host, ".io/", pkg)
  out <- paste0("man/figures/logo.png")
  dir.create("man/figures", showWarnings = FALSE)
  hex_plot <- function(out, mult = 1){
    g <- ggplot2::ggplot() + ggplot2::theme_void() + hexSticker::theme_transparent()
    hexSticker::sticker(g, package = pkg, p_y = 1, p_color = "gray20", p_size = 20,
                        h_color = "gray20", h_fill = "burlywood1", h_size =  1.4,
                        url = url, u_color = "gray20", u_size = 3, filename = out)
  }
  hex_plot(out)
}

# nolint start

#' Update README.Rmd template
#'
#' Update the README.Rmd template file created by \code{usethis::use_readme_rmd}.
#'
#' The file must exist. This function is for updating the Rmd file, not an md file.
#' It replaces the content in the \code{Installation} section of the original template.
#' Replacement content is conditional on \code{host} and whether or not \code{public} is \code{TRUE} or \code{FALSE}.
#' The package name is also inserted as needed, taken from \code{repo}.
#'
#' @param repo character, the repository name, e.g. \code{leonawicz/pkgpeanuts}.
#' @param host character, options are \code{"github"} or \code{"bitbucket"}.
#' @param public, logical, whether the remote repository is public or private.
#'
#' @return side effect of updating file.
#' @export
#'
#' @examples
#' \dontrun{update_readme_rmd}
update_readme_rmd <- function(repo, host = "github", public = TRUE){
  pkg <- strsplit(repo, "/")[[1]][2]
  if(!file.exists("README.Rmd")){
    message("README.Rmd does not exist. Nothing to update.")
    return(invisible())
  }
  pretext <- paste0(
    "## Installation\n\n",
    "You can install the released version of `", pkg,
    "` from [CRAN](https://CRAN.R-project.org) with:\n\n",
    "``` r\n",
    "install.packages(\"", pkg, "\")\n",
    "```\n\n",
    "You can install the development version of `", pkg)
  host_name <- switch(host, github = "GitHub", bitbucket = "BitBucket")
  auth_args <- switch(
    host,
    github = ifelse(public, "", "auth_token = your_token"),
    bitbucket = ifelse(public, "", ", auth_user = \"username\", password = \"password\"")
  )
  txt <- paste0(
    pretext, "` from ", host_name, " with:\n\n",
    "``` r\n",
    "# install.packages(\"remotes\")\n",
    "remotes::install_", host, "(\"", repo, "\"", auth_args, ")\n",
    "```\n")
  x <- readLines("README.Rmd")
  idx <- grep("^## Installation|::install_github\\(\"", x)
  x <- paste0(c(x[1:(idx[1] - 1)], txt, x[(idx[2] + 2):length(x)]), collapse = "\n")
  sink("README.Rmd")
  cat(paste0(x, "\n"))
  sink()
}

# nolint end

#' Generate package scaffolding for robust package setup
#'
#' Wrapper function around several package setup functions from the \code{usethis} and \code{pkgpeanuts} packages.
#'
#' Run this function from the new package's root directory. The project should be brand new, but must exist. Make a new package project in RStudio.
#' The eventual goal is to start a project from scratch, but currently this does not work due to issues involving \code{git2r} and \code{usethis} and making the initial git commit and push to new GitHub remote repo.
#' For now, just leave \code{path = "."}.
#'
#' \code{pour} wraps around the following functions: \code{use_description}, \code{use_license},
#' \code{use_github_links}, \code{use_clone_comments}, \code{use_cran_comments},\code{use_data_raw}, \code{use_news_md},
#' \code{use_testthat}, \code{use_vignette}, \code{use_readme_rmd}, \code{use_revdep}, \code{use_lintr}, \code{use_appveyor},
#'  \code{use_travis}, \code{use_coverage}, \code{use_hex}, and others. Most are optional, see arguments.
#'
#' If using \code{lintr} with the new package,  set \code{lintr = "user"} for interactive-only use of the \code{lintr} package.
#' Set to \code{"test"} for setting up linting as a component of unit testing. The default is \code{lintr = "none"}.
#' See \code{\link{use_lintr}} for important details regarding unit testing with \code{lintr} in an R package.
#'
#' If \code{hex = TRUE}, the default \code{logo.png} will not be created if the \code{magick} package is not installed.
#' Instead, a message is printed notifying of this requirement. Being able to generate a default logo (that will surely be replaced later) is a very minor and unimportant feature.
#' Therefore, \code{pkgpeanuts} does not have package dependencies or system requirements in this regard. This is optional.
#'
#' \code{pkgdown} for R package website building is also initialized, using a \code{pkgdown} directory in the package root
#' directory containing template \code{_pkgdown.yml} and \code{extra.css} files. Note that \code{pkgdown} is always included by \code{pour}.
#' The \code{docs} directory is used for website files and should be specified likewise in the remote repository settings.
#'
#' @param path character, package directory. Package name used by \code{pour} is taken from the path's \code{basename}.
#' @param account character, user account.
#' @param name character, given and family name \code{"given family"}, appears in licensing and \code{pkgdown} metadata. You can leave this \code{NULL} if pulling from global options or \code{.Rprofile} instead.
#' @param description a named list providing fields to \code{usethis::use_description} or \code{NULL} to pull from defaults.
#' Consider setting default fields in \code{options}, or even \code{.Rprofile} if you create a lot of packages.
#' @param license character, one of \code{"mit"}, \code{"gpl3"}, \code{"apl2"} or \code{"cc0"}.
#' @param host \code{"github"} (default) or \code{"bitbucket"}.
#' @param public logical, public remote repository.
#' @param testthat logical, use \code{testthat}.
#' @param appveyor logical, use Appveyor. Applicable if \code{host = "github"}.
#' @param travis logical, use Travis-CI. Applicable if \code{host = "github"}.
#' @param codecov logical, use \code{covr} package and integrate with \code{codecov.io}. Applicable if \code{host = "github"}.
#' @param lintr character, use \code{lintr} package. See details.
#' @param revdep logical, use revdep.
#' @param data_raw logical, use \code{data-raw} directory for raw repository data and package dataset preparation.
#' @param hex logical, place default hex sticker package logo at \code{man/figures/logo.png} (see details) and a template script for customization at \code{data-raw/hex.R}.
#' @param news logical, use \code{NEWS.md}.
#' @param code_of_conduct logical, include \code{CODE_OF_CONDUCT.md}.
#' @param cran_comments logical, add \code{cran-comments.md} template.
#' @param clone_comments logical, add \code{clone-comments.md} template.
#' @param readme logical, add \code{README.Rmd} template and update installation section. See \code{\link{update_readme_rmd}}.
#' @param vignette logical, add package vignette template.
#' @param depends character, vector of dependencies, e.g., \code{"R (>= 3.5.0)", "showtext"}.
#' @param imports character, as above.
#' @param suggests character, as above.
#' @param remotes character, as above.
#' @param spellcheck logical, spell checking as unit test and add \code{WORDLIST} file for whitelisted words.
#' @param tibble logical, for importing and re-exporting \code{tibble}.
#' @param pipe logical, for importing and re-exporting \code{magrittr} pipe operator {\%>\%}.
#' @param github_args named list, if you need to provide arguments to \code{usethis::use_github}.
#'
#' @return side effect of setting up various package files and configurations.
#' @export
#' @seealso \code{\link{use_lintr}}, \code{\link{use_hex}}, \code{\link{use_clone_comments}}, \code{\link{update_readme_rmd}}
#'
#' @examples
#' # Create new R package project with RStudio. Run command inside package root directory, e.g.:
#' \dontrun{pour(account = "github_username")}
pour <- function(path = ".", account, name = NULL, description = NULL, # nolint start
                 license = c("mit", "gpl3", "apl2", "cc0"),
                 host = "github", public = TRUE,
                 testthat = TRUE, appveyor = TRUE, travis = TRUE, codecov = TRUE,
                 lintr = c("none", "user", "test"), revdep = TRUE, data_raw = TRUE, hex = TRUE,
                 news = TRUE, code_of_conduct = TRUE, cran_comments = TRUE, clone_comments = TRUE,
                 readme = TRUE, vignette = TRUE,
                 depends = NULL, imports = NULL, suggests = NULL, remotes = NULL,
                 spellcheck = TRUE, tibble = FALSE, pipe = FALSE, github_args = NULL){
  package <- if(path == ".") basename(getwd()) else basename(path)
  # wd <- getwd()
  # print(0)
  # usethis::create_package(path)
  # setwd(path)
  # print(1)
  # usethis::use_git()
  # print(2)
  # usethis::use_git_ignore(".Rproj.user", ".Rhistory", ".RData", ".Ruserdata")
  # print(3)
  # if(host == "github"){
  #   github_args <- c(list(private = !public), github_args)
  #   print(4)
  #   github_args <- github_args[!duplicated(names(github_args))]
  #   print(5)
  #   if(Sys.info()[["sysname"]] == "Windows" & !"credentials" %in% names(github_args))
  #     github_args$credentials <- git2r::cred_ssh_key()
  #   print(6)
  #   print(github_args)
  #   do.call(usethis::use_github, github_args)
  # }
  license <- match.arg(license)
  lintr <- match.arg(lintr)
  fields <- if(length(description)) description else list()
  if(!is.null(depends)) fields$Depends <- paste0(depends, collapse = ",\n    ")
  usethis::use_description(fields)
  if(!is.null(imports)) usethis::use_package(imports, "Imports")
  if(!is.null(suggests)) usethis::use_package(suggests, "Suggests")
  if(!is.null(remotes)) usethis::use_dev_package(remotes)
  if(is.null(name) & is.null(options()$usethis.full_name)) options(usethis.full_name = "Author Name")
  if(is.null(name)){
    switch(license, mit = usethis::use_mit_license(), gpl3 = usethis::use_gpl3_license(),
           apl2 = usethis::use_apl2_license(), cc0 = usethis::use_cc0_license())
  } else {
    switch(license, mit = usethis::use_mit_license(name), gpl3 = usethis::use_gpl3_license(name),
           apl2 = usethis::use_apl2_license(name), cc0 = usethis::use_cc0_license(name))
  }
  if(host == "github") usethis::use_github_links()
  if(tibble) usethis::use_tibble()
  if(pipe) usethis::use_pipe()
  if(code_of_conduct) usethis::use_code_of_conduct()
  if(clone_comments) use_clone_comments()
  if(cran_comments) usethis::use_cran_comments()
  if(data_raw) usethis::use_data_raw()
  if(hex) use_hex(account, host)
  if(news) usethis::use_news_md()
  if(testthat) usethis::use_testthat()
  if(vignette & !file.exists(paste0("vignettes/", package, ".Rmd"))) usethis::use_vignette(package)
  if(readme){
    usethis::use_readme_rmd()
    update_readme_rmd(paste(account, package, sep = "/"), host, public)
  }
  if(revdep) usethis::use_revdep()
  if(lintr == "test"){
    try(use_lintr(lint_as_test = TRUE))
  } else if(lintr == "user"){
    use_lintr()
  }
  if(spellcheck) usethis::use_spell_check()
  message("Building favicons and initializing pkgdown...")
  pkgdown::build_favicon()
  pkgdown::init_site()
  pdfiles <- list.files(file.path(system.file(package = "pkgpeanuts"), "resources/pkgdown"),
                        full.names = TRUE)
  dir.create("pkgdown", showWarnings = FALSE)
  file.copy(pdfiles[2], file.path("pkgdown", basename(pdfiles[2])), overwrite = TRUE)
  usethis::use_build_ignore(c("docs", "pkgdown"))
  file <- "pkgdown/_pkgdown.yml"
  x <- paste(readLines(pdfiles[1]), collapse = "\n")
  if(host == "github"){
    r <- repo()
  } else {
    r <- list(account = account, repo = basename(getwd()))
  }
  x <- gsub("_ACCOUNT_", r$account, x)
  x <- gsub("_PACKAGE_", r$repo, x)
  if(is.null(name)) name <- options()$usethis.full_name
  x <- gsub("_GIVEN_FAMILY_", name, x)
  x <- gsub("_HOST_", host, x)
  x <- gsub("_HOSTURL_", switch(host, github = "github.com", bitbucket = "bitbucket.org"), x)
  sink(file)
  cat(paste0(x, "\n"))
  sink()

  if(host == "github"){
    if(appveyor) usethis::use_appveyor()
    if(travis) usethis::use_travis()
    if(codecov) usethis::use_coverage()
    if(readme){
      x <- readLines("README.Rmd")
      idx <- grep(paste("^#", package), x)
      if(hex & file.exists("man/figures/logo.png") & x[idx] == paste("#", package))
        x[idx] <- paste(x[idx], '<a href="man/figures/logo.png" _target="blank"><img src="man/figures/logo.png" style="margin-left:10px;margin-bottom:5px;" width="120" align="right"></a>') #nolint
      sink("README.Rmd")
      cat(paste0(x, collapse = "\n"), "\n", sep = "")
      sink()
    }
    if(travis & codecov){
      x <- readLines(".travis.yml")
      x <- paste0(c(x, "\nafter_success:\n  - Rscript -e 'covr::codecov()'\n"), collapse = "\n")
      sink(".travis.yml")
      cat(x)
      sink()
    }
  }
  message("Package peanuts added for protection.")
  invisible()
}
# nolint end

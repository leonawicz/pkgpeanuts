#' Add an existing Shiny app to a source package in development
#'
#' Copy an existing Shiny app to a source package (not an installed package) and create \code{DESCRIPTION} and \code{Readme.md} template files related to Shiny showcase mode.
#'
#' Since this function is for use in a package development context, it is assumed your working directory is the package root directory, hence the default \code{path = "inst"}.
#' You can create \code{description} and \code{readme} with \code{use_app} and then edit these files.
#' Alternatively, you can create them directly and with greater control using \code{\link{use_app_description}} and \code{\link{use_app_readme}}.
#' Also, if a \code{rsconnect} directory exists in \code{app} it is not retained in the new copy in \code{path}.
#'
#' When \code{overwrite = TRUE}, overwrite a previously added app at \code{path}.
#' Also overwrite any pre-existing \code{DESCRIPTION} or \code{Readme.md} files copied from the source \code{app} if \code{description = TRUE} or \code{readme = TRUE}.
#' In the latter case, existing files inside \code{app} are never overwritten. Rather, new template files are created in \code{path} in place of the copied files.
#'
#' @param app character, path to app directory.
#' @param path character, path app is copied to; should be under source package \code{inst} directory. See details.
#' @param description logical, add a \code{DESCRIPTION} file template for showcase mode. See details.
#' @param readme logical, add a \code{Readme.md} file template for showcase mode. See details.
#' @param overwrite logical, overwrite files. See details.
#'
#' @export
#' @seealso \code{\link{use_app_description}}, \code{\link{use_app_readme}}
#'
#' @examples
#' # Copy an external app to a 'shiny' subdirectory inside package source 'inst' directory.
#' \dontrun{use_app(app = "../external_app", path = "inst/shiny")}
use_app <- function(app, path = "inst",
                     description = TRUE, readme = TRUE, overwrite = FALSE){
  appname <- basename(app)
  app_path <- file.path(path, appname)
  if(!file.exists(path)){
    message(paste0("`", path, "` does not exist. Creating it now."))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  cur_files <- list.files(path)
  if(!length(cur_files)) cur_files <- NULL
  if(appname %in% cur_files & overwrite) unlink(app_path, recursive = TRUE, force = TRUE)
  if(!appname %in% cur_files || overwrite){
    dir.create(app_path, recursive = TRUE, showWarnings = FALSE)
    cat("Copying app to:", app_path, "\n")
    file.copy(app, path, recursive = TRUE)
    if(description) use_app_description(app_path)
    if(readme) use_app_readme(app_path)
    if("rsconnect" %in% list.files(app_path))
      unlink(file.path(app_path, "rsconnect"), recursive = TRUE, force = TRUE)
  }
  if(appname %in% cur_files & !overwrite) cat("App exists at destination. Set overwrite = TRUE to replace.\n")
  invisible()
}

#' Create an app DESCRIPTION file
#'
#' Add a DESCRIPTION template to a Shiny app.
#'
#' This file is used with Shiny app showcase mode.
#' Most arguments to \code{use_app_description} have defaults you must override, either when calling this function directly,
#' or making edits to the output after calling \code{\link{use_app}}.
#' \code{title} and \code{tags} do not have defaults. If missing, they should be updated directly in the generated DESCRIPTION file.
#' Tags passed in the form \code{tags = c("tag", "another tag")} will result in \code{tag, another-tag}.
#'
#' @param base_path output directory.
#' @param title app title.
#' @param author author name.
#' @param url author url.
#' @param license license type.
#' @param mode Shiny display mode.
#' @param tags optional tags. May be a vector. See details.
#'
#' @export
#' @seealso \code{\link{use_app}}, \code{\link{use_app_readme}}
#'
#' @examples
#' \dontrun{use_app_description()}
use_app_description <- function(base_path = ".", title, author = "Author Name",
                        url = "https://insert_author_account_here.github.io/", license = "MIT",
                        mode = "Showcase", tags){
  x <- readLines(file.path(system.file(package = "pkgpeanuts"), "resources/apps/DESCRIPTION"))
  if(!missing(title)) x <- gsub("__title__", title, x)
  if(!missing(tags)){
    if(!inherits(tags, "character")) stop("`tags` must be a character vector.")
    tags <- paste(gsub(" ", "-", tags), collapse = ", ")
    x <- gsub("__tags__", tags, x)
  }
  x <- gsub("__author__", author, x)
  x <- gsub("__authorurl__", url, x)
  x <- gsub("__license__", license, x)
  x <- gsub("__displaymode__", mode, x)
  x <- paste0(paste(x, collapse = "\n"), "\n")
  sink(file.path(base_path, "DESCRIPTION"))
  cat(x)
  sink()
  cat("Added DESCRIPTION file template.\n")
  invisible()
}

#' Create an app Readme.md file
#'
#' Add a Readme.md template to a Shiny app.
#'
#' This file is used to add a text description below a Shiny app when run in showcase mode.
#'
#' @param base_path output directory.
#'
#' @export
#' @seealso \code{\link{use_app}}, \code{\link{use_app_description}}
#'
#' @examples
#' \dontrun{use_app_readme()}
use_app_readme <- function(base_path = "."){
  sink(file.path(base_path, "Readme.md"))
  cat("This Shiny app...\n")
  sink()
  cat("Added Readme.md template.\n")
  invisible()
}

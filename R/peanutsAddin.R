#' @import shiny
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniTitleBarButton
NULL

# nolint start

#' Pour packing peanuts from Shiny gadget addin
#'
#' Launch a Shiny gadget to use \code{\link{pour}} in GUI form. See \code{pour} for details.
#'
#' @export
#'
#' @examples
#' \dontrun{peanutsAddin()}
peanutsAddin <- function(){
  doc_files <- c("README.Rmd" = "readme", "NEWS.md" = "news", "CODE_OF_CONDUCT.md" = "coc",
                 "cran-comments.md" = "cran-comments")
  licenses <- c("MIT" = "mit", "GPL3" = "gpl3", "APL2" = "apl2", "CC0" = "cc0")

  ui <- miniPage(
    gadgetTitleBar("Package peanuts: Pack well.", right = miniTitleBarButton("pour", "Pack", primary = TRUE)),
    miniContentPanel(
      bsCollapse(id = "bsc", open = "General information",
        bsCollapsePanel("General information", style = "info",
          splitLayout(
            radioButtons("host", "Host", c("GitHub" = "github", "BitBucket" = "bitbucket"),
                         inline = TRUE, width = "100%"),
            radioButtons("license", "License", licenses, inline = TRUE, width = "100%"),
            cellWidths = rep(260, 2)
          ),
          splitLayout(
            textInput("account", "Account", width = "100%", placeholder = "username"),
            textInput("name", "Author name", width = "100%", placeholder = "First Last")
          ),
          textInput("description", "Description fields", width = "100%",
                    placeholder = "list(Language = \"es\")")),
        bsCollapsePanel("Packages, import/export and data", style = "info",
          splitLayout(
            textInput("depends", "Depends", width = "100%", placeholder = "R (>= 3.5.0), sysfonts"),
            textInput("imports", "Imports", width = "100%", placeholder = "dplyr,purrr")
          ),
          splitLayout(
            textInput("suggests", "Suggests", width = "100%", placeholder = "knitr, rmarkdown"),
            textInput("remotes", "Remotes", width = "100%", placeholder = "ideally, nothing")
          ),
          checkboxInput("tibble", "Import and re-export tibble", width = "100%"),
          checkboxInput("pipe", "Import and re-export pipe (%>%)", width = "100%"),
          checkboxInput("data_raw", "Use data-raw", width = "100%")),
        bsCollapsePanel("Documentation", style = "info",
          checkboxGroupInput("docs", "Files", doc_files, doc_files, TRUE, width = "100%"),
          checkboxInput("vignette", "Add package vignette template", width = "100%"),
          checkboxInput("hex", "Add hex logo customization script", width = "100%"),
          conditionalPanel("input.testthat == true",
            checkboxInput("spellcheck", "Spell check as unit test", width = "100%"))),
        bsCollapsePanel("Unit testing", style = "info",
          checkboxInput("testthat", "testthat", width = "100%")),
        bsCollapsePanel("Continuous integration", style = "info",
          checkboxInput("travis", "Travis-CI", width = "100%"),
          checkboxInput("appveyor", "Appveyor", width = "100%")),
        bsCollapsePanel("Code coverage", style = "info",
          checkboxInput("codecov", "codecov.io", width = "100%")),
        bsCollapsePanel("Code linting", style = "info",
          p("Checkbox ignored. This function is currently being reworked."),
          checkboxInput("lintr", "lintr", width = "100%"),
          conditionalPanel("input.testthat == true && input.lintr == true",
            checkboxInput("lint_as_test", "Use lintr as testthat unit test", width = "100%"))),
        bsCollapsePanel("Reverse dependency checking", style = "info",
          checkboxInput("revdep", "revdepcheck", width = "100%"))
      ),
      #checkboxInput("prevent", "Prevent use in existing, non-empty directory.", TRUE, width = "100%"), # nolint
      br()
    )
  )

  server <- function(input, output){
    name <- reactive(if(input$name == "") "Author Name" else input$name)
    desc <- reactive(if(input$description == "") NULL else input$description)
    lintr_val <- reactive({
      "none"
      # if(!input$lintr) return("none")
      # if(is.null(input$lint_as_test) || !input$lint_as_test) "user" else "test"
    })
    readme <- reactive("readme" %in% input$docs)
    news <- reactive("news" %in% input$docs)
    coc <- reactive("coc" %in% input$docs)
    crancom <- reactive("cran-comments" %in% input$docs)
    clonecom <- reactive("clone-comments" %in% input$docs)
    depends <- reactive(if(input$depends == "") NULL else trimws(strsplit(input$depends, ",")[[1]]))
    imports <- reactive(if(input$imports == "") NULL else trimws(strsplit(input$imports, ",")[[1]]))
    suggests <- reactive(if(input$suggests == "") NULL else trimws(strsplit(input$suggests, ",")[[1]]))
    remotes <- reactive(if(input$remotes == "") NULL else trimws(strsplit(input$remotes, ",")[[1]]))

    observeEvent(input$pour, {
      #safe <- !file.exists(input$path) || !length(list.files(input$path))
      #if(safe || !input$prevent){
      #print(input$path)
      pour(".", input$account, name = name(), description = desc(), license = input$license, host = input$host,
           testthat = input$testthat, appveyor = input$appveyor, travis = input$travis, codecov = input$codecov,
           lintr = lintr_val(), revdep = input$revdep, data_raw = input$data_raw, hex = input$hex,
           news = news(), code_of_conduct = coc(), cran_comments = crancom(), #clone_comments = clonecom(),
           readme = readme(), vignette = input$vignette,
           depends = depends(), imports = imports(), suggests = suggests(), remotes = remotes(),
           spellcheck = input$spellcheck, tibble = input$tibble, pipe = input$pipe)
      #}
    })

  }

  viewer <- dialogViewer("Add package scaffolding to a new R package", 600, 600)
  runGadget(ui, server, viewer = viewer)
}

# nolint end

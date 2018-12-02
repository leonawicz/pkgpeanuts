library(shiny)
library(shinyBS)

doc_files <- c("README.Rmd" = "readme", "NEWS.md" = "news", "CODE_OF_CONDUCT.md" = "coc",
               "cran-comments.md" = "cran-comments", "clone-comments.md" = "clone-comments")
licenses <- c("MIT" = "mit", "GPL3" = "gpl3", "APL2" = "apl2", "CC0" = "cc0")

ui <- fluidPage(
  titlePanel("Package peanuts"),
  fluidRow(
    column(3,
      selectInput("host", "Host", c("GitHub" = "github", "BitBucket" = "bitbucket"), width = "100%"),
      textInput("path", "Path to new package", width = "100%", placeholder = "path/to/package"),
      textInput("account", "Account", width = "100%", placeholder = "username"),
      textInput("name", "Author name", width = "100%"),
      selectInput("license", "License", licenses, width = "100%"),
      textInput("description", "Description fields", width = "100%",
                placeholder = "list(Language = \"es\").", width = "100%"),
      checkboxInput("data_raw", "data-raw", TRUE, width = "100%"),

      h4("Packages, imports and exports"),
      textInput("depends", "Depends", width = "100%", placeholder = "R (>= 3.5.0), sysfonts"),
      textInput("imports", "Imports", width = "100%", placeholder = "dplyr,purrr"),
      textInput("suggests", "Suggests", width = "100%", placeholder = "knitr, rmarkdown"),
      textInput("remotes", "Remotes", width = "100%", placeholder = "ideally, nothing"),
      checkboxInput("tibble", "Import and re-export tibble", FALSE, width = "100%"),
      checkboxInput("pipe", "Import and re-export pipe (%>%)", FALSE, width = "100%")

    ),
    column(3,
      h4("Documentation"),
      checkboxGroupInput("docs", "Files", doc_files, doc_files[-5], TRUE, width = "100%"),
      checkboxInput("vignette", "Add package vignette template", TRUE, width = "100%"),
      checkboxInput("hex", "Add hex logo customization script", TRUE, width = "100%"),
      conditionalPanel("input.testthat == true",
                       checkboxInput("spellcheck", "Spell check as unit test", TRUE, width = "100%"))
    ),
    column(3,
      h4("Unit testing"),
      checkboxInput("testthat", "testthat", TRUE, width = "100%"),
      h4("Continuous integration"),
      checkboxInput("travis", "Travis-CI", TRUE, width = "100%"),
      checkboxInput("appveyor", "Appveyor", TRUE, width = "100%"),
      h4("Code coverage"),
      checkboxInput("codecov", "codecov.io", TRUE, width = "100%"),
      h4("Code linting"),
      checkboxInput("lintr", "lintr", TRUE, width = "100%"),
      conditionalPanel("input.testthat == true && input.lintr == true",
        checkboxInput("lint_as_test", "Use lintr as testthat unit test", FALSE, width = "100%")),
      h4("Reverse dependency checking"),
      checkboxInput("revdep", "revdepcheck", TRUE, width = "100%")
    ),
    column(3,
      actionButton("go_btn", "Create package", width = "100%")#,
      #checkboxInput("prevent", "Prevent use in existing, non-empty directory.", TRUE, width = "100%") # nolint
    )
  )
)

# nolint start

server <- function(input, output){
  name <- reactive(if(input$name == "") "Author Name" else input$name)
  desc <- reactive(if(input$description == "") NULL else input$description)
  lintr_val <- reactive({
    if(!input$lintr) return("none")
    if(is.null(input$lint_as_test) || !input$lint_as_test) "user" else "test"
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
# nolint end
  observeEvent(input$go_btn, {
    safe <- !file.exists(input$path) || !length(list.files(input$path))
    if(safe || !input$prevent){
      pour(input$path, input$account, name = name(), description = desc(), license = input$license, host = input$host,
           testthat = input$testthat, appveyor = input$appveyor, travis = input$travis, codecov = input$codecov,
           lintr = lintr_val(), revdep = input$revdep, data_raw = input$data_raw, hex = input$hex,
           news = news(), code_of_conduct = coc(), cran_comments = crancom(), clone_comments = clonecom(),
           readme = readme(), vignette = input$vignette,
           depends = depends(), imports = imports(), suggests = suggests(), remotes = remotes(),
           spellcheck = input$spellcheck, tibble = input$tibble, pipe = input$pipe)
    }
  })

}

shinyApp(ui = ui, server = server)

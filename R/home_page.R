suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(shinydashboard)
  library(shiny.router)
  library(shinyjs)
  library(stringr)
  library(tidyverse)
  library(jsTreeR)
  library(highcharter)
  library(DT)
})

home_page <- fluidPage(
    tags$style(HTML("
    .clickable-image {
      cursor: pointer;
    }
    .news-container {
      border: 1px solid black;
      padding: 10px;
      text-align: center;
    }
  ")),
  fluidRow(
    column(
        width = 4,
        tags$img(src = "img/logo.jpg", width = "5%")
    ),
    column(
        width = 8,
    p("The PreAtlas database provides comprehensive resources and an interactive
  analysis platform for exploring the vast amount of bulk and single-cell
  transcriptomic data, as well as curated knowledges specially pertaining to
  diverse premalignant disease for 19 cancer types across 15 body sites.",
    style = "text-align:justify;color:black;background-color:lavender;
        padding:15px;border-radius:10px"
  ))),
  fluidRow(
    column(
      12,
      searchInput(
        inputId = "search", label = "Quick search",
        placeholder = "e.g. TP53, Intestinal metaplasia, xx drug, ...",
        btnSearch = icon("search"),
        width = "500px"
      )
    ),
    align = "center"
  ),
  fluidRow(
    column(
        width = 4,
        div(
            class = "news-container",
            tags$h4("Recent News",
                style = "text-align: center;"
            )
        ),
        div(
            class = "news-container",
            tags$p("PreAtlas v1.0.0 released on 2023-04-11")
        ),
        br(),
        br(),
        tags$p("Citation:",
            style = "text-align:justify;color:black;background-color:lavender;
                padding:15px;border-radius:10px"
            )

    ),
    column(
        width = 4,
        highchartOutput("bodymap_hc", width = "50%"),
        style = "justify-content:right"),
    column(
      width = 4,
        fluidRow(
            column(
                width = 3,
                tags$img(src = "img/disease.svg", width = "60%"),
                tags$h5("79 diseases", style = "text-align: center;"),
                tags$img(src = "img/sample.svg", width = "60%"),
                tags$h5("xx samples", style = "text-align: center;"),
            ),
            column(
                width = 3,
                tags$img(src = "img/projects.svg", width = "60%"),
                tags$h5("xx projects", style = "text-align: center;"),
                tags$img(src = "img/cell.svg", width = "60%"),
                tags$h5("xx cells", style = "text-align: center;"),
            ),
            column(
                width = 3,
                tags$img(src = "img/bulk.svg", width = "60%"),
                tags$h5("86 bulk datasets", style = "text-align: center;"),
                tags$img(src = "img/drug.svg", width = "60%"),
                tags$h5("xx drugs", style = "text-align: center;"),
            ),
            column(
                width = 3,
                tags$img(src = "img/sc.svg", width = "60%"),
                tags$h5("xx single-cell datasets",
                    style = "text-align: center;"),
                tags$img(src = "img/network.svg", width = "60%"),
                tags$h5("xx network interactions",
                    style = "text-align: center;"),
            )
        ),
        br(),
        p(strong("Related databases:")),
        fluidRow(
            column(
                width = 3,
                tags$a(
                    tags$img(src = "img/GEO.gif", width = "100%"),
                    href = "https://www.ncbi.nlm.nih.gov/geo/",
                    target = "_blank"
                ),
                tags$a(
                    tags$img(src = "img/OLS.png", width = "100%"),
                    href = "https://www.ebi.ac.uk/ols/index",
                    target = "_blank"
                )
            ),
            column(
                width = 3,
                tags$a(
                    tags$img(src = "img/ArrayExpress.png", width = "100%"),
                    href = "https://www.ebi.ac.uk/arrayexpress/",
                    target = "_blank"
                ),
                tags$a(
                    tags$img(src = "img/HPO.png", width = "100%"),
                    href = "https://hpo.jax.org/app/",
                    target = "_blank"
                )
            ),
            column(
                width = 3,
                tags$a(
                    tags$img(src = "img/GSA.png", width = "100%"),
                    href = "https://ngdc.cncb.ac.cn/gsa/",
                    target = "_blank"
                ),
                tags$a(
                    tags$img(src = "img/GeneCard.webp", width = "100%"),
                    href = "https://www.genecards.org/",
                    target = "_blank"
                )
            ),
        )
    )
  ),
  style = "padding-left:40px;padding-right:40px"
)
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

search_page <- fluidPage(
  tags$head(
    tags$style(HTML("
      .column-divider {
        border-left: 1px solid #ccc;
        padding-left: 10px;
        margin-left: 10px;
      }
    "))
  ),
  fluidRow(
    column(
      width = 4,
      h4("Search by gene",
        style = "text-align: center;"
      ),
      tags$head(
        tags$style(HTML("
      .search-container {
        display: flex;
        align-items: center;
      }
      .search-btn {
        border: none;
        background: transparent;
        cursor: pointer;
      }
    "))
      ),
      fluidRow(
        column(
          9,
          tags$div(
            class = "search-container",
            selectizeInput("search_gene",
              "Search for a gene:",
              choices = NULL,
              multiple = FALSE,
              width = "100%",
              options = list(create = TRUE)
            ),
            tags$button(
              icon("search"),
              class = "search-btn",
              id = "search_gene_btn",
              onclick =
                "Shiny.onInputChange('searchGeneBtnClicked', Math.random())"
            )
          )
        )
      )
    ),
    column(
      width = 4,
      h4("Search by organ",
        style = "text-align: center;"
      ),
      fluidRow(
        column(
          9,
          tags$div(
            class = "search-container",
            selectizeInput("search_organ",
              "Search for an organ:",
              choices = NULL,
              multiple = FALSE,
              width = "100%"
            ),
            tags$button(
              icon("search"),
              class = "search-btn",
              id = "search_organ_btn",
              onclick =
                "Shiny.onInputChange('searchOrganBtnClicked', Math.random())"
            )
          )
        )
      )
    ),
    column(
      width = 4,
      h4("Search by project",
        style = "text-align: center;"
      ),
      selectizeInput(
        inputId = "search_project",
        label = "Project ID",
        choices = c(""),
        options = list(
          valueField = "id",
          labelField = "name",
          searchField = "name",
          create = FALSE,
          maxItems = 1,
          placeholder = "Input project"
        )
      )
    )
  ),
  p(
    "Search notes:
        The PreAtlas provides three main search modules, including",
    strong("Search by gene, Search by organ, Search by project."),
    " Here, the Search by gene module enable to provide basic ",
    "information, associated organs or",
    " premalignant diseases, ",
    "associated cell types, associated drugs and network ",
    "associations for any input gene symbol or Entrez ID. The",
    strong("Seach by organ"),
    "module enable to provide curated related transcriptomic ",
    "datasets,  associated cellular components and their ",
    "proportions, associated dynamic genes and associated ",
    "therapeutic drugs for diverse",
    " premalignant disease of any ",
    "input organ.",
    style = "text-align:justify;color:black;background-color:lavender;
          padding:15px;border-radius:10px"
  )
)

search_gene_page <- fluidPage(
  h3("Search gene", style = "text-align: center;"),
  textOutput("gene_name_text"),
  fluidRow(
    column(
      width = 6,
      h4("Basic information",
        style = "text-align: center;"
      ),
      # dataTableOutput("gene_basic_table"),
      uiOutput("gene_basic_table"),
      h4("Basic information 2",
        style = "text-align: center;"
      ),
      h5("Curated knowledges", style = "text-align: center;"),
      dataTableOutput("gene_knowledge_table"),
      h5("Omics-derived", style = "text-align: center;"),
      dataTableOutput("gene_omics_table")
    ),
    column(
      width = 6,
      h4("Associated cell types",
        style = "text-align: center;"
      ),
      dataTableOutput("gene_celltype_table"),
      h4("Associated drugs",
        style = "text-align: center;"
      ),
      dataTableOutput("gene_drug_table"),
      h4("Multiple network neighbours",
        style = "text-align: center;"
      ),
      dataTableOutput("gene_network_table")
    )
  )
)

search_organ_page <- fluidPage(
  h3("Search organ", style = "text-align: center;"),
  textOutput("organ_name_text"),
  fluidRow(
    column(
      width = 6,
      h4("Associated datasets",
        style = "text-align: center;"
      ),
      h5("Bulk",
        style = "text-align: center;"
      ),
      dataTableOutput("organ_bulk_table"),
      h5("Single-cell",
        style = "text-align: center;"
      ),
      dataTableOutput("organ_sc_table"),
      h4("Associated dynamic cellular components",
        style = "text-align: center;"
      ),
      h5("Bulk (Cibersort)",
        style = "text-align: center;"
      ),
      dataTableOutput("organ_dyn_bulk_table"),
      h5("Single-cell",
        style = "text-align: center;"
      ),
      dataTableOutput("organ_dyn_sc_table")
    ),
    column(
      width = 6,
      h4("Associated dynamic genes",
        style = "text-align: center;"
      ),
      h5("Bulk",
        style = "text-align: center;"
      ),
      dataTableOutput("organ_dyng_bulk_table"),
      h5("Single-cell",
        style = "text-align: center;"
      ),
      dataTableOutput("organ_dyng_sc_table"),
      h4("Associated drugs",
        style = "text-align: center;"
      ),
      h5("Database-derived",
        style = "text-align: center;"
      ),
      dataTableOutput("organ_drug_db_table"),
      h4("Literature-derived",
        style = "text-align: center;"
      ),
      dataTableOutput("organ_drug_lit_table")
    )
  )
)
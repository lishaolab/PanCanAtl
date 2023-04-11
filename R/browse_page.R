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

browse_page <- fluidPage(
  sidebarPanel(
    width = 3,
    jstreeOutput("browse_tree_organ"),
    jstreeOutput("browse_tree_bulk"),
  ),
  sidebarPanel(
    width = 9,
    prettyRadioButtons(
      "sc_bulk_choice",
      choices = c(
        "curated data",
        "bulk",
        "single-cell"
      ),
      label = "select experiment type",
      inline = TRUE
    ),
    DT::dataTableOutput(
      outputId = "data_overview"
    )
  )
)

make_brwose_disease_page <- function(dn, ns, vs) {
  fluidPage(
    h3(dn, style = "text-align: center;"),
    tags$table(
      style = "width: 100%;",
      lapply(3:length(ns), function(i) {
        tags$tr(
          tags$th(
            style = "
            text-align: right;
            font-weight: bold;
            width: 50%;
            padding: 10px;
            margin-right: -10px;
          ",
            ns[i]
          ),
          tags$td(style = "padding: 10px;", HTML(vs[i]))
        )
      })
    )
  )
}

browse_bulk_page <- fluidPage(
  h3("Bulk data detials", style = "text-align: center;"),
  textOutput("bulk_data_id_text"),
  fluidRow(
    column(
      width = 6,
      h4("Sample counts across premalignant lesions",
        style = "text-align: center;"
      ),
      highchartOutput("bulk_sample_hc"),
      h4("DEGs for each premalignant lesion",
        style = "text-align: center;"
      ),
      selectInput(
        "select_bulk_deg",
        label = "Select a group pair",
        choices = NULL
      ),
      h5("Volcano plot for DEGs",
        style = "text-align: center;"
      ),
      highchartOutput("bulk_volcano_hc"),
      h5("The DEG table",
        style = "text-align: center;"
      ),
      DT::dataTableOutput(
        outputId = "bulk_deg_table"
      ),
      p(
        "Notes: DEG: Differently Expressed Genes ",
        "(Premalignant lesion vs Normal control;
      FDR < 0.05 & fold change > 1.5, student t-test)",
        style = "text-align:justify;color:black;background-color:lavender;
      padding:15px;border-radius:10px"
      )
    ),
    column(
      width = 6,
      h4("Dynamic genes across premalignant lesions",
        style = "text-align: center;"
      ),
      h5("Line chart for top 5 dynamic genes (mean expression)",
        style = "text-align: center;"
      ),
      highchartOutput("bulk_dynamic_hc"),
      h5("The dynamic gene table",
        style = "text-align: center;"
      ),
      DT::dataTableOutput(
        outputId = "bulk_dynamic_table"
      ),
      p(
        "Notes: Dynamic genes referred to genes whose expression show gradually
      increase (UP) or  decrease (DOWN) trend along the evolution of
      premalignant diseases (FDR < 0.05, anova test)",
        style = "text-align:justify;color:black;background-color:lavender;
      padding:15px;border-radius:10px"
      ),
      h4("TME cell types deconvolution analysis",
        style = "text-align: center;"
      ),
      h5("Column plot for deconvoluted TME cells",
        style = "text-align: center;"
      ),
      highchartOutput("bulk_tme_box_hc"),
      h5("The deconvoluted TME cell table",
        style = "text-align: center;"
      ),
      DT::dataTableOutput(
        outputId = "bulk_tme_table"
      ),
      p(
        "Notes: The deconvolution analysis was performed by the
        CIBERSORTx platform (https://cibersortx.stanford.edu/).
        TME, tumor microenvironment.",
        style = "text-align:justify;color:black;background-color:lavender;
      padding:15px;border-radius:10px"
      )
    )
  )
)

browse_sc_page <- fluidPage(
  h3("Single-cell data detials", style = "text-align: center;"),
  textOutput("sc_data_id_text"),
  fluidRow(
    column(
      width = 6,
      h4("General information",
        style = "text-align: center;"
      ),
      fluidRow(
        tags$style(".highchart-container {min-height: 400px;}"),
        column(
          width = 6,
          h4("Cell clusters",
            style = "text-align: center;"
          ),
          highchartOutput("sc_cluster_hc")
        ),
        column(
          width = 6,
          h4("Disease lesions",
            style = "text-align: center;"
          ),
          highchartOutput("sc_lesion_hc")
        )
      ),
      h4("Putative cell marker analysis ",
        style = "text-align: center;"
      ),
      h5("Violin plot for top 2 putative markers across cell clusters",
        style = "text-align: center;"
      ),
      highchartOutput("sc_marker_hc"),
      h5("Putative cell marker table",
        style = "text-align: center;"
      ),
      DT::dataTableOutput(
        outputId = "sc_marker_table"
      )
    ),
    column(
      width = 6,
      h4("Cellular proportion analysis",
        style = "text-align: center;"
      ),
      h5("Boxplot of cellular proportions across lesions",
        style = "text-align: center;"
      ),
      highchartOutput("sc_proportion_hc"),
      h5("The table of complete cellular proportions across lesions",
        style = "text-align: center;"
      ),
      DT::dataTableOutput(
        outputId = "sc_proportion_table"
      ),
      h4("Pseudo-tumorigenesis trajectory analysis",
        style = "text-align: center;"
      ),
      h5("The putative pseudo-tumorigenesis trajectory",
        style = "text-align: center;"
      ),
      highchartOutput("sc_traj_hc"),
      h5("Table of  putative cellular dynamic genes",
        style = "text-align: center;"
      ),
      DT::dataTableOutput(
        outputId = "sc_traj_table"
      ),
      p(
        "Notes: The pseudo-tumorigenesis trajectory analysis was
        performed by the Monocle platform (http://cole-trapnell-lab.
        github.io/monocle-release/) and the putative cellular
         dynamic genes referred to those genes whose expression
          showed significant gradual pattern along the putative
           pseudo-tumorigenesis trajectory (pseudotime)",
        style = "text-align:justify;color:black;background-color:lavender;
      padding:15px;border-radius:10px"
      )
    )
  )
)
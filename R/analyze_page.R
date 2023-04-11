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

analyze_page <- fluidPage(
  h3("Brief introduction to the analysis modules in PreAtlas",
    style = "text-align: center;"
  ),
  tags$style(HTML("
    .clickable-image {
      cursor: pointer;
    }

    .image-container {
      border: 1px solid black;
      padding: 10px;
      text-align: center;
    }
  ")),
  p(
    "Here, four interactive analytical and visualization modules haven been
    packaged in PreAtlas to in-depth investigate these datasets, including
    Gene Expression Analyze (GEA), Cellular Component Analyze (CCA),
    Dynamic Gene Analyze (DGA) and Multiple Network analyze (MNA).
    Here, the GEA module enable to dissect the gene-level distribution
    across diverse cell types and/or pathological lesions while the CCA
    Module enable to dissect the cellular-level distribution based on either
    cell type annotations within scRNA-seq data or deconvoluted TME cells
    within the bulk transcriptomic data. Of note, the DBA module could identify
    premalignant-related dynamic genes as potential cancer risk-screening and
    early-diagnosis biomarkers, which were defined as those showing gradually
    dysregulated (increase or decrease) expression patterns along the
    pathologically dynamic evolution or putative pseudo-tumorigenesis
    trajectories. Finally, the MNA module enable to perform the network
    analysis for systematically associating premalignant-related multiple
    information, including diseases, genes, cell types and drugs, allowing
    for uncovering the evolution and intervention mechanism underlying
    premalignant diseases from the holistic view.
    ",
    style = "text-align:justify;color:black;background-color:lavender;
          padding:15px;border-radius:10px"
  ),
  fluidRow(
    column(
      4,
      div(
        class = "image-container",
        tags$h4("Gene Expression Analyze (GEA)",
          style = "text-align: center;"
        ),
        tags$img(
          class = "clickable-image",
          src = "img/ana_1.png",
          width = "50%",
          onclick = paste0(
            "window.location.href='",
            route_link("analyze/GEA"),
            "'"
          )
        ),
        tags$br()
      ),
      tags$br(),
      div(
        class = "image-container",
        tags$h4("Dynamic Gene Analyze (DGA)",
          style = "text-align: center;"
        ),
        tags$img(
          class = "clickable-image",
          src = "img/ana_3.png",
          width = "50%",
          onclick = paste0(
            "window.location.href='",
            route_link("analyze/DGA"),
            "'"
          )
        ),
        tags$br()
      )
    ),
    column(
      4,
      tags$img(
        src = "img/analyze_centre.jpg",
        width = "100%"
      )
    ),
    column(
      4,
      div(
        class = "image-container",
        tags$h4("Cellular Component Analyze (CCA)",
          style = "text-align: center;"
        ),
        tags$img(
          class = "clickable-image",
          src = "img/ana_2.png",
          width = "50%",
          onclick = paste0(
            "window.location.href='",
            route_link("analyze/CCA"),
            "'"
          )
        ),
        tags$br()
      ),
      tags$br(),
      div(
        class = "image-container",
        tags$h4("Multiple Network Analyze (MNA)",
          style = "text-align: center;"
        ),
        tags$img(
          class = "clickable-image",
          src = "img/ana_4.png",
          width = "50%",
          onclick = paste0(
            "window.location.href='",
            route_link("analyze/MNA"),
            "'"
          )
        ),
        tags$br()
      )
    )
  )
)

gea_page <- fluidPage(
  h3("Gene Expression Analyze (GEA) Module",
    style = "text-align: center;"
  ),
  h4("Gene expression distribution across cellular clusters",
    style = "text-align: center;"
  ),
  fluidRow(
    column(
      width = 5,
      h4("UMAP plot", style = "text-align: center;"),
      highchartOutput("gea_umap_plot")
    ),
    column(
      width = 1,
      selectizeInput(
        "gea_umap_organ",
        "Organ",
        choices = NULL
      ),
      selectizeInput(
        "gea_umap_proj",
        "Project ID",
        choices = NULL
      ),
      selectizeInput(
        "gea_umap_gene",
        "Gene",
        choices = NULL
      ),
      selectizeInput(
        "gea_umap_lesion",
        "Lesions",
        choices = NULL,
        multiple = TRUE
      ),
      selectizeInput(
        "gea_umap_cluter",
        "Clusters",
        choices = NULL,
        multiple = TRUE
      )
    ),
    column(
      width = 5,
      h4("Box plot", style = "text-align: center;"),
      highchartOutput("gea_box_plot")
    ),
    column(
      width = 1,
      selectizeInput(
        "gea_box_organ",
        "Organ",
        choices = NULL
      ),
      selectizeInput(
        "gea_box_proj",
        "Project ID",
        choices = NULL
      ),
      selectizeInput(
        "gea_box_gene",
        "Gene",
        choices = NULL
      )
    )
  )
)


cca_page <- fluidPage(
  h3("Cellular Component Analyze (CCA) Module",
    style = "text-align: center;"
  ),
  h4("G Cellular distribution across lesions",
    style = "text-align: center;"
  ),
  fluidRow(
    column(
      width = 5,
      h4("Boxplot plot (CIBERSORTx based on bulk data)",
        style = "text-align: center;"
      ),
      highchartOutput("cca_bulk_hc")
    ),
    column(
      width = 1,
      selectizeInput(
        "cca_bulk_organ",
        "Organ",
        choices = NULL
      ),
      selectizeInput(
        "cca_bulk_proj",
        "Project ID",
        choices = NULL
      )
    ),
    column(
      width = 5,
      h4("Barplot plot (based on single-cell data)",
        style = "text-align: center;"
      ),
      highchartOutput("cca_sc_hc")
    ),
    column(
      width = 1,
      selectizeInput(
        "cca_sc_organ",
        "Organ",
        choices = NULL
      ),
      selectizeInput(
        "cca_sc_proj",
        "Project ID",
        choices = NULL
      )
    )
  )
)

dga_page <- fluidPage(
  h3("Dynamic Gene Analyze (DGA) Module",
    style = "text-align: center;"
  ),
  h4("Dynamic genes across lesions",
    style = "text-align: center;"
  ),
  fluidRow(
    column(
      width = 4,
      h4("Boxplot (bulk data)",
        style = "text-align: center;"
      ),
      fluidRow(
        column(
          width = 8,
          highchartOutput("dga_bulk_hc")
        ),
        column(
          width = 4,
          selectizeInput(
            "dga_bulk_organ",
            "Organ",
            choices = NULL
          ),
          selectizeInput(
            "dga_bulk_proj",
            "Project ID",
            choices = NULL
          ),
          selectizeInput(
            "dga_bulk_gene",
            "Gene",
            choices = NULL
          )
        )
      )
    ),
    column(
      width = 8,
      h4("Pseudo-timurigenesis plot (single-cell data)",
        style = "text-align: center;"
      ),
      fluidRow(
        column(
          width = 6,
          h5("Lesions",
            style = "text-align: center;"
          ),
          fluidRow(
            column(
              width = 8,
              highchartOutput("dga_sc_lesion_hc")
            ),
            column(
              width = 4,
              selectizeInput(
                "dga_sc_lesion_organ",
                "Organ",
                choices = NULL
              ),
              selectizeInput(
                "dga_sc_lesion_proj",
                "Project ID",
                choices = NULL
              )
            )
          )
        ),
        column(
          width = 6,
          h5("Expression",
            style = "text-align: center;"
          ),
          fluidRow(
            column(
              width = 8,
              highchartOutput("dga_sc_exp_hc")
            ),
            column(
              width = 2,
              selectizeInput(
                "dga_sc_exp_organ",
                "Organ",
                choices = NULL
              ),
              selectizeInput(
                "dga_sc_exp_proj",
                "Project ID",
                choices = NULL
              ),
              selectizeInput(
                "dga_sc_exp_gene",
                "Gene",
                choices = NULL
              )
            )
          )
        )
      )
    )
  )
)

mna_page <- fluidPage(
  h3("Multiple Network Analyze (MNA) Module",
    style = "text-align: center;"
  ),
  h4("Associating disease, genes, cell types and drugs",
    style = "text-align: center;"),
  fluidRow(
    column(
      width = 8,
      highchartOutput("mna_hc")
    ),
    column(
      width = 4,
      selectizeInput(
        "mna_organ",
        "Organ",
        choices = NULL
      ),
      selectizeInput(
        "mna_proj",
        "Project ID",
        choices = NULL
      ),
    )
  )
)

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

options(shiny.port = 7777)
options(shiny.host = "166.111.130.215")

all_data <- readRDS("data/all_data.rds")

home_page <- fluidPage(
  p("Through this application, it is intended to develop a learning
     environment for anyone who is starting in the study of statistical
      modeling, specifically linear regression through the method of
       ordinary least squares. In any case, we will focus on the procedure
        (graphics, interpretations, hypotheses, etc.) and not on the
         mathematical processes.",
    strong("But do not worry!"),
    "you will find alternatives
          to learn all these technical aspects independently.",
    style = "text-align:justify;color:black;background-color:lavender;
        padding:15px;border-radius:10px"
  ),
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
    # column(
    #     highchartOutput("bodymap_hc", width = "50%"),
    #     width = 6,
    #     style = "justify-content:right"),
    column(
      tags$img(src = "img/data_overview.png", width = "100%"),
      width = 6
    )
  ),
  style = "padding-left:40px;padding-right:40px"
)

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
              choices = c("GeneA", "GeneB", "GeneC"),
              multiple = FALSE,
              width = "100%",
              options = list(create = TRUE)
            ),
            tags$button(
              icon("search"),
              class = "search-btn",
              id = "search_gene_btn",
              onclick = "Shiny.onInputChange('searchBtnClicked', Math.random())"
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
      selectizeInput(
        inputId = "search_organ",
        label = "Organ",
        choices = c(""),
        options = list(
          valueField = "id",
          labelField = "name",
          searchField = "name",
          create = FALSE,
          maxItems = 1,
          placeholder = "Input organ"
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

pages <- list()
pages[["/"]] <- home_page
pages[["browse"]] <- browse_page
for (i in 1:nrow(all_data[[4]])) {
  pages[[paste0("browse/", all_data[[4]]$ID[i])]] <-
    make_brwose_disease_page(
      all_data[[4]]$`Disease name`[i],
      colnames(all_data[[4]]),
      as.character(all_data[[4]][i, ])
    )
}
pages[["browse/bulk"]] <- browse_bulk_page
pages[["browse/sc"]] <- browse_sc_page
pages[["search"]] <- search_page
pages[["search/gene"]] <- search_gene_page

routes_list <- lapply(seq_along(pages), function(i) {
  route(names(pages)[i], pages[[i]])
})

router <- do.call(make_router, routes_list)


make_nodes <- function(leaves) {
  dfs <- lapply(strsplit(leaves, "/"), function(s) {
    item <-
      Reduce(function(a, b) paste0(a, "/", b),
        s[-1], s[1],
        accumulate = TRUE
      )
    data.frame(
      item = item,
      parent = c("root", item[-length(item)]),
      stringsAsFactors = FALSE
    )
  })
  dat <- dfs[[1]]
  for (i in 2:length(dfs)) {
    dat <- merge(dat, dfs[[i]], all = TRUE)
  }
  f <- function(parent) {
    i <- match(parent, dat$item)
    item <- dat$item[i]
    children <- dat$item[dat$parent == item]
    label <- tail(strsplit(item, "/")[[1]], 1)
    if (length(children)) {
      list(
        text = label,
        data = list(value = item),
        state = list(opened = TRUE),
        children = lapply(children, f)
      )
    } else {
      list(text = label, data = list(value = item))
    }
  }
  lapply(dat$item[dat$parent == "root"], f)
}

ui <- fluidPage(
  useShinyjs(),
  div(tags$img(src = "img/banner.jpg", width = "100%")),
  tags$ul(
    tags$li(a(href = route_link("/"), icon("home"), "Home")),
    tags$li(a(href = route_link("browse"), icon("eye"), "Browse")),
    tags$li(a(href = route_link("search"), icon("search"), "Search")),
    tags$li(a(href = route_link("analyze"), icon("laptop"), "Analyze")),
    tags$li(a(href = route_link("download"), icon("download"), "Download")),
    tags$li(a(
      href = route_link("documentation"), icon("book"),
      "Documentation"
    )),
    tags$li(a(href = route_link("contact"), icon("envelope"), "Contact")),
    tags$style(HTML("
      ul {
        background-color: #001529;
          display: flex;
        justify-content: center;
        list-style-type: none;
      }

      ul li a {
        color: #ffffff;
          display: block;
        font-size: 1.2rem;
        padding: 1.5rem 1.6rem;
        text-decoration: none;
        transition: all, 0.1s;
      }

      ul li a:link, ul li a:visited, ul li a:hover, ul li a:active {
        color: #ffffff;
          text-decoration: none;
      }

      ul li a:hover {
        background-color: #0A81AB;
          color: #ffffff;
      }
  "))
  ),
  tags$style(HTML("
    .jstree-container-ul {
      background-color: transparent !important;
    }
    .jstree-container-ul .jstree-node {
      background-color: transparent !important;
    }
    .jstree-container-ul .jstree-children {
      background-color: transparent !important;
    }
  ")),
  tags$style(HTML("
    .jstree-anchor {
      font-family: Arial, sans-serif;
    }
  ")),
  router$ui
)




server <- function(input, output, session) {
  router$server(input, output, session)
  ### browse_page
  output$browse_tree_organ <- renderJstree(
    jstree(
      make_nodes(
        str_c(
          "Pathological stages & lesions",
          unique(all_data[[1]]$Organ),
          # all_data[[1]]$`Disease Name`,
          sep = "/"
        )
      ),
      search = FALSE,
      checkboxes = TRUE,
      theme = "proton"
    )
  )

  observeEvent(input$sc_bulk_choice, {
    observeEvent(input$browse_tree_organ_selected, {
      mapping <- c(
        "curated data" = 1,
        "bulk" = 2,
        "single-cell" = 3
      )
      organ_overview <- purrr::map_chr(
        input$browse_tree_organ_selected,
        ~ .$text
      )
      if (length(organ_overview) == 0) {
        organ_overview <-
          unique(all_data[[mapping[[input$sc_bulk_choice]]]]$Organ)
      }
      data_for_overview <-
        all_data[[mapping[[input$sc_bulk_choice]]]] %>%
        filter(Organ %in% organ_overview)
      output$data_overview <- DT::renderDataTable({
        DT::datatable(
          data_for_overview,
          options = list(
            searching = FALSE,
            pageLength = 100,
            ordering = FALSE,
            info = FALSE,
            scrollX = TRUE,
            scrollY = "300px",
            scrollCollapse = TRUE,
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print")
          ),
          escape = FALSE,
          selection = "none"
        )
      })
    })
  })

  # Browse page for sc and bulk
  observe({
    query_param <- shiny.router::get_query_param()
    ## Render bulk data according to bulk_dataset
    if (!is.null(query_param$bulk_dataset)) {
      bulk_id <- query_param$bulk_dataset[1]
      if (bulk_id %in% all_data[[2]]$`Project ID`) {
        output$bulk_data_id_text <- renderText({
          paste0("Dataset ID: ", bulk_id)
        })
        ### pie chart sample
        output$bulk_sample_hc <- renderHighchart(
          all_data[[5]] %>%
            filter(`Project ID` %in% bulk_id) %>%
            mutate(
              n = `Sample Count`,
            ) %>%
            hchart("pie", hcaes(name = `Disease Name`, y = `Sample Count`)) %>%
            hc_tooltip(
              useHTML = T,
              formatter = JS("
              function() {
                  outHTML = '<br> <b>Number</b>: ' + this.point.n
                  return(outHTML)
              }
              ")
            )
        )

        ### DEG
        #### select lesion
        updateSelectInput(
          session,
          "select_bulk_deg",
          label = "Select a group pair",
          choices = all_data[[5]] %>%
            filter(`Project ID` %in% bulk_id & `Disease Name` != "Normal") %>%
            pull(`Disease Name`),
          selected = all_data[[5]] %>%
            filter(`Project ID` %in% bulk_id & `Disease Name` != "Normal") %>%
            pull(`Disease Name`) %>%
            .[1]
        )
        observeEvent(input$select_bulk_deg, {
          deg_data <- all_data[[6]] %>%
            filter(`Project ID` %in% bulk_id) %>%
            filter(`Disease Name` %in% input$select_bulk_deg)
          #### vocalno chart
          output$bulk_volcano_hc <- renderHighchart(
            deg_data %>%
              hchart(
                "scatter",
                hcaes(
                  x = log2FoldChange,
                  y = pvalue,
                  group = up_down
                )
              ) %>%
              hc_colors(c("navy", "gray", "darkred")) %>%
              hc_tooltip(
                useHTML = T,
                formatter = JS("
                function() {
                    outHTML = '<b>Gene</b>: ' + this.point.Gene +
                    '<br> <b>log2FoldChange</b>: ' + this.point.log2FoldChange +
                    '<br> <b>pvalue</b>: ' + this.point.pvalue
                    return(outHTML)
                }
                ")
              ) %>%
              hc_plotOptions(
                scatter = list(
                  marker = list(
                    radius = 2.5
                  )
                )
              )
              %>%
              hc_xAxis(
                title = list(text = "log2FoldChange")
              ) %>%
              hc_yAxis(
                title = list(text = "-log10(pvalue)")
              )
          )
          #### DEG table
          output$bulk_deg_table <- DT::renderDataTable({
            DT::datatable(
              deg_data,
              options = list(
                pageLength = 100,
                info = FALSE,
                scrollX = TRUE,
                scrollY = "300px",
                scrollCollapse = TRUE,
                dom = "Bfrtip",
                buttons = c("copy", "csv", "excel", "pdf", "print")
              ),
              escape = FALSE,
              selection = "none"
            )
          })
        })

        ### Dynamic
        plot_data <- all_data[[7]] %>%
          filter(`Project ID` %in% bulk_id)
        is_log <- max(plot_data$expr_mean) > 1000
        #### line chart
        output$bulk_dynamic_hc <- renderHighchart({
          hchart(plot_data, "line",
            hcaes(
              x = `Disease Name`,
              y = expr_mean,
              group = Gene,
              name = Gene
            ),
            showInLegend = TRUE
          ) %>%
            hc_yAxis(
              title = list(text = "Mean Expression"),
              type = ifelse(is_log, "logarithmic", "spline")
            ) %>%
            hc_xAxis(title = list(text = "Stages")) %>%
            hc_tooltip(
              useHTML = T,
              formatter = JS("
                function() {
                    outHTML = '<br> <b>Gene</b>: ' + this.point.Gene +
                      '<br> <b>Mean Expression</b>: ' + this.point.expr_mean +
                      '<br> <b>Direction</b>: ' + this.point.Direction +
                      '<br> <b>P-value (FDR)</b>: ' + this.point.FDR
                    return(outHTML)
                }
            ")
            )
        })
        #### dynamic table
        output$bulk_dynamic_table <- DT::renderDataTable({
          DT::datatable(
            plot_data %>%
              group_by(Gene, Organ, `Project ID`, Direction, FDR) %>%
              pivot_wider(names_from = `Disease Name`, values_from = expr_mean),
            options = list(
              pageLength = 100,
              info = FALSE,
              scrollX = TRUE,
              scrollY = "300px",
              scrollCollapse = TRUE,
              dom = "Bfrtip",
              buttons = c("copy", "csv", "excel", "pdf", "print")
            ),
            escape = FALSE,
            selection = "none"
          )
        })
        ### TME
        #### TME proportions
        bulk_tme_data <- all_data[[9]] %>%
          filter(`Project ID` %in% bulk_id)
        output$bulk_tme_box_hc <- renderHighchart({
          highchart() %>%
            hc_chart(type = "column") %>%
            hc_xAxis(categories = unique(plot_data$`Disease Name`)) %>%
            hc_yAxis(title = list(text = "Proportion"), min = 0, max = 1) %>%
            hc_plotOptions(column = list(stacking = "normal")) %>%
            hc_add_series(
              bulk_tme_data, "column",
              hcaes(x = `Disease Name`, y = Composition, group = `Cell Type`)
            ) %>%
            hc_plotOptions(
              column = list(
                stacking = "normal",
                pointPadding = 0
              )
            ) %>%
            hc_legend(reversed = TRUE)
        })
        #### TME table
        output$bulk_tme_table <- DT::renderDataTable({
          DT::datatable(
            bulk_tme_data %>%
              group_by(`Project ID`, `Cell Type`, Organ) %>%
              pivot_wider(
                names_from = `Disease Name`,
                values_from = Composition
              ),
            options = list(
              pageLength = 100,
              info = FALSE,
              scrollX = TRUE,
              scrollY = "300px",
              scrollCollapse = TRUE,
              dom = "Bfrtip",
              buttons = c("copy", "csv", "excel", "pdf", "print")
            ),
            escape = FALSE,
            selection = "none"
          )
        })
      }
    }


    ## Render sc data according to sc_dataset
    if (!is.null(query_param$sc_dataset)) {
      sc_id <- query_param$sc_dataset[1]
      if (sc_id %in% all_data[[3]]$`Project ID`) {
        print(sc_id)
        output$sc_data_id_text <- renderText({
          paste0("Dataset ID: ", sc_id)
        })
        output$sc_cluster_hc <- renderHighchart({
          all_data[[11]] %>%
            filter(`Project ID` == sc_id) %>%
            hchart(
              "scatter",
              hcaes(
                x = TSNE_1,
                y = TSNE_2,
                group = cell_type
              )
            ) %>%
            hc_tooltip(
              useHTML = T,
              formatter = JS("
                function() {
                    outHTML = '<b>Cell ID</b>: ' + this.point.cell_id +
                    '<br> <b>Disease lesion</b>: ' + this.point.lesion +
                    '<br> <b>Soruce organ</b>: ' + this.point.Organ +
                    '<br> <b>Cell type</b>: ' + this.point.cell_type
                    return(outHTML)
                }
                ")
            ) %>%
            hc_plotOptions(
              scatter = list(
                marker = list(
                  radius = 2.5
                )
              )
            ) %>%
            hc_xAxis(
              title = list(text = "Component 1")
            ) %>%
            hc_yAxis(
              title = list(text = "Component 2")
            )
        })
        output$sc_lesion_hc <- renderHighchart({
          all_data[[11]] %>%
            filter(`Project ID` == sc_id) %>%
            hchart(
              "scatter",
              hcaes(
                x = TSNE_1,
                y = TSNE_2,
                group = lesion
              )
            ) %>%
            hc_tooltip(
              useHTML = T,
              formatter = JS("
                function() {
                    outHTML = '<b>Cell ID</b>: ' + this.point.cell_id +
                    '<br> <b>Disease lesion</b>: ' + this.point.lesion +
                    '<br> <b>Soruce organ</b>: ' + this.point.Organ +
                    '<br> <b>Cell type</b>: ' + this.point.cell_type
                    return(outHTML)
                }
                ")
            ) %>%
            hc_plotOptions(
              scatter = list(
                marker = list(
                  radius = 2.5
                )
              )
            ) %>%
            hc_xAxis(
              title = list(text = "Component 1")
            ) %>%
            hc_yAxis(
              title = list(text = "Component 2")
            )
        })
        ### Marker
        hp_data <- all_data[[12]] %>%
          filter(`Project ID` == sc_id) %>%
          mutate(
            cell_type_idx = match(cell_type, unique(cell_type)) - 1,
            Gene_idx = match(Gene, unique(Gene)) - 1,
            z_score = ave(expr_mean, Gene, FUN = function(x) {
              (x - mean(x)) / sd(x)
            })
          )


        output$sc_marker_hc <- renderHighchart({
          highchart() %>%
            hc_chart(type = "heatmap") %>%
            hc_title(text = "Heatmap") %>%
            hc_xAxis(
              title = list(text = "Cell type"),
              categories = unique(hp_data$cell_type)
            ) %>%
            hc_yAxis(
              title = list(text = "Marker gene"),
              categories = unique(hp_data$Gene)
            ) %>%
            hc_colorAxis(
              min = min(hp_data$z_score),
              max = max(hp_data$z_score),
              stops = color_stops(10, colors = c("purple", "black", "yellow"))
            ) %>%
            hc_add_series(
              data = list_parse2(hp_data[c(
                "cell_type_idx",
                "Gene_idx", "z_score"
              )]),
              mapping = hcaes(
                x = cell_type_idx,
                y = Gene_idx,
                value = z_score
              ),
              name = "Value",
              borderWidth = 1
            ) %>%
            hc_tooltip(
              useHTML = T,
              formatter = JS("
                function() {
                    outHTML = '<b>Gene</b>: ' +
                    this.point.series.yAxis.categories[this.point.y] +
                    '<br> <b>Cell type</b>: ' +
                    this.point.series.xAxis.categories[this.point.x] +
                    '<br> <b>Mean expression</b>: ' + this.point.value
                    return(outHTML)
                }
                ")
            )
        })
        output$sc_marker_table <- DT::renderDataTable({
          DT::datatable(
            all_data[[12]] %>%
              filter(`Project ID` == sc_id) %>%
              group_by(`Project ID`, Gene) %>%
              pivot_wider(
                names_from = cell_type,
                values_from = expr_mean
              ),
            options = list(
              pageLength = 100,
              info = FALSE,
              scrollX = TRUE,
              scrollY = "300px",
              scrollCollapse = TRUE,
              dom = "Bfrtip",
              buttons = c("copy", "csv", "excel", "pdf", "print")
            ),
            escape = FALSE,
            selection = "none"
          )
        })

        ### Cell proportions
        sc_ppt_data <- all_data[[14]] %>%
          filter(`Project ID` == sc_id)
        output$sc_proportion_hc <- renderHighchart({
          highchart() %>%
            hc_chart(type = "column") %>%
            hc_xAxis(categories = unique(sc_ppt_data$lesion)) %>%
            hc_yAxis(title = list(text = "Proportion"), min = 0, max = 1) %>%
            hc_plotOptions(column = list(stacking = "normal")) %>%
            hc_add_series(
              sc_ppt_data, "column",
              hcaes(
                x = lesion, y = cell_proportion,
                group = cell_type, custom = cell_count
              )
            ) %>%
            hc_plotOptions(
              column = list(
                stacking = "normal",
                pointPadding = 0
              )
            ) %>%
            hc_legend(reversed = TRUE)
        })
        output$sc_proportion_table <- DT::renderDataTable({
          DT::datatable(
            sc_ppt_data %>%
              group_by(cell_type, `Project ID`, Organ) %>%
              mutate(
                chi_square_test = NULL,
                cell_proportion = NULL
              ) %>%
              pivot_wider(
                names_from = lesion,
                values_from = cell_count
              ),
            options = list(
              pageLength = 100,
              info = FALSE,
              scrollX = TRUE,
              scrollY = "300px",
              scrollCollapse = TRUE,
              dom = "Bfrtip",
              buttons = c("copy", "csv", "excel", "pdf", "print")
            ),
            escape = FALSE,
            selection = "none"
          )
        })

        ### Traj
        sc_tjc_dt <- all_data[[15]] %>%
          filter(`Project ID` == "BLCA_sc_1")
        output$sc_traj_hc <- renderHighchart({
          sc_tjc_dt %>%
            hchart(
              "scatter",
              hcaes(
                x = component1,
                y = component2,
                group = lesion
              )
            ) %>%
            hc_tooltip(
              useHTML = T,
              formatter = JS("
                function() {
                    outHTML = '<b>Pseudotime</b>: ' + this.point.value +
                    '<br> <b>Disease lesion</b>: ' + this.point.lesion
                    return(outHTML)
                }
                ")
            ) %>%
            hc_plotOptions(
              scatter = list(
                marker = list(
                  radius = 2.5
                )
              )
            ) %>%
            hc_xAxis(
              title = list(text = "Component 1")
            ) %>%
            hc_yAxis(
              title = list(text = "Component 2")
            )
        })
        output$sc_traj_table <- DT::renderDataTable({
          DT::datatable(
            sc_tjc_dt,
            options = list(
              pageLength = 100,
              info = FALSE,
              scrollX = TRUE,
              scrollY = "300px",
              scrollCollapse = TRUE,
              dom = "Bfrtip",
              buttons = c("copy", "csv", "excel", "pdf", "print")
            ),
            escape = FALSE,
            selection = "none"
          )
        })
      }
    }


    ## Render search gene
    if (!is.null(query_param$gene_query)) {
      print(str_c("gene query listened! ", query_param$gene_query))
      query_gene <- query_param$gene_query[1]
      if (query_gene %in% all_data[[17]]$Gene) {
        output$gene_name_text <- renderText({
          query_gene
        })

        output$gene_basic_table <- renderUI({
          filtered_data <- all_data[[17]] %>%
            filter(Gene == query_gene)
          print(as.character(filtered_data[1,5]))
          table <- tags$table(
            style = "width: 100%;",
            lapply(seq_along(filtered_data), function(i) {
              tags$tr(
                tags$th(
                  style = "
            text-align: right;
            font-weight: bold;
            width: 50%;
            padding: 10px;
            margin-right: -10px;
          ",
                  colnames(filtered_data)[i]
                ),
                tags$td(
                  style = "padding: 10px;",
                  HTML(as.character(filtered_data[1, i]))
                )
              )
            })
          )

          table
        })

        output$gene_knowledge_table <- renderDataTable(
          datatable(
            all_data[[18]] %>%
              filter(Gene == query_gene),
            options = list(
              pageLength = 100,
              info = FALSE,
              scrollX = TRUE,
              scrollY = "300px",
              scrollCollapse = TRUE,
              dom = "Bfrtip",
              buttons = c("copy", "csv", "excel", "pdf", "print")
            ),
            escape = FALSE,
            selection = "none"
          )
        )
      }
    }
  })

  # Search page
  updateSelectizeInput(
    session,
    "search_gene",
    choices = unique(all_data[[17]]$Gene),
    server = T
  )
  observeEvent(input$searchBtnClicked, {
    target_url <- paste0(
      "#!/search/gene?gene_query=",
      input$search_gene
    )
    print(target_url)
    runjs(sprintf("window.open('%s', '_self')", target_url))
  })
}



shinyApp(ui, server)
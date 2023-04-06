suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(shinydashboard)
  library(shiny.router)
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
  observe({
    query_param <- shiny.router::get_query_param()
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
      }
    }
  })
}



shinyApp(ui, server)
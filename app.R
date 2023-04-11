options(shiny.port = 7777)
options(shiny.host = "166.111.130.215")

all_data <- readRDS("data/all_data.rds")


source("R/home_page.R")
source("R/browse_page.R")
source("R/search_page.R")
source("R/analyze_page.R")
source("R/other_pages.R")


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
pages[["search/organ"]] <- search_organ_page
pages[["analyze"]] <- analyze_page
pages[["analyze/GEA"]] <- gea_page
pages[["analyze/CCA"]] <- cca_page
pages[["analyze/DGA"]] <- dga_page
pages[["analyze/MNA"]] <- mna_page
pages[["download"]] <- download_page
pages[["contact"]] <- contact_page
pages[["documentation"]] <- document_page


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
   tags$head(
    # 设置网站图标
    tags$link(rel = "shortcut icon", href = "img/logo.ico"),
    # 设置页面标题
    tags$title("Premalignant Disease Atlas")
  ),
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


my_data_table <- function(data) {
  datatable(
    data,
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
}

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
        my_data_table(data_for_overview)
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
          output$bulk_deg_table <- renderDataTable(my_data_table(deg_data))
        })

        ### Dynamic
        plot_data <- all_data[[7]] %>%
          filter(`Project ID` %in% bulk_id)
        if (dim(plot_data)[1] > 0) {
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
        }
        #### dynamic table
        output$bulk_dynamic_table <- renderDataTable(
          my_data_table(plot_data %>%
            group_by(Gene, Organ, `Project ID`, Direction, FDR) %>%
            pivot_wider(
              names_from = `Disease Name`,
              values_from = expr_mean
            ))
        )
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
        output$bulk_tme_table <- renderDataTable(
          my_data_table(bulk_tme_data %>%
            group_by(`Project ID`, `Cell Type`, Organ) %>%
            pivot_wider(
              names_from = `Disease Name`,
              values_from = Composition
            ))
        )
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
        output$sc_marker_table <- renderDataTable(
          my_data_table(
            all_data[[12]] %>%
              filter(`Project ID` == sc_id) %>%
              group_by(`Project ID`, Gene) %>%
              pivot_wider(
                names_from = cell_type,
                values_from = expr_mean
              )
          )
        )

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
        output$sc_proportion_table <- renderDataTable(
          my_data_table(
            sc_ppt_data %>%
              group_by(cell_type, `Project ID`, Organ) %>%
              mutate(
                chi_square_test = NULL,
                cell_proportion = NULL
              ) %>%
              pivot_wider(
                names_from = lesion,
                values_from = cell_count
              )
          )
        )

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
        output$sc_traj_table <- renderDataTable(my_data_table(sc_tjc_dt))
      }
    }


    ## Render search gene
    if (!is.null(query_param$gene_query)) {
      query_gene <- query_param$gene_query[1]
      if (query_gene %in% all_data[[17]]$Gene) {
        output$gene_name_text <- renderText({
          query_gene
        })
        make_html_table <- function(data) {
          tags$table(
            style = "width: 100%;",
            lapply(seq_along(data), function(i) {
              tags$tr(
                tags$th(
                  style = "
            text-align: right;
            font-weight: bold;
            width: 50%;
            padding: 10px;
            margin-right: -10px;
          ",
                  colnames(data)[i]
                ),
                tags$td(
                  style = "padding: 10px;",
                  HTML(as.character(data[1, i]))
                )
              )
            })
          )
        }
        output$gene_basic_table <- renderUI({
          filtered_data <- all_data[[17]] %>%
            filter(Gene == query_gene)
          make_html_table(filtered_data)
        })

        output$gene_knowledge_table <- renderDataTable(
          my_data_table(
            all_data[[18]] %>%
              filter(Gene == query_gene)
          )
        )

        output$gene_omics_table <- renderDataTable(
          my_data_table(all_data[[6]] %>%
            filter(Gene == query_gene) %>%
            select(-Gene))
        )

        output$gene_celltype_table <- renderDataTable(
          my_data_table(all_data[[13]] %>%
            filter(Gene == query_gene) %>%
            select(-Gene))
        )

        output$gene_drug_table <- renderDataTable(
          all_data[[19]] %>%
            filter(Gene == query_gene) %>%
            select(-Gene) %>%
            my_data_table()
        )

        output$gene_network_table <- renderDataTable(
          all_data[[20]] %>%
            filter(Gene == query_gene) %>%
            select(-Gene) %>%
            my_data_table()
        )
      }
    }

    ## Render search organ
    if (!is.null(query_param$organ_query)) {
      query_organ <- query_param$organ_query[1]
      if (query_organ %in% all_data[[1]]$Organ) {
        output$organ_name_text <- renderText({
          query_organ
        })
        output$organ_bulk_table <- renderDataTable(
          my_data_table(
            all_data[[2]] %>%
              filter(Organ == query_organ)
          )
        )
        output$organ_sc_table <- renderDataTable(
          my_data_table(
            all_data[[3]] %>%
              filter(Organ == query_organ)
          )
        )
        output$organ_dyn_bulk_table <- renderDataTable(
          my_data_table(
            all_data[[9]] %>%
              filter(Organ == query_organ)
          )
        )
        output$organ_dyn_sc_table <- renderDataTable(
          my_data_table(
            all_data[[14]] %>%
              filter(Organ == query_organ)
          )
        )
        output$organ_dyng_bulk_table <- renderDataTable(
          my_data_table(
            all_data[[8]] %>%
              filter(Organ == query_organ)
          )
        )
        output$organ_dyng_sc_table <- renderDataTable(
          my_data_table(
            all_data[[16]] %>%
              filter(Organ == query_organ)
          )
        )
        output$organ_drug_db_table <- renderDataTable(
          my_data_table(
            all_data[[21]] %>%
              filter(Organ == query_organ)
          )
        )
        output$organ_drug_lit_table <- renderDataTable(
          my_data_table(
            all_data[[22]] %>%
              filter(Organ == query_organ)
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

  observeEvent(input$searchGeneBtnClicked, {
    target_url <- paste0(
      "#!/search/gene?gene_query=",
      input$search_gene
    )
    runjs(sprintf("window.open('%s', '_self')", target_url))
  })

  updateSelectizeInput(
    session,
    "search_organ",
    choices = unique(all_data[[1]]$Organ),
    server = T
  )
  observeEvent(input$searchOrganBtnClicked, {
    target_url <- paste0(
      "#!/search/organ?organ_query=",
      input$search_organ
    )
    runjs(sprintf("window.open('%s', '_self')", target_url))
  })


  # GEA
  ## GEA UMAP
  updateSelectizeInput(
    session,
    "gea_umap_organ",
    choices = unique(all_data[[23]]$Organ),
    server = T
  )

  observeEvent(input$gea_umap_organ, {
    data_filter_1 <- all_data[[23]] %>%
      filter(Organ == input$gea_umap_organ)
    updateSelectizeInput(
      session,
      "gea_umap_proj",
      choices = unique(data_filter_1 %>%
        pull(`Project ID`) %>%
        unlist()),
      server = T
    )

    observeEvent(input$gea_umap_proj, {
      data_filter_2 <- data_filter_1 %>%
        filter(`Project ID` == input$gea_umap_proj)
      updateSelectizeInput(
        session,
        "gea_umap_gene",
        choices = unique(data_filter_2 %>%
          pull(Gene) %>%
          unlist()),
        server = T
      )

      observeEvent(input$gea_umap_gene, {
        data_filter_3 <- data_filter_2 %>%
          filter(Gene == input$gea_umap_gene)
        updateSelectizeInput(
          session,
          "gea_umap_lesion",
          choices = unique(data_filter_3 %>%
            pull(lesion) %>%
            unlist()),
          selected = unique(data_filter_3 %>%
            pull(lesion) %>%
            unlist()),
          server = T
        )
        observeEvent(input$gea_umap_lesion, {
          data_filter_4 <- data_filter_3 %>%
            filter(lesion %in% input$gea_umap_lesion)
          updateSelectizeInput(
            session,
            "gea_umap_cluter",
            choices = unique(data_filter_4 %>%
              pull(cell_type) %>%
              unlist()),
            selected = unique(data_filter_4 %>%
              pull(cell_type) %>%
              unlist()),
            server = T
          )
          observeEvent(input$gea_umap_cluter, {
            data_filter_5 <- data_filter_4 %>%
              filter(cell_type %in% input$gea_umap_cluter)
            if (dim(data_filter_5)[1] > 0) {
              output$gea_umap_plot <- renderHighchart({
                data_filter_5 %>%
                  mutate(color = colorRampPalette(
                    c("lightgray", "purple")
                  )(100)[
                    cut(Value, breaks = 100)
                  ]) %>%
                  hchart(
                    "scatter",
                    hcaes(
                      x = umap_1,
                      y = umap_2,
                      color = color
                    )
                  ) %>%
                  # hc_title(text = paste0(
                  #   input$gea_umap_gene,
                  #   " expression in ",
                  #   input$gea_umap_proj,
                  # )) %>%
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
                  ) %>%
                  hc_colorAxis(
                    minColor = "lightgray",
                    maxColor = "purple"
                  )
              })
            }
          })
        })
      })
    })
  })

  ## GEA Boxplot

  updateSelectizeInput(
    session,
    "gea_box_organ",
    choices = unique(all_data[[30]]$Organ),
    server = T
  )

  observeEvent(input$gea_box_organ, {
    data_filter_1 <- all_data[[30]] %>%
      filter(Organ == input$gea_box_organ)
    updateSelectizeInput(
      session,
      "gea_box_proj",
      choices = unique(data_filter_1 %>%
        pull(`Project ID`) %>%
        unlist()),
      server = T
    )
    observeEvent(input$gea_box_proj, {
      data_filter_2 <- data_filter_1 %>%
        filter(`Project ID` == input$gea_box_proj)
      updateSelectizeInput(
        session,
        "gea_box_gene",
        choices = unique(data_filter_2 %>%
          pull(Gene) %>%
          unlist()),
        server = T
      )
      observeEvent(input$gea_box_gene, {
        if (dim(data_filter_2)[1] > 0) {
          output$gea_box_plot <- renderHighchart({
            hchart(
              data_filter_2 %>% filter(Gene == input$gea_box_gene),
              "boxplot",
              hcaes(
                x = cell_type,
                low = min,
                q1 = lower_quartile,
                median = median,
                q3 = upper_quartile,
                high = max,
                group = cell_type
              )
            ) %>%
              # hc_title(text = paste0(
              #   input$gea_box_gene,
              #   " expression in ",
              #   input$gea_box_proj,
              # )) %>%
              identity()
          })
        }
      })
    })
  })

  # CCA
  updateSelectizeInput(
    session,
    "cca_bulk_organ",
    choices = unique(all_data[[10]]$Organ),
    server = T
  )

  observeEvent(input$cca_bulk_organ, {
    data_filter_1 <- all_data[[10]] %>%
      filter(Organ == input$cca_bulk_organ)
    updateSelectizeInput(
      session,
      "cca_bulk_proj",
      choices = unique(data_filter_1 %>%
        pull(`Project ID`) %>%
        unlist()),
      server = T
    )

    observeEvent(input$cca_bulk_proj, {
      data_filter_2 <- data_filter_1 %>%
        filter(`Project ID` == input$cca_bulk_proj)
      if (dim(data_filter_2)[1] > 0) {
        output$cca_bulk_hc <- renderHighchart({
          highchart() %>%
            hc_chart(type = "column") %>%
            hc_xAxis(categories = unique(data_filter_2$`Disease Name`)) %>%
            hc_yAxis(title = list(text = "Proportion"), min = 0, max = 1) %>%
            hc_plotOptions(column = list(stacking = "normal")) %>%
            hc_add_series(
              data_filter_2, "column",
              hcaes(x = disease, y = Composition, group = `Cell Type`)
            ) %>%
            hc_plotOptions(
              column = list(
                stacking = "normal",
                pointPadding = 0
              )
            ) %>%
            hc_legend(reversed = TRUE)
        })
      }
    })
  })

  updateSelectizeInput(
    session,
    "cca_sc_organ",
    choices = unique(all_data[[14]]$Organ),
    server = T
  )

  observeEvent(input$cca_sc_organ, {
    data_filter_1 <- all_data[[14]] %>%
      filter(Organ == input$cca_sc_organ)
    updateSelectizeInput(
      session,
      "cca_sc_proj",
      choices = unique(data_filter_1 %>%
        pull(`Project ID`) %>%
        unlist()),
      server = T
    )
    observeEvent(input$cca_sc_proj, {
      data_filter_2 <- data_filter_1 %>%
        filter(`Project ID` == input$cca_sc_proj)
      if (dim(data_filter_2)[1] > 0) {
        output$cca_sc_hc <- renderHighchart({
          highchart() %>%
            hc_chart(type = "column") %>%
            hc_xAxis(categories = unique(data_filter_2$lesion)) %>%
            hc_yAxis(title = list(text = "Proportion"), min = 0, max = 1) %>%
            hc_plotOptions(column = list(stacking = "normal")) %>%
            hc_add_series(
              data_filter_2, "column",
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
      }
    })
  })

  # DGA
  updateSelectizeInput(
    session,
    "dga_bulk_organ",
    choices = unique(all_data[[24]]$Organ),
    server = T
  )

  observeEvent(input$dga_bulk_organ, {
    data_filter_1 <- all_data[[24]] %>%
      filter(Organ == input$dga_bulk_organ)
    updateSelectizeInput(
      session,
      "dga_bulk_proj",
      choices = unique(data_filter_1 %>%
        pull(`Project ID`) %>%
        unlist()),
      server = T
    )
    observeEvent(input$dga_bulk_proj, {
      data_filter_2 <- data_filter_1 %>%
        filter(`Project ID` == input$dga_bulk_proj)
      updateSelectizeInput(
        session,
        "dga_bulk_gene",
        choices = unique(data_filter_2 %>%
          pull(Gene) %>%
          unlist()),
        server = T
      )

      observeEvent(input$dga_bulk_gene, {
        data_filter_3 <- data_filter_2 %>%
          filter(Gene == input$dga_bulk_gene)
        if (dim(data_filter_3)[1] > 0) {
          output$dga_bulk_hc <- renderHighchart({
            highchart() %>%
              hc_add_series(
                data_filter_3,
                "boxplot",
                hcaes(
                  x = disease,
                  low = min,
                  q1 = lower_quartile,
                  median = median,
                  q3 = upper_quartile,
                  high = max,
                  group = lesion_name
                )
              ) %>%
              hc_add_series(
                data_filter_3,
                type = "line",
                hcaes(x = disease, y = mean)
              ) %>%
              hc_xAxis(
                categories = unique(data_filter_3$lesion_name),
                title = list(text = "Lesion Name")
              )
          })
        }
      })
    })
  })

  updateSelectizeInput(
    session,
    "dga_sc_lesion_organ",
    choices = unique(all_data[[25]]$Organ),
    server = T
  )

  observeEvent(input$dga_sc_lesion_organ, {
    data_filter_1 <- all_data[[25]] %>%
      filter(Organ == input$dga_sc_lesion_organ)
    updateSelectizeInput(
      session,
      "dga_sc_lesion_proj",
      choices = unique(data_filter_1 %>%
        pull(`Project ID`) %>%
        unlist()),
      server = T
    )

    # observeEvent(input$dga_sc_lesion_proj, {
    #   data_filter_2 <- data_filter_1 %>%
    #     filter(`Project ID` == input$dga_sc_lesion_proj)
    #   if (dim(data_filter_2)[1] > 0) {
    #     output$dga_sc_lesion_hc <- renderHighchart({
    #       data_filter_2 %>%
    #         select(-c(Gene, Expression_value)) %>%
    #         distinct() %>%
    #         hchart(
    #           "scatter",
    #           hcaes(
    #             x = component1,
    #             y = component2,
    #             group = Lesion
    #           )
    #         ) %>%
    #         hc_tooltip(
    #           useHTML = T,
    #           formatter = JS("
    #             function() {
    #                 outHTML = '<b>Pseudotime</b>: ' +
    #                  this.point.Pseudotime_Value +
    #                 '<br> <b>Disease lesion</b>: ' + this.point.Lesion
    #                 return(outHTML)
    #             }
    #             ")
    #         ) %>%
    #         hc_plotOptions(
    #           scatter = list(
    #             marker = list(
    #               radius = 2.5
    #             )
    #           )
    #         ) %>%
    #         hc_xAxis(
    #           title = list(text = "Component 1")
    #         ) %>%
    #         hc_yAxis(
    #           title = list(text = "Component 2")
    #         )
    #     })
    #   }
    # })
  })

  updateSelectizeInput(
    session,
    "dga_sc_exp_organ",
    choices = unique(all_data[[25]]$Organ),
    server = T
  )

  # observeEvent(input$dga_sc_exp_organ, {
  #   data_filter_1 <- all_data[[25]] %>%
  #     filter(Organ == input$dga_sc_exp_organ)
  #   updateSelectizeInput(
  #     session,
  #     "dga_sc_exp_proj",
  #     choices = unique(data_filter_1 %>%
  #       pull(`Project ID`) %>%
  #       unlist()),
  #     server = T
  #   )
  #   observeEvent(input$dga_sc_exp_proj, {
  #     data_filter_2 <- data_filter_1 %>%
  #       filter(`Project ID` == input$dga_sc_exp_proj)
  #     updateSelectizeInput(
  #       session,
  #       "dga_sc_exp_gene",
  #       choices = unique(data_filter_2 %>%
  #         pull(Gene) %>%
  #         unlist()),
  #       server = T
  #     )
  #     observeEvent(input$dga_sc_exp_gene, {
  #       data_filter_3 <- data_filter_2 %>%
  #         filter(Gene == input$dga_sc_exp_gene)
  #       if (dim(data_filter_3)[1] > 0) {
  #         output$dga_sc_exp_hc <- renderHighchart({
  #           data_filter_3 %>%
  #             arrange(Pseudotime_Value) %>%
  #             hchart(
  #               type = "spline",
  #               hcaes(x = Pseudotime_Value, y = fitted_expr)
  #             )
  #         })
  #       }
  #     })
  #   })
  # })


  updateSelectizeInput(
    session,
    "mna_organ",
    choices = unique(all_data[[26]]$Organ),
  )
}



shinyApp(ui, server)
suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(shinydashboard)
  library(shiny.router)
  library(stringr)
  library(tidyverse)
  library(jsTreeR)
  library(highcharter)
})

options(shiny.port = 7777)
options(shiny.host = "127.0.0.1")

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
    # checkboxGroupInput(
    #   inputId = "browse_organ",
    #   label = "Select organ",
    #   choices = Reduce(
    #     union,
    #     map(1:3, ~unique(all_data[[.]]$Organ))
    #   )
    # ),
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

make_browse_bulk_page <- fluidPage(
  h3(dn, style = "text-align: center;"),
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
      h5("Vocalno plot for DEGs",
        style = "text-align: center;"
      ),
      fluidRow(
        column(
          width = 6,
          highchartOutput("bulk_vocalno_hc")
        ),
        column(
          width = 6,
          selectInput(
            "select_bulk_deg",
            label = "Select a group pair",
            choices = NULL
          )
        )
      ),
      p(
        "Notes: DEG: Differently Expressed Genes ",
        "(Premalignant lesion vs Normal control;
      FDR < 0.05 & fold change > 1.5, student t-test)",
        style = "text-align:justify;color:black;background-color:lavender;
      padding:15px;border-radius:10px"
      ),
      h5("The DEG table",
        style = "text-align: center;"
      ),
      DT::dataTableOutput(
        outputId = "bulk_deg_table"
      )
    ),
    column(
      width = 6,
      h4("Dynamic genes across premalignant lesions",
        style = "text-align: center;"
      ),
      h5("Violin plot for top 5 dynamic genes",
        style = "text-align: center;"
      ),
      highchartOutput("bulk_violin_hc"),
      p(
      "Notes: Dynamic genes referred to genes whose expression show gradually
      increase (UP) or  decrease (DOWN) trend along the evolution of
      premalignant diseases (FDR < 0.05, anova test)",
        style = "text-align:justify;color:black;background-color:lavender;
      padding:15px;border-radius:10px"
      ),
      h5("The dynamic gene table",
        style = "text-align: center;"
      ),
      DT::dataTableOutput(
        outputId = "bulk_dynamic_table"
      ),
      h4("TME cell types deconvolution analysis",
        style = "text-align: center;"
      ),
      fluidRow(
        column(
          width = 6,
          h5("Violin plot for deconvoluted TME cells",
            style = "text-align: center;"
          ),
          highchartOutput("bulk_tme_violin_hc")
        ),
        column(
          width = 6,
          h5("Box plot for deconvoluted TME cells",
            style = "text-align: center;"
          ),
          highchartOutput("bulk_tme_box_hc")
        )
      ),
      p(
        "Notes: The deconvolution analysis was performed by the
        CIBERSORTx platform (https://cibersortx.stanford.edu/).
        TME, tumor microenvironment.",
        style = "text-align:justify;color:black;background-color:lavender;
      padding:15px;border-radius:10px"
      ),
      h5("The deconvoluted TME cell table",
        style = "text-align: center;"
      ),
      DT::dataTableOutput(
        outputId = "bulk_tme_table"
      )
    )
  )
)


# router <- make_router(
#   route("/", home_page),
#   route("browse", browse_page),
#   lapply(1:nrow(all_data[[4]]), function(i) {
#     route(
#       paste0("browse/", all_data[[4]]$ID[i]),
#       get(paste0("brwose_disease_page_", all_data[[4]]$ID[i]))
#     )
#   })
#   # route("bulk_dataset", bulk_page)
#   # route("sc_dataset", sc_page),
#   # route("search", search_page)
#   # route("analyze", home_page),
#   # route("download", home_page),
#   # route("documentation", home_page),
#   # route("conctact", home_page)
# )

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
  tags$head(tags$script(HTML("
  $(document).on('shiny:connected', function(event) {
    var searchParams = new URLSearchParams(window.location.search);
    if (searchParams.has('bulk_dataset')) {
      Shiny.setInputValue('bulk_dataset', searchParams.get('bulk_dataset'));
    }
    if (searchParams.has('sc_dataset')) {
      Shiny.setInputValue('sc_dataset', searchParams.get('sc_dataset'));
    }
    if (searchParams.has('curated_disease')) {
      Shiny.setInputValue('curated_disease',
       searchParams.get('curated_disease'));
    }
  });
  "))),
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
      organ_overview <- map_chr(input$browse_tree_organ_selected, ~ .$text)
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
}


shinyApp(ui, server)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shiny.router)
library(stringr)
library(tidyverse)

options(shiny.port = 7777)

options(shiny.host = "127.0.0.1")

getwd()

if (FALSE) {
  all_data <- list()
  for (i in 1:26) {
      all_data[[as.character(i)]] <-
      readxl::read_excel(str_c("data/S", i, ".xlsx"))
  }
  saveRDS(all_data, "data/all_data.rds")
}
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
        padding:15px;border-radius:10px"),
    fluidRow(
        column(12,
            searchInput(
                inputId = "search", label = "Quick search",
                placeholder = "e.g. TP53, Intestinal metaplasia, xx drug, ...",
                btnSearch = icon("search"),
                width = "500px"
            )
        ), align = "center"
    ),
    fluidRow(
        # column(
        #     highchartOutput("bodymap_hc", width = "50%"),
        #     width = 6,
        #     style = "justify-content:right"),
        column(
            tags$img(src = "img/data_overview.png", width = "100%"),
            width = 6)
    ),
    style = "padding-left:40px;padding-right:40px"
)

browse_page <- fluidPage(
  sidebarPanel(
    width = 3,
    checkboxGroupInput(
      inputId = "browse_organ",
      label = "Select organ",
      choices = Reduce(
        union,
        map(1:3, ~unique(all_data[[.]]$Organ))
      )
    ),
    # jstreeOutput("browse_tree_organ"),
    # jstreeOutput("browse_tree_bulk"),
  ),
  sidebarPanel(
    width = 9,
    prettyRadioButtons(
      "sc_bulk_choice",
      choices = c(
        "curated data",
        "bulk",
        "single-cell"),
      label = "select experiment type",
      inline = TRUE
    ),
    DT::dataTableOutput(
      outputId = "data_overview"
    )
  )
)





router <- make_router(
    route("/", home_page),
    route("browse", browse_page)
    # route("bulk_dataset", bulk_page)
    # route("sc_dataset", sc_page),
    # route("search", search_page)
    # route("analyze", home_page),
    # route("download", home_page),
    # route("documentation", home_page),
    # route("conctact", home_page)
)

ui <- fluidPage(
    div(tags$img(src = "img/banner.jpg", width = "100%")),
    tags$ul(
        tags$li(a(href = route_link("/"), icon("home"), "Home")),
        tags$li(a(href = route_link("browse"), icon("eye"), "Browse")),
        tags$li(a(href = route_link("search"), icon("search"), "Search")),
        tags$li(a(href = route_link("analyze"), icon("laptop"), "Analyze")),
        tags$li(a(href = route_link("download"), icon("download"), "Download")),
        tags$li(a(href = route_link("documentation"), icon("book"),
         "Documentation")),
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
        "))),
    router$ui
)






server <- function(input, output, session) {
  router$server(input, output, session)
  observeEvent(list(input$sc_bulk_choice, input$browse_organ), {
    req(input$sc_bulk_choice, input$browse_organ)
    mapping <- c(
      "curated data" = 1,
      "bulk" = 2,
      "single-cell" = 3
    )
    print(unlist(input$browse_organ))
    print(length(input$browse_organ))
    print(class(unlist(input$browse_organ)))
    organ_for_overview  <- ((str(input$browse_organ)))
    data_for_overview <-
      all_data[[mapping[[input$sc_bulk_choice]]]] %>%
      filter(Organ %in% organ_for_overview)
    output$data_overview <- DT::renderDataTable({
      DT::datatable(
        data_for_overview,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "300px",
          scrollCollapse = TRUE,
          dom = "Bfrtip",
          buttons = c(
            "copy", "csv", "excel", "pdf", "print"
            )
        )
      )
    })
  }, ignoreNULL = FALSE)
}


shinyApp(ui, server)

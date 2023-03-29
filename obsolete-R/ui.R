library(shiny)
library(shinyWidgets)
library(shiny.router)

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
)

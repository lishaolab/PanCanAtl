source("R/global.R")

shinyUI(fluidPage(
    # includeCSS("www/main.css"),
    tags$ul(
        tags$li(a(href = route_link("/"), "Home")),
        tags$li(a(href = route_link("browse"), "Browse")),
        tags$li(a(href = route_link("gene"), "Gene")),
        tags$style(HTML("
            ul {
              background-color: #0099f9;
                display: flex;
              justify-content: flex-end;
              list-style-type: none;
            }

            ul li a {
              color: #ffffff;
                display: block;
              font-size: 1.6rem;
              padding: 1.5rem 1.6rem;
              text-decoration: none;
              transition: all, 0.1s;
            }

            ul li a:link, ul li a:visited, ul li a:hover, ul li a:active {
              color: #ffffff;
                text-decoration: none;
            }

            ul li a:hover {
              background-color: #1589d1;
                color: #ffffff;
            }
        "))
    ),
    router$ui
))

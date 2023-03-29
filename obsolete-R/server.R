library(shiny)
library(shiny.router)

router <- make_router(
    # route("/", home_page)
    route("home", "/home", renderPage(home_page))
    # route("browse", browse_page),
    # route("bulk_dataset", bulk_page),
    # route("sc_dataset", sc_page),
    # route("search", search_page)
    # route("analyze", home_page),
    # route("download", home_page),
    # route("documentation", home_page),
    # route("conctact", home_page)
)


server <- function(input, output, session) {
    router$server(input, output, session)
}

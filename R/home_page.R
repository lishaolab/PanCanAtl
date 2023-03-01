set_body_map_width <- 250
set_body_map_height <- 500

imageMap <- function(inputId, imgsrc, opts) {
    areas <- lapply(names(opts), function(n)
        shiny::tags$area(title = n, coords = opts[[n]],
            href = "#", shape = "poly"))
    js <- paste0("$(document).on('click', 'map area', function(evt) {
  evt.preventDefault();
  var val = evt.target.title;
  Shiny.onInputChange('", inputId, "', val);})")
    list(
        shiny::tags$img(width = set_body_map_width,
            height = set_body_map_height,
            src = imgsrc, usemap = paste0("#", inputId),
            shiny::tags$head(tags$script(shiny::HTML(js)))),
        shiny::tags$map(name = inputId, areas)
      )
}

organs <- c(
    # "leukocyte",
    "lung",
    "breast",
    "pancreas",
    "liver",
    "colon",
    "stomach",
    "esophagus",
    "kidney",
    "ovary",
    "uterine_cervix"
    # "parotid_gland"
)

all_organ_data <- gganatogram::hgFemale_list
all_organ_name <- names(all_organ_data)

organ_plot <- lapply(organs, function(each_organ) {
  all_organ_data[all_organ_name %in% each_organ] %>%
    reduce(rbind) %>%
    dplyr::transmute(x = set_body_map_width/106*X1,
      y = set_body_map_height/200*X2) %>%
    na.omit() %>% t %>% c %>%
    paste(collapse = ",") %>%
    identity()
})

names(organ_plot) <- organs <- c(organs[1:(length(organs) - 1)], "cervix")

home_page <- fluidPage(
    headerPanel("Home"),
    sidebarLayout(
        sidebarPanel(width = 4,
          fluidRow(box(selectInput("choose_organ", label = "Choose an organ",
            choices = organs, width = "100%")),
            imageMap("body_map", "imgs/test1.png", organ_plot))
        ),
        mainPanel(width = 8,
            column(width = 6,
                uiOutput("text_pml")
            ),
            column(width = 6,
                h1("Data distribution"),
                actionButton("homepage_browse_organ",
                    "Detailed dataset",
                    onclick = paste0("location.href='", route_link("browse"),
                        "';")))
        )
    )

)

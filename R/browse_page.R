browse_page <- fluidPage(
    sidebarLayout(
        sidebarPanel(width = 3,
            # checkboxGroupInput("data_type_select", "Data type", choices = c(
            #     "MicroArray", "RNA-seq", "scRNA-seq")),
            # checkboxGroupInput("cancer_type_select",
            #     "Cancer type", choices = c("BRCA, CESC, COAD, ESCA, HNSC, LAML,
            #         LUAD/LUSC, PAAD, UNKNOWN"))
            jstreeOutput("browse_tree_organ"),
            jstreeOutput("browse_tree_bulk"),
            ),
        sidebarPanel(width = 9,
            # fluidRow(column(6, actionButton("sc_browse")),
            #     column(6, actionButton("bulk_browse"))),
            shinyWidgets::prettyRadioButtons("sc_bulk_choose",
                choices = c("bulk", "single-cell"),
                "select experiment type", inline = T),
            DT::dataTableOutput(outputId = "data_overview"))
    )
)

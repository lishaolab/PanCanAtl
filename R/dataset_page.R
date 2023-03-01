dataset_page <- fluidPage(
    fluidRow(
        column(width = 4,
            # textInput("study_select", "Select a study"),
            highchartOutput("plot_pie_dataset")),
        column(width = 8,
            tabsetPanel(
                header = textOutput("tabset_panel_header_text"),
                # tabPanel("description",
                #     DT::dataTableOutput("table_description")),
                tabPanel("samples",
                    DT::dataTableOutput("table_samples")),
            ))),
    fluidRow(
        # tabsetPanel(
            # tabPanel("Defferentially expressed genes",
                fluidRow(
                    column(selectInput("select_deg_lesion_group",
                        label = "Select a group pair", choices = NULL),
                        width = 4),
                    column(width = 4),
                    column(selectInput("up_down_regulated",
                        label = "up & down",
                        choices = c("up-regulated", "down-regulated"),
                        selected = "up-regulated"),
                        width = 2),
                    column(selectInput("enrich_term",
                        label = "enrich term",
                        choices = c("GO", "KEGG"),
                        selected = "GO"), width = 2)),
                fluidRow(
                    column(width = 4, uiOutput("plot_deg_volcano")),
                    column(width = 4, uiOutput("plot_deg_heatmap")),
                    column(width = 4,
                        uiOutput("plot_enrichment"),
                        br()
                    )
                ),
                fluidRow(
                    column(width = 2),
                    column(DT::dataTableOutput("datatable_deg"), width = 8),
                    column(width = 2))),
            # tabPanel("TME analysis result",
            #     fluidRow(
            #         column(width = 3),
            #         column(plotOutput("plot_tme_heatmap"), width = 6),
            #         column(width = 3)),
            #     fluidRow(
            #         column(width = 2),
            #         column(DT::dataTableOutput("datatable_tme"), width = 8),
            #         column(width = 2))
            # )))

)

sc_page <- fluidRow(
    column(2),
    column(8,
        uiOutput("qc"),
        selectInput("reduction_select", "reduction",
            choices = c("tSNE", "UMAP"), selected = "tSNE"),
        highchartOutput("reduction_hc"),
        highchartOutput("cell_proportion_hc"),
        selectInput("select_cell_type",
            label = "select a cell type",
            choices = NULL, selected = NULL),
        dataTableOutput("sc_deg_table"),
        highchartOutput("cell_crosstalk")
    ),
    column(2)
)


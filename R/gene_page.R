gene_page <- div(
    titlePanel("Gene"),
    selectizeInput("gene_search", "Select a gene", selected = FALSE,
        multiple = FALSE, choices = NULL),
    # p("Under construction and designation..."),
    # fluidRow(column(12)),
    # tags$table(style = "width: 50%",
    #     tags$tr(tags$td(
    #         style = "width: 70%",
    #         textInput("gene_serach_text",
    #             label = "",
    #             placeholder = "Query a gene")),
    #         tags$td(
    #             style = "width: 30%",
    #             actionButton(inputId = "launch_gene_search", label = "GO")))),
    # fluidRow(
    #     column(width = 6,
    #         plotOutput("query_gene_deg_acc")),
    #     column(width = 6,
    #         plotOutput("query_gene_deg_logfc")),
    # )
    dataTableOutput("gene_info_table"),
    fluidRow(
        column(width = 6,
            fluidRow(
                column(width = 6,
                    selectizeInput("gene_sc_organ", "organ",
                    selected = F, choices = c(
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
                    )),
                    highchartOutput("gene_sc_cluster")),
                column(width = 6,
                    selectizeInput(
                        "gene_sc_dataset",
                        "sc dataset",
                        choices = NULL),
                    highchartOutput("gene_sc_scatter")
                )
            ),
        ),
        column(width = 6,
            fluidRow(
                selectizeInput("gene_bulk_dataset",
                    "bulk dataset", choices = NULL),
                highchartOutput("gene_bulk_box"))
        )
    )

)


if (F) {
    certain_gene <- "A1BG"


load("D:/Work/Projects/NAR2022/test01/test01/www/data/sc/GSE150290_Pat01/processed.RData")
dt <- processed@reductions$tsne@cell.embeddings %>% data.frame %>%
    bind_cols(processed@reductions$umap@cell.embeddings) %>%
    mutate(annt = processed$annotation) %>%
    bind_cols(data.frame(gene = processed@assays$RNA@data[certain_gene,]))
ii <- cut(dt$gene, breaks = seq(min(dt$gene), max(dt$gene), len = 100),
    include.lowest = T)
color_bar <- colorRampPalette(c("lightgray", "navy"))(99)


dt %>%
hchart('scatter', hcaes(x = tSNE_1, y = tSNE_2,
    color = color_bar[ii])) %>%
hc_colorAxis(min = min(dt$gene), max = max(dt$gene),
    stops = color_stops(10, color_bar)) %>%
hc_xAxis(
    plotLines = list(
        list(
            color = "#'FF0000",
            width = 2,
            value = 5.5))) %>%
hc_yAxis(lineWidth = 1, gridLineWidth = 0) %>%
hc_xAxis(lineWidth = 1, tickWidth = 0) %>%
hc_exporting(enabled = TRUE, buttons = list(contextButton = list(
    menuItems = c("downloadPNG", "downloadJPEG",
        "downloadPDF", "downloadSVG")
))) %>%
hc_tooltip(
    useHTML = T,
    formatter = JS(
        paste0("
        function() {
            outHTML = '<b>Cell type</b>: ' + this.point.annt +
        '<br> ", certain_gene, ": ' + this.point.gene.toFixed(2)
            return(outHTML)
        }
        ")
    ))

load("test01/www/data/GSE117606.RData")
bulk$expr_matrix
bulk$sample_info
write.csv(bulk$expr_matrix, file = "test01/www/data/bulk/GSE117606/expr_matrix.csv")
df <- data.frame(
    stages = factor(bulk$sample_info$stage1),
    expr_data = unlist(bulk$expr_matrix["A1BG", rownames(bulk$sample_info)]))

dat <- data_to_boxplot(pokemon, height, type_1)
highchart() %>%
    hc_xAxis(type = "category") %>%
    hc_add_series_list(data_to_boxplot(df,
        expr_data, stages, stages, color = "black",
        fillColor = ggsci::pal_npg("nrc",
            alpha = 0.7)(length(unique(df$stages)))
    ))




highchart() %>%
    hc_xAxis(type = "category") %>%
    hc_add_series_list(data_to_boxplot(df,
        expr_data, stages,
        group_var = stages, add_outliers = F,
        fillColor = ggsci::pal_npg("nrc",
            alpha = 0.7)(length(unique(df$stages))),
        color = "black"))
}



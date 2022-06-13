
source("R/global.R")

server <- function(input, output, session) {
    router$server(input, output, session)
    now_path <- reactive(getUrlHash())
    ### home_page
    observeEvent(input$body_map, {
        updateSelectInput(inputId = "choose_organ",
            selected = input$body_map)
    })

    observeEvent(input$choose_organ, {
        output$text_pml <- renderUI(HTML(paste0(
            "<h3>", "Premalignant lesions of ", input$choose_organ, ":</h3>",
            pml %>%
                dplyr::filter(organ == input$choose_organ) %>%
                pull(html) %>%
                paste(., collapse = ""))))
    })

    ### browse_page
    output$browse_tree_organ <- renderJstree(
        jstree(makeNodes(paste("Pathological stage & lesion",
            pml$organ, pml$lesion_name, sep = "/")),
            search = FALSE, checkboxes = TRUE, theme = "proton")
    )
    mode_bulk <- reactive(input$sc_bulk_choose == "bulk")
    observeEvent(mode_bulk(), {
        if (mode_bulk()) {
            output$browse_tree_bulk <- renderJstree(
                jstree(
                    makeNodes(paste("Data type", c("RNA-seq", "MicroArray"),
                        sep = "/")),
                    search = FALSE, checkboxes = TRUE, theme = "proton"
            ))
        } else {
            output$browse_tree_bulk <- renderJstree(div())
        }
    })
    sdt <- reactive({
        selected_dt <- sapply(input$browse_tree_bulk_selected,
            function(x) {x$text}
        )
        if (length(selected_dt) == 0) {
            c("MicroArray", "RNA-seq")
        } else {
            selected_dt
        }})
    sls <- reactive({
        selected_lesions <- sapply(input$browse_tree_organ_selected,
            function(x) {x$text}
        )
        if (length(selected_lesions) == 0) {
            pml$lesion_name
        } else {
            selected_lesions
        }})
    dataset_table <- reactive({
        if (mode_bulk()) {
            gse <- exp_lesions %>%
                dplyr::filter(lesion_name %in% sls()) %>%
                pull(geo_accession)
            dt <- exp_data %>%
                dplyr::filter(`Data type` %in% sdt()) %>%
                dplyr::filter(geo_accession %in% gse)
        } else {
            dt <- exp_data_sc
            # gse <- exp_lesions_sc %>%
            #     dplyr::filter(lesion_name %in% sls()) %>%
            #     pull(geo_accession)
            # dt <- exp_data_sc %>%
            # dplyr::filter(geo_accession %in% gse)
        }
        dt
    })

    my_table <- function(my_tbl) {
        DT::renderDataTable(datatable(
            my_tbl,
            escape = F, selection = "none", options = list(
                searching = FALSE, scrollX = TRUE)))
    }

    observeEvent(dataset_table, {
    print(dim(dataset_table()))
        output$data_overview <- DT::renderDataTable({
            datatable(dataset_table(),
                escape = F,
                selection = "none",
                options = list(scrollX = TRUE))
        })
    })
    observeEvent(input$browse_tree_organ_selected, {

    })



    ### Browse certain dataset
    observeEvent(input$select_button, {
        print(input$select_button)
        if (mode_bulk()) {
            output$tabset_panel_header_text <- renderText("GSE117606")
            output$up_down_regulated <- renderUI(selectInput("ud_select",
                label = "regulated",
                choices = c("up-regulated", "down-regulated"),
                selected = "down-regulated"))
            main_dir <- paste0("www/data/bulk/GSE117606/")
            lesions <- list.dirs(main_dir, full.names = F, recursive = F)
            updateSelectInput(session = session,
                inputId = "select_deg_lesion_group", choices = lesions,
                selected = lesions[1])
            current_dir <- paste0("www/data/bulk/GSE117606/",
                "conventional adenoma/")
            img_dir <- paste0("data/bulk/GSE117606/conventional adenoma/")
            # lesions <- list.dirs(current_dir, full.names = F, recursive = F)
            output$plot_deg_volcano <- renderUI(shiny::tags$img(
                src = paste0(img_dir, "volcano.svg")
            ))
            output$plot_deg_heatmap <- renderUI(shiny::tags$img(
                src = paste0(img_dir, "heatmap.svg")
            ))
            output$plot_enrichment <- renderUI(shiny::tags$img(
                src = paste0(img_dir, "up_go.svg")
            ))
            sample_info <- read.csv(paste0(main_dir, "sample_info.csv"),
                row.names = 1)
            output$plot_pie_dataset <- renderHighchart(
                sample_info %>%
                    group_by(stage1) %>%
                    summarise(n = n(), pct = n()/nrow(.) * 100) %>%
                    hchart("pie", hcaes(name = stage1, y = n)) %>%
                    hc_tooltip(
                        useHTML = T,
                        formatter = JS("
            function() {
                outHTML = '<b>Stage</b>: ' + this.point.stage1 +
                '<br> <b>Number</b>: ' + this.point.n +
                '<br> <b>Percentage</b>: ' + this.point.pct.toFixed(2) + '%'
                return(outHTML)
            }
            "))
            )
            output$table_samples <- renderDataTable(sample_info)
            DEG <- read.csv(paste0(current_dir, "deg.csv"), row.names = 1)
            output$datatable_deg <- my_table(DEG %>%
                filter(adj.P.Val < 0.05) %>%
                transmute(logFC = round(logFC, 3),
                    AveExpr = round(AveExpr, 3),
                    P.Value = format(P.Value, digits = 3),
                    adj.P.Val = format(adj.P.Val, digits = 3),
                    logP = round(logP, 3)) %>%
                rownames_to_column("Gene") %>%
                mutate(Gene = paste0("<a id=\"", Gene,
                    "\" href=\"#\" class=\"action-button\"
                    onclick=\"location.href=&quot;./#!/gene&quot;;
                    Shiny.onInputChange(&quot;select_gene_button&quot;
                    ,  this.id)\">", Gene, "</a>")))
        } else {
            current_dir <- paste0("www/data/sc/", input$select_button, "/")
            img_dir <- paste0("data/sc/", input$select_button, "/")
            cell_type <- list.dirs(current_dir, full.names = F, recursive = F)
            output$qc <- renderUI(shiny::tags$img(
                src = paste0(img_dir, "QC.svg")
            ))
            deg_s <- read.csv(paste0(current_dir, "deg.csv"), row.names = 1) %>%
                filter(avg_log2FC > 1 & p_val_adj < 0.05)
            cell_type <- unique((deg_s$cluster))
            updateSelectInput(inputId = "select_cell_type", choices = cell_type,
                selected = cell_type[1])
            observeEvent(input$select_cell_type, {
                output$sc_deg_table <- renderDataTable(
                    deg_s %>%
                        filter(cluster == input$select_cell_type) %>%
                        dplyr::rename("cell type" = "cluster")
                )
                output$sc_deg_table <- my_table(deg_s %>%
                        select(-gene) %>%
                        filter(cluster == input$select_cell_type) %>%
                        dplyr::rename("cell type" = "cluster") %>%
                        mutate(avg_log2FC = round(avg_log2FC, 3),
                            p_val = format(p_val, digits = 3),
                            p_val_adj = format(p_val_adj, digits = 3)) %>%
                        rownames_to_column("Gene") %>%
                        mutate(Gene = paste0("<a id=\"", Gene,
                            "\" href=\"#\" class=\"action-button\"
                    onclick=\"location.href=&quot;./#!/gene&quot;;
                    Shiny.onInputChange(&quot;select_gene_button&quot;
                    ,  this.id)\">", Gene, "</a>")) %>%
                        rename("logFC" = "avg_log2FC")
                    )
            })
            crosstalk_dt <- sapply(cell_type, function(x) {
                sapply(cell_type, function(y) {
                    ga <- deg_s %>% filter(cluster == x) %>% pull(gene)
                    gb <- deg_s %>% filter(cluster == y) %>% pull(gene)
                    ppi %>% filter((target %in% ga & source %in% gb) |
                            (target %in% gb & source %in% ga)) %>%
                        nrow
                })}) %>% data.frame() %>%
                rownames_to_column("from") %>%
                pivot_longer(cols = colnames(.) %>% .[2:length(.)],
                    names_to = "to", values_to = "weight")
            output$cell_crosstalk <- renderHighchart(
                crosstalk_dt %>%
                    hchart(type = 'dependencywheel') %>%
                    hc_exporting(enabled = TRUE, buttons = list(
                        contextButton = list(
                            menuItems = c("downloadPNG", "downloadJPEG",
                                "downloadPDF", "downloadSVG")
                        )))
            )
            # updateSelectInput(inputId = "select_deg_lesion_group",
            #     label = "Select cell type", choices = cell_type,
            #     selected = cell_type)
            processed <- get(load(paste0(current_dir, "processed.RData")))
            dt <- processed@reductions$tsne@cell.embeddings %>%
                data.frame %>%
                bind_cols(processed@reductions$umap@cell.embeddings) %>%
                bind_cols(processed@meta.data) %>%
                dplyr::rename("mt" = "percent.mt")
            output$cell_proportion_hc <- renderHighchart(
                dt %>% group_by(annotation) %>%
                    summarise(n = n(), pct = n()/nrow(.) * 100) %>%
                    hchart("pie", hcaes(name = annotation, y = n)) %>%
                    hc_tooltip(
                        useHTML = T,
                        formatter = JS("
                            function() {
                                outHTML = '<b>Stage</b>: ' +
                                this.point.annotation +
                                '<br> <b>Number</b>: ' + this.point.n +
                                '<br> <b>Percentage</b>: ' +
                                this.point.pct.toFixed(2) + '%'
                                return(outHTML)
                            }")) %>%
                    hc_exporting(enabled = TRUE, buttons = list(
                        contextButton = list(
                            menuItems = c("downloadPNG", "downloadJPEG",
                                "downloadPDF", "downloadSVG")
                        )))
            )
            observeEvent(input$reduction_select, {
                if (!mode_bulk()) {
                    x1 <- paste0(input$reduction_select, "_1")
                    x2 <- paste0(input$reduction_select, "_2")
                    if (input$reduction_select == "tSNE") {
                        dt <- dt %>% dplyr::rename(x1 = tSNE_1, x2 = tSNE_2)
                    } else {
                        dt <- dt %>% dplyr::rename(x1 = UMAP_1, x2 = UMAP_2)
                    }
                    output$reduction_hc <- renderHighchart(
                        dt %>% hchart('scatter', hcaes(x = x1, y = x2,
                            group = annotation)) %>%
                            hc_xAxis(
                                plotLines = list(
                                    list(
                                        color = "#'FF0000",
                                        width = 2,
                                        value = 5.5))) %>%
                            hc_yAxis(lineWidth = 1, gridLineWidth = 0) %>%
                            hc_xAxis(lineWidth = 1, tickWidth = 0) %>%
                            hc_exporting(enabled = TRUE, buttons = list(
                                contextButton = list(
                                    menuItems = c("downloadPNG", "downloadJPEG",
                                        "downloadPDF", "downloadSVG")
                                ))) %>%
                            hc_tooltip(
                                useHTML = T,
                                formatter = JS(
                                    paste0("
            function() {
                outHTML = '<b>Cell type</b>: ' + this.point.annotation +
                '<br> <b>RNA count</b>: ' + this.point.nCount_RNA +
                '<br> <b>Mt-percent</b>: ' + this.point.mt.toFixed(2) +
                '<br> <b>Seurat cluster</b>: ' + this.point.seurat_clusters +
                '<br> <b>patient</b>: ' + this.point.patient +
                '<br> <b>tissue</b>: ' + this.point.tissue
                return(outHTML)
            }
            ")
                                )) %>%
                            identity()
                    )
                }
            })
        }
    })

    observeEvent(c(input$select_deg_lesion_group, input$enrich_term), {
        if (mode_bulk()) {
            1
        } else {
            current_dir <- paste0("www/data/sc/", input$select_button, "/",
                input$select_deg_lesion_group, "/")
            img_dir <- paste0("data/sc/", input$select_button, "/",
                input$select_deg_lesion_group, "/")
            output$plot_enrichment <- renderUI(shiny::tags$img(
                src = paste0(img_dir, tolower(input$enrich_term), ".svg"),
                width = "100%"
            ))
        }
    })

    ### gene_page
    updateSelectizeInput(inputId = "gene_search",
        choices = gene_info$gene_symbol, server = T)
    updateSelectizeInput(inputId = "gene_sc_dataset",
        choices = exp_data_sc$ID, server = T)
    observeEvent(input$select_gene_button, {
        updateSelectizeInput(session = session, inputId = "gene_search",
            selected = input$select_gene_button)
    })
    observeEvent(input$gene_search, {
        output$gene_info_table <- renderDataTable(DT::datatable(gene_info %>%
                filter(gene_symbol == input$gene_search) %>%
                select(-gene_symbol) %>% t,
            selection = "none", escape = F, colnames = "", options = list(
                searching = F, paging = F, dom = "t",
                autoWidth = T,
                columDefs = list(list(width = "300px", targets = c(1))),
                headerCallback = JS(
                    "function(thead, data, start, end, display){",
                    "  $(thead).remove();",
                    "}")
            )))

    })
    observeEvent(c(input$gene_search, input$gene_sc_dataset), {
        if (input$gene_search != "") {
        processed <- get(load(paste0("www/data/sc/", input$gene_sc_dataset,
            "/processed.RData")))
        dt <- processed@reductions$tsne@cell.embeddings %>% data.frame %>%
            bind_cols(processed@reductions$umap@cell.embeddings) %>%
            mutate(annt = processed$annotation)
        if (input$gene_search %in% rownames(processed@assays$RNA@data)) {
            dt$gene <- processed@assays$RNA@data[input$gene_search,]
        } else {
            dt$gene <- 0
        }
        output$gene_sc_scatter <- renderHighchart({
            ii <- cut(dt$gene, breaks = seq(min(dt$gene),
                ifelse(max(dt$gene) > 0, max(dt$gene), 1), len = 100),
                include.lowest = T)
            color_bar <- colorRampPalette(c("lightgray", "navy"))(99)
            dt %>%
                mutate(my_color_bar = color_bar[ii]) %>%
                hchart('scatter', hcaes(x = tSNE_1, y = tSNE_2,
                    color = my_color_bar)) %>%
                hc_colorAxis(min = min(dt$gene),
                    max = ifelse(max(dt$gene) > 0, max(dt$gene), 1),
                    stops = color_stops(10, color_bar)) %>%
                hc_xAxis(
                    plotLines = list(
                        list(
                            color = "#'FF0000",
                            width = 2,
                            value = 5.5))) %>%
                hc_yAxis(lineWidth = 1, gridLineWidth = 0) %>%
                hc_xAxis(lineWidth = 1, tickWidth = 0) %>%
                hc_exporting(enabled = TRUE,
                    buttons = list(contextButton = list(
                        menuItems = c("downloadPNG", "downloadJPEG",
                            "downloadPDF", "downloadSVG")
                    ))) %>%
                hc_tooltip(
                    useHTML = T,
                    formatter = JS(
                        paste0("
                                function() {
                                    outHTML = '<b>Cell type</b>: ' +
                                    this.point.annt +
                                    '<br> ", input$gene_search, ": ' +
                                    this.point.gene.toFixed(2)
                                return(outHTML)
                    }
            ")
                    ))})
        output$gene_sc_cluster <- renderHighchart(
            dt %>% hchart('scatter', hcaes(x = tSNE_1, y = tSNE_2,
                group = annt)) %>%
                hc_xAxis(
                    plotLines = list(
                        list(
                            color = "#'FF0000",
                            width = 2,
                            value = 5.5))) %>%
                hc_yAxis(lineWidth = 1, gridLineWidth = 0) %>%
                hc_xAxis(lineWidth = 1, tickWidth = 0) %>%
                hc_exporting(enabled = TRUE, buttons = list(
                    contextButton = list(
                        menuItems = c("downloadPNG", "downloadJPEG",
                            "downloadPDF", "downloadSVG")
                    ))) %>%
                hc_tooltip(
                    useHTML = T,
                    formatter = JS(
                        paste0("
            function() {
                outHTML = '<b>Cell type</b>: ' + this.point.annt
                return(outHTML)
            }
            ")
                    )) %>%
                identity()
        )
        }
    }, ignoreNULL = T, ignoreInit = T)
    updateSelectizeInput(inputId = "gene_bulk_dataset",
        choices = exp_data$geo_accession)
    observeEvent(input$gene_sc_organ, {
        updateSelectizeInput(inputId = "gene_bulk_dataset",
            choices = exp_data %>%
                filter(organ == input$gene_sc_organ) %>%
                pull(geo_accession))
    })
    observeEvent(c(input$gene_search, input$gene_bulk_dataset), {
        if (input$gene_search != "") {
        con <- dbConnect(RMariaDB::MariaDB(), user = "root",
            password = "wbynb", host = "localhost",
            port = "3306")
        dbExecute(con, "USE PCA;")
        rs <- dbSendQuery(con,
            paste0("SELECT * FROM bulk WHERE gene='",
                input$gene_search, " ' AND experiment='",
                "GSE117606", "'"))
        ts <- dbFetch(rs)
        dbClearResult(rs)
        dbDisconnect(con)
        sample_info <- read.csv("www/data/bulk/GSE117606/sample_info.csv")

        df <- data.frame(
            stages = factor(sample_info$stage1),
            expr_data = ts$val)
        output$gene_bulk_box <- renderHighchart(
            highchart() %>%
                hc_xAxis(type = "category") %>%
                hc_add_series_list(data_to_boxplot(df,
                    expr_data, stages, stages, color = "black",
                    fillColor = ggsci::pal_npg("nrc",
                        alpha = 0.7)(length(unique(df$stages)))
                ))
        )}
    })
    # dt <- reactive(get(load(paste0("www/data/",
    #     geo_accessions[input$select_button], ".RData"))))
    # observeEvent(input$select_button, {
    #     tt <- sample_table %>%
    #         filter(experiment_id == input$select_button) %>%
    #         inner_join(lesion_table) %>%
    #         group_by(lesion_name) %>%
    #         summarise(n = n()) %>%
    #         arrange(n) %>%
    #         mutate(lesion_name = factor(lesion_name, levels = lesion_name))
    #     output$plot_pie_dataset <- renderPlot(ggplot(tt,
    #         aes(x = 1, y = n, fill=lesion_name)) +
    #             geom_col() +
    #             coord_polar(theta = "y") +
    #             labs(x = '', y = '', title = '') +
    #             theme(axis.text = element_blank()) +
    #             theme(panel.grid = element_blank()) +
    #             theme(panel.border = element_blank()) +
    #             theme(axis.ticks = element_blank()) +
    #             theme(panel.background = element_blank()) +
    #             theme(legend.position = "none") +
    #             geom_text(aes(y = sum(n) - (n/2 + c(0, cumsum(n)[-length(n)])),
    #                 x = 1.25,
    #                 label = str_wrap(paste0(lesion_name, "(", n, ")"), 20))))
    #     output$table_description <- my_table(data.frame(sapply(dt()$meta_data,
    #         function(x) {paste(x, collapse = " ")})))
    #     output$table_samples <- my_table(dt()$sample_info %>%
    #             dplyr::select(stage1:colnames(.)[1]) %>% rename(Group = stage1))
    #     updateSelectInput(inputId = "select_deg_lesion_group",
    #         choices = names(dt()$degs),
    #         selected = names(dt()$degs)[1])
    #     annotation_col <- dt()$sample_info %>%
    #         transmute(lesions = factor(stage1))
    #     ls_col <- hue_pal()(length(unique(annotation_col$lesions)))
    #     names(ls_col) <- unique(annotation_col$lesions)
    #     annotation_colors <- list(lesions = ls_col)
    #     output$plot_tme_heatmap <- renderPlot(
    #         (pheatmap::pheatmap(dt()$TME_score, scale = "row",
    #             annotation_col = annotation_col,
    #             annotation_colors = annotation_colors,
    #             annotation_names_col = T,
    #             cutree_rows = 2,
    #             border_color = NA,treeheight_col = 0, treeheight_row = 0,
    #             color = colorRampPalette(c("navy", "white", "firebrick3"))(50)
    #         )))
    #     output$datatable_tme <- my_table(round(dt()$TME_score, 3))
    # }, ignoreInit = T)
    #
    # observeEvent(input$select_deg_lesion_group, {
    #     updateSelectInput(inputId = "up_down_regulated",
    #         selected = "up-regulated")
    #     updateSelectInput(inputId = "enrich_term", selected = "GO")
    #     DEG <- dt()$degs[[input$select_deg_lesion_group]]$DEG
    #     output$plot_deg_volcano <- renderPlot(
    #         ggscatter(
    #             DEG,
    #             x = "logFC",
    #             y = "logP",
    #             color = "Group",
    #             palette = c("navy","gray","firebrick3"),
    #             # label = DEG$label,
    #             repel = T,
    #             ylab = "-log10(Padj)",
    #             size = 1) +
    #             theme_bw() +
    #             theme(element_line(size = 0),element_rect(size = 1)) +
    #             xlim(c(min(DEG$logFC) - 0.5, max(DEG$logFC) + 0.5)) +
    #             geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
    #             geom_vline(xintercept = c(-1, 1), linetype = "dashed")
    #     )
    #     pg <- (DEG %>%
    #             filter(Group != "non-significant") %>%
    #             arrange(desc(logFC)))
    #     dd <- dt()$expr_matrix[rownames(pg), ]
    #     annotation_row <- pg %>% transmute(Group = factor(Group))
    #     annotation_col <- dt()$sample_info %>%
    #         transmute(lesions = factor(stage1))
    #     gp_col <- c("firebrick3", "navy")
    #     names(gp_col) <- unique(pg$Group)
    #     ls_col <- hue_pal()(length(unique(annotation_col$lesions)))
    #     names(ls_col) <- unique(annotation_col$lesions)
    #     annotation_colors <- list(Group = gp_col, lesions = ls_col)
    #     output$plot_deg_heatmap <- renderPlot(
    #         (pheatmap::pheatmap(dd, scale = "row",
    #             annotation_row = annotation_row,
    #             annotation_col = annotation_col,
    #             annotation_colors = annotation_colors,
    #             show_rownames = F,
    #             show_colnames = F,
    #             annotation_names_col = T,
    #             annotation_names_row = T,
    #             cutree_rows = 2,
    #             gaps_row = 1,
    #             border_color = NA,treeheight_col = 0, treeheight_row = 0,
    #             color = colorRampPalette(c("navy", "white", "firebrick3"))(50)
    #         )))
    #     output$datatable_deg <- my_table(DEG %>%
    #             filter(adj.P.Val < 0.05) %>%
    #             transmute(logFC = round(logFC, 3),
    #                 AveExpr = round(AveExpr, 3),
    #                 P.Value = format(P.Value, digits = 3),
    #                 adj.P.Val = format(adj.P.Val, digits = 3),
    #                 logP = round(logP, 3)) %>%
    #             rownames_to_column("Gene") %>%
    #             mutate(Gene = paste0("<a id=\"", Gene,
    #                 "\" href=\"#\" class=\"action-button\"
    #                 onclick=\"location.href=&quot;./#!/gene&quot;;
    #                 Shiny.onInputChange(&quot;select_gene_button&quot;
    #                 ,  this.id)\">", Gene, "</a>")))
    # }, ignoreInit = T)
    #
    # observeEvent(c(input$up_down_regulated, input$enrich_term), {
    #     enrich_plot <- function(enrich_data, term) {
    #         # options(repr.plot.width = 12, repr.plot.height = 4)
    #         ggplot(enrich_data, aes(GeneRatio, Description,
    #             fill = logP)) +
    #             geom_bar(stat = "identity") +
    #             theme_bw() +
    #             theme(panel.background = element_blank(),
    #                 panel.grid = element_blank()) +
    #             theme(axis.title.y = element_blank()) +
    #             theme(axis.text = element_text(size = 10),
    #                 axis.title.x = element_blank()) +
    #             scale_fill_gradient(low = '#8B0000', high = 'red') +
    #             labs(title = paste0(term, " enrichment")) +
    #             theme(plot.title = element_text(size = 10, hjust = 0.5)) +
    #             # NoLegend() +
    #             NULL
    #     }
    #     ud = ifelse(input$up_down_regulated == "up-regulated", "enrich_up",
    #         "enrich_down")
    #     ko = paste0("e", tolower(input$enrich_term))
    #     rs <- dt()$degs[[input$select_deg_lesion_group]][[ud]][[ko]]@result %>%
    #         transmute(
    #             GeneRatio = sapply(.$GeneRatio,
    #                 function(x) {
    #                     round(eval(parse(text = x)),digits = 3)}),
    #             Description = str_wrap(Description, width = 20),
    #             logP = round(-log10(p.adjust), digits = 3)) %>%
    #         arrange(desc(GeneRatio), desc(logP)) %>%
    #         .[1:min(10, nrow(.)), ] %>%
    #         mutate(Description = factor(Description,
    #             levels = rev(Description)))
    #     try({output$plot_enrichment <- renderPlot(
    #         enrich_plot(rs, input$enrich_term))}, silent = T)
    # }, ignoreInit = T)
    # observeEvent(input$select_gene_button, {
    #     updateTextInput(inputId = "gene_serach_text",
    #         value = input$select_gene_button)
    # })
}

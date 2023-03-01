rm(list = ls())
suppressMessages({
    library(ggplotify)
    library(shiny)
    library(shiny.router)
    library(DT)
    library(tidyverse)
    library(scales)
    library(DBI)
    library(shinydashboard)
    library(enrichplot)
    library(stringr)
    library(ggpubr)
    library(jsTreeR)
    library(highcharter)
    library(pheatmap)
    library(plotly)
    library(shinyWidgets)
    library(ggthemes)
    library(ggplotify)
})


source("R/home_page.R")
source("R/browse_page.R")
source("R/dataset_page.R")
source("R/gene_page.R")

router <- make_router(
    route("/", home_page),
    route("browse", browse_page),
    route("gene", gene_page),
    route("dataset", dataset_page),
    route("dataset_sc", sc_page)
)



con <- dbConnect(RMariaDB::MariaDB(), user = "root",
    password = "wbynb", host = "localhost",
    port = "3306")
dbExecute(con, "USE PCA;")
cancer_table <- dbReadTable(con, "cancers")
cancers <- cancer_table$cancer_name
names(cancers) <- cancer_table$cancer_id
experiment_table <- dbReadTable(con, "experiments")
geo_accessions  <- experiment_table$geo_accession
names(geo_accessions) <- experiment_table$experiment_id
sample_table <- dbReadTable(con, "samples")
lesion_table <- dbReadTable(con, "lesions")
gene_table <- dbReadTable(con, "genes")
dbDisconnect(con)

shinyInput <- function(FUN, len, id, label, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(id[i], label[i], ...))
    }
    inputs
}

# exp_data <- experiment_table %>% transmute(
#     ID = shinyInput(actionLink,
#         nrow(experiment_table),
#         experiment_id,
#         label = experiment_name,
#         onclick = paste0("location.href=\"",
#             route_link("dataset"),"\";
#             Shiny.onInputChange(\"select_button\",  this.id)"),
#         # style = "color: #337ab7;
#         # background-color: #fff;
#         # border-color: #fff"
#     ),
#     `Cancer type` = cancers[cancer_id],
#     # Details = as.character(a(href = route_link(paste0("dataset/",
#     # experiment_name)), "Details")),
#     Description = paste0("Desc of ", geo_accession),
#     `Sample numbers` = table(sample_table$experiment_id)[experiment_id],
#     `Data type` = data_type,
#     Source = sprintf("<a
#         href=\"https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s\">GEO</a>",
        # geo_accession))

pml <- read.csv("www/pml.csv")

load("www/gene_info.RData")

exp_lesions <- read.csv("www/bulk.csv")
exp_data <- exp_lesions %>%
    select(-lesion_name) %>%
    distinct() %>%
    mutate(
        detail = paste0("<a id=\"", geo_accession,
            "\" href=\"#\" class=\"action-button\"
            onclick=\"location.href=&quot;./#!/dataset&quot;;&#10;
            Shiny.onInputChange(&quot;select_button&quot;,this.id)\">View</a>"),
        Description = paste0("Desc of ", geo_accession),
        `Data type` = data_type,
        Source = sprintf("<a
            href=\"https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s\">
            GEO</a>",
            geo_accession))

exp_data_sc <- read.csv("Www/sc.csv")

makeNodes <- function(leaves){
    dfs <- lapply(strsplit(leaves, "/"), function(s){
        item <-
            Reduce(function(a,b) paste0(a,"/",b),
                s[-1], s[1], accumulate = TRUE)
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
    f <- function(parent){
        i <- match(parent, dat$item)
        item <- dat$item[i]
        children <- dat$item[dat$parent == item]
        label <- tail(strsplit(item, "/")[[1]], 1)
        if (length(children)) {
            list(
                text = label,
                data = list(value = item),
                children = lapply(children, f)
            )
        }else{
            list(text = label, data = list(value = item))
        }
    }
    lapply(dat$item[dat$parent == "root"], f)
}


ppi <- read.table("D:/Data/public_processed/blab_ppi2016.txt", header = T)

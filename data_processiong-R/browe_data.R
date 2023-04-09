library(tidyverse)
library(stringr)
library(DT)


if (FALSE) {
    all_data <- list()
    for (i in 1:26) {
        all_data[[as.character(i)]] <-
            readxl::read_excel(str_c("data/S", i, ".xlsx"))
    }
}

all_data <- readRDS("data/all_data.rds")



url_html_generate <- function(text_vec, url_vec, external = T){
    text_list <- strsplit(text_vec, "|", fixed = TRUE)
    url_list <- strsplit(url_vec, "|", fixed = TRUE)
    html_links <- mapply(function(texts, urls) {
        links <- mapply(function(text, url) {
            if (is.na(text)) {return(NA)}
            if (external){
            return(paste0("<a href='", url, "' target='_blank'>", text, "</a>"))
            } else {
                    return(paste0("<a href='", url, "'>", text, "</a>"))
                }
        }, texts, urls)
        if (is.na(links)) {return(NA)}
        paste(links, collapse = ", ")
    }, text_list, url_list)
}


### S1
all_data[[1]]$`Disease ID (UMLS)` <- url_html_generate(
    all_data[[1]]$`Disease ID (UMLS)`,
    all_data[[1]]$`Disease ID (UMLS)（超链接）`
    )
all_data[[1]] %>%
    mutate(`Disease ID (UMLS)（超链接）` = NULL) %>%
    rename(Detail = `Detail（超链接到P3）`) -> all_data[[1]]
all_data[[1]]$Detail <- url_html_generate(
    "View",
    str_c("#!/browse/", all_data[[1]]$`Disease ID`), external = F
)



### S2
all_data[[2]] <- readxl::read_excel("data/S2.xlsx")
glimpse(all_data[[2]])
dt2 <- all_data[[2]] %>%
    mutate(
        `Accession ID` = url_html_generate(
            `Accession ID`,
            `Accession ID （超链接，不显示）`),
        `Detail (提供链接P4)` = NULL,
        `Accession ID （超链接，不显示）` = NULL,
        Detail = url_html_generate(
            "View",
            str_c("#!/browse/bulk?bulk_dataset=", `Project ID`), external = F
        )
    )
datatable(dt2, escape = F)
all_data[[2]] <- dt2



### S3
glimpse(all_data[[3]])
dt3 <- all_data[[3]] %>%
  mutate(
    `Accession ID` = url_html_generate(
      `Accession ID`,
      `Accession ID（超链接，不展示）`),
    `Detail（提供超链接）` = NULL,
    `Accession ID（超链接，不展示）` = NULL,
    Detail = url_html_generate(
      "View",
      str_c("#!/browse/sc?sc_dataset=", `Project ID`), external = F
    )
  )
datatable(dt3, escape = F)
all_data[[3]] <- dt3

### S4
dt4 <- all_data[[4]] %>%
    transmute(
        ID = `Disease ID`,
        `Disease name` = `Disease me`,
        `UMLS ID` = url_html_generate(`Disease ID (UMLS)`,
            `Disease ID (UMLS)（超链接，不显示）`),
        `MESH ID` = url_html_generate(
            `Disease ID (MESH)`,
            `Disease ID (MESH) 超链接（不显示）`
        ),
        `NCIterms ID` = url_html_generate(
            `Disease ID (NCIterms)`,
            `Disease ID (NCIterms) --超链接（不显示）`
        ),
        `Ontology terms ID` = url_html_generate(
            `Disease ID (Ontology_terms)`,
            `Disease ID (Ontology) --超链接（不显示）`
        ),
        `HDO ID` = url_html_generate(
            `Disease ID (HDO)`,
            `Disease ID (HDO)--提供超链接`
        ),
        `HPO ID` = url_html_generate(
            `Disease ID (HPO)`,
            `Disease ID (HPO)--提供超链接`
        ),
        `ICD10CM ID` = str_replace_all(Disease_ID_ICD10CM, "\\|", ", "),
        `Associated genes (DisGeNet)` = str_replace_all(
            `Associated genes (DisGeNet)`, "\\|", ", "),
    )

datatable(t(dt4), escape = F)
all_data[[4]] <- dt4

### S5
glimpse(all_data[[5]])
### S6
glimpse(all_data[[6]])
all_data[[6]] <- readxl::read_excel("data/S6.xlsx")
plot_data <- all_data[[6]] %>%
    rename(
        pvalue = `log10(FDR)`,
        log2FoldChange = `log2(FoldChange)`,
        up_down = Color.列表不显示.
    )
all_data[[6]] <- plot_data

### S7
glimpse(all_data[[7]])
all_data[[7]] %>%
  rename(
    expr_mean = `Mean Expression Value`,
  ) -> all_data[[7]]
all_data[[7]] %>%
  filter(`Project ID` == "CRC_bulk_2") %>%
  group_by(Gene, Organ, `Project ID`, Direction, FDR) %>%
  pivot_wider(names_from = `Disease Name`, values_from = expr_mean )

### S8


### S9
glimpse(all_data[[9]])



### S11
glimpse(all_data[[11]])
all_data[[11]] %>%
  rename(
    TSNE_1 = `Component 1`,
    TSNE_2 = `Component 2`,
    cell_id = `Cell ID`,
    lesion = `Disease Lesion`,
    cell_type = `Cell Type`
  ) %>%
  mutate(TSNE_1 = as.numeric(TSNE_1), TSNE_2 = as.numeric(TSNE_2)) -> all_data[[11]]

### S12
glimpse(all_data[[12]])
all_data[[12]] <- all_data[[12]] %>%
  rename(cell_type = `Cell Type`, expr_mean = `Mean Value`) %>%
  mutate(expr_mean = as.numeric(expr_mean))




### S14
glimpse(all_data[[14]])
all_data[[14]] <- all_data[[14]] %>%
  rename(cell_type = `Cell Type`, lesion = `Disease Lesion`,
    cell_count = `Cell Count`, cell_proportion = `Cell Proportion`,
    chi_square_test = `Chi Square Test Stdres`) %>%
  mutate(cell_count = as.numeric(cell_count),
    cell_proportion = as.numeric(cell_proportion),
    chi_square_test = as.numeric(chi_square_test))

### S15
glimpse(all_data[[15]])
all_data[[15]] <- all_data[[15]] %>%
  rename(lesion = `Disease Lesion`)
sc_tjc_dt <- all_data[[15]] %>%
  filter(`Project ID` == "BLCA_sc_1")
sc_tjc_dt %>%
  hchart(
    "scatter",
    hcaes(
      x = component1,
      y = component2,
      group = lesion
    )
  ) %>%
  hc_tooltip(
    useHTML = T,
    formatter = JS("
                function() {
                    outHTML = '<b>Pseudotime</b>: ' + this.point.value +
                    '<br> <b>Disease lesion</b>: ' + this.point.lesion
                    return(outHTML)
                }
                ")) %>%
  hc_plotOptions(
    scatter = list(
      marker = list(
        radius = 2.5
      )
    )
  ) %>%
  hc_xAxis(
    title = list(text = "Component 1")
  ) %>%
  hc_yAxis(
    title = list(text = "Component 2")
  )

saveRDS(all_data, "data/all_data.rds")




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
glimpse(all_data[[2]])
dtall_data[[2]] %>%
    mutate(
        `Accession ID` = url_html_generate(
            `Accession ID`,
            `Accession ID （超链接，不显示）`),
        `Detail (提供链接P4)` = NULL,
        Detail = url_html_generate(
            "View",
            str_c("#!/browse/", `Project ID`), external = F
        )
    )
### S3

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
saveRDS(all_data, "data/all_data.rds")



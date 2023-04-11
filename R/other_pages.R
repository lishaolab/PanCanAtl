suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(shinydashboard)
  library(shiny.router)
  library(shinyjs)
  library(stringr)
  library(tidyverse)
  library(jsTreeR)
  library(highcharter)
  library(DT)
})

download_page <- fluidPage(
  h3("Curated knowledge for premalignant lesions",
    style = "text-align: center;"),

  h3("Cell markers for premalignant lesions",
    style = "text-align: center;"),

)

document_page <- fluidPage(
  p("The PreAtlas database provides the comprehensive resource and interactive
    analysis platform for exploring the vast amount of bulk and single-cell
    transcriptomic data, as well as curated knowledges specially pertaining
    to diverse premalignant diseases.

    To date, PreAtlas version 1.0 contains the gene expression data from a total
    of 4,857 samples and 1,955,023 cells from high-quality curated 86 bulk and
    34 single-cell transcriptomic datasets, for 79 manually standardized
    premalignant disease entries for 19 cancer types across 15 body sites.

    Via the web interface, uses can browse, search, perform online basic
    bioinformatics analysis upon and download manually curated normalized data
    and distilled knowledges.
"),
br(),
h4("1. Homepage"),
p("Once users visit PreAtlas website, logo and a brief introduction of PreAtlas
    are shown, along with a glimpse of data status, including curated
    premalignant disease across multiple organs of human body, statistics of
    curated transcriptome data related to them, recent news about the database
    and hyperlinks towards related database as data source or reference in
    integration.
"),
br(),
h4("2. Browse"),
p("Users can browse the entry terms of PreAtlas by clicking the \"Browse\"
 button
    on the navigation bar in any page within the site. By selecting any of the
    three buttons at the top of the page, tables of curated bulk (Bulk
    datasets), scRNA-seq transcriptome datasets (Single-cell datasets) and
    disease term entries (Curated knowledges) are rendered correspondingly with
    limited attributes and links towards the source dataset or viewing details.
    Users can filter the browse dataset entries
    by applying checkboxes on the left panel in order to view entries related to
    premalignant lesions of certain interested organs only."),
p("Moreover, users are able to obtain detailed information through clicking the
    view links in tables of bulk, scRNA-seq transcriptome datasets and
    knowledges of disease entries."),
p("For each bulk dataset, in the detail page, we provide:"),
p("a)	Pie chart of sample counts overview of premalignant lesions;"),
p("b)	Volcano plot and downloadable table of differentially expressed genes of
    different stages versus normal control samples;"),
p("Notes: DEG: Differently Expressed Genes (Premalignant lesion vs Normal
    control;
    FDR < 0.05 & fold change > 1.5, student t-test)."),
p("c)	Dot plot heatmap and downloadable table of dynamic genes across
    premalignant lesions along the known tumorigenesis pathway;"),
p("Notes: Dynamic genes referred to genes whose expression show
    gradually increase (UP) or decrease (DOWN) trend along the evolution
    of premalignant diseases (FDR < 0.05, anova test)."),
p("d)	Dot plot heatmap and downloadable table of tumor microenvironment (TME)
    cell types deconvolution analysis.
    Notes: The deconvolution analysis was performed by the CIBERSORTx
    platform (https://cibersortx.stanford.edu/). TME, tumor microenvironment."),
p("For each scRNA-seq dataset, in the detail page, we provide:"),
p("a)	UMAP 2-D dimension reduction plot with both manually annotated cell
    clusters and disease lesions of all cells;"),
p("b)	Violin plot and downloadable table of putative cell markers analysis;"),
p("c)	Boxplot and downloadable table of cellular proportion analysis across
    disease lesions within the dataset;"),
p("d)	Trajectory plot and downloadable table of pseudo-tumorigenesis
    trajectory analysis;"),
p("Notes: The pseudo-tumorigenesis trajectory analysis was performed by the
    Monocle platform (http://cole-trapnell-lab.github.io/monocle-release/)
    and the putative cellular dynamic genes referred to those genes whose
    expression showed significant gradual pattern along the putative
    pseudo-tumorigenesis trajectory (pseudotime)."),
p("For each curated knowledge entry term of premalignant disease, we offer
    the disease ID mapped in various popular medical term database (HDO, HPO,
    MESH, ICD10CM, etc.), and solid reference for the very disease or lesion
    being a stage of premalignant lesion of certain cancer."),
br(),
h4("3. Search"),
p("By clicking the \"Search\" button on the navigation bar in any pages
    within PreAtlas. We provide three main search modules, including
    \"Search by gene\", \"Search by organ\" and \"Search by project\". Here,
    the \"Search by gene\" module is enabled to provide basic information,
    associated organs or premalignant diseases, associated cell types,
    associated drugs and network associations for any input gene symbol
    or Ensembl ID. The \"Seach by organ\" module is enable to provide curated
    related transcriptomic datasets, associated cellular components and their
    proportions, associated dynamic genes and associated therapeutic drugs
    for diverse premalignant disease of any input organ. The \"Search by
    project\"
    module provides precise investigation of database including transcriptome
    dataset, or all datasets of bulk or scRNA-seq."),
h4("4. Online analysis"),
p("Users can enter analysis page by clicking the \"Analysis\" button on the
    navigation bar on any page. Here, four interactive analytical and
    visualization modules haven been packaged in PreAtlas to in-depth
    investigate these datasets, including Gene Expression Analyze (GEA),
    Cellular Component Analyze (CCA), Dynamic Gene Analyze (DGA) and Multiple
    Network analyze (MNA). Here, the GEA module enable to dissect the gene-level
    distribution across diverse cell types and/or pathological lesions while
    the CCA Module enable to dissect the cellular-level distribution based
    on either cell type annotations within scRNA-seq data or deconvoluted
    TME cells within the bulk transcriptomic data. Of note, the DBA module
    could identify premalignant-related dynamic genes as potential cancer
    risk-screening and early-diagnosis biomarkers, which were defined as
    those showing gradually dysregulated (increase or decrease) expression
    patterns along the pathologically dynamic evolution or putative
    pseudo-tumorigenesis trajectories. Finally, the MNA module enable to
    perform the network analysis for systematically associating
    premalignant-related multiple information, including diseases,
    genes, cell types and drugs, allowing for uncovering the evolution
    and intervention mechanism underlying premalignant diseases
    from the holistic view."),
br(),
h4("5. Download"),
p("PreAtlas provides users with downloadable resources of
    curated knowledges of premalignant diseases, full results
    of DEG, DGA and putative cell markers for premalignant disease.")
)

contact_page <- fluidPage(
  h3("Contact information", style = "text-align: center;"),
  p("If you have any question or any suggestion/comment,
    please feel free to contact us via email."),
p("Prof. Shao Li (corresponding author): shaoli@mail.tsinghua.edu.cn,
    Dr. Peng Zhang: zpzx@mail.tsinghua.edu.cn
",
    style = "text-align:justify;color:black;background-color:lavender;
          padding:15px;border-radius:10px"),
    br(),
    h3("Address", style = "text-align: center;"),
    p("Institute for TCM-X, MOE Key Laboratory of Bioinformatics,
     Bioinformatics Division, BNRIST, Department of Automation,
      Tsinghua University, 100084 Beijing, China
", style = "text-align:justify;color:black;background-color:lavender;
          padding:15px;border-radius:10px")

)
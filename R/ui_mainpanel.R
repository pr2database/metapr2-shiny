  mainpanel <- function() {mainPanel(
  tabsetPanel(
    id = 'panel' ,
    
    tabPanel("About", column(8, includeMarkdown("README.md"))),
    
    tabPanel("Datasets",
             h4("Select and deselect datasets by clicking on the corresponding row."), 
             DT::DTOutput('table_datasets')
    ),
    tabPanel("Download",
             h4("Samples, datasets and taxa in a zip file"),
             p(),
             downloadButton('download_datasets', 'Download data'),
             p(),
             includeMarkdown("readme_download.md")
    ),
    tabPanel("Treemap",
             # htmlOutput("taxo_selected"),
             p("Number of reads have been normalized (not rarefield) to 100 with 3 decimals."), 
             shinycssloaders::withSpinner(uiOutput('treemap'))
    ),
    
    tabPanel("Map",
             h4("Map of the number of reads of the  selected taxon relative to the total number of eukaryotic reads."), 
             p("The color of the circles represent the dominant taxon at the next rank: for example if you select a division, the dominant class at each point will be shown. Crosses indicate samples for which the group was not found."), 
            p("You can select the taxonomic group as well as the type of samples on the left panel. Datasets can be selected in the tab 'Datasets'"),
             p(),
             htmlOutput("taxo_selected"),
             htmlOutput("sample_number"),
             p(),
             fluidRow(
               column(2, radioButtons(
                 inputId = "map_type",
                 label = "Map type",
                 choices = c("pie chart" = "pie", "dominant taxon" = "dominant"),
                 selected = "pie",
                 inline = TRUE,
                 width = NULL)
               ),column(1,
                      h4("Change scale")
               ),
               column(3, 
                      # sliderInput("pct_max", label ="% max", min = 0, max = 100, value = 100),
                      shinyWidgets::sliderTextInput("pct_max", label ="% max",  
                                                    choices = c(seq(from = 100, to = 10, by = -10),
                                                                seq(from = 9, to = 0, by = -1))  ,
                                                    selected = 100, grid = TRUE, post = " %",
                                                    width = "200%"
                      )
               )
             ),
             
             shinycssloaders::withSpinner(leafletOutput("map_1", width ="auto", height = 800))
    ), 
    tabPanel("Alpha diversity",
             # htmlOutput("taxo_selected"),
             p("Number of reads not rarefield or normalized."),
             p("BE PATIENT, plotting takes some time..."),
             checkboxGroupInput("alpha_method", "Diversity Measure", inline = TRUE,  
                                choices = c("Chao1", "Shannon", "Simpson", "Fisher"), 
                                selected = c("Chao1", "Shannon")),
             shinycssloaders::withSpinner(uiOutput('ps_alpha'))
    ),
    tabPanel("Beta diversity",
             # htmlOutput("taxo_selected"),
             p("Number of reads not rarefield or normalized."),
             p("BE PATIENT, plotting takes some time..."),
             radioButtons("beta_method", "Ordination method", inline = TRUE,
                          choices = c("NMDS", "CCA", "RDA", "MDS", "PCoA"),
                          selected = c("NMDS")),
             shinycssloaders::withSpinner(uiOutput('ps_beta'))
    ),
    
    tabPanel("Query",
             h4("BLAST-like searchfor  metabarcode similar to existing sequence."),
             p("Aligns query sequence to all metabarcodes and return those above a fixed threshold"),
             p(),
             
             sliderInput("pct_id_min", label ="% identity min", min = 98.0, max = 100.0, 
                         step = 0.1, value = 100, width = "300px"),
             
             textAreaInput("query", label = "Query - at least 300 bp", value = "", 
                           width = "100%", height = "100px",
                           cols = NULL, rows = NULL, 
                           placeholder = "GTAGTTGGATTTCTGTTGAGGACGGC...", resize = NULL),
             
             actionButton("button_match", "Search"),
             
             shinycssloaders::withSpinner(uiOutput('asv_filtered'))
    ),
    
    tabPanel("Debug",
             # textOutput("test1"),
             # textOutput("ps")
             # dataTableOutput("test3")
             # dataTableOutput("test4")
    )
  )
)

}
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
             shinycssloaders::withSpinner(uiOutput('treemap'))
    ),
    
    tabPanel("Map",
             h4("Map of the number of reads of the  selected taxon relative to the total number of eukaryotic reads."), 
             p("The color of the circles represent the dominant taxon at the next rank: for example if you select a division, the dominant class at each point will be shown. You can select the taxonomic group as well as the type of samples on the left panel. Datasets can be selected in the tab 'Datasets'"),
             p(),
             htmlOutput("taxo_selected"),
             htmlOutput("sample_number"),
             p(),
             fluidRow(
               column(2,
                      h4("Change scale")
               ),
               column(4, 
                      sliderInput("pct_max", label ="% max", min = 0, max = 100, value = 100)
               )
             ),
             
             shinycssloaders::withSpinner(leafletOutput("map_1", width ="auto", height = 900))
    ), 
    tabPanel("Query",
             h4("BLAST-like searchfor  metabarcode similar to existing sequence."),
             p("Aligns query sequence to all metabarcodes and return those above a fixed threshold"),
             p(),
             
             sliderInput("pct_id_min", label ="% identity min", min = 98.0, max = 100.0, step = 0.1, value = 100, width = "300px"),
             
             textAreaInput("query", label = "Query - at least 300 bp", value = "", width = "100%", height = "100px",
                           cols = NULL, rows = NULL, 
                           placeholder = "GTAGTTGGATTTCTGTTGAGGACGGCCGGTCCGCACTATGTGCGTGTATCTGGTTCGGCCTCGGCATCCTC...", resize = NULL),
             
             actionButton("button_match", "Search"),
             
             shinycssloaders::withSpinner(uiOutput('asv_filtered'))
    )
    
    # tabPanel("Debug",
    #          textOutput("test1"),
    #          textOutput("test2"),
    #          # dataTableOutput("test3"),
    #          # dataTableOutput("test4")
    # )
    
    # tabPanel("Amplification - one set", dataTableOutput("test"))
  )
)

}
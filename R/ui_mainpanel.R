  mainpanel <- function() {mainPanel(
  tabsetPanel(
    id = 'panel' ,
    
    tabPanel("About", column(8, includeMarkdown("README.md"))),
    
    tabPanel("Datasets",
             h4("Select and deselect datasets by clicking on the corresponding row."), 
             DT::DTOutput('table_datasets')
    ),
    tabPanel("Download",
             uiOutput('ui_download')
             
    ),
    tabPanel("Treemap",
             # htmlOutput("taxo_selected"),
             p("Number of reads have been normalized (not rarefield) to 100 with 3 decimals."), 
             shinycssloaders::withSpinner(uiOutput('treemap'))
    ),
    
    tabPanel("Map",
             uiOutput('ui_map'),
    ),    
    # tabPanel("Barplot",
    #          uiOutput('ui_barplot'),
    #          shinycssloaders::withSpinner(uiOutput('graph_barplot'))
    # ),
    
    tabPanel("Alpha diversity",
             uiOutput('ui_ps_alpha'),
             shinycssloaders::withSpinner(uiOutput('graph_ps_alpha'))
    ),
    tabPanel("Beta diversity",
             uiOutput('ui_ps_beta'),
             shinycssloaders::withSpinner(uiOutput('graph_ps_beta'))
    ),
    
    tabPanel("Query",
             uiOutput('ui_query'),
             # h4("BLAST-like searchfor  metabarcode similar to existing sequence."),
             # p("Aligns query sequence to all metabarcodes and return those above a fixed threshold"),
             # p(),
             # 
             # sliderInput("pct_id_min", label ="% identity min", min = 98.0, max = 100.0, 
             #             step = 0.1, value = 100, width = "300px"),
             # 
             # textAreaInput("query", label = "Query - at least 300 bp", value = "", 
             #               width = "100%", height = "100px",
             #               cols = NULL, rows = NULL, 
             #               placeholder = "GTAGTTGGATTTCTGTTGAGGACGGC...", resize = NULL),
             # 
             # actionButton("button_match", "Search"),
             
             shinycssloaders::withSpinner(uiOutput('ui_query_results'))
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
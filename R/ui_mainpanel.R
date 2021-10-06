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
    tabPanel("Barplot",
             uiOutput('ui_barplot'),
             shinycssloaders::withSpinner(uiOutput('graph_barplot'))
    ),
    
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
  mainpanel <- function() {mainPanel(
  tabsetPanel(
    id = 'panel' ,
    
    tabPanel("About", column(8, includeMarkdown(system.file("readme", 'about.md', package = "metapr2")))),
    
    tabPanel("Datasets",
             h4("Select and deselect datasets by clicking on the corresponding row."), 
             DT::DTOutput('table_datasets')
    ),
    tabPanel("Download",
             downloadUI("download")
             
    ),
    tabPanel("Treemap",
             treemapUI("treemap")
    ),
    
    tabPanel("Map",
             mapUI("map")
    ),    
    tabPanel("Barplot",
             barplotUI("barplot") 
    ),
    
    tabPanel("Alpha diversity",
             phyloseq_alpha_UI("phyloseq")
    ),
    tabPanel("Beta diversity",
             phyloseq_beta_UI("phyloseq")
    ),
    
    tabPanel("Query",
             queryUI("query")
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
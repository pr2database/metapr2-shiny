  mainpanel <- function() {mainPanel(
  tabsetPanel(
    id = 'panel' ,
    
    tabPanel("About",
             column(8, includeMarkdown(system.file("readme", 'about.md', package = "metapr2")))),

    tabPanel("Datasets",
             data_datasets_table_UI("data")
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
    tabPanel("Download",
             downloadUI("download")
             
    )
    
    # tabPanel("Debug",
    #          # textOutput("test1"),
    #          # textOutput("ps")
    #          # dataTableOutput("test3")
    #          # dataTableOutput("test4")
    # )
  )
)

}
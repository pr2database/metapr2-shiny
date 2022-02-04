# --- Side bar Panel
sidebar <- function() {sidebarPanel(width = 3,
                                    # The next line create a scrolling window for the sidebar panel
                                    # See: https://stackoverflow.com/questions/66582335/dynamically-sized-sidebarpanel-and-mainpanel-in-shiny
                                    style = "height: 100vh; overflow-y: auto;position:fixed;;width:22%;", 
                                    
                # conditionalPanel(
                #   condition ='input.panel == "Datasets"',
                #   data_datasets_UI("data")
                #  ),
              div(tags$img(src='img/metapr2_logo.png', width="80"), style="text-align: center;"),
              h4("The metaPR2 database", align = "center"),
              div(actionButton("button_help", "Help", class = "btn-info", 
                         onclick ="window.open('https://pr2database.github.io/metapr2-shiny/articles/', '_blank')"), 
                  style="text-align: center;"),
             
             conditionalPanel(
               condition = '
                 input.panel == "Datasets" ||
                 input.panel == "Treemap" ||
                 input.panel == "Barplot" ||
                 input.panel == "Diversity" ||
                 input.panel == "Map" ||
                 input.panel == "Query" ||
                 input.panel == "Download"',
               data_datasets_UI("data"),
               data_samples_UI("data")
             ),
             conditionalPanel(
               condition = '
                 input.panel == "Datasets" ||
                 input.panel == "Treemap" ||
                 input.panel == "Barplot" ||
                 input.panel == "Diversity" ||
                 input.panel == "Map" ||
                 input.panel == "Download"',
               data_reads_min_UI("data"),
               taxoUI("taxo")
             ),
             
             
             
)
  
}
  
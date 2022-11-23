# --- Side bar Panel
sidebar <- function(session) {sidebarPanel(width = 3,
                                    # The next line create a scrolling window for the sidebar panel
                                    # See: https://stackoverflow.com/questions/66582335/dynamically-sized-sidebarpanel-and-mainpanel-in-shiny
                                    style = "height: 100vh; overflow-y: auto;position:fixed;;width:20%;", 
                                    
                # conditionalPanel(
                #   condition ='input.panel == "Datasets"',
                #   data_datasets_UI("data")
                #  ),
              # div(tags$img(src='img/metapr2_logo.png', width="80"), style="text-align: center;"),
              div(tags$img(src="https://github.com/pr2database/metapr2-shiny/blob/main/inst/img/metapr2_logo.png?raw=true", 
                           width="80"),
                  p(),
                  actionButton("button_help", "Help", class = "btn-info", 
                         onclick ="window.open('https://pr2database.github.io/metapr2-shiny/articles/', '_blank')"),  
                  actionButton("button_disconnect", "Disconnect", class = "btn-info"),  
                  display_info_ui("info"),
                  style="text-align: center;"),
             
             conditionalPanel(
               condition = '
                 input.panel == "Datasets" ||
                 input.panel == "Taxonomy" ||
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
                 input.panel == "Taxonomy" ||
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
  
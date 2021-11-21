# --- Side bar Panel
sidebar <- function() {sidebarPanel(width = 3,
                                    style = "position:fixed;width:22%;",

                # conditionalPanel(
                #   condition ='input.panel == "Datasets"',
                #   data_datasets_UI("data")
                #  ),
              div(img(src='img/metapr2_logo.png', width="80"), style="text-align: center;"),
              h4("The metaPR2 database", align = "center"),
             
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
  
# --- Side bar Panel
sidebar <- function() {sidebarPanel(width = 3,

                conditionalPanel(
                  condition ='input.panel == "Datasets"',
                  data_datasets_UI("data")
                 ),
             
             conditionalPanel(
               condition = 'input.panel == "Treemap" ||
                 input.panel == "Barplot" ||
                 input.panel == "Alpha diversity" ||
                 input.panel == "Beta diversity" ||
                 input.panel == "Map" ||
                 input.panel == "Query" ||
                 input.panel == "Download"',
               data_samples_UI("data"),
               data_reads_min_UI("data"),
               taxoUI("taxo")
               
             )
             
             
)
  
}
  
# --- Side bar Panel
sidebar <- function() {sidebarPanel(width = 3,
                  conditionalPanel(
                    condition = 'input.panel == "Alpha diversity" ||
                                 input.panel == "Beta diversity"' ,
                    numericInput(
                      "ps_reads_min",
                      "Minimum number of total reads per ASV",
                      100,
                      min = 1,
                      max = 10000,
                      step = NA,
                      width = NULL
                      )
                    ),
                        
                  conditionalPanel(
                    condition ='input.panel == "Datasets"',
                
                  shinyWidgets::multiInput(
                    inputId = "datasets_selected_id",
                    label = h3("Select datasets"),
                    # choices = asv_set$datasets$dataset_id,
                    choiceNames = str_c(str_replace(asv_set$datasets$dataset_code, "_V4", ""), sep=" - "),
                    choiceValues = asv_set$datasets$dataset_id,
                    selected = asv_set$datasets$dataset_id,
                    options= list(
                      enable_search = TRUE,
                      search_placeholder ="Search...",
                      non_selected_header = "Available",
                      selected_header = "Selected ")
                )
                 ),
             
              conditionalPanel(
                condition = 'input.panel == "Datasets" ||
                 input.panel == "Treemap" ||
                 input.panel == "Barplot" ||
                 input.panel == "Alpha diversity" ||
                 input.panel == "Beta diversity" ||
                 input.panel == "Map" ||
                 input.panel == "Download"',
                strong("Datasets selected:"),
                textOutput("datasets_selected_id")
                ), 
              
              
              
             conditionalPanel(
               condition = 'input.panel == "Treemap" ||
                 input.panel == "Barplot" ||
                 input.panel == "Alpha diversity" ||
                 input.panel == "Beta diversity" ||
                 input.panel == "Map" ||
                 input.panel == "Download"',
               
               h3("Select Samples"),
               
               # checkboxGroupInput("gene_region", "Gene regions", inline = TRUE,  choices = global$gene_regions, selected = global$gene_regions),
               checkboxGroupInput("DNA_RNA", "DNA or RNA", inline = TRUE,  choices = global$DNA_RNAs, selected = "DNA"),
               checkboxGroupInput("substrate", "Substrates", inline = TRUE,  choices = global$substrates, selected = global$substrates),
               checkboxGroupInput("fraction_name", "Fractions", inline = TRUE,  choices = global$fraction_names, selected = global$fraction_names),
               checkboxGroupInput("depth_level", "Depth levels", inline = TRUE,  choices = global$depth_levels, selected = "surface"),
               
               taxoUI("taxo")
               
             )
             
             
)
  
}
  
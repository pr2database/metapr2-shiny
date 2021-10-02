# --- Side bar Panel
sidebar <- function() {sidebarPanel(width = 3,
                        
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
                 input.panel == "Alpha diversity" ||
                 input.panel == "Beta diversity" ||
                 input.panel == "Map" ||
                 input.panel == "Download"',
                strong("Datasets selected:"),
                textOutput("datasets_selected_id")
                ), 
              
              
              
             conditionalPanel(
               condition = 'input.panel == "Treemap" ||
                 input.panel == "Alpha diversity" ||
                 input.panel == "Beta diversity" ||
                 input.panel == "Map" ||
                 input.panel == "Download"',
               
               h3("Select Samples"),
               
               checkboxGroupInput("gene_region", "Gene regions", inline = TRUE,  choices = global$gene_regions, selected = global$gene_regions),
               checkboxGroupInput("DNA_RNA", "DNA or RNA", inline = TRUE,  choices = global$DNA_RNAs, selected = "DNA"),
               checkboxGroupInput("substrate", "Substrates", inline = TRUE,  choices = global$substrates, selected = global$substrates),
               checkboxGroupInput("fraction_name", "Fractions", inline = TRUE,  choices = global$fraction_names, selected = global$fraction_names),
               checkboxGroupInput("depth_level", "Depth levels", inline = TRUE,  choices = global$depth_levels, selected = "surface"),
               
               # actionButton("button_map_update", "Draw map"),
             #   
             # ),
             # 
             # # --- Dynamic boxes for taxonomy - See https://mastering-shiny.org/action-dynamic.html
             # conditionalPanel(
             #   'input.panel %in% c("Treemap", "Alpha diversity", "Beta diversity", "Map","Download")"', 
               
               h3("Select Taxa"),
               
               selectInput("supergroup", "Supergroup",
                           choices = c("All", unique(global$pr2_taxo$supergroup))),
               selectInput("division", "Division",
                           choices = "All"),
               selectInput("class", "Class",
                           choices = "All"),
               selectInput("order", "Order",
                           choices = "All"),
               selectInput("family", "Family",
                           choices = "All"),
               selectInput("genus", "Genus",
                           choices = "All"),
               selectInput("species", "Species",
                           choices = "All")
               
             )
             
             
)
  
}
  
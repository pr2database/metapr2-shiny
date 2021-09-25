# --- Side bar Panel
sidebar <- function() {sidebarPanel(width = 3,
                        
                  conditionalPanel(
                 'input.panel == "Datasets"',
                 
                 h3("Select datasets")
                 ),
             
              conditionalPanel(
                'input.panel == "Datasets"||
                 input.panel == "Map"||
                 input.panel == "Download"
                ',
                
                strong("Datasets selected:"),
                textOutput("datasets_id_selected")
                ),           
             conditionalPanel(
               'input.panel == "Map"||
                 input.panel == "Download"
                ',
               
               h3("Select Samples"),
               
               checkboxGroupInput("gene_region", "Gene regions", inline = TRUE,  choices = gene_regions, selected = gene_regions),
               checkboxGroupInput("DNA_RNA", "DNA or RNA", inline = TRUE,  choices = DNA_RNAs, selected = "DNA"),
               checkboxGroupInput("substrate", "Substrates", inline = TRUE,  choices = substrates, selected = substrates),
               checkboxGroupInput("fraction_name", "Fractions", inline = TRUE,  choices = fraction_names, selected = fraction_names),
               checkboxGroupInput("depth_level", "Depth levels", inline = TRUE,  choices = depth_levels, selected = "surface"),
               
               # actionButton("button_map_update", "Draw map"),
               
             ),
             
             # --- Dynamic boxes for taxonomy - See https://mastering-shiny.org/action-dynamic.html
             conditionalPanel(
               'input.panel == "Map" ||
                 input.panel == "Download"
                ', 
               
               h3("Select Taxa"),
               
               selectInput("supergroup", "Supergroup",
                           choices = unique(pr2_taxo$supergroup)),
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
  
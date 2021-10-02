
metapr2App <- function() {

# User interface ----------------------------------------------------------

ui <- fluidPage(
  
  # Script to close the windows after some inactivity
  
  tags$script(inactivity),  
  
  # include shinyFeedback

  shinyFeedback::useShinyFeedback(),
  
  # To include the favicon.ico
  tags$head(tags$link(rel="shortcut icon", href="www/favicon.ico")),
  
  # Title
  title = "MetaPR2",
  titlePanel(div(img(src='www/metapr2_logo.png', width="80"),"The MetaPR2 database")),
  
  
  # --- Side bar layout
  sidebarLayout( sidebar(),
                 mainpanel()
                 )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Stop the application of the session is closed (after 10 min) - Only for web application
  # session$onSessionEnded(stopApp)

  # Datasets - Reformat the datasets and creates output for download  
  
  source("R/server/server_datasets.R", local = TRUE)
  
  # Panel - Treemap
  
  source("R/server/server_treemap.R", local = TRUE)
  
  # Panel - Leaflet map
  
  source("R/server/server_map.R", local = TRUE)
  
  # Panels - Alpha and beta diversity
  
  source("R/server/server_phyloseq.R", local = TRUE)
  
  # Panel - Matching ASV
  
  source("R/server/server_match.R", local = TRUE)
  
  
  # Utils - Dynamic taxonomy boxes
  
  source("R/server/server_taxonomy_dynamic_boxes.R", local = TRUE)
  
}

# Run the shiny app -------------------------------------------------------


shinyApp(ui, server)

}

#' @export

metapr2App <- function() {

# User interface ----------------------------------------------------------

ui <- fluidPage(
  
  # Script to close the windows after some inactivity - ACTIVARE for web application
  # tags$script(inactivity),  
  
  # 2021-10-05 - Do not use ShinyFeedback (replaced by shinyvalidate)
  # shinyFeedback::useShinyFeedback(),
  


  # To include the favicon.ico
  tags$head(tags$link(rel="shortcut icon", href="img/favicon.ico")),
  # tags$head(tags$link(rel="shortcut icon", href=system.file("img", 'favicon.ico', package = "metapr2"))),
  
  
  # Title
  title = "MetaPR2",
  titlePanel(div(img(src='img/metapr2_logo.png', width="80"),"The MetaPR2 database")),
  # titlePanel(div(img(src=system.file("img", 'metapr2_logo.png', package = "metapr2"), width="80"),"The MetaPR2 database")),
  
  
  # --- Side bar layout
  sidebarLayout( sidebar(),
                 mainpanel()
                 )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Stop the application of the session is closed (after 10 min) - ACTIVATE  for web application
  session$onSessionEnded(stopApp)
  
  
  # Validate the sample selection
  # See: https://rstudio.github.io/shinyvalidate/articles/shinyvalidate.html
  
  # Datasets - Reformat the datasets and creates output for download  
  
  source("R/server/server_datasets.R", local = TRUE)
  
  # Panel - Download
  
  source("R/server/server_download.R", local = TRUE)
  
  # Panel - Treemap
  
  source("R/server/server_treemap.R", local = TRUE)
  
  # Panel - Leaflet map
  
  source("R/server/server_map.R", local = TRUE)
  
  # Panels - Barplot
  
  # source("R/server/server_barplot.R", local = TRUE)
  barplotServer("barplot", df_selected(), taxo())
  
  
  # Panels - Alpha and beta diversity
  
  # source("R/server/server_phyloseq.R", local = TRUE)
  phyloseqServer("phyloseq", ps_selected(), taxo())
  
  # Panel - Matching ASV
  
  source("R/server/server_query.R", local = TRUE)
  
  
  # Utils - Dynamic taxonomy boxes
  
  source("R/server/server_taxonomy_dynamic_boxes.R", local = TRUE)
  
}

# Run the shiny app -------------------------------------------------------


shinyApp(ui, server)

}

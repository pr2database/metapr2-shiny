#metapr2App -------------------------------------------------------
#' @title Launch metapr2 shiny app
#' @examples
#' # Starts shiny application
#'
#' \dontrun{
#'  metapr2::run_app()
#'  }
#'
#' @export

run_app <- function() {
  
# Build the whole dataset ---------------------------------------------------------
  
# Function for computing object sizes

  # obj_size <- function(x) {
  #   cat("Object:",deparse(substitute(x)), "- size: ", round(pryr::object_size(x)/10**6, 2), " Mb \n")
  # }

# Remove warnings: -1 remove, 0 save, 1 print
# https://docs.tibco.com/pub/enterprise-runtime-for-R/5.0.0/doc/html/Language_Reference/base/stop.html#:~:text=The%20warn%20option%20(see%20options,by%20calling%20the%20function%20warnings.
  
options(warn = - 1)

messages <- list()
messages$no_data = tags$div(
                      tags$h4(tags$b("No data for this taxon in selected samples:")),
                      # tags$br(),
                      tags$span(style="color:red","You may want to change minimum number of reads or select more samples")
                      )

shinymanager::set_labels(
  language = "en",
  "Please authenticate" = "Choose datasets ",
  "Username:" = "Datasets (Leave blank for public version):",
  "Password:" = "Password (Leave blank for public version):",
  "Login" = "Enter metaPR2"
)


# User interface ----------------------------------------------------------

ui <- fluidPage(
  
  # Booststrap theme:https://rstudio.github.io/shinythemes/
  # theme = bslib::bs_theme(bootswatch = "yeti"),
  
  # Tracking not necessary in ui
  # shinylogs::use_tracking(),
  
  # Script to close the windows after some inactivity - ACTIVATE for web application
  tags$script(inactivity),  
  
  # To activate shinyjs
  # shinyjs::useShinyjs(),
  

  # To include the favicon.ico
  tags$head(tags$link(rel="shortcut icon", href="img/favicon.ico")),
  # tags$head(tags$link(rel="shortcut icon", href=system.file("img", 'favicon.ico', package = "metapr2"))),
  
  # Authentification
  
  shinymanager::auth_ui(
    id = "auth",
    # add image on top ?
    tags_top = 
      tags$div(
        # tags$h1("metaPR2", style = "align:center"),
        # tags$img(src = "img/metapr2_logo.png", width = 80)
        tags$img(src = "https://github.com/pr2database/metapr2-shiny/blob/main/inst/img/metapr2_logo.png?raw=true", width = 80)
      ),
    # add information on bottom ?
    tags_bottom = tags$div(
      checkboxInput("asv_clustered", "Use clustered ASVs (see Help)", value = TRUE, width = NULL),
      tags$p("  "),
      tags$h4("metaPR2  version: 2.1.0"),
      tags$br(),
      tags$h4("Datasets version: 2.1"),
      tags$h5("Datasets #: 59 (identical to version 2.0)"),
      tags$h5("Assignment: PR2 version 5.0.0"),
      tags$br(),
      tags$p("No password needed. For other datasets, please  contact ",
        tags$a(href = "mailto:vaulot@gmail.com", target="_top", "Daniel Vaulot")
      )
    )
   ),

  # Message for disconnection
  
  shinydisconnect::disconnectMessage(
    text = "Server lost connection.",
    refresh = "Reload now"
  ),
  
  # Title
  title = "MetaPR2",
  # titlePanel(div(img(src='img/metapr2_logo.png', width="80"),"The MetaPR2 database")),
  # titlePanel(div(img(src=system.file("img", 'metapr2_logo.png', package = "metapr2"), width="80"),"The MetaPR2 database")),

  
  
  # --- Side bar layout
  sidebarLayout( sidebar(),
                 mainpanel()
                 )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Stop the application of the session is closed (after 30 min) - ACTIVATE  for web application
  session$onSessionEnded(stopApp)
  
  # To track usage
  shinylogs::track_usage(storage_mode = shinylogs::store_sqlite(path = "logs/"))
  
  # Authentification
  
  authentification <- callModule(module = shinymanager::auth_server,
                                 id = "auth",
                                 check_credentials = shinymanager::check_credentials(credentials))
  
  # Disconnection
  
  observeEvent(input$button_disconnect, {session$close() } )

  # Validate the sample selection
  # See: https://rstudio.github.io/shinyvalidate/articles/shinyvalidate.html
  
  # Datasets - Reformat the datasets and creates output for download  
  
    asv_set <- dataServer("data", taxo, authentification, input$asv_clustered)
    
  # Just print version and whether ASVs are clustered or
    
    display_info_server("info", authentification, input$asv_clustered)

  # Utils - Dynamic taxonomy boxes

    taxo <- taxoServer("taxo", asv_set$fasta_all)

  # Panel - Download

    downloadServer("download", asv_set$datasets_selected, asv_set$samples_selected, asv_set$df_selected, asv_set$fasta_selected, taxo, messages)

  # Panel - Treemap

    treemapServer("treemap", asv_set$df_selected, taxo, messages)

  # Panel - Leaflet map

    mapServer("map", asv_set$df_selected, asv_set$samples_selected, taxo)

  # Panels - Barplot

    barplotServer("barplot", asv_set$df_selected, asv_set$samples_selected, taxo, messages)


  # Panels - Alpha and beta diversity

    phyloseqServer("phyloseq", asv_set$samples_selected, asv_set$df_selected, asv_set$fasta_selected, taxo, messages)

  # Panel - Matching ASV

    queryServer("query", asv_set$samples_selected, asv_set$df_all, asv_set$fasta_all)
    
    # Panel - Taxonomy table
    
    taxo_table_Server("taxo_table", asv_set$fasta_selected)


    # cat("Server: ")
    # print(pryr::mem_used())
  
  # Debug
  # output$test1 <- renderText(getwd())
  # output$test2 <- renderText(nrow(asv_set$df))
  # output$test3 <- DT::renderDT(asv_set$df)
  
}

# Run the shiny app -------------------------------------------------------


shinyApp(ui, server)

}

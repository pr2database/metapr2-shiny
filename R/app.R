#metapr2App -------------------------------------------------------
#' @title Launch metapr2 shiny app
#' @examples
#' # Starts shiny application
#'
#' \dontrun{
#'  metapr2::metapr2App()
#'  }
#'
#' @export

metapr2App <- function() {
  
# Build the whole dataset ---------------------------------------------------------
  
# Function for computing object sizes

  # obj_size <- function(x) {
  #   cat("Object:",deparse(substitute(x)), "- size: ", round(pryr::object_size(x)/10**6, 2), " Mb \n")
  # }

# Remove warnings
  
options(warn=-1)

messages <- list()
messages$no_data = tags$div(
                      tags$h4(tags$b("No data for this taxon in selected samples:")),
                      # tags$br(),
                      tags$span(style="color:red","You may want to change minimum number of reads or select more samples")
                      )
shinymanager::set_labels(
  language = "en",
  "Please authenticate" = "Choose datasets",
  "Username:" = "Datasets (leave blank for public datasets):",
  "Password:" = "Password (leave blank for public datasets):",
  "Login" = "Enter metaPR2"
)

# User interface ----------------------------------------------------------

ui <- fluidPage(
  
  # Script to close the windows after some inactivity - ACTIVATE for web application
  tags$script(inactivity),  
  

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
        tags$img(src = "img/metapr2_logo.png", width = 80)
      ),
    # add information on bottom ?
    tags_bottom = tags$div(
      tags$h4("Datasets"),
      tags$h5("41 public datasets (V4 and V9), no password needed"),
      tags$p("For other datasets, please  contact ",
        tags$a(href = "mailto:vaulot@gmail.com", target="_top", "Daniel Vaulot")
      )
    )
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
  
  # Authentification
  
  authentification <- callModule(module = shinymanager::auth_server,
                                 id = "auth",
                                 check_credentials = shinymanager::check_credentials(credentials))
  
  
  # Validate the sample selection
  # See: https://rstudio.github.io/shinyvalidate/articles/shinyvalidate.html
  
  # Datasets - Reformat the datasets and creates output for download  
  
    r <- dataServer("data", taxo, authentification)

  # Panel - Download

    downloadServer("download", r$datasets_selected, r$samples_selected, r$df_selected, r$fasta_selected, taxo, messages)

  # Panel - Treemap

    treemapServer("treemap", r$df_selected, taxo, messages)

  # Panel - Leaflet map

    mapServer("map", r$df_selected, r$samples_selected, taxo)

  # Panels - Barplot

    barplotServer("barplot", r$df_selected, taxo, messages)


  # Panels - Alpha and beta diversity

   phyloseqServer("phyloseq", r$samples_selected, r$df_selected, r$fasta_selected, taxo, messages)

  # Panel - Matching ASV

    queryServer("query", r$samples_selected, r$df_all, r$fasta_all)


  # Utils - Dynamic taxonomy boxes

    taxo <- taxoServer("taxo")

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
